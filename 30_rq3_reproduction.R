# 30_rq3_reproduction.R
# RQ3: when a product description names a certification, do the reviews of that
# product name it too? And if not, what do reviewers say instead?
#
# This replaces three separate scripts and produces five files:
#   rq2_reproduction_by_category.csv     per-category summary
#   rq2_substitute_language_by_category.csv  what gets said instead
#   rq2_panelA_cert_reproduction.csv     per-certification reproduction (Panel A)
#   rq2_panelB_cert_informal_lift.csv    certification x informal term (Panel B)
#   rq2_overall_cooccurrence.csv         the same, pooled over categories
#
# Throughout, a review "reproduces" a certification only if it names the same
# certification the product carries, not just any certification.

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")
source("/Users/fmeier/Documents/AmazonReviewsStudy/R/01_lexicon.R")

con <- db_connect()

# The informal terms after merging variants, used as the column set for the
# co-occurrence grids so every certification has a complete row.
informal_vocab <- unique(clean_informal_labels(names(informal_terms)))

# Cut-offs for the co-occurrence grids
min_group_reviews   <- 50L   # ignore a (category, certification) group below this
min_cell_percat     <- 5L    # per-category cell below this is unreliable
min_cell_pooled     <- 20L   # pooled cells can afford a higher bar
smoothing_alpha     <- 0.5   # Jeffreys smoothing, so lift is always finite

year_filter <- paste0("timestamp IS NOT NULL AND year(epoch_ms(timestamp)) = ", analysis_year)

product_path <- function(i) file.path(data_dir, categories$products_file[i])
review_path  <- function(i) file.path(data_dir, categories$reviews_file[i])

# Reviews of certified products, with flags for what the review says ---------
certified_join <- function(i) {
  paste0(
    "WITH certified_products AS (
       SELECT parent_asin, certset(certification) AS product_certs
       FROM read_parquet('", product_path(i), "') WHERE certification IS NOT NULL),
     reviews AS (
       SELECT parent_asin, certification AS review_certs, sustainable_intent AS review_terms
       FROM read_parquet('", review_path(i), "') WHERE ", year_filter, "),
     joined AS (
       SELECT certified_products.parent_asin,
              (review_certs IS NOT NULL) AS any_cert,
              (review_certs IS NOT NULL
               AND len(list_intersect(product_certs, certset(review_certs))) > 0) AS same_cert,
              (review_terms IS NOT NULL) AS has_informal,
              review_terms
       FROM certified_products JOIN reviews USING (parent_asin))")
}


# 1. Per-category summary --------------------------------------------------
message("per-category reproduction ...")
summary_rows <- list()
substitute_rows <- list()

for (i in seq_len(nrow(categories))) {
  category <- categories$category[i]
  message("   ", category)

  n_cert_products <- dbGetQuery(con, paste0(
    "SELECT count(*) AS n FROM read_parquet('", product_path(i), "')
     WHERE certification IS NOT NULL"))$n

  summary_rows[[category]] <- dbGetQuery(con, paste0(certified_join(i), "
    SELECT '", category, "' AS product_category,
           ", n_cert_products, " AS n_cert_products,
           count(DISTINCT parent_asin) AS n_cert_products_with_2023_reviews,
           count(*) AS n_2023_reviews_on_cert_products,
           count(*) FILTER (WHERE same_cert) AS n_reproduce_same_cert,
           count(*) FILTER (WHERE any_cert) AS n_any_cert_mention,
           count(*) FILTER (WHERE NOT any_cert AND has_informal) AS n_informal_only,
           count(*) FILTER (WHERE NOT any_cert AND NOT has_informal) AS n_neither,
           count(DISTINCT parent_asin) FILTER (WHERE same_cert) AS n_cert_products_with_reproducing_review
    FROM joined"))

  # What reviewers say when they do not reproduce the certification.
  n_nonreproducing <- dbGetQuery(con, paste0(certified_join(i),
    " SELECT count(*) AS n FROM joined WHERE NOT same_cert"))$n

  terms <- dbGetQuery(con, paste0(certified_join(i), "
    SELECT term, count(*) AS n_reviews_with_term
    FROM (SELECT unnest(informalset(review_terms)) AS term
          FROM joined WHERE NOT same_cert AND review_terms IS NOT NULL)
    GROUP BY term"))
  terms$product_category <- category
  terms$n_nonreproducing_reviews <- n_nonreproducing
  substitute_rows[[category]] <- terms
}

per_category <- bind_rows(summary_rows) %>%
  mutate(share_reproduce_same_cert = round(n_reproduce_same_cert / n_2023_reviews_on_cert_products, 6),
         share_any_cert_mention    = round(n_any_cert_mention / n_2023_reviews_on_cert_products, 6),
         share_informal_only       = round(n_informal_only / n_2023_reviews_on_cert_products, 6),
         share_neither             = round(n_neither / n_2023_reviews_on_cert_products, 6),
         share_cert_products_reproduced = round(n_cert_products_with_reproducing_review /
                                                n_cert_products_with_2023_reviews, 6)) %>%
  select(product_category, n_cert_products, n_cert_products_with_2023_reviews,
         n_2023_reviews_on_cert_products,
         n_reproduce_same_cert, share_reproduce_same_cert,
         n_any_cert_mention, share_any_cert_mention,
         n_informal_only, share_informal_only,
         n_neither, share_neither,
         n_cert_products_with_reproducing_review, share_cert_products_reproduced)
write_csv(per_category, file.path(data_dir, "rq2_reproduction_by_category.csv"))

substitutes <- bind_rows(substitute_rows) %>%
  mutate(share_of_nonreproducing = round(n_reviews_with_term / n_nonreproducing_reviews, 6)) %>%
  select(product_category, term, n_reviews_with_term, n_nonreproducing_reviews, share_of_nonreproducing) %>%
  arrange(product_category, desc(n_reviews_with_term))
write_csv(substitutes, file.path(data_dir, "rq2_substitute_language_by_category.csv"))


# 2. Panel A: reproduction per certification -------------------------------
message("Panel A: per-certification reproduction ...")
panel_a_rows <- list()

for (i in seq_len(nrow(categories))) {
  message("   ", categories$category[i])

  products_per_cert <- dbGetQuery(con, paste0(
    "SELECT certification, count(DISTINCT parent_asin) AS n_products_with_cert
     FROM (SELECT DISTINCT parent_asin, unnest(certset(certification)) AS certification
           FROM read_parquet('", product_path(i), "') WHERE certification IS NOT NULL)
     GROUP BY certification"))

  reproduction <- dbGetQuery(con, paste0(
    "WITH product_certs AS (
       SELECT DISTINCT parent_asin, unnest(certset(certification)) AS cert
       FROM read_parquet('", product_path(i), "') WHERE certification IS NOT NULL),
     reviews AS (
       SELECT parent_asin, certset(certification) AS review_certs
       FROM read_parquet('", review_path(i), "') WHERE ", year_filter, ")
     SELECT product_certs.cert AS certification,
            count(DISTINCT product_certs.parent_asin) AS n_products_with_2023_reviews,
            count(*) AS n_2023_reviews,
            count(*) FILTER (WHERE review_certs IS NOT NULL
                             AND list_contains(review_certs, product_certs.cert)) AS n_reviews_reproducing
     FROM product_certs JOIN reviews USING (parent_asin)
     GROUP BY product_certs.cert"))

  panel_a_rows[[i]] <- full_join(products_per_cert, reproduction, by = "certification")
}

panel_a <- bind_rows(panel_a_rows) %>%
  group_by(certification) %>%
  summarise(across(c(n_products_with_cert, n_products_with_2023_reviews,
                     n_2023_reviews, n_reviews_reproducing), ~ sum(., na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(reproduction_rate = ifelse(n_2023_reviews > 0,
                                    round(n_reviews_reproducing / n_2023_reviews, 6), NA_real_)) %>%
  arrange(desc(reproduction_rate))
write_csv(panel_a, file.path(data_dir, "rq2_panelA_cert_reproduction.csv"))


# 3. Panel B: which informal terms accompany each certification ------------
# Compares reviews of certified products against reviews of uncertified ones in
# the same category, so a term that is simply common in that category does not
# look like an effect of the certification. Only reviews that do NOT name a
# certification are used, so we are looking at substitute language.
message("Panel B: certification x informal term ...")

cooccurrence_join <- function(i) {
  paste0(
    "WITH reviews AS (
       SELECT parent_asin, informalset(sustainable_intent) AS review_terms
       FROM read_parquet('", review_path(i), "')
       WHERE ", year_filter, " AND certification IS NULL),
     products AS (
       SELECT parent_asin, (certification IS NOT NULL) AS has_cert,
              CASE WHEN certification IS NOT NULL THEN certset(certification) END AS product_certs
       FROM read_parquet('", product_path(i), "')),
     joined AS (
       SELECT products.has_cert, products.product_certs, reviews.review_terms
       FROM reviews JOIN products USING (parent_asin))")
}

panel_b_rows <- list()

for (i in seq_len(nrow(categories))) {
  category <- categories$category[i]
  message("   ", category)
  join_sql <- cooccurrence_join(i)

  group_sizes <- dbGetQuery(con, paste0(join_sql, "
    SELECT cert, count(*) AS n_target
    FROM (SELECT unnest(product_certs) AS cert FROM joined WHERE has_cert) GROUP BY cert"))

  group_terms <- dbGetQuery(con, paste0(join_sql, "
    SELECT cert, term, count(*) AS n_term
    FROM (SELECT unnest(product_certs) AS cert, unnest(review_terms) AS term
          FROM joined WHERE has_cert AND review_terms IS NOT NULL)
    GROUP BY cert, term"))

  n_baseline <- dbGetQuery(con, paste0(join_sql,
    " SELECT count(*) AS n FROM joined WHERE NOT has_cert"))$n

  baseline_terms <- dbGetQuery(con, paste0(join_sql, "
    SELECT term, count(*) AS n_term_base
    FROM (SELECT unnest(review_terms) AS term
          FROM joined WHERE NOT has_cert AND review_terms IS NOT NULL)
    GROUP BY term"))

  keep_certs <- group_sizes$cert[group_sizes$n_target >= min_group_reviews]
  if (length(keep_certs) == 0 || is.null(n_baseline) || n_baseline == 0) next

  panel_b_rows[[i]] <- crossing(certification = keep_certs, term = informal_vocab) %>%
    left_join(group_terms, by = c("certification" = "cert", "term")) %>%
    left_join(group_sizes, by = c("certification" = "cert")) %>%
    left_join(baseline_terms, by = "term") %>%
    mutate(
      n_term      = replace_na(n_term, 0L),
      n_term_base = replace_na(n_term_base, 0L),
      rate_cert_group = n_term / n_target,
      rate_baseline   = n_term_base / n_baseline,
      lift = ifelse(rate_baseline > 0, rate_cert_group / rate_baseline, NA_real_),
      # Smoothed so the baseline is never zero and lift is always defined.
      lift_smoothed = ((n_term + smoothing_alpha) / (n_target + 2 * smoothing_alpha)) /
                      ((n_term_base + smoothing_alpha) / (n_baseline + 2 * smoothing_alpha)),
      log2_lift_smoothed = log2(lift_smoothed),
      expected_n_term = n_target * (n_term_base / n_baseline),
      excess_pp = (rate_cert_group - rate_baseline) * 100,
      low_support = n_term < min_cell_percat,
      product_category = category,
      n_reviews_baseline = n_baseline
    ) %>%
    transmute(product_category, certification, term,
              n_reviews_cert_group = n_target, n_term_cert_group = n_term,
              expected_n_term = round(expected_n_term, 3),
              rate_cert_group = round(rate_cert_group, 6),
              n_reviews_baseline, n_term_baseline = n_term_base,
              rate_baseline = round(rate_baseline, 6),
              excess_pp = round(excess_pp, 4),
              lift = round(lift, 4),
              lift_smoothed = round(lift_smoothed, 4),
              log2_lift_smoothed = round(log2_lift_smoothed, 4),
              low_support)
}

panel_b <- bind_rows(panel_b_rows)
write_csv(panel_b, file.path(data_dir, "rq2_panelB_cert_informal_lift.csv"))


# 4. The same co-occurrence, pooled across categories ----------------------
message("pooled co-occurrence ...")
denominator_rows <- list()
numerator_rows   <- list()

for (i in seq_len(nrow(categories))) {
  message("   ", categories$category[i])
  join_sql <- paste0(
    "WITH reviews AS (
       SELECT parent_asin, informalset(sustainable_intent) AS review_terms
       FROM read_parquet('", review_path(i), "')
       WHERE ", year_filter, " AND certification IS NULL),
     products AS (
       SELECT parent_asin, certset(certification) AS product_certs
       FROM read_parquet('", product_path(i), "') WHERE certification IS NOT NULL),
     joined AS (
       SELECT products.product_certs, reviews.review_terms
       FROM reviews JOIN products USING (parent_asin))")

  denominator_rows[[i]] <- dbGetQuery(con, paste0(join_sql, "
    SELECT cert, count(*) AS n_reviews
    FROM (SELECT unnest(product_certs) AS cert FROM joined) GROUP BY cert"))

  numerator_rows[[i]] <- dbGetQuery(con, paste0(join_sql, "
    SELECT cert, term, count(*) AS n_term
    FROM (SELECT unnest(product_certs) AS cert, unnest(review_terms) AS term
          FROM joined WHERE review_terms IS NOT NULL)
    GROUP BY cert, term"))
}

denominators <- bind_rows(denominator_rows) %>%
  group_by(certification = cert) %>%
  summarise(n_reviews = sum(n_reviews), .groups = "drop")

numerators <- bind_rows(numerator_rows) %>%
  group_by(certification = cert, term) %>%
  summarise(n_term = sum(n_term), .groups = "drop")

pooled <- crossing(certification = denominators$certification, term = informal_vocab) %>%
  left_join(numerators, by = c("certification", "term")) %>%
  left_join(denominators, by = "certification") %>%
  mutate(n_term = replace_na(n_term, 0L),
         rate = round(n_term / n_reviews, 6),
         low_support = n_term < min_cell_pooled) %>%
  arrange(certification, desc(rate))
write_csv(pooled, file.path(data_dir, "rq2_overall_cooccurrence.csv"))


# Report -------------------------------------------------------------------
cat(sprintf("\npooled reproduction: %.3f%% (%s of %s reviews)\n",
            100 * sum(per_category$n_reproduce_same_cert) / sum(per_category$n_2023_reviews_on_cert_products),
            format(sum(per_category$n_reproduce_same_cert), big.mark = ","),
            format(sum(per_category$n_2023_reviews_on_cert_products), big.mark = ",")))

cat("\nPanel A, certifications with at least 100 reviews\n")
panel_a %>% filter(n_2023_reviews >= 100) %>%
  select(certification, n_2023_reviews, n_reviews_reproducing, reproduction_rate) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat(sprintf("\nPanel A rows: %d | Panel B rows: %d | pooled grid: %d cells\n",
            nrow(panel_a), nrow(panel_b), nrow(pooled)))

dbDisconnect(con, shutdown = TRUE)
db_cleanup()
