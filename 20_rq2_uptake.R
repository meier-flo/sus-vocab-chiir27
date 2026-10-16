# 20_rq2_uptake.R
# RQ2: does the sustainability language of a product description show up in the
# reviews of that product, and does it matter whether the claim is certified?
#
# Every product falls into one of three groups by what its description says:
#   certified   - names at least one certification
#   claim-only  - uses informal sustainability terms but no certification
#   no-claim    - neither
# We then join each product to its 2023 reviews and look at what language the
# reviews use. The no-claim group is the baseline: how often do reviewers bring
# up sustainability when the product page never raised it?
#
# We also run a matched test that holds the term fixed and varies only
# certification, which controls for the term itself being more or less common.
#
# Writes rq2_verification_groups.csv, rq2_verification_terms_by_category.csv
# and rq2_verification_terms_pooled.csv

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")

con <- db_connect()

year_filter <- paste0("timestamp IS NOT NULL AND year(epoch_ms(timestamp)) = ", analysis_year)

# Note: products are grouped on the raw certification column rather than
# certset(), so a product whose only tag is a dropped term still counts as
# certified. That affects 1,311 products and 639 reviews, which does not move
# any figure at the precision we report.

group_rows <- list()
term_rows  <- list()

for (i in seq_len(nrow(categories))) {
  category     <- categories$category[i]
  product_file <- file.path(data_dir, categories$products_file[i])
  review_file  <- file.path(data_dir, categories$reviews_file[i])

  message("[", i, "/", nrow(categories), "] ", category)

  # --- the three groups and what their reviews say ---
  group_rows[[category]] <- dbGetQuery(con, paste0(
    "WITH products AS (
       SELECT parent_asin,
              CASE WHEN certification IS NOT NULL THEN 'certified'
                   WHEN sustainable_intent IS NOT NULL THEN 'claim-only'
                   ELSE 'no-claim' END AS grp
       FROM read_parquet('", product_file, "')),
     reviews AS (
       SELECT parent_asin, certification, sustainable_intent
       FROM read_parquet('", review_file, "') WHERE ", year_filter, ")
     SELECT '", category, "' AS product_category, products.grp,
            count(DISTINCT products.parent_asin) AS n_products_with_reviews,
            count(*) AS n_reviews,
            count(*) FILTER (WHERE certification IS NOT NULL OR sustainable_intent IS NOT NULL) AS n_rev_any_sust,
            count(*) FILTER (WHERE certification IS NOT NULL) AS n_rev_cert,
            count(*) FILTER (WHERE sustainable_intent IS NOT NULL) AS n_rev_informal
     FROM products JOIN reviews USING (parent_asin)
     GROUP BY products.grp"))

  # --- matched test: same term on the description, with and without a cert ---
  term_rows[[category]] <- dbGetQuery(con, paste0(
    "WITH products AS (
       SELECT parent_asin, (certification IS NOT NULL) AS certified,
              informalset(sustainable_intent) AS terms
       FROM read_parquet('", product_file, "') WHERE sustainable_intent IS NOT NULL),
     product_terms AS (
       SELECT parent_asin, certified, unnest(terms) AS term FROM products),
     reviews AS (
       SELECT parent_asin, informalset(sustainable_intent) AS terms
       FROM read_parquet('", review_file, "') WHERE ", year_filter, ")
     SELECT '", category, "' AS product_category, product_terms.term, product_terms.certified,
            count(*) AS n_reviews,
            count(*) FILTER (WHERE reviews.terms IS NOT NULL
                             AND list_contains(reviews.terms, product_terms.term)) AS n_reviews_echo
     FROM product_terms JOIN reviews USING (parent_asin)
     GROUP BY product_terms.term, product_terms.certified"))
}

groups <- bind_rows(group_rows) %>%
  arrange(product_category, grp) %>%
  mutate(share_any_sust = n_rev_any_sust / n_reviews,
         share_cert     = n_rev_cert / n_reviews,
         share_informal = n_rev_informal / n_reviews)
write_csv(groups, file.path(data_dir, "rq2_verification_groups.csv"))

by_category <- bind_rows(term_rows) %>%
  arrange(product_category, term, certified)
write_csv(by_category, file.path(data_dir, "rq2_verification_terms_by_category.csv"))

# Pool the categories, then keep only terms with enough reviews on both sides
# for the comparison to mean anything.
pooled <- by_category %>%
  group_by(term, certified) %>%
  summarise(n_reviews = sum(n_reviews), n_echo = sum(n_reviews_echo), .groups = "drop") %>%
  mutate(echo_rate = n_echo / n_reviews) %>%
  pivot_wider(names_from = certified,
              values_from = c(n_reviews, n_echo, echo_rate),
              names_glue = "{.value}_{ifelse(certified, 'cert', 'nocert')}") %>%
  filter(!is.na(n_reviews_cert), !is.na(n_reviews_nocert),
         n_reviews_cert >= 1000, n_reviews_nocert >= 1000) %>%
  mutate(ratio = echo_rate_cert / echo_rate_nocert) %>%
  arrange(desc(n_reviews_nocert))
write_csv(pooled, file.path(data_dir, "rq2_verification_terms_pooled.csv"))

# Report -------------------------------------------------------------------
cat("\nreview-side sustainability language by product group\n")
groups %>%
  group_by(grp) %>%
  summarise(across(c(n_products_with_reviews, n_reviews, n_rev_any_sust, n_rev_cert, n_rev_informal), sum),
            .groups = "drop") %>%
  mutate(pct_any      = round(100 * n_rev_any_sust / n_reviews, 2),
         pct_cert     = round(100 * n_rev_cert / n_reviews, 3),
         pct_informal = round(100 * n_rev_informal / n_reviews, 2)) %>%
  select(grp, n_products_with_reviews, n_reviews, pct_any, pct_cert, pct_informal) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\nmatched test: same term, certified vs not (echo rate %)\n")
pooled %>%
  transmute(term,
            n_cert = n_reviews_cert, echo_cert = round(100 * echo_rate_cert, 2),
            n_nocert = n_reviews_nocert, echo_nocert = round(100 * echo_rate_nocert, 2),
            ratio = round(ratio, 2)) %>%
  as.data.frame() %>% print(row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
db_cleanup()
