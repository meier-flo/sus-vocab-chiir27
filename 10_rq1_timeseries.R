# 10_rq1_timeseries.R
# RQ1: how sustainability language in reviews develops from 2015 to 2023.
#
# Builds a per-category, per-year table of review counts and how many of those
# reviews mention a certification or an informal sustainability term, then adds
# confidence intervals for the plot.
#
# Products are counted against the year of their FIRST review across the whole
# corpus, which is the closest we can get to when a product entered the market
# (product descriptions carry no date of their own).
#
# Writes data/category_year_summary.csv and data/rq1_review_timeseries.csv

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")

con <- db_connect()

# When did each product first get reviewed? --------------------------------
# A temporary table, because every category needs to look products up in it.
message("working out each product's first review year ...")
dbExecute(con, paste0(
  "CREATE TEMP TABLE first_year AS
     SELECT parent_asin, year(epoch_ms(min(timestamp))) AS year
     FROM read_parquet('", data_dir, "/*_reviews.parquet')
     WHERE parent_asin IS NOT NULL
     GROUP BY parent_asin;"))

product_rows <- list()
review_rows  <- list()

for (i in seq_len(nrow(categories))) {
  category <- categories$category[i]
  message("[", i, "/", nrow(categories), "] ", category)

  product_rows[[category]] <- dbGetQuery(con, paste0(
    "SELECT '", category, "' AS product_category, first_year.year AS year,
            count(*) AS n_products,
            count(*) FILTER (WHERE p.certification IS NOT NULL) AS n_certification_mention,
            count(*) FILTER (WHERE p.sustainable_intent IS NOT NULL) AS n_sustainability_mention
     FROM read_parquet('", data_dir, "/", categories$products_file[i], "') p
     JOIN first_year USING (parent_asin)
     GROUP BY first_year.year"))

  review_rows[[category]] <- dbGetQuery(con, paste0(
    "SELECT '", category, "' AS product_category, year(epoch_ms(timestamp)) AS year,
            count(*) AS n_reviews,
            count(*) FILTER (WHERE certification IS NOT NULL) AS n_review_certification_mention,
            count(*) FILTER (WHERE sustainable_intent IS NOT NULL) AS n_review_sustainability_mention
     FROM read_parquet('", data_dir, "/", categories$reviews_file[i], "')
     WHERE timestamp IS NOT NULL
     GROUP BY year"))
}

products <- bind_rows(product_rows)
reviews  <- bind_rows(review_rows)

summary <- full_join(products, reviews, by = c("product_category", "year")) %>%
  mutate(across(c(n_products, n_certification_mention, n_sustainability_mention,
                  n_reviews, n_review_certification_mention, n_review_sustainability_mention),
                ~ replace_na(., 0L))) %>%
  mutate(
    share_certification_mention  = ifelse(n_products > 0, n_certification_mention / n_products, NA_real_),
    share_sustainability_mention = ifelse(n_products > 0, n_sustainability_mention / n_products, NA_real_),
    share_review_certification_mention  = ifelse(n_reviews > 0, n_review_certification_mention / n_reviews, NA_real_),
    share_review_sustainability_mention = ifelse(n_reviews > 0, n_review_sustainability_mention / n_reviews, NA_real_)
  ) %>%
  transmute(year, product_category,
            n_products, n_certification_mention, n_sustainability_mention,
            share_certification_mention  = round(share_certification_mention, 6),
            share_sustainability_mention = round(share_sustainability_mention, 6),
            n_reviews, n_review_certification_mention, n_review_sustainability_mention,
            share_review_certification_mention  = round(share_review_certification_mention, 6),
            share_review_sustainability_mention = round(share_review_sustainability_mention, 6)) %>%
  arrange(product_category, year)

write_csv(summary, file.path(data_dir, "category_year_summary.csv"))

# The plotting table: reviews only, with intervals -------------------------
# The product columns are dropped here because the first-review-year proxy is
# too indirect to plot alongside the review series.
timeseries <- summary %>%
  select(year, product_category, n_reviews,
         n_review_certification_mention, share_review_certification_mention,
         n_review_sustainability_mention, share_review_sustainability_mention)

cert_ci <- t(mapply(wilson, timeseries$n_review_certification_mention, timeseries$n_reviews))
sust_ci <- t(mapply(wilson, timeseries$n_review_sustainability_mention, timeseries$n_reviews))

timeseries <- timeseries %>%
  mutate(ci_lo_review_certification  = round(cert_ci[, "lo"], 6),
         ci_hi_review_certification  = round(cert_ci[, "hi"], 6),
         ci_lo_review_sustainability = round(sust_ci[, "lo"], 6),
         ci_hi_review_sustainability = round(sust_ci[, "hi"], 6)) %>%
  select(year, product_category, n_reviews,
         n_review_certification_mention, share_review_certification_mention,
         ci_lo_review_certification, ci_hi_review_certification,
         n_review_sustainability_mention, share_review_sustainability_mention,
         ci_lo_review_sustainability, ci_hi_review_sustainability)

write_csv(timeseries, file.path(data_dir, "rq1_review_timeseries.csv"))

cat(sprintf("\n%d rows -> category_year_summary.csv and rq1_review_timeseries.csv\n", nrow(summary)))

pooled <- summary %>%
  group_by(year) %>%
  summarise(n = sum(n_reviews),
            informal = sum(n_review_sustainability_mention),
            cert = sum(n_review_certification_mention), .groups = "drop") %>%
  mutate(pct_informal = round(100 * informal / n, 3),
         pct_cert = round(100 * cert / n, 4))
cat("\npooled across categories\n")
as.data.frame(pooled) %>% print(row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
db_cleanup()
