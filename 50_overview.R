# 50_overview.R
# The dataset table for the paper: how many reviews, products and reviewers per
# category, the time span, average review length and the share of verified
# purchases. Also a one-line summary of the query corpus.
#
# Replaces three separate scripts that each re-read the same parquet files.
#
# Writes overview_reviews.csv, overview_products.csv, overview_search.csv,
# overview_reviews_periods.csv and overview_reviews_final.csv

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")

con <- db_connect()

review_rows  <- list()
product_rows <- list()

for (i in seq_len(nrow(categories))) {
  category <- categories$category[i]
  message("[", i, "/", nrow(categories), "] ", category)

  review_rows[[category]] <- dbGetQuery(con, paste0(
    "SELECT '", category, "' AS category,
            count(*) AS n_reviews,
            min(year(epoch_ms(timestamp))) AS first_year,
            max(year(epoch_ms(timestamp))) AS last_year,
            strftime(epoch_ms(min(timestamp)), '%Y-%m') AS first_month,
            strftime(epoch_ms(max(timestamp)), '%Y-%m') AS last_month,
            count(DISTINCT parent_asin) AS n_products,
            count(DISTINCT user_id) AS n_users,
            count(*) FILTER (WHERE certification IS NOT NULL) AS n_cert,
            count(*) FILTER (WHERE sustainable_intent IS NOT NULL) AS n_informal,
            avg(len(regexp_split_to_array(trim(coalesce(text,'')), '\\s+'))) AS avg_len_words,
            avg(CASE WHEN verified_purchase THEN 1.0 ELSE 0 END) AS pct_verified
     FROM read_parquet('", data_dir, "/", categories$reviews_file[i], "')
     WHERE timestamp IS NOT NULL"))

  product_rows[[category]] <- dbGetQuery(con, paste0(
    "SELECT '", category, "' AS category, count(*) AS n_products,
            count(*) FILTER (WHERE certification IS NOT NULL) AS n_cert,
            count(*) FILTER (WHERE sustainable_intent IS NOT NULL) AS n_informal
     FROM read_parquet('", data_dir, "/", categories$products_file[i], "')"))
}

per_category <- bind_rows(review_rows) %>% arrange(category)

# The totals row ------------------------------------------------------------
# Reviews and mentions simply add up. Products do too, because a product only
# ever belongs to one category - we checked, the per-category counts sum
# exactly to the global distinct count. Users do NOT: the same person reviews
# in several categories, so that one needs its own pass over everything, and we
# use an approximate count because an exact one does not fit in memory.
message("totals across all categories ...")
all_reviews <- dbGetQuery(con, paste0(
  "SELECT count(*) AS n_reviews,
          min(year(epoch_ms(timestamp))) AS first_year,
          max(year(epoch_ms(timestamp))) AS last_year,
          strftime(epoch_ms(min(timestamp)), '%Y-%m') AS first_month,
          strftime(epoch_ms(max(timestamp)), '%Y-%m') AS last_month,
          approx_count_distinct(user_id) AS n_users,
          count(*) FILTER (WHERE certification IS NOT NULL) AS n_cert,
          count(*) FILTER (WHERE sustainable_intent IS NOT NULL) AS n_informal,
          avg(len(regexp_split_to_array(trim(coalesce(text,'')), '\\s+'))) AS avg_len_words,
          avg(CASE WHEN verified_purchase THEN 1.0 ELSE 0 END) AS pct_verified
   FROM read_parquet('", data_dir, "/*_reviews.parquet') WHERE timestamp IS NOT NULL"))
all_reviews$category   <- "ALL (users approx.)"
all_reviews$n_products <- sum(per_category$n_products)

overview <- bind_rows(per_category, all_reviews) %>%
  mutate(pct_cert     = round(100 * n_cert / n_reviews, 3),
         pct_informal = round(100 * n_informal / n_reviews, 2))

write_csv(overview %>% select(category, n_reviews, first_year, last_year, n_products, n_users,
                              n_cert, n_informal, pct_cert, pct_informal),
          file.path(data_dir, "overview_reviews.csv"))

# Products ------------------------------------------------------------------
products <- bind_rows(product_rows) %>%
  arrange(category) %>%
  mutate(pct_cert = round(100 * n_cert / n_products, 2),
         pct_informal = round(100 * n_informal / n_products, 2))
product_totals <- products %>%
  summarise(category = "ALL", n_products = sum(n_products),
            n_cert = sum(n_cert), n_informal = sum(n_informal)) %>%
  mutate(pct_cert = round(100 * n_cert / n_products, 2),
         pct_informal = round(100 * n_informal / n_products, 2))
write_csv(bind_rows(products, product_totals), file.path(data_dir, "overview_products.csv"))

# Time span -----------------------------------------------------------------
write_csv(overview %>% transmute(category, n_reviews, n_products, n_users,
                                 period = paste(first_month, "\u2013", last_month)),
          file.path(data_dir, "overview_reviews_periods.csv"))

# The table that goes in the paper ------------------------------------------
write_csv(overview %>% transmute(category, n_reviews, n_products, n_users,
                                 avg_reviews_per_product = round(n_reviews / n_products, 1),
                                 avg_len_words = round(avg_len_words, 1),
                                 pct_verified = round(100 * pct_verified, 1)),
          file.path(data_dir, "overview_reviews_final.csv"))

# The query corpus, for the same table --------------------------------------
stage_totals <- read_csv(file.path(data_dir, "funnel_stage_totals.csv"), show_col_types = FALSE)
search <- stage_totals %>% filter(stage == "search")
write_csv(data.frame(corpus = "AmazonQAC (US, Sep 2023)",
                     n_distinct_queries = search$n_total,
                     n_sustainability_queries = search$n_any,
                     pct_sustainability = round(100 * search$n_any / search$n_total, 2)),
          file.path(data_dir, "overview_search.csv"))

cat("\ndataset overview\n")
read_csv(file.path(data_dir, "overview_reviews_final.csv"), show_col_types = FALSE) %>%
  as.data.frame() %>% print(row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
db_cleanup()
