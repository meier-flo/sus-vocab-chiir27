# 04_build_to_keep.R
# The list of products we actually need metadata for: every parent_asin that
# appears in at least one review. Product metadata has no date, so this is how
# we keep the product corpus aligned with the review corpus.
#
# Run after 02_process_reviews.R and before 03_process_products.R.

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")

con <- db_connect()

dbExecute(con, paste0(
  "COPY (
     SELECT DISTINCT parent_asin
     FROM read_parquet('", data_dir, "/*_reviews.parquet')
     WHERE parent_asin IS NOT NULL AND parent_asin <> ''
   ) TO '", file.path(data_dir, "to_keep.csv"), "' (HEADER, DELIMITER ',');"))

n_products <- dbGetQuery(con, paste0(
  "SELECT count(*) AS n FROM read_csv_auto('", file.path(data_dir, "to_keep.csv"), "')"))$n
cat(format(n_products, big.mark = ","), "products written to to_keep.csv\n")

dbDisconnect(con, shutdown = TRUE)
db_cleanup()
