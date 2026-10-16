# 03_process_products.R
# The same idea for the product metadata files (meta_*.jsonl.gz).
#
# NOTE: like 02_process_reviews.R, this cannot be re-run without downloading
# the raw files again; see the note there.
#
# Two differences from the review pipeline:
#   - product records have no timestamp, so instead of a date filter we keep
#     only the products that actually appear in our reviews (to_keep.csv)
#   - the text we tag is title + features + description + details combined

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")
source("/Users/fmeier/Documents/AmazonReviewsStudy/R/01_lexicon.R")

raw_dir <- file.path(base_dir, "raw")

product_sources <- list(
  Grocery_and_Gourmet_Food    = list(files = "meta_Grocery_and_Gourmet_Food.jsonl.gz", dedup = FALSE),
  Home_and_Kitchen            = list(files = "meta_Home_and_Kitchen.jsonl.gz",         dedup = FALSE),
  Health_Household_and_Beauty = list(files = c("meta_Health_and_Household.jsonl.gz",
                                               "meta_Beauty_and_Personal_Care.jsonl.gz"), dedup = TRUE),
  Electronics                 = list(files = "meta_Electronics.jsonl.gz",              dedup = FALSE),
  Fashion_and_Clothing        = list(files = c("meta_Amazon_Fashion.jsonl.gz",
                                               "meta_Clothing_Shoes_and_Jewelry.jsonl.gz"), dedup = TRUE),
  Baby_Products               = list(files = "meta_Baby_Products.jsonl.gz",            dedup = FALSE)
)

# "images", "videos" and "store" are dropped: large and unused.
product_columns <- "{
  main_category: 'VARCHAR', title: 'VARCHAR', average_rating: 'DOUBLE',
  rating_number: 'BIGINT', features: 'VARCHAR[]', description: 'VARCHAR[]',
  price: 'VARCHAR', categories: 'VARCHAR[]', details: 'JSON',
  parent_asin: 'VARCHAR', bought_together: 'VARCHAR'
}"

# The text we tag, with NUL bytes stripped.
combined_text <- "replace(
     coalesce(title,'') || ' ' ||
     coalesce(array_to_string(features, ' '),'') || ' ' ||
     coalesce(array_to_string(description, ' '),'') || ' ' ||
     coalesce(CAST(details AS VARCHAR),''), chr(0), '')"

con <- db_connect()

keep_file <- file.path(data_dir, "to_keep.csv")
if (!file.exists(keep_file)) stop("run 04_build_to_keep.R first")

for (category in names(product_sources)) {
  source_info <- product_sources[[category]]
  output_file <- file.path(data_dir, paste0(category, "_products.parquet"))
  message("=== ", category, " ===")

  if (file.exists(output_file)) { message("   already built, skipping"); next }

  paths <- file.path(raw_dir, source_info$files)
  if (!all(file.exists(paths))) {
    message("   raw files missing, skipping (see note at the top of this script)")
    next
  }

  file_list <- paste0("['", paste(paths, collapse = "','"), "']")
  intermediate <- file.path(data_dir, paste0("_tmp_", category, "_meta.parquet"))

  message("   reading metadata and filtering to reviewed products ...")
  dbExecute(con, paste0(
    "COPY (
       SELECT m.* FROM read_json(", file_list, ",
         format = 'newline_delimited', columns = ", product_columns, ") m
       JOIN (SELECT DISTINCT parent_asin FROM read_csv_auto('", keep_file, "')) k
         ON m.parent_asin = k.parent_asin
     ) TO '", intermediate, "' (FORMAT PARQUET, COMPRESSION ZSTD);"))

  if (source_info$dedup) {
    # The same product can appear in both source files. Keep the copy with the
    # most ratings. arg_max is a plain aggregate, which fits in memory; the
    # equivalent window function ran out of memory at around 12 GB.
    message("   removing duplicate products ...")
    deduped <- file.path(data_dir, paste0("_tmp_", category, "_dedup.parquet"))
    dbExecute(con, paste0(
      "CREATE OR REPLACE TEMP TABLE keep_ids AS
         SELECT arg_max(rowid, coalesce(rating_number, -1)) AS id
         FROM read_parquet('", intermediate, "') GROUP BY parent_asin;"))
    dbExecute(con, paste0(
      "COPY (
         SELECT p.* FROM read_parquet('", intermediate, "') p
         JOIN keep_ids ON keep_ids.id = p.rowid
       ) TO '", deduped, "' (FORMAT PARQUET, COMPRESSION ZSTD);"))
    file.remove(intermediate)
    intermediate <- deduped
  }

  message("   finding candidate rows ...")
  candidates <- dbGetQuery(con, paste0(
    "SELECT rowid AS id, ", combined_text, " AS combined
     FROM read_parquet('", intermediate, "')
     WHERE regexp_matches(lower(", combined_text, "), ?)"),
    params = list(union_pattern))
  message("   ", format(nrow(candidates), big.mark = ","), " candidates to tag")

  tags <- tag_reviews_parallel(candidates$combined)
  tags$id <- candidates$id
  duckdb_register(con, "tags", tags)

  message("   writing ", basename(output_file), " ...")
  dbExecute(con, paste0(
    "COPY (
       SELECT p.main_category, replace(p.title, chr(0), '') AS title,
              p.average_rating, p.rating_number,
              list_transform(p.features,    x -> replace(x, chr(0), '')) AS features,
              list_transform(p.description, x -> replace(x, chr(0), '')) AS description,
              p.price, p.categories, p.details, p.parent_asin, p.bought_together,
              tags.certification, tags.sustainable_intent
       FROM read_parquet('", intermediate, "') p
       LEFT JOIN tags ON tags.id = p.rowid
     ) TO '", output_file, "' (FORMAT PARQUET, COMPRESSION ZSTD);"))

  duckdb_unregister(con, "tags")
  file.remove(intermediate)
  message("   done")
}

dbDisconnect(con, shutdown = TRUE)
db_cleanup()
