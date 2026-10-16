# 02_process_reviews.R
# Turns the raw UCSD Amazon Reviews 2023 files into the tagged parquet files
# the analysis uses. One script for all six categories; the differences between
# them live in the table below rather than in six near-identical scripts.
#
# NOTE: this cannot be re-run as it stands. The raw .jsonl.gz downloads were
# deleted after processing to save disk space, so the parquet files in data/
# are the artefact of record. The script is kept so the pipeline is documented
# and can be repeated by anyone who downloads the sources again.
#
# What it does per category:
#   1. read the raw file(s), keep everything except the image column
#   2. keep reviews from 2015 onward
#   3. find the rows that could contain a lexicon term and tag those in R
#   4. write one parquet per category, zstd compressed

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")
source("/Users/fmeier/Documents/AmazonReviewsStudy/R/01_lexicon.R")

raw_dir <- file.path(base_dir, "raw")   # where the .jsonl.gz downloads went

# Per-category input files. Fashion and Health & Beauty were each published as
# two separate files, which we merge. Health & Beauty can then contain the same
# review twice, so it needs deduplicating.
review_sources <- list(
  Grocery_and_Gourmet_Food    = list(files = "Grocery_and_Gourmet_Food.jsonl.gz", dedup = FALSE),
  Home_and_Kitchen            = list(files = "Home_and_Kitchen.jsonl.gz",         dedup = FALSE),
  Health_Household_and_Beauty = list(files = c("Health_and_Household.jsonl.gz",
                                               "Beauty_and_Personal_Care.jsonl.gz"), dedup = TRUE),
  Electronics                 = list(files = "Electronics.jsonl.gz",              dedup = FALSE),
  Fashion_and_Clothing        = list(files = c("Amazon_Fashion.jsonl.gz",
                                               "Clothing_Shoes_and_Jewelry.jsonl.gz"), dedup = FALSE),
  Baby_Products               = list(files = "Baby_Products.jsonl.gz",            dedup = FALSE)
)

# Columns we keep. "images" is dropped: it is large and we never use it.
review_columns <- "{
  rating: 'DOUBLE', title: 'VARCHAR', text: 'VARCHAR', asin: 'VARCHAR',
  parent_asin: 'VARCHAR', user_id: 'VARCHAR', timestamp: 'BIGINT',
  helpful_vote: 'BIGINT', verified_purchase: 'BOOLEAN'
}"

# Some review text contains NUL bytes, which R cannot hold in a string, so we
# strip them everywhere rather than only where we first hit the problem.
clean <- function(column) paste0("replace(", column, ", chr(0), '')")

con <- db_connect()

for (category in names(review_sources)) {
  source_info <- review_sources[[category]]
  output_file <- file.path(data_dir, paste0(category, "_reviews.parquet"))
  message("=== ", category, " ===")

  if (file.exists(output_file)) {
    message("   already built, skipping")
    next
  }

  paths <- file.path(raw_dir, source_info$files)
  if (!all(file.exists(paths))) {
    message("   raw files missing, skipping (see note at the top of this script)")
    next
  }

  # 1-2. read, drop images, keep 2015 onward -------------------------------
  # The filter uses the raw epoch milliseconds rather than a date function,
  # because converting to a date needs an extension that is not always loaded.
  file_list <- paste0("['", paste(paths, collapse = "','"), "']")
  intermediate <- file.path(data_dir, paste0("_tmp_", category, "_raw.parquet"))

  message("   reading raw files ...")
  dbExecute(con, paste0(
    "COPY (
       SELECT * FROM read_json(", file_list, ",
         format = 'newline_delimited', columns = ", review_columns, ")
       WHERE timestamp >= ", start_2015_ms, "
     ) TO '", intermediate, "' (FORMAT PARQUET, COMPRESSION ZSTD);"))

  if (source_info$dedup) {
    message("   removing duplicate reviews ...")
    deduped <- file.path(data_dir, paste0("_tmp_", category, "_dedup.parquet"))
    dbExecute(con, paste0(
      "COPY (
         SELECT DISTINCT ON (user_id, parent_asin, timestamp) *
         FROM read_parquet('", intermediate, "')
       ) TO '", deduped, "' (FORMAT PARQUET, COMPRESSION ZSTD);"))
    file.remove(intermediate)
    intermediate <- deduped
  }

  # 3. tag the rows that could match --------------------------------------
  message("   finding candidate rows ...")
  candidates <- dbGetQuery(con, paste0(
    "SELECT rowid AS id, ", clean("coalesce(title,'')"), " AS title, ",
            clean("coalesce(text,'')"), " AS text
     FROM read_parquet('", intermediate, "')
     WHERE regexp_matches(lower(coalesce(title,'') || ' ' || coalesce(text,'')), ?)"),
    params = list(union_pattern))
  message("   ", format(nrow(candidates), big.mark = ","), " candidates to tag")

  tags <- tag_reviews_parallel(paste(candidates$title, candidates$text))
  tags$id <- candidates$id
  duckdb_register(con, "tags", tags)

  # 4. write the final file ------------------------------------------------
  message("   writing ", basename(output_file), " ...")
  dbExecute(con, paste0(
    "COPY (
       SELECT r.rating, ", clean("r.title"), " AS title, ", clean("r.text"), " AS text,
              r.asin, r.parent_asin, r.user_id, r.timestamp,
              r.helpful_vote, r.verified_purchase,
              tags.certification, tags.sustainable_intent
       FROM read_parquet('", intermediate, "') r
       LEFT JOIN tags ON tags.id = r.rowid
     ) TO '", output_file, "' (FORMAT PARQUET, COMPRESSION ZSTD);"))

  duckdb_unregister(con, "tags")
  file.remove(intermediate)
  message("   done")
}

dbDisconnect(con, shutdown = TRUE)
db_cleanup()
