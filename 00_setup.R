# 00_setup.R
# Shared settings used by every other script. Source this first.
#
# Everything here used to be copy-pasted into each script, which made it easy
# for the six category names or the DuckDB settings to drift apart.

library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)
library(readr)
library(tidyr)
library(stringi)

base_dir <- "/Users/fmeier/Documents/AmazonReviewsStudy"
data_dir <- file.path(base_dir, "data")

# The six product categories. Fashion and Health & Beauty were each built from
# two source files, which is why they have more than one entry in `sources`.
categories <- data.frame(
  category = c("Grocery_and_Gourmet_Food", "Home_and_Kitchen",
               "Health_Household_and_Beauty", "Electronics",
               "Fashion_and_Clothing", "Baby_Products"),
  stringsAsFactors = FALSE
)
categories$reviews_file  <- paste0(categories$category, "_reviews.parquet")
categories$products_file <- paste0(categories$category, "_products.parquet")

# Short labels for tables in the paper
categories$label <- c("Grocery and Gourmet Food", "Home and Kitchen",
                      "Health, Household and Beauty", "Electronics",
                      "Fashion and Clothing", "Baby")

# 2023 is the year the product descriptions were crawled, so most analyses that
# link reviews to descriptions use 2023 reviews only.
analysis_year <- 2023

# Review timestamps are stored as epoch milliseconds. This is 2015-01-01 UTC,
# the start of our analysis window.
start_2015_ms <- 1420070400000


# Open a DuckDB connection with the settings we need -----------------------
# The parquet files are far larger than memory, so we keep DuckDB's temp files
# on the project disk and cap the threads. preserve_insertion_order=false lets
# DuckDB stream results instead of buffering them.
db_connect <- function() {
  con <- dbConnect(duckdb(), dbdir = ":memory:")
  dbExecute(con, paste0("PRAGMA temp_directory='", data_dir, "/_duckdb_tmp';"))
  dbExecute(con, "SET preserve_insertion_order=false;")
  dbExecute(con, "SET threads=4;")
  add_lexicon_macros(con)
  con
}

# Delete DuckDB's scratch directory once a script is done.
db_cleanup <- function() {
  unlink(file.path(data_dir, "_duckdb_tmp"), recursive = TRUE)
}


# Label clean-up rules -----------------------------------------------------
# Some schemes were tagged under two names (an abbreviation and the full name).
# These macros map them onto one label so we do not count the same scheme twice.
# STANDARD 100 is dropped: it is the Oeko-Tex sub-standard, so Oeko-Tex already
# covers it, and on its own it matches text like "Standard 100% cotton".
add_lexicon_macros <- function(con) {
  dbExecute(con, "CREATE OR REPLACE MACRO normcert(x) AS CASE x
    WHEN 'Global Organic Textile Standard' THEN 'GOTS'
    WHEN 'B Corporation' THEN 'B Corp'
    WHEN 'GREENGUARD Gold' THEN 'GREENGUARD'
    WHEN 'Fair Trade Certified' THEN 'Fairtrade'
    WHEN 'Forest Stewardship Council' THEN 'FSC'
    ELSE x END;")

  dbExecute(con, "CREATE OR REPLACE MACRO norminformal(x) AS CASE x
    WHEN 'recyclable' THEN 'recycle'
    WHEN 'recycled' THEN 'recycle'
    WHEN 'refillable' THEN 'refill'
    WHEN 'sustainably' THEN 'sustainable'
    WHEN 'sustainably-sourced' THEN 'sustainable'
    ELSE x END;")

  # Turn a ';'-separated certification string into a clean list of labels.
  dbExecute(con, "CREATE OR REPLACE MACRO certset(s) AS
    list_distinct(list_transform(
      list_filter(string_split(s, ';'), x -> x <> 'STANDARD 100'),
      x -> normcert(x)));")

  # Same for the informal terms.
  dbExecute(con, "CREATE OR REPLACE MACRO informalset(s) AS
    list_distinct(list_transform(string_split(s, ';'), x -> norminformal(x)));")

  invisible(con)
}

# The same clean-up in R, for the search queries which we tag in R rather than
# in the database.
clean_cert_labels <- function(labels) {
  labels[labels == "Global Organic Textile Standard"] <- "GOTS"
  labels[labels == "B Corporation"] <- "B Corp"
  labels[labels == "GREENGUARD Gold"] <- "GREENGUARD"
  labels[labels == "Fair Trade Certified"] <- "Fairtrade"
  labels[labels == "Forest Stewardship Council"] <- "FSC"
  labels <- labels[labels != "STANDARD 100"]
  unique(labels)
}

clean_informal_labels <- function(labels) {
  labels[labels %in% c("recyclable", "recycled")] <- "recycle"
  labels[labels == "refillable"] <- "refill"
  labels[labels %in% c("sustainably", "sustainably-sourced")] <- "sustainable"
  unique(labels)
}


# Helpers ------------------------------------------------------------------
# Wilson score interval for a proportion. We use this instead of the simple
# normal approximation because some of our rates are very close to zero.
wilson <- function(x, n, z = 1.96) {
  if (n == 0) return(c(lo = NA_real_, hi = NA_real_))
  p <- x / n
  centre <- p + z^2 / (2 * n)
  spread <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  c(lo = (centre - spread) / (1 + z^2 / n),
    hi = (centre + spread) / (1 + z^2 / n))
}

# Point dbplyr at a parquet file so we can use dplyr verbs on it.
parquet_tbl <- function(con, file) {
  path <- file.path(data_dir, file)
  tbl(con, sql(paste0("SELECT * FROM read_parquet('", path, "')")))
}

# All review files or all product files at once (DuckDB accepts a glob).
all_reviews_tbl <- function(con) {
  tbl(con, sql(paste0("SELECT * FROM read_parquet('", data_dir, "/*_reviews.parquet')")))
}

all_products_tbl <- function(con) {
  tbl(con, sql(paste0("SELECT * FROM read_parquet('", data_dir, "/*_products.parquet')")))
}
