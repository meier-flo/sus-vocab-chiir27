# run_all.R
# Runs the whole analysis in the right order.
#
# The order is not arbitrary. Two dependencies are easy to get wrong:
#   - 41_rq4_uncertainty.R reads Panel A, so it has to come AFTER
#     30_rq3_reproduction.R, not before it
#   - 42_rq4_tables.R reads the stability file to decide which typology labels
#     get a dagger, so it has to come AFTER 41_rq4_uncertainty.R
#
# The data preparation steps (02-04) are commented out: they need the raw
# .jsonl.gz downloads, which were deleted after processing. The parquet files
# in data/ are the artefact of record.
#
# Rough runtimes on a laptop: the funnel step takes a few minutes because it
# reads the 42M-row query log; the RQ3 step is the slowest as it scans every
# parquet several times. Allow around an hour for the whole thing.

base_dir <- "/Users/fmeier/Documents/AmazonReviewsStudy"

scripts <- c(
  # --- data preparation (needs the raw downloads) ---
  # "R/02_process_reviews.R",
  # "R/04_build_to_keep.R",
  # "R/03_process_products.R",

  # --- RQ1: sustainability language in reviews over time ---
  "R/10_rq1_timeseries.R",
  "R/11_rq1_decompose.R",
  "R/12_rq1_durable.R",

  # --- RQ2: does description language reach the reviews? ---
  "R/20_rq2_uptake.R",

  # --- RQ3: is the certification itself reproduced? ---
  "R/30_rq3_reproduction.R",

  # --- RQ4: the three-stage funnel ---
  "R/40_rq4_funnel.R",
  "R/41_rq4_uncertainty.R",   # after RQ3, before the tables
  "R/42_rq4_tables.R",

  # --- dataset table and figures ---
  "R/50_overview.R",          # reads the funnel totals for the query corpus row
)

for (script in scripts) {
  message("\n==================== ", script, " ====================")
  started <- Sys.time()
  source(file.path(base_dir, script), echo = FALSE)
  message("   finished in ", round(difftime(Sys.time(), started, units = "mins"), 1), " min")
}

message("\nall done")
message("the lexicon precision check is run separately, in two steps:")
