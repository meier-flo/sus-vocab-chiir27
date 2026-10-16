# Labels Without Language

Code for *Labels Without Language: Tracing Sustainability Vocabulary Across
Queries, Product Pages, and Reviews* (CHIIR).

The study measures how sustainability vocabulary — both formal certification
schemes and everyday informal terms — is distributed across three stages of an
online marketplace: the queries people type, the descriptions sellers write,
and the reviews consumers leave.

## Data

| stage | source | size |
|---|---|---|
| search | AmazonQAC query log (US, Sept 2023) | 3.95 M distinct queries |
| supply | Amazon Reviews 2023 product metadata | 14.8 M product descriptions |
| experience | Amazon Reviews 2023 reviews, Jan 2015 – Sept 2023 | 228.6 M reviews |

Six product categories: Grocery & Gourmet Food, Home & Kitchen, Health
Household & Beauty, Electronics, Fashion & Clothing, Baby Products.

The raw downloads are not included — they are available from the
[Amazon Reviews 2023](https://amazon-reviews-2023.github.io/) project and the
AmazonQAC dataset. The derived, tagged Parquet files live in `data/` and are
the artefact of record for the analysis.

## The lexicon

One lexicon is applied identically at all three stages: **22 certification
schemes** and **33 informal sustainability terms** after merging naming
variants. Matching is case-insensitive and word-bounded, and multi-word terms
match hyphen, space and no-space spellings alike (`plant-based`, `plant based`,
`plantbased`). The longest matching term at a given position wins, so
`USDA Organic` is not also counted as `organic`.

## Layout

```
00_setup.R      shared paths, category table, database connection, helpers
01_lexicon.R    the lexicon and the tagger
02-04_*         data preparation (see note below)
10-12_*         RQ1  sustainability language in reviews over time
20_*            RQ2  does description language reach the reviews?
30_*            RQ3  is the certification itself reproduced?
40-42_*         RQ4  the three-stage funnel
50_overview.R   the dataset table
rqs_plot.R
run_all.R         runs everything in the right order
archive/          earlier versions, superseded
```

## Running it

```r
Rscript run_all.R
```

Needs R with `DBI`, `duckdb`, `dbplyr`, `dplyr`, `tidyr`, `readr`, `stringi`
and `ggplot2`. Allow roughly an hour: most steps scan the full Parquet set, and
the funnel step also reads the 42 M-row query log.

The order matters in two places, which `run_all.R` documents:
`41_rq4_uncertainty.R` must run after `30_rq3_reproduction.R`, and
`42_rq4_tables.R` after `41_rq4_uncertainty.R`.

The precision check runs separately, with manual coding in between:

```r
Rscript R/60_validate_lexicon.R sample   # build the coding sheet
Rscript R/60_validate_lexicon.R score    # after filling in the `correct` column
```

**Data preparation (`R/02`–`R/04`) cannot be re-run as it stands.** The raw
`.jsonl.gz` downloads were deleted after processing to save disk space. The
scripts are kept so the pipeline is documented and can be repeated by anyone
who downloads the sources again.

## Memory

The corpora are far larger than memory, so everything is read from Parquet in
place through DuckDB rather than loaded into R. Deduplication uses the
`arg_max` aggregate rather than a window function, which ran out of memory at
around 12 GB on the larger categories.
