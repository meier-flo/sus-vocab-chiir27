# 40_rq4_funnel.R
# RQ4: how sustainability vocabulary is distributed across the three stages of
# the funnel - search (queries), supply (product descriptions) and experience
# (reviews).
#
# For every term and stage we count how many items mention it, then work out:
#   prevalence  - share of all items at that stage
#   composition - share of that stage's sustainability vocabulary
# Composition is what we compare across stages, because the stages differ so
# much in how often they mention sustainability at all.
#
# Writes data/funnel_term_stage.csv and data/funnel_stage_totals.csv

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")
source("/Users/fmeier/Documents/AmazonReviewsStudy/R/01_lexicon.R")

con <- db_connect()

# How many items are there at each stage, and how many mention anything? ----
# certset()/informalset() clean up the stored labels first, so an item whose
# only tag was a dropped term does not count as a mention.
count_stage <- function(where_clause, glob) {
  sql <- paste0(
    "SELECT count(*) AS n_total,
            count(*) FILTER (WHERE coalesce(len(certset(certification)), 0) > 0
                             OR sustainable_intent IS NOT NULL) AS n_any
     FROM read_parquet('", data_dir, "/", glob, "')
     WHERE ", where_clause)
  dbGetQuery(con, sql)
}

# How often does each individual term appear? ------------------------------
# Each term counts once per item, however often it is repeated in the text.
count_terms <- function(where_clause, glob) {
  sql <- paste0(
    "WITH items AS (
       SELECT certification, sustainable_intent
       FROM read_parquet('", data_dir, "/", glob, "')
       WHERE ", where_clause, "
     )
     SELECT 'certification' AS term_type, term, count(*) AS n_with
     FROM (SELECT unnest(certset(certification)) AS term FROM items)
     GROUP BY term
     UNION ALL
     SELECT 'informal' AS term_type, term, count(*) AS n_with
     FROM (SELECT unnest(informalset(sustainable_intent)) AS term FROM items
           WHERE sustainable_intent IS NOT NULL)
     GROUP BY term")
  dbGetQuery(con, sql)
}

message("supply stage: product descriptions ...")
supply_totals <- count_stage("TRUE", "*_products.parquet")
supply_terms  <- count_terms("TRUE", "*_products.parquet")

message("experience stage: reviews from ", analysis_year, " ...")
year_filter <- paste0("timestamp IS NOT NULL AND year(epoch_ms(timestamp)) = ", analysis_year)
experience_totals <- count_stage(year_filter, "*_reviews.parquet")
experience_terms  <- count_terms(year_filter, "*_reviews.parquet")

# Search stage -------------------------------------------------------------
# The query log has one row per typed prefix, so we first reduce it to one
# final search term per query. The log has no stored tags, so we tag it here.
message("search stage: query log ...")

queries <- tbl(con, sql(paste0(
  "SELECT query_id, max(final_search_term) AS final_query
   FROM read_csv('", base_dir, "/amazon_qac_tokens_all.csv', ignore_errors = true)
   GROUP BY query_id")))

n_queries <- queries %>% count() %>% pull(n)
message("   ", format(n_queries, big.mark = ","), " distinct queries")

# Only pull the queries that could possibly match, rather than all 3.95M.
candidates <- dbGetQuery(con, paste0(
  "SELECT final_query FROM (", dbplyr::sql_render(queries), ")
   WHERE final_query IS NOT NULL AND regexp_matches(lower(final_query), ?)"),
  params = list(union_pattern))
message("   ", format(nrow(candidates), big.mark = ","), " could match a term")

tagged <- tag_reviews_parallel(candidates$final_query)

# Split the ';'-separated labels and clean them up the same way the database
# macros do, so the search stage is comparable with the other two.
split_labels <- function(column, clean_fun) {
  lapply(column, function(value) {
    if (is.na(value)) character(0) else clean_fun(strsplit(value, ";")[[1]])
  })
}
query_certs    <- split_labels(tagged$certification, clean_cert_labels)
query_informal <- split_labels(tagged$sustainable_intent, clean_informal_labels)

search_terms <- bind_rows(
  data.frame(term_type = "certification", term = unlist(query_certs)),
  data.frame(term_type = "informal",      term = unlist(query_informal))
) %>% count(term_type, term, name = "n_with")

search_totals <- data.frame(
  n_total = n_queries,
  n_any   = sum(lengths(query_certs) > 0 | lengths(query_informal) > 0)
)

# Put the three stages together -------------------------------------------
totals <- data.frame(
  stage   = c("search", "supply", "experience"),
  n_total = c(search_totals$n_total, supply_totals$n_total, experience_totals$n_total),
  n_any   = c(search_totals$n_any,   supply_totals$n_any,   experience_totals$n_any)
)

long <- bind_rows(
  mutate(search_terms,     stage = "search"),
  mutate(supply_terms,     stage = "supply"),
  mutate(experience_terms, stage = "experience")
) %>%
  group_by(stage) %>%
  mutate(composition = n_with / sum(n_with)) %>%
  ungroup() %>%
  left_join(totals, by = "stage") %>%
  mutate(prevalence = n_with / n_total)

# Every term needs a row for every stage, with zeros where it never appeared.
grid <- long %>%
  distinct(term_type, term) %>%
  crossing(stage = c("search", "supply", "experience")) %>%
  left_join(select(long, term_type, term, stage, n_with, prevalence, composition),
            by = c("term_type", "term", "stage")) %>%
  left_join(select(totals, stage, n_total), by = "stage") %>%
  mutate(across(c(n_with, prevalence, composition), ~ replace_na(., 0)))

# Which stage does each term belong to? ------------------------------------
# Rescale a term's three composition values so they add up to 1, then see
# whether any single stage holds at least half of that.
wide <- grid %>%
  select(term_type, term, stage, n_with, prevalence, composition) %>%
  pivot_wider(names_from = stage, values_from = c(n_with, prevalence, composition)) %>%
  mutate(
    tot = composition_search + composition_supply + composition_experience,
    p_search     = ifelse(tot > 0, composition_search / tot, 0),
    p_supply     = ifelse(tot > 0, composition_supply / tot, 0),
    p_experience = ifelse(tot > 0, composition_experience / tot, 0),
    typology = case_when(
      tot == 0            ~ "absent",
      p_search >= 0.5     ~ "search-led",
      p_supply >= 0.5     ~ "supply-led",
      p_experience >= 0.5 ~ "experience-led",
      TRUE                ~ "shared"),
    # A term carries demand at the other stages but barely shows up in search.
    not_searched = p_search < 0.05,
    across(c(p_search, p_supply, p_experience), ~ round(., 3))
  ) %>%
  arrange(term_type, desc(composition_experience))

write_csv(wide, file.path(data_dir, "funnel_term_stage.csv"))
write_csv(totals, file.path(data_dir, "funnel_stage_totals.csv"))

cat("\nstage densities (share of items mentioning anything)\n")
totals %>% mutate(density = round(n_any / n_total, 4)) %>% as.data.frame() %>% print(row.names = FALSE)

cat("\ntypology counts\n")
wide %>% count(typology, sort = TRUE) %>% as.data.frame() %>% print(row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
db_cleanup()
