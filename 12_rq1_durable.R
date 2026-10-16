# 12_rq1_durable.R
# A closer look at "durable", the term that carries almost the whole rise in
# RQ1. Three questions:
#   is it praise or complaint?      -> star ratings and negation
#   what words surround it?         -> co-occurring words in its sentences
#   is it framed environmentally?   -> environmental vs quality vocabulary
#
# Ratings and negation use every year; the sentence analysis uses 2023 only, to
# match the stage the funnel calls "experience".
#
# Writes durable_rating_split.csv, durable_negation.csv, durable_context_2023.csv,
# durable_cooccurrence.csv and durable_sentence_context.csv

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")

con <- db_connect()

all_reviews  <- paste0(data_dir, "/*_reviews.parquet")
review_text  <- "lower(coalesce(title,'') || ' ' || coalesce(text,''))"
is_durable   <- "list_contains(string_split(sustainable_intent,';'), 'durable')"
year_filter  <- paste0("timestamp IS NOT NULL AND year(epoch_ms(timestamp)) = ", analysis_year)

# "not durable", "isn't very durable", "hardly durable" and so on.
negation_pattern <- "'\\b(not|never|isn''?t|wasn''?t|aren''?t|hardly|barely|no|less|nothing)\\s+(\\w+\\s+){0,2}durable'"


# 1. Star ratings ----------------------------------------------------------
message("star ratings ...")
ratings <- dbGetQuery(con, paste0(
  "SELECT rating,
          count(*) AS n_all,
          count(*) FILTER (WHERE sustainable_intent IS NOT NULL) AS n_informal,
          count(*) FILTER (WHERE ", is_durable, ") AS n_durable
   FROM read_parquet('", all_reviews, "')
   WHERE rating IS NOT NULL
   GROUP BY rating ORDER BY rating"))

ratings <- ratings %>%
  mutate(p_all      = n_all / sum(n_all),
         p_informal = n_informal / sum(n_informal),
         p_durable  = n_durable / sum(n_durable))
write_csv(ratings, file.path(data_dir, "durable_rating_split.csv"))


# 2. Negation --------------------------------------------------------------
message("negation ...")
negation <- dbGetQuery(con, paste0(
  "WITH durable_reviews AS (
     SELECT ", review_text, " AS t, rating
     FROM read_parquet('", all_reviews, "') WHERE ", is_durable, ")
   SELECT count(*) AS n_durable,
          count(*) FILTER (WHERE regexp_matches(t, ", negation_pattern, ")) AS n_negated,
          count(*) FILTER (WHERE rating <= 2) AS n_low_star,
          count(*) FILTER (WHERE regexp_matches(t, ", negation_pattern, ") AND rating <= 2) AS n_negated_low
   FROM durable_reviews"))
write_csv(negation, file.path(data_dir, "durable_negation.csv"))


# 3. The same for 2023 only, to match the funnel's experience stage --------
message("2023 ratings and negation ...")
context_2023 <- dbGetQuery(con, paste0(
  "WITH y AS (
     SELECT rating, ", review_text, " AS t, sustainable_intent
     FROM read_parquet('", all_reviews, "')
     WHERE ", year_filter, " AND rating IS NOT NULL)
   SELECT count(*) AS n_all,
          count(*) FILTER (WHERE rating >= 5) AS n_all_5,
          count(*) FILTER (WHERE rating <= 2) AS n_all_low,
          count(*) FILTER (WHERE ", is_durable, ") AS n_dur,
          count(*) FILTER (WHERE ", is_durable, " AND rating >= 5) AS n_dur_5,
          count(*) FILTER (WHERE ", is_durable, " AND rating <= 2) AS n_dur_low,
          count(*) FILTER (WHERE ", is_durable, " AND regexp_matches(t, ", negation_pattern, ")) AS n_dur_neg
   FROM y"))
write_csv(context_2023, file.path(data_dir, "durable_context_2023.csv"))


# 4. Which words share a sentence with "durable"? --------------------------
message("sentence co-occurrence ...")
durable_sentences <- paste0(
  "WITH durable_reviews AS (
     SELECT lower(coalesce(title,'') || '. ' || coalesce(text,'')) AS t
     FROM read_parquet('", all_reviews, "')
     WHERE ", year_filter, " AND ", is_durable, "),
   sentences AS (
     SELECT unnest(regexp_extract_all(t, '[^.!?]*durable[^.!?]*')) AS s FROM durable_reviews)")

words <- dbGetQuery(con, paste0(durable_sentences, ",
   words AS (SELECT unnest(string_split_regex(regexp_replace(s, '[^a-z ]', ' ', 'g'), '\\s+')) AS w
             FROM sentences)
   SELECT w AS word, count(*) AS n FROM words
   WHERE len(w) >= 3 GROUP BY w ORDER BY n DESC LIMIT 400"))

# Ordinary English words that tell us nothing about how durable is used.
stopwords <- c("the","and","for","are","was","were","this","that","with","have","has","had","but","you","its",
  "they","them","their","these","those","from","very","just","will","would","can","could","all","out","get","got",
  "than","then","when","what","which","also","been","being","because","after","before","more","most","some","one",
  "two","use","used","using","it's","don","doesn","didn","isn","wasn","your","our","his","her","she","him","who",
  "about","into","over","only","much","many","there","here","how","too","really","well","even","still","far","way",
  "does","did","make","made","makes","seem","seems","like","though","while","where","any","other","not","durable")

cooccurrence <- words %>% filter(!word %in% stopwords) %>% head(60)
write_csv(cooccurrence, file.path(data_dir, "durable_cooccurrence.csv"))


# 5. Environmental framing or quality framing? ----------------------------
message("sentence framing ...")
framing <- dbGetQuery(con, paste0(durable_sentences, "
   SELECT count(*) AS n_sent,
          count(*) FILTER (WHERE regexp_matches(s,
            'environment|planet|eco\\b|eco[- ]|sustainab|landfill|waste|recycl|carbon|climate|green\\b')) AS n_eco,
          count(*) FILTER (WHERE regexp_matches(s,
            'quality|sturdy|material|strong|thick|solid|well[- ]made|built|last')) AS n_quality
   FROM sentences"))
write_csv(framing, file.path(data_dir, "durable_sentence_context.csv"))


# Report -------------------------------------------------------------------
cat("\nstar rating distribution\n")
ratings %>% transmute(rating, p_all = round(p_all, 3),
                      p_informal = round(p_informal, 3), p_durable = round(p_durable, 3)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat(sprintf("\nnegated 'durable' mentions: %.2f%% of all durable reviews\n",
            100 * negation$n_negated / negation$n_durable))
cat(sprintf("2023: %.1f%% five-star (corpus %.1f%%), %.1f%% low-star (corpus %.1f%%)\n",
            100 * context_2023$n_dur_5 / context_2023$n_dur, 100 * context_2023$n_all_5 / context_2023$n_all,
            100 * context_2023$n_dur_low / context_2023$n_dur, 100 * context_2023$n_all_low / context_2023$n_all))
cat(sprintf("\nsentences containing 'durable': %s | environmental words %.2f%% | quality words %.1f%%\n",
            format(framing$n_sent, big.mark = ","),
            100 * framing$n_eco / framing$n_sent, 100 * framing$n_quality / framing$n_sent))
cat("\ntop words sharing a sentence with 'durable'\n")
print(as.data.frame(head(cooccurrence, 12)), row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
db_cleanup()
