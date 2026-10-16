# 11_rq1_decompose.R
# RQ1 follow-up: what is actually driving the rise in informal sustainability
# language? We recount the yearly share three ways - with the full lexicon,
# without "durable", and without the whole quality/longevity cluster - and also
# look at each term separately.
#
# Writes rq1_robustness_timeseries.csv, rq1_term_prevalence_by_year.csv,
# rq1_breadth_by_year.csv, rq1_durable_smallplot.csv and the small plot itself.

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")
library(ggplot2)

con <- db_connect()

# Terms that describe how long a product lasts rather than its environmental
# impact. "durable" alone carries most of the rise; the others are its cluster.
quality_terms <- c("durable", "reusable", "rechargeable", "refurbished")
quality_list  <- paste0("['", paste(quality_terms, collapse = "','"), "']")

years_filter <- "timestamp IS NOT NULL AND year(epoch_ms(timestamp)) BETWEEN 2015 AND 2023"


# 1. Yearly shares under three lexicon definitions -------------------------
message("yearly shares, three lexicon definitions ...")
share_rows <- list()

for (i in seq_len(nrow(categories))) {
  message("   ", categories$category[i])
  share_rows[[i]] <- dbGetQuery(con, paste0(
    "SELECT '", categories$category[i], "' AS product_category,
            year(epoch_ms(timestamp)) AS year,
            count(*) AS n_reviews,
            count(*) FILTER (WHERE sustainable_intent IS NOT NULL) AS n_full,
            count(*) FILTER (WHERE sustainable_intent IS NOT NULL
              AND len(list_filter(string_split(sustainable_intent,';'),
                                  x -> x <> 'durable')) > 0) AS n_excl_durable,
            count(*) FILTER (WHERE sustainable_intent IS NOT NULL
              AND len(list_filter(string_split(sustainable_intent,';'),
                                  x -> NOT list_contains(", quality_list, ", x))) > 0) AS n_excl_quality
     FROM read_parquet('", data_dir, "/", categories$reviews_file[i], "')
     WHERE ", years_filter, "
     GROUP BY year"))
}

robustness <- bind_rows(share_rows) %>%
  group_by(product_category, year) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  mutate(share_full          = round(n_full / n_reviews, 6),
         share_excl_durable  = round(n_excl_durable / n_reviews, 6),
         share_excl_quality  = round(n_excl_quality / n_reviews, 6))
write_csv(robustness, file.path(data_dir, "rq1_robustness_timeseries.csv"))


# 2. Each term separately, by year ----------------------------------------
# Note: terms are NOT merged here, so "recyclable" and "recycled" stay apart.
# We want to see individual wording, not the merged concept.
message("per-term prevalence by year ...")
term_rows <- list()

for (i in seq_len(nrow(categories))) {
  term_rows[[i]] <- dbGetQuery(con, paste0(
    "SELECT year, term, count(*) AS n_term FROM (
       SELECT year(epoch_ms(timestamp)) AS year,
              unnest(list_distinct(string_split(sustainable_intent,';'))) AS term
       FROM read_parquet('", data_dir, "/", categories$reviews_file[i], "')
       WHERE ", years_filter, " AND sustainable_intent IS NOT NULL)
     GROUP BY year, term"))
}

reviews_per_year <- robustness %>%
  group_by(year) %>%
  summarise(n_reviews = sum(n_reviews), .groups = "drop")

term_prevalence <- bind_rows(term_rows) %>%
  group_by(year, term) %>%
  summarise(n_term = sum(n_term), .groups = "drop") %>%
  left_join(reviews_per_year, by = "year") %>%
  mutate(prevalence = round(n_term / n_reviews, 6))
write_csv(term_prevalence, file.path(data_dir, "rq1_term_prevalence_by_year.csv"))


# 3. Is the vocabulary getting broader or more concentrated? ---------------
# effective number of terms = exp(Shannon entropy): how many terms the
# vocabulary behaves like, if they were all used equally often.
breadth <- term_prevalence %>%
  group_by(year) %>%
  mutate(p = n_term / sum(n_term)) %>%
  summarise(top1_share    = round(max(p), 4),
            eff_num_terms = round(exp(-sum(p * log(p))), 2),
            .groups = "drop")
write_csv(breadth, file.path(data_dir, "rq1_breadth_by_year.csv"))


# 4. The small plot: with and without "durable" ---------------------------
pooled <- robustness %>%
  group_by(year) %>%
  summarise(n_reviews = sum(n_reviews),
            n_full = sum(n_full),
            n_excl = sum(n_excl_durable), .groups = "drop") %>%
  mutate(`full lexicon`        = 100 * n_full / n_reviews,
         `excluding "durable"` = 100 * n_excl / n_reviews)

write_csv(pooled %>% select(year, n_reviews,
                            full_pct = `full lexicon`,
                            excl_durable_pct = `excluding "durable"`),
          file.path(data_dir, "rq1_durable_smallplot.csv"))

for_plot <- pooled %>%
  select(year, `full lexicon`, `excluding "durable"`) %>%
  pivot_longer(-year, names_to = "lexicon", values_to = "share")

plot <- ggplot(for_plot, aes(year, share, colour = lexicon, linetype = lexicon)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = c("full lexicon" = "#1B7837", `excluding "durable"` = "#999999")) +
  scale_linetype_manual(values = c("full lexicon" = "solid", `excluding "durable"` = "22")) +
  scale_x_continuous(breaks = 2015:2023) +
  labs(x = NULL, y = "reviews with informal\nsustainability term (%)",
       colour = NULL, linetype = NULL) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 10) +
  theme(legend.position = c(0.30, 0.88),
        legend.background = element_rect(fill = "white", colour = NA),
        panel.grid.minor = element_blank())

ggsave(file.path(data_dir, "rq1_durable_smallplot.pdf"), plot, width = 4.2, height = 3)
ggsave(file.path(data_dir, "rq1_durable_smallplot.png"), plot, width = 4.2, height = 3, dpi = 150)

cat("\npooled share with and without 'durable'\n")
pooled %>% select(year, full_pct = `full lexicon`, excl_durable_pct = `excluding "durable"`) %>%
  as.data.frame() %>% print(row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
db_cleanup()
