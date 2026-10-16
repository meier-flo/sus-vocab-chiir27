# 41_rq4_uncertainty.R
# Robustness checks for RQ4.
#
# Two different questions, two different methods:
#  - Spearman correlations: the effective sample is the 55 terms, not the
#    millions of items, so we bootstrap by resampling terms.
#  - Typology labels: a term sitting near the 50% threshold could flip if the
#    counts had come out slightly differently, so we resample the counts
#    themselves and see how often each label survives.
#
# We report no confidence intervals for the share-based results: the review and
# product corpora are near-complete censuses, and at this scale every
# difference is "significant", which would be misleading rather than useful.
#
# Must run AFTER 30_rq3_reproduction.R (it reads panel A) and BEFORE
# 42_rq4_tables.R (which uses the stability file to place the dagger markers).

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")

set.seed(42)

terms  <- read_csv(file.path(data_dir, "funnel_term_stage.csv"), show_col_types = FALSE)
totals <- read_csv(file.path(data_dir, "funnel_stage_totals.csv"), show_col_types = FALSE)

stages <- c("search", "supply", "experience")
counts <- as.matrix(terms[, paste0("n_with_", stages)])
rownames(counts) <- terms$term
stage_mass <- colSums(counts)     # total term-item mentions per stage
n_terms    <- nrow(counts)

# Composition: each term's share of its stage's vocabulary.
composition <- sweep(counts, 2, stage_mass, "/")


# 1. Spearman correlations with a bootstrap interval -----------------------
n_boot_rho <- 2000
pairs <- list(c("search", "supply"), c("search", "experience"), c("supply", "experience"))

spearman_rows <- list()
for (p in pairs) {
  first  <- composition[, paste0("n_with_", p[1])]
  second <- composition[, paste0("n_with_", p[2])]

  observed <- cor(first, second, method = "spearman")

  # Resample the terms, not the items.
  boot_values <- replicate(n_boot_rho, {
    picked <- sample.int(n_terms, replace = TRUE)
    suppressWarnings(cor(first[picked], second[picked], method = "spearman"))
  })

  # Terms missing from both stages sit tied at the bottom of both rankings and
  # could inflate the correlation, so we also check the terms present in both.
  in_both <- terms[[paste0("n_with_", p[1])]] > 0 & terms[[paste0("n_with_", p[2])]] > 0

  spearman_rows[[length(spearman_rows) + 1]] <- data.frame(
    pair  = paste(p, collapse = "-"),
    rho   = observed,
    ci_lo = quantile(boot_values, 0.025, na.rm = TRUE),
    ci_hi = quantile(boot_values, 0.975, na.rm = TRUE),
    n_both_present   = sum(in_both),
    rho_both_present = cor(first[in_both], second[in_both], method = "spearman")
  )
}
spearman <- bind_rows(spearman_rows)
write_csv(spearman, file.path(data_dir, "funnel_spearman_ci.csv"))


# 2. Typology stability ----------------------------------------------------
n_boot_typology <- 1000

# Draw fresh term counts for each stage, keeping the stage's total fixed.
resampled <- list()
for (s in stages) {
  column <- paste0("n_with_", s)
  resampled[[s]] <- rmultinom(n_boot_typology, stage_mass[[column]], counts[, column]) /
                    stage_mass[[column]]
}

# The same labelling rule as in 40_rq4_funnel.R, with the threshold as an
# argument so we can also test how sensitive the labels are to the 50% cut.
label_terms <- function(search, supply, experience, threshold) {
  total <- search + supply + experience
  ifelse(total == 0, "absent",
  ifelse(search / total     >= threshold, "search-led",
  ifelse(supply / total     >= threshold, "supply-led",
  ifelse(experience / total >= threshold, "experience-led", "shared"))))
}

boot_labels <- sapply(seq_len(n_boot_typology), function(b) {
  label_terms(resampled$search[, b], resampled$supply[, b], resampled$experience[, b], 0.5)
})

observed_labels <- terms$typology
stability <- sapply(seq_len(n_terms), function(i) mean(boot_labels[i, ] == observed_labels[i]))

labels_045 <- label_terms(composition[, 1], composition[, 2], composition[, 3], 0.45)
labels_055 <- label_terms(composition[, 1], composition[, 2], composition[, 3], 0.55)

typology_stability <- data.frame(
  term = terms$term, term_type = terms$term_type, typology = observed_labels,
  stability = stability,
  comp_search_lo = apply(resampled$search, 1, quantile, 0.025),
  comp_search_hi = apply(resampled$search, 1, quantile, 0.975),
  comp_supply_lo = apply(resampled$supply, 1, quantile, 0.025),
  comp_supply_hi = apply(resampled$supply, 1, quantile, 0.975),
  comp_exp_lo    = apply(resampled$experience, 1, quantile, 0.025),
  comp_exp_hi    = apply(resampled$experience, 1, quantile, 0.975),
  lab_045 = labels_045,
  lab_055 = labels_055
)
typology_stability$threshold_sensitive <-
  typology_stability$lab_045 != observed_labels | typology_stability$lab_055 != observed_labels

write_csv(typology_stability, file.path(data_dir, "funnel_typology_stability.csv"))


# 3. Wilson intervals where a proportion really is estimated ---------------
densities <- totals
densities$density <- densities$n_any / densities$n_total
densities$ci_lo <- NA_real_
densities$ci_hi <- NA_real_
for (i in seq_len(nrow(densities))) {
  interval <- wilson(densities$n_any[i], densities$n_total[i])
  densities$ci_lo[i] <- interval[["lo"]]
  densities$ci_hi[i] <- interval[["hi"]]
}
write_csv(densities, file.path(data_dir, "funnel_density_ci.csv"))

panel_a <- read_csv(file.path(data_dir, "rq2_panelA_cert_reproduction.csv"), show_col_types = FALSE)
panel_a$ci_lo <- NA_real_
panel_a$ci_hi <- NA_real_
for (i in seq_len(nrow(panel_a))) {
  interval <- wilson(panel_a$n_reviews_reproducing[i], panel_a$n_2023_reviews[i])
  panel_a$ci_lo[i] <- interval[["lo"]]
  panel_a$ci_hi[i] <- interval[["hi"]]
}
write_csv(panel_a, file.path(data_dir, "rq2_panelA_cert_reproduction_ci.csv"))


# Report -------------------------------------------------------------------
cat("Spearman correlations with 95% bootstrap intervals\n")
spearman %>% mutate(across(where(is.numeric), ~ round(., 3))) %>% as.data.frame() %>% print(row.names = FALSE)

cat(sprintf("\ntypology labels stable in >=95%% of resamples: %d of %d\n",
            sum(stability >= 0.95), n_terms))
cat("unstable:", paste(sort(typology_stability$term[stability < 0.95]), collapse = ", "), "\n")
cat("threshold-sensitive at 0.45/0.55:",
    paste(typology_stability$term[typology_stability$threshold_sensitive], collapse = ", "), "\n")

cat("\nstage densities with Wilson intervals\n")
densities %>% mutate(across(c(density, ci_lo, ci_hi), ~ sprintf("%.4f%%", . * 100))) %>%
  as.data.frame() %>% print(row.names = FALSE)

pooled <- wilson(sum(panel_a$n_reviews_reproducing), sum(panel_a$n_2023_reviews))
cat(sprintf("\npooled reproduction: %d/%d = %.4f%% [%.4f%%, %.4f%%]\n",
            sum(panel_a$n_reviews_reproducing), sum(panel_a$n_2023_reviews),
            100 * sum(panel_a$n_reviews_reproducing) / sum(panel_a$n_2023_reviews),
            100 * pooled[["lo"]], 100 * pooled[["hi"]]))
