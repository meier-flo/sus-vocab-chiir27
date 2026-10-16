# 42_rq4_tables.R
# The appendix table of every term across the funnel, plus a shorter version
# for the main text. Run after 41_rq4_uncertainty.R, which decides which
# typology labels get a dagger.

source("/Users/fmeier/Documents/AmazonReviewsStudy/R/00_setup.R")

terms     <- read_csv(file.path(data_dir, "funnel_term_stage.csv"), show_col_types = FALSE)
stability <- read_csv(file.path(data_dir, "funnel_typology_stability.csv"), show_col_types = FALSE)

# Mark the labels that did not survive the bootstrap.
terms <- terms %>%
  left_join(select(stability, term, stability), by = "term") %>%
  mutate(typology = ifelse(!is.na(stability) & stability < 0.95,
                           paste0(typology, "$^\\dagger$"), typology))

# Counts with thousands separators; percentages with more decimals when the
# value is tiny, so a rare term does not print as a bare 0.00.
format_count <- function(x) formatC(as.integer(x), big.mark = ",", format = "d")
format_pct <- function(x) {
  pct <- x * 100
  ifelse(pct == 0, "0", ifelse(pct >= 0.01, sprintf("%.2f", pct), sprintf("%.4f", pct)))
}

stage_cells <- function(df, stage) {
  paste(format_count(df[[paste0("n_with_", stage)]]),
        format_pct(df[[paste0("prevalence_", stage)]]),
        format_pct(df[[paste0("composition_", stage)]]),
        sep = " & ")
}

# Work the caption numbers out from the data. The old version had them typed
# in by hand, which meant they still said "56 terms" long after the lexicon had
# changed.
totals <- read_csv(file.path(data_dir, "funnel_stage_totals.csv"), show_col_types = FALSE)
tex_number <- function(x) formatC(as.integer(x), big.mark = "{,}", format = "d")

stage_n <- setNames(totals$n_total, totals$stage)
stage_m <- c(search     = sum(terms$n_with_search),
             supply     = sum(terms$n_with_supply),
             experience = sum(terms$n_with_experience))

caption_notes <- paste0(
  "$n$ = items mentioning the term; prev.\\ = prevalence, $n$ as a share of the stage size ",
  "($N=", tex_number(stage_n[["search"]]), "$ queries; $",
  tex_number(stage_n[["supply"]]), "$ products; $",
  tex_number(stage_n[["experience"]]), "$ reviews from ", analysis_year, "); ",
  "comp.\\ = composition, $n$ as a share of the stage's term--item occurrences ",
  "($M=", tex_number(stage_m[["search"]]), "$; $", tex_number(stage_m[["supply"]]),
  "$; $", tex_number(stage_m[["experience"]]), "$). ",
  "Typology labels the stage holding $\\geq$50\\% of the term's normalised composition profile; ",
  "$^\\dagger$ marks labels retained in fewer than 95\\% of multinomial bootstrap resamples.")

build_table <- function(df, caption, label) {
  df <- arrange(df, tolower(term))

  rows <- paste(df$term,
                ifelse(df$term_type == "certification", "cert", "inf."),
                stage_cells(df, "search"),
                stage_cells(df, "supply"),
                stage_cells(df, "experience"),
                df$typology,
                sep = " & ")
  rows <- paste0(rows, " \\\\")

  c("\\begin{table*}[t]", "\\centering\\footnotesize",
    paste0("\\caption{", caption, "}"), paste0("\\label{", label, "}"),
    "\\begin{tabular}{ll rrr rrr rrr l}", "\\toprule",
    " & & \\multicolumn{3}{c}{\\textsc{search}} & \\multicolumn{3}{c}{\\textsc{supply}} & \\multicolumn{3}{c}{\\textsc{experience}} & \\\\",
    "\\cmidrule(lr){3-5}\\cmidrule(lr){6-8}\\cmidrule(lr){9-11}",
    "Term & Type & $n$ & prev.\\% & comp.\\% & $n$ & prev.\\% & comp.\\% & $n$ & prev.\\% & comp.\\% & Typology \\\\",
    "\\midrule", rows, "\\bottomrule", "\\end{tabular}", "\\end{table*}")
}

full_table <- build_table(terms,
  paste0("Sustainability vocabulary across the discovery funnel (all ", nrow(terms), " terms, alphabetical). ", caption_notes),
  "tab:funnel-full")
writeLines(full_table, file.path(data_dir, "funnel_results_table.tex"))

# For the main text, keep only terms that reach 1% of some stage's vocabulary.
main_terms <- terms %>%
  filter(pmax(composition_search, composition_supply, composition_experience) >= 0.01)

main_table <- build_table(main_terms,
  paste("Sustainability vocabulary across the discovery funnel, for terms reaching $\\geq$1\\% of any stage's vocabulary (full list in Appendix Table~\\ref{tab:funnel-full}).", caption_notes),
  "tab:funnel")
writeLines(main_table, file.path(data_dir, "funnel_results_table_main.tex"))

cat(sprintf("full: %d terms -> funnel_results_table.tex\n", nrow(terms)))
cat(sprintf("main: %d terms -> funnel_results_table_main.tex\n", nrow(main_terms)))
