# 01_lexicon.R
# The sustainability lexicon and the tagger that applies it.
#
# This replaces the two older pattern files. They existed because "refill" was
# added for product descriptions after the reviews had already been tagged;
# the reviews have since been re-tagged, so there is now one lexicon for all
# three stages (queries, product descriptions, reviews).
#
# How matching works:
#  - we lower-case the text, so the patterns are written in lower case
#  - word parts are joined with [-\s]* so "plant-based", "plant based" and
#    "plantbased" all match
#  - terms are tried longest first and every match is blanked out, so a longer
#    term wins over a shorter one in the same spot ("sustainably-sourced"
#    swallows "sustainably", "USDA Organic" swallows "organic")
#  - certifications are scanned before informal terms, so an informal word that
#    is part of a certification name is not counted twice

library(stringi)

# Certification schemes -----------------------------------------------------
# Several schemes are listed under two names (abbreviation and full name).
# normcert() in 00_setup.R maps them onto one label before we count anything.
cert_terms <- list(
  "USDA Organic"                    = c("usda", "organic"),
  "Fair Trade Certified"            = c("fair", "trade", "certified"),
  "Fairtrade"                       = c("fair", "trade"),
  # Matches any standalone "non-gmo" / "non gmo" / "nongmo", not just the full
  # scheme name, because sellers usually write only the short form.
  "Non-GMO Project"                 = c("non", "gmo"),
  "Energy Star"                     = c("energy", "star"),
  "EPEAT"                           = c("epeat"),
  "GOTS"                            = c("gots"),
  "Global Organic Textile Standard" = c("global", "organic", "textile", "standard"),
  "Oeko-Tex"                        = c("oeko", "tex"),
  # STANDARD 100 is the Oeko-Tex sub-standard. It is tagged here for
  # completeness but dropped by certset() before analysis: on its own it mostly
  # matches text like "Standard 100% cotton", and where it is genuine the
  # product already carries the Oeko-Tex label.
  "STANDARD 100"                    = c("standard", "100"),
  "FSC"                             = c("fsc"),
  "Forest Stewardship Council"      = c("forest", "stewardship", "council"),
  "Rainforest Alliance"             = c("rainforest", "alliance"),
  "Certified Humane"                = c("certified", "humane"),
  "Leaping Bunny"                   = c("leaping", "bunny"),
  "GREENGUARD Gold"                 = c("greenguard", "gold"),
  "GREENGUARD"                      = c("greenguard"),
  "B Corporation"                   = c("b", "corporation"),
  "B Corp"                          = c("b", "corp"),
  "Cradle to Cradle"                = c("cradle", "to", "cradle"),
  "NSF"                             = c("nsf"),
  "EPA Safer Choice"                = c("epa", "safer", "choice"),
  "Certified Vegan"                 = c("certified", "vegan"),
  "Ecocert"                         = c("ecocert"),
  "Bluesign"                        = c("bluesign"),
  "Global Recycled Standard"        = c("global", "recycled", "standard"),
  "Responsible Wool Standard"       = c("responsible", "wool", "standard"),
  "CertiPUR-US"                     = c("certipur", "us")
)

# Informal sustainability terms ---------------------------------------------
# Everyday words people use instead of a certification name. Spelling variants
# of the same idea (recycle/recyclable/recycled) are merged later by
# norminformal() in 00_setup.R.
informal_terms <- list(
  "organic"             = c("organic"),
  "non-toxic"           = c("non", "toxic"),
  "chemical-free"       = c("chemical", "free"),
  "plastic-free"        = c("plastic", "free"),
  "plant-based"         = c("plant", "based"),
  "durable"             = c("durable"),
  "reusable"            = c("reusable"),
  "refill"              = c("refill"),
  "refillable"          = c("refillable"),
  "biodegradable"       = c("biodegradable"),
  "compostable"         = c("compostable"),
  "recyclable"          = c("recyclable"),
  "recycle"             = c("recycle"),
  "recycled"            = c("recycled"),
  "zerowaste"           = c("zero", "waste"),
  "upcycled"            = c("upcycled"),
  "refurbished"         = c("refurbished"),
  "waste-free"          = c("waste", "free"),
  "closed-loop"         = c("closed", "loop"),
  "rechargeable"        = c("rechargeable"),
  "energy-efficient"    = c("energy", "efficient"),
  "water-saving"        = c("water", "saving"),
  "low-carbon"          = c("low", "carbon"),
  "carbon-neutral"      = c("carbon", "neutral"),
  "low-impact"          = c("low", "impact"),
  "net-zero"            = c("net", "zero"),
  "renewable"           = c("renewable"),
  "emission-free"       = c("emission", "free"),
  "energy-saving"       = c("energy", "saving"),
  "solar"               = c("solar"),
  "sustainably-sourced" = c("sustainably", "sourced"),
  "eco-friendly"        = c("eco", "friendly"),
  "cruelty-free"        = c("cruelty", "free"),
  "vegan"               = c("vegan"),
  "free-range"          = c("free", "range"),
  "vegetarian"          = c("vegetarian"),
  "sustainable"         = c("sustainable"),
  "sustainably"         = c("sustainably")
)


# Building the regular expressions ------------------------------------------

# Turn word parts into one pattern, e.g. c("plant","based") becomes
# \bplant[-\s]*based\b
build_pattern <- function(parts) {
  paste0("\\b", paste(parts, collapse = "[-\\s]*"), "\\b")
}

# Sort a term list so the longest terms come first. We compare total characters
# because that is what decides which term should win when two overlap.
order_terms <- function(terms) {
  lengths <- sapply(terms, function(parts) sum(nchar(parts)))
  terms[order(lengths, decreasing = TRUE)]
}

cert_ordered     <- order_terms(cert_terms)
informal_ordered <- order_terms(informal_terms)

cert_patterns     <- sapply(cert_ordered, build_pattern)
informal_patterns <- sapply(informal_ordered, build_pattern)

# One big pattern matching any term at all. We use it in SQL to throw away the
# rows that cannot possibly match before pulling anything into R.
union_pattern <- paste(c(cert_patterns, informal_patterns), collapse = "|")


# The tagger ----------------------------------------------------------------

# Look for each pattern in turn, record how often it matched, then blank the
# match out so shorter terms cannot match the same piece of text.
tag_scan <- function(work, patterns) {
  found <- rep("", length(work))

  for (i in seq_along(patterns)) {
    pattern <- patterns[[i]]
    label   <- names(patterns)[i]

    counts <- stri_count_regex(work, pattern)
    hits   <- which(counts > 0)

    if (length(hits) > 0) {
      found[hits] <- paste0(found[hits], strrep(paste0(label, ";"), counts[hits]))
    }

    # Replace with a space, not nothing, so neighbouring words stay separate.
    work <- stri_replace_all_regex(work, pattern, " ")
  }

  list(labels = found, work = work)
}

# Tag a vector of texts. Returns one row per text with two ';'-separated
# columns, or NA where nothing matched.
tag_reviews <- function(combined) {
  work <- stri_trans_tolower(combined)

  certs    <- tag_scan(work, cert_patterns)
  informal <- tag_scan(certs$work, informal_patterns)

  cert_labels <- sub(";$", "", certs$labels)
  cert_labels[cert_labels == ""] <- NA_character_

  informal_labels <- sub(";$", "", informal$labels)
  informal_labels[informal_labels == ""] <- NA_character_

  data.frame(certification = cert_labels,
             sustainable_intent = informal_labels,
             stringsAsFactors = FALSE)
}

# Same thing, split over several cores. Tagging millions of product
# descriptions one after another took hours; this brings it down to minutes.
tag_reviews_parallel <- function(combined, cores = 6L, chunk_size = 50000L) {
  if (length(combined) <= chunk_size) return(tag_reviews(combined))

  starts <- seq(1, length(combined), by = chunk_size)
  chunks <- lapply(starts, function(start) {
    combined[start:min(start + chunk_size - 1, length(combined))]
  })

  results <- parallel::mclapply(chunks, tag_reviews, mc.cores = cores)

  failed <- sapply(results, function(r) inherits(r, "try-error"))
  if (any(failed)) stop("tagging failed in a worker process")

  do.call(rbind, results)
}
