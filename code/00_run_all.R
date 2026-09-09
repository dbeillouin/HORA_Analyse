# ==============================================================================
# 00_run_all.R — reproduce the analyses and figures of
# Beillouin et al., "Non-cereal agroforestry as a globally overlooked reservoir
# of biodiversity and nutritional diversity", Cell Reports Sustainability.
#
# Run from the repository root:   source("code/00_run_all.R")
# or from the code/ folder:       source("00_run_all.R")
#
# The scripts share a single R environment: Data_Load.R builds the objects
# (TAB_FINALE, DATAT, DATAC, CARACT, ...) that every figure script uses, so
# they must be run in this order and in one session.
# ==============================================================================

root <- if (dir.exists("code") && dir.exists("data")) "." else ".."
code_dir <- file.path(root, "code")
data_dir <- file.path(root, "data")
fig_dir  <- file.path(root, "figures")
dir.create(fig_dir, showWarnings = FALSE)

stopifnot(dir.exists(code_dir), dir.exists(data_dir))

scripts <- c(
  "Data_Load.R",          # database, cleaning, filters, counts of Results 1 and 3
  "Fig_1.R",              # shared functional preparation + Figure 1
  "Prop_Regions.R",       # regional aggregation
  "FIG_3.R",              # Figure 3 - structural complexity
  "FIG_4.R",              # Figure 4 - FAO group co-occurrence
  "Fig_5.R",              # Figure 5 - threatened taxa, mixed models
  "FIG_6.R",              # Figure 6 - ecosystem services
  "Pair_wise_service.R",  # pairwise service panel
  "Flux_sankey.R"         # supplementary flow diagram
)

for (s in scripts) {
  message("\n=== ", s, " ", strrep("=", max(0, 60 - nchar(s))))
  source(file.path(code_dir, s), local = FALSE, echo = FALSE)
}

message("\nDone. Figures written to: ", normalizePath(fig_dir))
