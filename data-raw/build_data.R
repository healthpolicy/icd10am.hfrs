# data-raw/build_data.R
# Build internal lookup data for icd10am.hfrs.
# Run this script interactively from the package root to regenerate R/sysdata.rda.
#
# Source file:
#   hfrs_icd10_weights.csv — 109 ICD-10 group weights from Gilbert et al. (2018)

hfrs_map <- read.csv("data-raw/hfrs_icd10_weights.csv", stringsAsFactors = FALSE)

stopifnot(nrow(hfrs_map) == 109)
stopifnot(!anyDuplicated(hfrs_map$icd10_group))

usethis::use_data(hfrs_map, internal = TRUE, overwrite = TRUE)
