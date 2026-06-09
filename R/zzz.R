## Package-level setup.
##
## Declare the column names referenced via non-standard evaluation in dplyr /
## tidyr pipelines (e.g. in set_taxon() and the create_* helpers) so that
## `R CMD check` does not report "no visible binding for global variable".
## R CMD check will reveal any additional names to add here.
utils::globalVariables(c(
  "scientificName", "order", "family", "genus", "data", "data2",
  "taxonomicStatus", "mediaID", "observationID"
))
