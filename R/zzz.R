.ihs_cache <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("ihsMW: Dedicated offline cleaning suite loaded.")
}
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "IHS_survey", "file_name", "file_id", "n_variables", "harmonised_name", "label",
    "module", "topic", "ihs2_name", "ihs3_name", "ihs4_name", "ihs5_name",
    "n_rounds", "needs_review", "round_name"
  ))
}
