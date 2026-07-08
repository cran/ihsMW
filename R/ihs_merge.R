#' Merge Multiple Harmonised IHS Data Frames
#'
#' Merges two or more harmonised data.frames, auto-detecting common ID columns
#' when \code{by} is not specified. Warns on unexpected row expansion from
#' many-to-many joins.
#'
#' @param ... Two or more data.frames to merge.
#' @param by Character vector of columns to join by. If \code{NULL} (default),
#'   auto-detects common ID columns from a standard set.
#' @param type Join type: \code{"left"} (default), \code{"inner"}, or
#'   \code{"full"}.
#'
#' @return A merged data.frame with an \code{ihs_merge_log} attribute
#'   containing row counts at each merge step.
#'
#' @examples
#' \dontrun{
#'   hh <- haven::read_dta("hh_mod_a.dta") |> ihs_harmonise("IHS5")
#'   ag <- haven::read_dta("ag_mod_a.dta") |> ihs_harmonise("IHS5")
#'   merged <- ihs_merge(hh, ag)
#' }
#'
#' @export
ihs_merge <- function(..., by = NULL, type = "left") {
  dfs <- list(...)

  if (length(dfs) < 2) {
    cli::cli_abort("{.fn ihs_merge} requires at least 2 data.frames.")
  }

  for (i in seq_along(dfs)) {
    if (!is.data.frame(dfs[[i]])) {
      cli::cli_abort("Argument {i} is not a data.frame.")
    }
  }

  type <- rlang::arg_match(type, values = c("left", "inner", "full"))

  join_fn <- switch(type,
    left  = dplyr::left_join,
    inner = dplyr::inner_join,
    full  = dplyr::full_join
  )

  # Auto-detect common ID columns
  if (is.null(by)) {
    standard_ids <- c("case_id", "hhid", "hh_id", "HHID", "ea_id", "PID")
    common_cols <- Reduce(intersect, lapply(dfs, names))
    by <- intersect(standard_ids, common_cols)

    if (length(by) == 0) {
      cli::cli_abort(c(
        "No common ID columns detected across all data.frames.",
        "i" = "Specify {.arg by} explicitly."
      ))
    }

    cli::cli_inform("Auto-detected join columns: {.val {by}}")
  }

  merge_log <- list()
  result <- dfs[[1]]
  merge_log[[1]] <- list(step = 0, rows = nrow(result))

  for (i in 2:length(dfs)) {
    pre_rows <- nrow(result)
    result <- join_fn(result, dfs[[i]], by = by)
    post_rows <- nrow(result)

    merge_log[[i]] <- list(
      step = i - 1,
      rows_before = pre_rows,
      rows_after = post_rows,
      rows_right = nrow(dfs[[i]])
    )

    if (post_rows > max(pre_rows, nrow(dfs[[i]]))) {
      cli::cli_warn(c(
        "Merge step {i - 1} expanded rows from {pre_rows} to {post_rows}.",
        "i" = "This may indicate a many-to-many join. Check your ID columns."
      ))
    }
  }

  cli::cli_inform("Merged {length(dfs)} data.frames: {nrow(result)} rows, {ncol(result)} columns.")
  attr(result, "ihs_merge_log") <- merge_log
  result
}
