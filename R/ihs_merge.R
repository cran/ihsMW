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
#' @section Auto-detected keys:
#' When \code{by} is \code{NULL} the join keys are the intersection of
#' \code{case_id}, \code{hhid}, \code{hh_id}, \code{HHID}, \code{ea_id} and
#' \code{PID} with the columns common to every input. The detected keys are
#' always printed - read them, because joining household modules on
#' \code{PID} when you meant \code{case_id} silently changes your sample.
#'
#' @examples
#' hh <- data.frame(case_id = c("A", "B", "C"), hhsize = c(4, 6, 3))
#' ag <- data.frame(case_id = c("A", "B", "D"), harvest_kg = c(120, 340, 90))
#'
#' # Left join on the auto-detected key, keeping every household
#' merged <- ihs_merge(hh, ag)
#' merged
#'
#' # Only households present in both modules
#' ihs_merge(hh, ag, type = "inner")
#'
#' # Row counts at each step
#' attr(merged, "ihs_merge_log")
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

    # A full join legitimately returns more rows than either input whenever the
    # two sides have non-overlapping keys, so only left/inner joins can signal
    # a many-to-many blow-up this way.
    if (type != "full" && post_rows > max(pre_rows, nrow(dfs[[i]]))) {
      cli::cli_warn(c(
        "Merge step {i - 1} expanded rows from {pre_rows} to {post_rows}.",
        "i" = "This may indicate a many-to-many join. Check your ID columns."
      ))
    }
  }

  cli::cli_inform("Merged {length(dfs)} data.frames: {nrow(result)} rows, {ncol(result)} columns.")

  # Columns present in more than one input but not used as a join key get
  # dplyr's .x/.y suffixes. This is easy to miss and quietly breaks downstream
  # auto-detection: once `hh_wgt` becomes `hh_wgt.x`, ihs_svydesign() can no
  # longer find the survey weight.
  suffixed <- grep("[.](x|y)$", names(result), value = TRUE)
  if (length(suffixed) > 0) {
    stems <- unique(sub("[.](x|y)$", "", suffixed))
    cli::cli_warn(c(
      "{length(stems)} column{?s} appeared in more than one input and {?was/were} suffixed: {.var {stems}}",
      "i" = "Drop the duplicates or add them to {.arg by} before relying on
             auto-detected weight, strata or PSU columns."
    ))
  }

  attr(result, "ihs_merge_log") <- merge_log
  result
}
