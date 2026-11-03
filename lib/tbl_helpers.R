#' Apply a `gt` verb on the HTML branch only
#'
#' `tbl_qmd()` and `tbl_format()` return a `flextable` under
#' `options(hebstr.docx = TRUE)`, which no `gt` verb accepts.
#'
#' @param x A `gt_tbl` or a `flextable`.
#' @param .f A `gt` verb, applied to `x` when it is a `gt_tbl`.
#' @param ... Arguments passed to `.f`.
#'
#' @return `.f(x, ...)` on a `gt_tbl`, `x` unchanged otherwise.
gt_only <- \(x, .f, ...) {
  if (inherits(x, "gt_tbl")) .f(x, ...) else x
}
