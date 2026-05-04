tbl_bv_qt_fun <- \(var) {
  .tbl <- df_setup |>
    use_vars() |>
    tbl_summary(
      include = opts$data$qt$vars$total,
      by = !!var,
      statistic = opts$vars$stat,
      digits = list(~1, proba_surv ~ 2),
      missing = "no"
    ) |>
    add_stat_label(label = opts$vars$label) |>
    gtsum_format() |>
    gt_format()

  clear_vars()

  .tbl
}

tbl_bv_bin_fun <- \(var) {
  df_setup |>
    tbl_summary(
      include = opts$data$bin$vars,
      by = !!var,
      statistic = opts$vars$stat[[3]],
      digits = opts$digit,
      missing = "no"
    ) |>
    add_stat_label() |>
    gtsum_format() |>
    gt_format()
}

tbl_bv_qt_params <- list(qt = 750, bin = 720)

tbl_bv_qt <- list(
  qt = map(set_names(.bv$vars), tbl_bv_qt_fun),
  bin = map(set_names(.bv$vars), tbl_bv_bin_fun)
)

iwalk(
  tbl_bv_qt,
  \(tbls, type) {
    easy_out_map(
      x = set_names(tbls, ~ str_glue("{type}_{.}")),
      filename = "tbl_bv_qt",
      width = tbl_bv_qt_params[[type]]
    )
  }
)
