.tbl_bv_qt_width <- list(qt = 750, bin = 720)

.tbl_bv_qt_fun <- \(var) {
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
    tbl_format(width = .tbl_bv_qt_width$qt)

  clear_vars()

  .tbl
}

.tbl_bv_bin_fun <- \(var) {
  df_setup |>
    tbl_summary(
      include = opts$data$bin$vars,
      by = !!var,
      statistic = opts$vars$stat[[3]],
      digits = opts$digits,
      missing = "no"
    ) |>
    add_stat_label() |>
    gtsum_format() |>
    tbl_format(width = .tbl_bv_qt_width$bin)
}

tbl_bv_qt <- list(
  qt = map(set_names(.bv$vars), .tbl_bv_qt_fun),
  bin = map(set_names(.bv$vars), .tbl_bv_bin_fun)
) |>
  list_flatten(name_spec = "{outer}_{inner}")

easy_out(tbl_bv_qt)
