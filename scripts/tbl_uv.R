### QT -------------------------------------------------------------------------

df_setup |>
  tbl_wide_summary(
    include = opts$data$qt$vars$total,
    statistic = opts$qt_stat_wide,
    digits = list(~ 1, proba_surv ~ 2)
  ) |>
  modify_header(
    label ~ glue("**{opts$labs$header}**"),
    stat_6 ~ glue("**{names(opts$qt_stat$mean)}**")
  ) |>
  gt_format(width = 650) |>
  easy_out("tbl_uv_qt")

### BIN ------------------------------------------------------------------------

df_setup |>
  tbl_summary(
    include = opts$data$bin$vars,
    statistic = opts$vars$stat[[3]],
    digits = opts$digits,
    missing = "no"
  ) |>
  gtsum_format(label_stat = names(opts$ql_stat$n)) |>
  gt_format(width = 400) |>
  easy_out("tbl_uv_bin")
