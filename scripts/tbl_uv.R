### QT -------------------------------------------------------------------------

tbl_uv_qt <- df_setup |>
  tbl_wide_summary(
    include = opts$data$qt$vars$total,
    statistic = opts$qt_stat_wide,
    digits = list(~1, proba_surv ~ 2)
  ) |>
  modify_header(
    label ~ str_glue("**{opts$labs$header}**"),
    stat_6 ~ str_glue("**{names(opts$qt_stat$mean)}**")
  ) |>
  gt_format()

easy_out(tbl_uv_qt, width = 650)

### BIN ------------------------------------------------------------------------

tbl_uv_bin <- df_setup |>
  tbl_summary(
    include = opts$data$bin$vars,
    statistic = opts$vars$stat[[3]],
    digits = opts$digits,
    missing = "no"
  ) |>
  gtsum_format(label_stat = names(opts$ql_stat$n)) |>
  gt_format()

easy_out(tbl_uv_bin, width = 400)
