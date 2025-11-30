tbl_bv_qt <- \(var) {

### QT -------------------------------------------------------------------------

  df_setup |>
    use_vars() |>
    tbl_summary(
      include = opts$data$qt$vars$total,
      by = all_of(var),
      statistic = opts$vars$stat,
      digits = list(~1, proba_surv ~ 2),
      missing = "no"
    ) |>
    add_stat_label(label = opts$vars$label) |>
    gtsum_format() |>
    gt_format(width = 750) |>
    easy_out(filename = glue("tbl_bv_{var}_qt"))

### BIN ------------------------------------------------------------------------

  df_setup |>
    tbl_summary(
      include = opts$data$bin$vars,
      by = all_of(var),
      statistic = opts$vars$stat[[3]],
      digits = opts$digit,
      missing = "no"
    ) |>
    add_stat_label() |>
    gtsum_format() |>
    gt_format(width = 720) |>
    easy_out(filename = glue("tbl_bv_{var}_bin"))

}

walk(.bv$vars, tbl_bv_qt)
