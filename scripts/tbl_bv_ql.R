tbl_bv_ql_fun <- \(type) {
  .tbls <- .bv$vars |>
    set_names() |>
    map(
      ~ df_setup |>
        select(with(.bv, c(vars, group[[type]]))) |>
        use_vars() |>
        tbl_summary(
          by = all_of(.),
          statistic = opts$vars$stat,
          digits = opts$digits,
          missing = "no"
        ) |>
        add_stat_label(label = opts$vars$label) |>
        gtsum_format() |>
        gt_format()
    ) |>
    set_names(~ str_glue("{type}_{.}"))

  clear_vars()

  .tbls
}

tbl_bv_ql_params <- list(
  demo = 630,
  atcd = 680,
  pv = 700,
  bio = 600
)

tbl_bv_ql <- names(tbl_bv_ql_params) |>
  set_names() |>
  map(tbl_bv_ql_fun)

walk2(
  tbl_bv_ql,
  tbl_bv_ql_params,
  ~ easy_out_map(.x, filename = "tbl_bv_ql", width = .y)
)
