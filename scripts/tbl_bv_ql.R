.tbl_bv_ql_width <- list(
  demo = 630,
  atcd = 680,
  pv = 700,
  bio = 600
)

.tbl_bv_ql_fun <- \(width, type) {
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
        tbl_format(width = width)
    )

  clear_vars()

  .tbls
}

tbl_bv_ql <- .tbl_bv_ql_width |>
  imap(.tbl_bv_ql_fun) |>
  list_flatten(name_spec = "{outer}_{inner}")

easy_out(tbl_bv_ql)
