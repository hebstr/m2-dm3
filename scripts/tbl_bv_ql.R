tbl_bv_ql <- \(type) {

  .width <-
  list(demo = 630,
       atcd = 680,
       pv = 700,
       bio = 600)

  .bv$vars |>
    map(~ df_setup |>
          select(with(.bv, c(vars, group[[type]]))) |>
          use_vars() |>
          tbl_summary(by = all_of(.),
                      statistic = opts$vars$stat,
                      digits = opts$digits,
                      missing = "no") |>
          add_stat_label(label = opts$vars$label) |>
          gtsum_format() |>
          gt_format(width = .width[[type]]) |>
          easy_out(filename = glue("tbl_bv_{.}_{type}")))

}

map(names(.bv$group), tbl_bv_ql)

clear_vars()
