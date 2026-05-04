fig_uv <- \(type) {
  .size <- list(
    demo = c(10, 7.5),
    atcd = c(7.5, 8.5),
    pv = c(12.5, 7),
    bio = c(6.5, 7)
  )

  fig <- .bv$group[[type]] |>
    map(
      ~ df_setup |>
        ggcount(var = ., size = 3, color = opts$palette[2]) +
        scale_y_continuous(n.breaks = 6)
    ) |>
    wrap_plots(ncol = 2) +
    plot_layout(axis_titles = "collect_y")

  easy_out(
    x = fig,
    filename = str_glue("fig_uv_{type}"),
    height = .size[[type]][1],
    width = .size[[type]][2]
  )
}

walk(names(.bv$group), fig_uv)
