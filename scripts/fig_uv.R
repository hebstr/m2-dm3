.fig_uv_size <- list(
  demo = c(10, 7.5),
  atcd = c(7.5, 8.5),
  pv = c(12.5, 7),
  bio = c(6.5, 7)
)

fig_uv <- .fig_uv_size |>
  imap(
    \(size, type) {
      .bv$group[[type]] |>
        map(
          ~ df_setup |>
            ggcount(var = ., size = 3, color = opts$palette[2]) +
            scale_y_continuous(n.breaks = 6)
        ) |>
        wrap_plots(ncol = 2) +
        plot_layout(axis_titles = "collect_y")
    }
  )

iwalk(
  fig_uv,
  \(fig, type) {
    easy_out(
      x = fig,
      filename = "fig_uv",
      suffix = type,
      height = .fig_uv_size[[type]][1],
      width = .fig_uv_size[[type]][2],
      export = TRUE
    )
  }
)
