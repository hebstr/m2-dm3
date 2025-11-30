test <- as_mapper(~
  ggplot(.x) +
    aes(y = sp) +
    geom_mirror_histogram(
      mapping = aes(fill = rhc),
      color = "grey95",
      alpha = 0.7,
      binwidth = 0.04
    ) +
    geom_vline(xintercept = 0, alpha = 0.75) +
    geom_hline(yintercept = 0.5, !!!opts$line) |> inject() +
    geom_label(
      data = summarise(
        .data = .x,
        n = n(),
        median = round(median(sp), 2),
        .by = rhc
      ) |>
        arrange(desc(rhc)),
      mapping = aes(
        label = glue("n={n}\nmédiane={median}"),
        y = median
      ),
      x = c(50, -50),
      size = 3,
      family = opts$font,
      color = rev(opts$palette)
    ) +
    labs(
      x = "Effectif",
      y = label_attribute(.x$sp)
    ) +
    scale_x_continuous(labels = abs, n.breaks = 10) +
    scale_y_continuous(limits = -0.02:1) +
    scale_fill_manual(values = opts$palette) +
    theme_bar(panel.background = element_rect(fill = "grey95"))
)

list(global = df_sp, match = df_match) |>
  map(test) |>
  easy_out_map(filename = glue("fig_dist"), size = c(3.5, 6))
