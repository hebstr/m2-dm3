.fig_surv_fun <- \(x, y) {
  .surv <-
    lst(
      data = mutate(
        .data = x,
        tt_death = if_else(tt_death >= 30, 30, tt_death, missing = 30)
      ),
      model = lst(
        fit = exprs(Surv(tt_death, ev_death_30) ~ rhc, data = data),
        tte = lst(
          obj = do.call("survfit2", fit),
          tidy = tidy_survfit(obj) |>
            split(~strata) |>
            map(
              ~ . |>
                unite(
                  "n.event/censor",
                  n.event:n.censor,
                  sep = "/",
                  remove = TRUE
                ) |>
                unite(
                  "cum.event/censor",
                  cum.event:cum.censor,
                  sep = "/",
                  remove = TRUE
                ) |>
                merge_estim_ci(ci_data = opts$ci$data) |>
                select(time, starts_with(c("n", "cum")), estimate_ci)
            )
        ),
        cox = lst(
          obj = do.call("coxph", fit),
          tidy = tidy(obj, exponentiate = TRUE, conf.int = TRUE) |>
            merge_estim_ci(ci_data = opts$ci$data) |>
            mutate(
              str = "Hazard ratio pour le décès",
              p.value = style_pvalue(p.value, prepend_p = TRUE)
            )
        )
      )
    )

  .fig <-
    .surv$model$tte$obj |>
    ggsurvfit() +
    add_risktable(
      risktable_stats = c("{n.risk} ({cum.event})"),
      stats_label = list("{n.risk} ({cum.event})" = "No. at Risk (Events)"),
      risktable_group = "risktable_stats",
      size = 2,
      family = opts$font$alpha,
      theme = theme_risktable(plot_margin = 5)
    ) +
    add_risktable_strata_symbol(symbol = "\U2014", size = 8) +
    geom_text(
      data = .surv$model$cox$tidy,
      mapping = aes(
        label = str_glue(
          "{str} {opts$ci$label}{opts$sep$int}{estimate_ci}
          Log rank {p.value}"
        ),
        x = 30,
        y = 1
      ),
      size = 2.25,
      hjust = 1,
      vjust = 1,
      family = opts$font$alpha
    ) +
    scale_color_manual(values = opts$palette) +
    scale_ggsurvfit(
      x_scales = list(
        name = "Durée jusqu'au décès (jours)",
        breaks = seq(0, 30, by = 5),
        expand = expansion(mult = 0.05)
      ),
      y_scales = list(
        name = "Probabilité de survie (%)",
        labels = style_percent,
        breaks = seq(0, 1, by = 0.2)
      )
    ) +
    theme_tte()
}

fig_surv <- imap(compar_df, .fig_surv_fun)

easy_out(fig_surv, height = 3.25, width = 6, export = TRUE)
