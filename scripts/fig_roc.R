fig_roc <-
df_sp |>
  roc_curve(.pred_0, truth = rhc) |>
  ggplot() +
  aes(x = 1 - specificity,
      y = sensitivity) +
  geom_line(alpha = 0.7) +
  geom_text(data = df_sp |> roc_auc(.pred_0, truth = rhc),
            mapping = aes(label = glue("AUC={round(.estimate, 2)}")),
            x = 0.2, y = 0.85,
            size = 4,
            family = opts$font) +
  annotation_custom(grob =
                      df_sp |>
                        conf_mat(.pred_class, truth = rhc) |>
                        autoplot(type = "heatmap") |>
                        ggplotGrob(),
                    xmin = 0.47, xmax = 1.05,
                    ymin = -0.05, ymax = 0.55) +
  list(geom_abline(!!!opts$line),
       geom_hline(yintercept = 1, !!!opts$line),
       geom_vline(xintercept = 0, !!!opts$line)) |>
  inject() +
  coord_equal() +
  labs(x = "Non-spécificité",
       y = "Sensibilité") +
  theme_bar(grid = FALSE)

easy_out(fig_roc, size = c(4, 4))
