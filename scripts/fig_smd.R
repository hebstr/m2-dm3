fig_smd <-
compar_df |>
  imap(~ tidy_smd(.df = .x,
                  .vars = all_of(sp_vars),
                  .group = rhc) |>
         mutate(smd = abs(smd),
                group = .y)) |>
  list_c() |>
  ggplot() +
  aes(x = smd,
      y = reorder(variable, smd)) +
  list(geom_vline(xintercept = 0, !!!opts$line[-1]),
       geom_vline(xintercept = 0.1, !!!opts$line)) |>
  inject() +
  geom_line(color = opts$palette[2],
            linewidth = 0.3) +
  geom_point(aes(color = group)) +
  labs(x = "SMD",
       y = "Variable") +
  scale_color_manual(values = rev(opts$palette)) +
  theme_bar(grid = FALSE)

easy_out(fig_smd, size = c(6.5, 5))
