# Scatter plot between scores

scores <- results[["scores"]]

cor_scores <- scores %>%
  dplyr::select(-hash, -bicleaner_ai, -xcomet_metricx24, -cometkiwi_wmt)

levels <- c("machines", "students", "experts", "adequacy", "fluency")

df_long <- expand.grid(x = levels, y = levels, stringsAsFactors = FALSE) %>%
  left_join(cor_scores %>%
              mutate(id = row_number()), by = character()) %>%
  pivot_longer(
    cols = all_of(levels), names_to = "var", values_to = "value"
  ) %>%
  group_by(x, y, id) %>%
  summarise(xval = value[var == x],
            yval = value[var == y], .groups = "drop") %>%
  mutate(
    x = factor(x, levels = levels),
    y = factor(y, levels = levels),
  )

ggplot(df_long, aes(x = xval, y = yval)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(y ~ x, scales = "free") +
  theme_minimal() +
  labs(x = NULL, y = NULL)

custom_ggsave(
  result_path("Figure 8 - Scatterplots for scores.png"),
  width = 180
)

results[["scatterplot_data"]] <- df_long

rm(
  scores,
  cor_scores,
  levels,
  df_long
)
