# Chart of relations between ICC and duration

irr_ira <- results[["irr_ira"]]
duration_stats <- results[["duration_stats"]]

irr_ira %>%
  left_join(duration_stats, by = "evaluator") %>%
  ggplot(aes(x = median, y = ICC, color = type)) +
  labs(color = "Exercise type", x = "Median time") +
  geom_point()

custom_ggsave(result_path("Figure 1 - ICC vs Duration.png"))

rm(irr_ira, duration_stats)
