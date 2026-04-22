# Get stats about the duration of the exercise

duration_stats <-
  custom_describe(
    data$enriched_expert_scores %>%
      filter(duration < 120) %>%
      group_by(evaluator),
    duration
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

results[["duration_stats"]] <- duration_stats

write.csv(
  duration_stats,
  file = result_path("Table 2 - Test duration.csv"),
  row.names = FALSE
)

rm(duration_stats)
