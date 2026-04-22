# Influence of the duration on the resulting scores by experts

model <- lmer(
  log(duration) ~ evaluator_type + (1 | hash) + (1 | evaluator),
  data = data$enriched_expert_scores %>%
    filter(duration < 120)
)

writeLines(
  capture.output(summary(model)),
  result_path("Model 3 - Expert duration.txt")
)

results[["model_expert_duration"]] <- model

rm(model)
