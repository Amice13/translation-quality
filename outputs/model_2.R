# Shift of expert scores during the evaluation

model <- lmer(
  answer ~ order + (1 | hash) + (1 | evaluator),
  data = data$enriched_expert_scores %>%
    filter(duration < 120)
)

writeLines(
  capture.output(summary(model)),
  result_path("Model 2 - Expert shift.txt")
)

results[["model_expert_shift"]] <- model

rm(
  model
)
