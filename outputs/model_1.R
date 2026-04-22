# Expert learning during the evaluation

model <- lmer(
  duration ~ order + (1 | hash) + (1 | evaluator),
  data = data$enriched_expert_scores %>%
    filter(duration < 120)
)

writeLines(
  capture.output(summary(model)),
  result_path("Model 1 - Expert learning.txt")
)

results[["model_expert_learning"]] <- model

rm(
  model
)
