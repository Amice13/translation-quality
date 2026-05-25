# Data sources

expert_scores <- read.csv("data/expert-scores.csv")
machine_scores <- read.csv("data/machine-scores.csv")
source_pairs <- read.csv("data/pairs.csv")
llm_scores <- read.csv("data/llm-scores.csv")

# Data enrichment

enriched_expert_scores <- expert_scores %>%
  mutate(
    evaluator = factor(evaluator, levels = paste0("Expert ", 1:12)),
    evaluator_type = factor(evaluator_type)
  ) %>%
  group_by(evaluator) %>%
  mutate(
    z_score = as.numeric(scale(answer)),
    z_score_fluency = as.numeric(scale(fluency)),
    z_score_adequacy = as.numeric(scale(adequacy)),
  ) %>%
  ungroup()

# Metricx23, Metricx24 and gemma3-27B were measured with the different scales
enriched_machine_scores <- machine_scores %>%
  group_by(evaluator) %>%
  mutate(
    answer = score,
    answer = case_when(
      evaluator == "metricx23" ~ 1 - answer / 25,
      evaluator == "metricx24" ~ 1 - answer / 25,
      evaluator == "gemma3-27b" ~ answer / 100,
      TRUE ~ answer
    ),
    z_score = as.numeric(scale(answer))
  ) %>%
  dplyr::select(-score) %>%
  ungroup()

#LLM as a judge scores
enriched_llm_scores <- llm_scores %>%
  filter(!is.na(answer)) %>%
  mutate(
    z_score = as.numeric(scale(answer))
  )

enriched_llm_fluency_adequacy <- llm_scores %>%
  filter(is.na(answer)) %>%
  mutate(
    z_score_fluency = as.numeric(scale(fluency)),
    z_score_adequacy = as.numeric(scale(adequacy)),
  )

# Generate results
result <- bind_rows(
  enriched_expert_scores,
  enriched_machine_scores,
  enriched_llm_scores,
) %>%
  left_join(source_pairs, by = "hash")

write.csv(result, 'all-data.csv')
