# Correlations between expert raw scores and LLM scores

experts <- results[["experts"]]

expert_scores <- data$experts_1f_scores %>%
  dplyr::select(all_of(c(experts, "hash"))) %>%
  mutate(
    experts = rowMeans(dplyr::pick(all_of(experts)))
  ) %>%
  dplyr::select(hash, experts)

expert_and_llm_scores <- expert_scores %>%
  left_join(data$llm_1f_scores, by = "hash") %>%
  dplyr::select(-hash)

cors <- cor(
  expert_and_llm_scores,
  use = "pairwise.complete.obs",
  method = "spearman"
)

means <- data$llm_1f_scores %>%
  dplyr::select(-hash) %>%
  dplyr::summarise(
    dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE))
  )

cors_means <- cors %>%
  as_tibble(rownames = "Model") %>%
  filter(Model != "experts") %>%
  dplyr::select(Model, experts) %>%
  mutate(
    r = experts,
    Mean = as.numeric(means[Model])
  ) %>%
  dplyr::select(
    `Model`,
    `r`,
    `Mean`,
  )

results[["cors_means"]] <- cors_means

write.csv(
  cors_means,
  file = result_path("Table 4 - LLM and Holistic correlations.csv"),
  row.names = FALSE
)

rm(
  cors,
  means,
  cors_means,
  expert_scores,
  expert_and_llm_scores,
  experts
)
