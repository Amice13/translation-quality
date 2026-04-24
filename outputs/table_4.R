# Correlations between expert raw scores and LLM scores

experts <- results[["experts"]]
machines <- c("gemma3-27b", "cometkiwi_xxl", "xcomet")

expert_scores <- data$experts_1f_z_scores %>%
  dplyr::select(all_of(c(experts, "hash"))) %>%
  mutate(
    experts = rowMeans(dplyr::pick(all_of(experts)))
  ) %>%
  dplyr::select(hash, experts)

machine_scores <- data$machine_1f_z_scores %>%
  dplyr::select(all_of(c(machines, "hash"))) %>%
  mutate(
    experts = rowMeans(dplyr::pick(all_of(machines)))
  ) %>%
  dplyr::select(hash, machines)

expert_and_llm_scores <- expert_scores %>%
  left_join(data$llm_1f_z_scores, by = "hash") %>%
  left_join(machine_scores, by = "hash") %>%
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

means_df <- means %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "Model",
                      values_to = "Mean")

cors_means <- cors %>%
  as_tibble(rownames = "Model") %>%
  filter(Model != "experts") %>%
  dplyr::select(Model, experts) %>%
  dplyr::rename(r = experts) %>%
  left_join(means_df, by = "Model")

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
  means_df,
  experts,
  machine_scores,
  machines
)
