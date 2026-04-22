# Correlation plot of correlations between all raw scores

experts <- results[["experts"]]
experts_2fa <- results[["experts_2fa"]]

expert_scores <- data$experts_1f_scores %>%
  dplyr::select(all_of(c(experts, "hash"))) %>%
  mutate(
    experts = rowMeans(dplyr::pick(all_of(experts)))
  ) %>%
  dplyr::select(hash, experts)

expert_adequacy <- data$experts_2f_adequacy %>%
  dplyr::select(all_of(c(experts_2fa, "hash"))) %>%
  mutate(
    adequacy = rowMeans(dplyr::pick(all_of(experts_2fa)))
  ) %>%
  dplyr::select(hash, adequacy)

expert_fluency <- data$experts_2f_fluency %>%
  dplyr::select(all_of(c(experts_2fa, "hash"))) %>%
  mutate(
    fluency = rowMeans(dplyr::pick(all_of(experts_2fa)))
  ) %>%
  dplyr::select(hash, fluency)

raw_scores <- expert_scores %>%
  left_join(expert_adequacy, by = "hash") %>%
  left_join(expert_fluency, by = "hash") %>%
  left_join(
    data$llm_1f_scores %>% rename_with(~ paste0("llm_", .), -1),
    by = "hash"
  ) %>%
  left_join(
    data$llm_2f_adequacy %>% rename_with(~ paste0("a_", .), -1),
    by = "hash"
  ) %>%
  left_join(
    data$llm_2f_fluency %>% rename_with(~ paste0("f_", .), -1),
    by = "hash"
  ) %>%
  dplyr::select(-hash)

cors <- cor(raw_scores,
            use = "pairwise.complete.obs",
            method = "spearman"
)

plot_cor(cors)

results[["raw_correlations"]] <- cors

custom_ggsave(
  result_path("Figure 7 - Raw scores correlations between experts and LLM.png"),
  width = 150,
  ratio = 1
)

results[["correlations_llm_experts"]] <- cors

rm(
  experts,
  experts_2fa,
  expert_scores,
  expert_adequacy,
  expert_fluency,
  raw_scores,
  cors
)
