# Standard deviations and complexity

experts <- results[["experts"]]
experts_2fa <- results[["experts_2fa"]]

student_scores <- data$students_1f_z_scores %>%
  mutate(
    students = apply(dplyr::select(., -hash), 1, sd)
  ) %>%
  dplyr::select(hash, students)

expert_scores <- data$experts_1f_z_scores %>%
  dplyr::select(all_of(c(experts, "hash"))) %>%
  mutate(
    experts = apply(dplyr::pick(all_of(experts)), 1, sd)
  ) %>%
  dplyr::select(hash, experts)

fluency_scores <- data$experts_2f_z_fluency %>%
  dplyr::select(all_of(c(experts_2fa, "hash"))) %>%
  mutate(
    fluency = apply(dplyr::pick(all_of(experts_2fa)), 1, sd)
  ) %>%
  dplyr::select(hash, fluency)

adequacy_scores <- data$experts_2f_z_adequacy %>%
  dplyr::select(all_of(c(experts_2fa, "hash"))) %>%
  mutate(
    adequacy = apply(dplyr::pick(all_of(experts_2fa)), 1, sd)
  ) %>%
  dplyr::select(hash, adequacy)

scores <- expert_scores %>%
  left_join(student_scores, by = "hash") %>%
  left_join(fluency_scores, by = "hash") %>%
  left_join(adequacy_scores, by = "hash")

complete <- data$pairs %>%
  left_join(scores, by = "hash")

cors <- complete %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x))) %>%
  dplyr::select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs")

plot_cor(cors)
custom_ggsave(
  result_path("Figure 9 - Text characteristics correlation matrix.png"),
  width = 220
)
results[["text_scores_cors"]] <- cors

rm(
  cors,
  complete,
  experts,
  experts_2fa,
  scores,
  student_scores,
  expert_scores,
  fluency_scores,
  adequacy_scores
)
