# Analysis of correlations between averaged z-scores

experts <- results[["experts"]]
experts_2fa <- results[["experts_2fa"]]
cometkiwi_wmt <- results[["cometkiwi_wmt"]]
xcomet_metricx24 <- results[["xcomet_metricx24"]]

student_scores <- data$students_1f_z_scores %>%
  mutate(
    students = rowMeans(dplyr::pick(-hash))
  ) %>%
  dplyr::select(hash, students)

expert_scores <- data$experts_1f_z_scores %>%
  dplyr::select(all_of(c(experts, "hash"))) %>%
  mutate(
    experts = rowMeans(dplyr::pick(all_of(experts)))
  ) %>%
  dplyr::select(hash, experts)

fluency_scores <- data$experts_2f_z_fluency %>%
  dplyr::select(all_of(c(experts_2fa, "hash"))) %>%
  mutate(
    fluency = rowMeans(dplyr::pick(all_of(experts_2fa)))
  ) %>%
  dplyr::select(hash, fluency)

adequacy_scores <- data$experts_2f_z_adequacy %>%
  dplyr::select(all_of(c(experts_2fa, "hash"))) %>%
  mutate(
    adequacy = rowMeans(dplyr::pick(all_of(experts_2fa)))
  ) %>%
  dplyr::select(hash, adequacy)

cometkiwi_wmt_scores <- data$machine_1f_z_scores %>%
  dplyr::select(all_of(c(cometkiwi_wmt, "hash"))) %>%
  mutate(
    cometkiwi_wmt = rowMeans(dplyr::pick(all_of(cometkiwi_wmt)))
  ) %>%
  dplyr::select(hash, cometkiwi_wmt)

xcomet_metricx24_scores <- data$machine_1f_z_scores %>%
  dplyr::select(all_of(c(xcomet_metricx24, "hash"))) %>%
  mutate(
    xcomet_metricx24 = rowMeans(dplyr::pick(all_of(xcomet_metricx24)))
  ) %>%
  dplyr::select(hash, xcomet_metricx24)

machine_scores <- data$machine_1f_z_scores %>%
  dplyr::select(-bicleaner_ai_score) %>%
  mutate(
    machines = rowMeans(dplyr::pick(
      -any_of(c("bicleaner_ai_score", "hash", "sd_norm", "average"))
    ))
  ) %>%
  dplyr::select(hash, machines)

bicleaner_ai_score <- data$machine_1f_z_scores %>%
  dplyr::select(bicleaner_ai_score, hash) %>%
  mutate(
    bicleaner_ai = bicleaner_ai_score
  ) %>%
  dplyr::select(hash, bicleaner_ai)

llm_scores <- data$llm_1f_z_scores %>%
  mutate(
    llm = rowMeans(dplyr::pick(-hash))
  ) %>%
  dplyr::select(hash, llm)

llm_fluency <- data$llm_2f_z_fluency %>%
  mutate(
    llm_fluency = rowMeans(dplyr::pick(-hash))
  ) %>%
  dplyr::select(hash, llm_fluency)

llm_adequacy <- data$llm_2f_z_adequacy %>%
  mutate(
    llm_adequacy = rowMeans(dplyr::pick(-hash))
  ) %>%
  dplyr::select(hash, llm_adequacy)

fluency_scores <- data$experts_2f_z_fluency %>%
  dplyr::select(all_of(c(experts_2fa, "hash"))) %>%
  mutate(
    fluency = rowMeans(dplyr::pick(all_of(experts_2fa)))
  ) %>%
  dplyr::select(hash, fluency)

adequacy_scores <- data$experts_2f_z_adequacy %>%
  dplyr::select(all_of(c(experts_2fa, "hash"))) %>%
  mutate(
    adequacy = rowMeans(dplyr::pick(all_of(experts_2fa)))
  ) %>%
  dplyr::select(hash, adequacy)

scores <- expert_scores %>%
  left_join(student_scores, by = "hash") %>%
  left_join(fluency_scores, by = "hash") %>%
  left_join(adequacy_scores, by = "hash") %>%
  left_join(cometkiwi_wmt_scores, by = "hash") %>%
  left_join(xcomet_metricx24_scores, by = "hash") %>%
  left_join(machine_scores, by = "hash") %>%
  left_join(bicleaner_ai_score, by = "hash") %>%
  left_join(llm_scores, by = "hash") %>%
  left_join(llm_fluency, by = "hash") %>%
  left_join(llm_adequacy, by = "hash")

complete <- data$pairs %>%
  left_join(scores, by = "hash") %>%
  mutate(holistic = experts)

# Correlation matrix between groups

cors <- cor(
  complete %>%
    dplyr::select(
      students,
      holistic,
      fluency,
      adequacy,
      cometkiwi_wmt,
      xcomet_metricx24,
      machines,
      bicleaner_ai,
      llm,
      llm_fluency,
      llm_adequacy
    ),
  use = "pairwise.complete.obs",
  method = "spearman"
)

plot_cor(cors)

custom_ggsave(
  result_path("Figure 6 - Group correlation matrix.png"),
  width = 130,
  ratio = 1
)

results[["group_cors"]] <- cors
results[["scores"]] <- scores

rm(
  cors,
  complete,
  student_scores,
  scores,
  fluency_scores,
  adequacy_scores,
  llm_adequacy,
  llm_fluency,
  llm_scores,
  bicleaner_ai_score,
  cometkiwi_wmt_scores,
  xcomet_metricx24_scores,
  expert_scores,
  machine_scores,
  experts,
  experts_2fa,
  cometkiwi_wmt,
  xcomet_metricx24
)
