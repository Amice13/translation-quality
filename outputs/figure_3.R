# Remove expert analysis

results[["one_out_machine"]] <- get_expert_influence(data$machine_1f_z_scores)
ggplot(
  results[["one_out_machine"]],
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed model") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave(result_path("Figure 3 - Machine influence.png"))

results[["one_out_students"]] <- get_expert_influence(data$students_1f_z_scores)

ggplot(
  results[["one_out_students"]],
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed expert") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave(result_path("Figure 3 - Student influence.png"))

results[["one_out_experts"]] <- get_expert_influence(data$experts_1f_z_scores)

ggplot(
  results[["one_out_experts"]],
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed expert") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave(result_path("Figure 3 - Expert influence.png"))

results[["one_out_fluency"]] <- get_expert_influence(data$experts_2f_z_fluency)

ggplot(
  results[["one_out_fluency"]],
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed expert") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave(result_path("Figure 3 - Fluency influence.png"))

results[["one_out_adequacy"]] <-
  get_expert_influence(data$experts_2f_z_adequacy)

ggplot(
  results[["one_out_adequacy"]],
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed expert") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave(result_path("Figure 3 - Adequacy influence.png"))

results[["one_out_llm"]] <-
  get_expert_influence(data$llm_1f_z_scores)

ggplot(
  results[["one_out_llm"]],
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed LLM") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave(result_path("Figure 3 - LLM influence.png"))

results[["one_out_llm_fluency"]] <-
  get_expert_influence(data$llm_2f_z_fluency)

ggplot(
  results[["one_out_llm_fluency"]],
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed LLM") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave(result_path("Figure 3 - LLM fluency influence.png"))

results[["one_out_llm_adequacy"]] <-
  get_expert_influence(data$llm_2f_z_adequacy)

ggplot(
  results[["one_out_llm_adequacy"]],
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed LLM") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave(result_path("Figure 3 - LLM fluency influence.png"))
