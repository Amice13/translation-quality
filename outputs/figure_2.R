# Assessment of inter-rater reliability

irr <- bind_rows(
  get_group_icc(data$machine_1f_scores, "Machine"),
  get_group_icc(data$machine_1f_z_scores, "Machine z"),
  get_group_icc(data$machine_1f_perc, "Machine perc"),
  get_group_icc(data$machine_1f_norm, "Machine norm"),
  
  get_group_icc(data$llm_1f_scores, "LLM"),
  get_group_icc(data$llm_1f_z_scores, "LLM z"),
  get_group_icc(data$llm_1f_perc, "LLM perc"),
  get_group_icc(data$llm_1f_norm, "LLM norm"),
  
  get_group_icc(data$students_1f_scores, "Student"),
  get_group_icc(data$students_1f_z_scores, "Student z"),
  get_group_icc(data$students_1f_perc, "Student perc"),
  get_group_icc(data$students_1f_norm, "Student norm"),
  
  get_group_icc(data$experts_1f_scores, "Expert"),
  get_group_icc(data$experts_1f_z_scores, "Expert z"),
  get_group_icc(data$experts_1f_perc, "Expert perc"),
  get_group_icc(data$experts_1f_norm, "Expert norm"),
  
  get_group_icc(data$experts_2f_fluency, "Fluency"),
  get_group_icc(data$experts_2f_z_fluency, "Fluency z"),
  get_group_icc(data$experts_2f_fluency_perc, "Fluency perc"),
  get_group_icc(data$experts_2f_fluency_norm, "Fluency norm"),
  
  get_group_icc(data$experts_2f_fluency, "Fluency"),
  get_group_icc(data$experts_2f_z_fluency, "Fluency z"),
  get_group_icc(data$experts_2f_fluency_perc, "Fluency perc"),
  get_group_icc(data$experts_2f_fluency_norm, "Fluency norm"),
  
  get_group_icc(data$llm_2f_fluency, "LLM fluency"),
  get_group_icc(data$llm_2f_z_fluency, "LLM fluency z"),
  get_group_icc(data$llm_2f_fluency_perc, "LLM fluency perc"),
  get_group_icc(data$llm_2f_fluency_norm, "LLM fluency norm"),
  
  get_group_icc(data$experts_2f_adequacy, "Adequacy"),
  get_group_icc(data$experts_2f_z_adequacy, "Adequacy z"),
  get_group_icc(data$experts_2f_adequacy_perc, "Adequacy perc"),
  get_group_icc(data$experts_2f_adequacy_norm, "Adequacy norm"),
  
  get_group_icc(data$llm_2f_adequacy, "LLM adequacy"),
  get_group_icc(data$llm_2f_z_adequacy, "LLM adequacy z"),
  get_group_icc(data$llm_2f_adequacy_perc, "LLM adequacy perc"),
  get_group_icc(data$llm_2f_adequacy_norm, "LLM adequacy norm")
)

irr <- irr %>%
  mutate(
    group = factor(group, levels = c(
      "Machine",
      "Student",
      "Expert",
      "LLM",
      "Fluency",
      "Adequacy",
      "LLM fluency",
      "LLM adequacy",
      "Machine z",
      "LLM z",
      "Student z",
      "Expert z",
      "Fluency z",
      "Adequacy z",
      "LLM adequacy z",
      "LLM fluency z",
      "Machine perc",
      "Student perc",
      "Expert perc",
      "LLM perc",
      "Fluency perc",
      "Adequacy perc",
      "LLM fluency perc",
      "LLM adequacy perc",
      "Machine norm",
      "Student norm",
      "Expert norm",
      "LLM norm",
      "Fluency norm",
      "Adequacy norm",
      "LLM fluency norm",
      "LLM adequacy norm"
    ))
  )

ggplot(irr, aes(x = group, y = ICC)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = ICC_lower_bound, ymax = ICC_upper_bound),
    width = 0.1
  ) +
  coord_flip() +
  labs(x = "Group")

custom_ggsave(
  result_path("Figure 2 - Inter-rater ICC (all experts).png"),
  width = 220
)

results[["irr"]] <- irr

rm(irr)
