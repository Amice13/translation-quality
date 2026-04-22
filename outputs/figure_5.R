# Grouped scores

experts <- c("Expert 5", "Expert 6", "Expert 7")
experts_2fa <- c("Expert 8", "Expert 9", "Expert 10", "Expert 12")
cometkiwi_wmt <- c("cometkiwi_xl", "cometkiwi_xxl", "wmt22")
xcomet_metricx24 <- c("xcomet", "metricx24")

updated_irr <- bind_rows(
  get_group_icc(data$students_1f_z_scores, "Students"),
  get_group_icc(data$experts_1f_z_scores %>%
                  dplyr::select(all_of(experts)), "Experts"),
  get_group_icc(data$experts_2f_z_adequacy %>%
                  dplyr::select(all_of(experts_2fa)), "Adequacy"),
  get_group_icc(data$experts_2f_z_fluency %>%
                  dplyr::select(all_of(experts_2fa)), "Fluency"),
  get_group_icc(data$machine_1f_z_scores %>%
                  dplyr::select(all_of(cometkiwi_wmt)), "2 Cometkiwi+Wmt22"),
  get_group_icc(data$machine_1f_z_scores %>%
                  dplyr::select(all_of(xcomet_metricx24)), "Xcomet+Metricx24"),
  get_group_icc(
    data$machine_1f_z_scores %>% dplyr::select(-bicleaner_ai_score),
    "No bicleaner_ai_score"
  ),
  get_group_icc(data$llm_1f_z_scores, "LLM"),
  get_group_icc(data$llm_2f_z_fluency, "LLM fluency"),
  get_group_icc(data$llm_2f_z_adequacy, "LLM adequacy")
)

irr_chart <- updated_irr %>%
  mutate(
    group = factor(group, levels = c(
      "Students",
      "Fluency",
      "Adequacy",
      "Experts",
      "No bicleaner_ai_score",
      "Xcomet+Metricx24",
      "2 Cometkiwi+Wmt22",
      "LLM",
      "LLM fluency",
      "LLM adequacy"
    ))
  )

ggplot(irr_chart, aes(x = group, y = ICC)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = ICC_lower_bound, ymax = ICC_upper_bound),
    width = 0.1
  ) +
  coord_flip() +
  labs(x = "Group")

custom_ggsave(result_path("Figure 5 - Updated ICC.png"))

results[["experts"]] <- experts
results[["experts_2fa"]] <- experts_2fa
results[["cometkiwi_wmt"]] <- cometkiwi_wmt
results[["xcomet_metricx24"]] <- xcomet_metricx24
results[["updated_irr"]] <- updated_irr

rm(
  experts,
  experts_2fa,
  cometkiwi_wmt,
  xcomet_metricx24,
  irr_chart,
  updated_irr
)
