# Setup environment

source("setup.R")
source("dependencies.R")
source("config.R")
source("data.R")
source("functions.R")

# Get stats about the duration of the exercise

duration_stats <-
  custom_describe(
    data$enriched_expert_scores %>%
      filter(duration < 120) %>%
      group_by(evaluator),
    duration
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

# Assessment of inter-rater reliability
irr_ira_single <- do.call(
  rbind,
  lapply(
    c(4:7, "average"),
    function(x) get_ira_irr(data$experts_single_icc, x, "Single score")
  )
)

irr_ira_fluency <- do.call(
  rbind,
  lapply(
    c(8:10, 12, "average"),
    function(x) get_ira_irr(data$experts_2f_fluency_icc, x, "Fluency")
  )
)

irr_ira_adequacy <- do.call(
  rbind,
  lapply(
    c(8:10, 12, "average"),
    function(x) {
      get_ira_irr(
        data$experts_2f_adequacy_icc, x, "Adequacy"
      )
    }
  )
)

irr_ira <- bind_rows(
  irr_ira_single,
  irr_ira_fluency,
  irr_ira_adequacy
)

rm(
  irr_ira_single,
  irr_ira_fluency,
  irr_ira_adequacy
)

irr_results <- irr_ira %>%
  left_join(duration_stats, by = "evaluator") %>%
  left_join(data$expert_batch_difference, by = "evaluator") %>%
  mutate(
    `Expert` = evaluator,
    `Average Time (s)` = sprintf("%.2f", mean),
    `ICC` = sprintf("%.2f", ICC),
    `ICC (CI)` = sprintf("(%.2f; %.2f)", ICC_lower_bound, ICC_upper_bound),
    `Bias` = sprintf("%.2f", bias),
    `Bias (CI)` = sprintf("(%.2f; %.2f)", loa_lower, loa_upper),
    `Paired T` = round(t_statistic, 2),
    `P value` = signif(t_p_value, 2),
    `First–last batch interval` = signif(diff, 2)
  ) %>%
  dplyr::select(
    `Expert`,
    `Average Time (s)`,
    `ICC`,
    `ICC (CI)`,
    `Bias`,
    `Bias (CI)`,
    `Paired T`,
    `P value`,
    `First–last batch interval`
  )

write.csv(
  irr_results,
  file = "./results/Table 1 - IRR Results.csv",
  row.names = FALSE
)
write.csv(
  duration_stats,
  file = "./results/Table 2 - Test duration.csv",
  row.names = FALSE
)

irr_ira %>%
  left_join(duration_stats, by = "evaluator") %>%
  ggplot(aes(x = median, y = ICC, color = type)) +
  labs(color = "Exercise type", x = "Median time") +
  geom_point()

custom_ggsave("./results/Figure 1 - ICC vs Duration.png")

results[["irr_results"]] <- irr_results
results[["duration_stats"]] <- duration_stats
rm(irr_results, duration_stats, irr_ira)


# Assessment of inter-rater reliability

irr <- bind_rows(
  get_group_icc(data$machine_1f_scores, "Machine"),
  get_group_icc(data$machine_1f_z_scores, "Machine z"),
  get_group_icc(data$students_1f_perc, "Machine perc"),
  get_group_icc(data$students_1f_norm, "Machine norm"),
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
  get_group_icc(data$experts_2f_adequacy, "Adequacy"),
  get_group_icc(data$experts_2f_z_adequacy, "Adequacy z"),
  get_group_icc(data$experts_2f_adequacy_perc, "Adequacy perc"),
  get_group_icc(data$experts_2f_adequacy_norm, "Adequacy norm")
)

irr <- irr %>%
  mutate(
    group = factor(group, levels = c(
      "Machine",
      "Student",
      "Expert",
      "Fluency",
      "Adequacy",
      "Machine z",
      "Student z",
      "Expert z",
      "Fluency z",
      "Adequacy z",
      "Machine perc",
      "Student perc",
      "Expert perc",
      "Fluency perc",
      "Adequacy perc",
      "Machine norm",
      "Student norm",
      "Expert norm",
      "Fluency norm",
      "Adequacy norm"
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

results[["irr"]] <- irr
custom_ggsave("./results/Figure 2 - Inter-rater ICC (all experts).png")
rm(irr)

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

custom_ggsave("./results/Figure 3 - Machine influence.png")

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

custom_ggsave("./results/Figure 3 - Student influence.png")

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

custom_ggsave("./results/Figure 3 - Expert influence.png")

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

custom_ggsave("./results/Figure 3 - Fluency influence.png")

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

custom_ggsave("./results/Figure 3 - Adequacy influence.png")

# Machine evaluations clustering
scores <- data$machine_1f_z_scores %>%
  dplyr::select(-hash)

machine_cluster <- hclust(
  as.dist(1 - cor(scores, method = "spearman")),
  method = "average"
)
ggdendrogram(machine_cluster, rotate = TRUE, size = 2)
results[["machine_cluster"]] <- machine_cluster

custom_ggsave("./results/Figure 4 - Machine models dendrogram.png")
rm(scores, machine_cluster)

# Grouped scores

experts <- c("Expert 5", "Expert 6", "Expert 7")
experts_2fa <- c("Expert 8", "Expert 9", "Expert 10", "Expert 12")
cometkiwi_wmt <- c("cometkiwi_xl", "cometkiwi_xxl", "wmt22")
xcomet_metricx24 <- c("xcomet", "metricx24")

irr <- bind_rows(
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
  )
)

irr <- irr %>%
  mutate(
    group = factor(group, levels = c(
      "Students",
      "Fluency",
      "Adequacy",
      "Experts",
      "No bicleaner_ai_score",
      "Xcomet+Metricx24",
      "2 Cometkiwi+Wmt22"
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

custom_ggsave("./results/Figure 5 - Updated ICC.png")

write.csv(
  irr,
  file = "./results/Table 3 - ICC Revised.csv",
  row.names = FALSE
)
results[["updated_irr"]] <- irr
rm(irr)

# Average scores

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

scores <- expert_scores %>%
  left_join(student_scores, by = "hash") %>%
  left_join(fluency_scores, by = "hash") %>%
  left_join(adequacy_scores, by = "hash") %>%
  left_join(cometkiwi_wmt_scores, by = "hash") %>%
  left_join(xcomet_metricx24_scores, by = "hash") %>%
  left_join(machine_scores, by = "hash") %>%
  left_join(bicleaner_ai_score, by = "hash")

complete <- data$pairs %>%
  left_join(scores, by = "hash") %>%
  mutate(holistic = experts)

rm(
  cometkiwi_wmt,
  xcomet_metricx24
)

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
      bicleaner_ai
    ),
  use = "pairwise.complete.obs",
  method = "spearman"
)

plot_cor(cors)

custom_ggsave("./results/Figure 6 - Group correlation matrix.png", width = 180)
results[["group_cors"]] <- cors

rm(
  student_scores,
  expert_scores,
  fluency_scores,
  adequacy_scores,
  machine_scores,
  xcomet_metricx24_scores,
  cometkiwi_wmt_scores,
  bicleaner_ai_score,
  cors
)

# Scatter plot between scores

cor_scores <- scores %>%
  dplyr::select(-hash, -bicleaner_ai, -xcomet_metricx24, -cometkiwi_wmt)

levels <- c("machines", "students", "experts", "adequacy", "fluency")

df_long <- expand.grid(x = levels, y = levels, stringsAsFactors = FALSE) %>%
  left_join(
            cor_scores %>%
              mutate(id = row_number()), by = character()) %>%
  pivot_longer(
    cols = all_of(levels), names_to = "var", values_to = "value"
  ) %>%
  group_by(x, y, id) %>%
  summarise(xval = value[var == x],
            yval = value[var == y], .groups = "drop") %>%
  mutate(
    x = factor(x, levels = levels),
    y = factor(y, levels = levels),
  )

ggplot(df_long, aes(x = xval, y = yval)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(y ~ x, scales = "free") +
  theme_minimal() +
  labs(x = NULL, y = NULL)

results[["scatterplot_data"]] <- df_long
custom_ggsave("./results/Figure 7 - Scatterplots for scores.png", width = 180)
rm(cor_scores, levels, df_long)

# Standard deviations and complexity

student_scores <- data$students_1f_scores %>%
  mutate(
    students = apply(dplyr::select(., -hash), 1, sd)
  ) %>%
  dplyr::select(hash, students)

expert_scores <- data$experts_1f_scores %>%
  dplyr::select(all_of(c(experts, "hash"))) %>%
  mutate(
    experts = apply(dplyr::pick(all_of(experts)), 1, sd)
  ) %>%
  dplyr::select(hash, experts)

fluency_scores <- data$experts_2f_fluency %>%
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

rm(
  student_scores,
  expert_scores,
  fluency_scores,
  adequacy_scores
)

complete <- data$pairs %>%
  left_join(scores, by = "hash")

cors <- complete %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x))) %>%
  dplyr::select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs")

plot_cor(cors)
custom_ggsave(
  "./results/Figure 8 - Text characteristics correlation matrix.png",
  width = 220
)
results[["text_scores_cors"]] <- cors

rm(
  cors,
  complete,
  experts,
  experts_2fa
)

# Expert learning

model <- lmer(
  duration ~ order + (1 | hash) + (1 | evaluator),
  data = data$enriched_expert_scores %>%
    filter(duration < 120)
)

writeLines(
  capture.output(summary(model)),
  "./results/Model 1 - Expert learning.txt"
)
results[["model_expert_learning"]] <- model

# Expert shift

model <- lmer(
  answer ~ order + (1 | hash) + (1 | evaluator),
  data = data$enriched_expert_scores %>%
    filter(duration < 120)
)
summary(model)
writeLines(
  capture.output(summary(model)),
  "./results/Model 2 - Expert shift.txt"
)
results[["model_expert_shift"]] <- model

# Expert duration
model <- lmer(
  log(duration) ~ evaluator_type + (1 | hash) + (1 | evaluator),
  data = data$enriched_expert_scores %>%
    filter(duration < 120)
)
summary(model)
writeLines(
  capture.output(summary(model)),
  "./results/Model 3 - Expert duration.txt"
)
results[["model_expert_duration"]] <- model

rm(scores, model)

source("tests.R")
