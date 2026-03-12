# Setup environment
if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}
library("rstudioapi")
SOURCE_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(SOURCE_FOLDER)

source("dependencies.R")
source("config.R")
source("data.R")
source("functions.R")

# Get stats about the duration of the exericse

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
    function(x) get_ira_irr(data$experts_single_icc, x, 'Single score')
  )
)

irr_ira_fluency <- do.call(
  rbind,
  lapply(
    c(8:10, 12, "average"),
    function(x) get_ira_irr(data$experts_two_factors_fluency_icc, x, "Fluency")
  )
)

irr_ira_adequacy <- do.call(
  rbind,
  lapply(
    c(8:10, 12, "average"),
    function(x) get_ira_irr(data$experts_two_factors_adequacy_icc, x, "Adequacy")
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
  mutate(
    `Expert` = evaluator,
    `Average Time (s)` = sprintf("%.2f", mean),
    `ICC` = sprintf("%.2f", ICC),
    `ICC (CI)` = sprintf("(%.2f; %.2f)",ICC_lower_bound, ICC_upper_bound),
    `Bias` = sprintf("%.2f", bias),
    `Bias (CI)` = sprintf("(%.2f; %.2f)", loa_lower, loa_upper),
    `Paired T` = round(t_statistic, 2),
    `P value` = signif(t_p_value, 2)
  ) %>%
  dplyr::select(
    `Expert`,
    `Average Time (s)`,
    `ICC`,
    `ICC (CI)`,
    `Bias`,
    `Bias (CI)`,
    `Paired T`,
    `P value`
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
  ggplot(aes(x = median, y = ICC, color = type )) + 
  labs(color ="Exercise type", x = "Median time") +
  geom_point()

custom_ggsave("./results/Figure 1 - ICC vs Duration.png")
rm(irr_results, duration_stats, irr_ira)


# Assessment of inter-rater reliability

irr <- bind_rows(
  get_group_icc(data$machine_1F_scores, "Machine"),
  get_group_icc(data$machine_1F_z_score, "Machine z"),
  get_group_icc(data$students_1F_percentiles, "Machine perc"),
  get_group_icc(data$students_1F_norm_scores, "Machine norm"),
  get_group_icc(data$students_1F_scores, "Student"),
  get_group_icc(data$students_1F_z_scores, "Student z"),
  get_group_icc(data$students_1F_percentiles, "Student perc"),
  get_group_icc(data$students_1F_norm_scores, "Student norm"),
  get_group_icc(data$experts_1F_scores, "Expert"),
  get_group_icc(data$experts_1F_z_scores, "Expert z"),
  get_group_icc(data$experts_1F_percentiles, "Expert perc"),
  get_group_icc(data$experts_1F_norm_scores, "Expert norm"),
  get_group_icc(data$experts_2F_fluency, "Fluency"),
  get_group_icc(data$experts_2F_z_fluency, "Fluency z"),
  get_group_icc(data$experts_2F_fluency_percentiles, "Fluency perc"),
  get_group_icc(data$experts_2F_fluency_norm_scores, "Fluency norm"),
  get_group_icc(data$experts_2F_adequacy, "Adequacy"),
  get_group_icc(data$experts_2F_z_adequacy, "Adequacy z"),
  get_group_icc(data$experts_2F_adequacy_percentiles, "Adequacy perc"),
  get_group_icc(data$experts_2F_adequacy_norm_scores, "Adequacy norm")
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
  geom_errorbar(aes(ymin = ICC_lower_bound, ymax = ICC_upper_bound), width = 0.1) +
  coord_flip() +
  labs(x = "Group")

custom_ggsave("./results/Figure 2 - Inter-rater ICC (all experts).png")

# Remove expert analysis

ggplot(
  get_expert_influence(data$machine_1F_z_scores),
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed model") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave("./results/Figure 3 - Machine influence.png")

ggplot(
  get_expert_influence(data$students_1F_z_scores),
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed expert") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave("./results/Figure 3 - Student influence.png")

ggplot(
  get_expert_influence(data$experts_1F_z_scores),
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed expert") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave("./results/Figure 3 - Expert influence.png")

ggplot(
  get_expert_influence(data$experts_2F_z_fluency),
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed expert") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave("./results/Figure 3 - Fluency influence.png")

ggplot(
  get_expert_influence(data$experts_2F_z_adequacy),
  aes(x = removed_expert, y = ICC)
) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() +
  xlab("Removed expert") +
  ylab("ICC (with 95% confidence interval)")

custom_ggsave("./results/Figure 3 - Adequacy influence.png")
rm(irr)

# Machine evaluations clustering

scores <- data$machine_1F_z_scores %>%
  dplyr::select(-hash, -sd_norm, -average)

hc <- hclust(as.dist(1 - cor(scores, method = "spearman")), method = "average")
ggdendrogram(hc, rotate = TRUE, size = 2)

custom_ggsave("./results/Figure 4 - Machine models dendrogram.png")
rm(scores, hc)

# Grouped scores

experts <- c("Expert 5", "Expert 6", "Expert 7")
experts_2FA <- c("Expert 8", "Expert 9", "Expert 10", "Expert 12")
cometkiwi_wmt <- c("cometkiwi_xl", "cometkiwi_xxl", "wmt22")
xcomet_metricx24 <- c("xcomet", "metricx24")

irr <- bind_rows(
  get_group_icc(data$students_1F_z_scores, "Students"),
  get_group_icc(data$experts_1F_z_scores %>% dplyr::select(all_of(experts)), "Experts"),
  get_group_icc(data$experts_2F_z_adequacy %>% dplyr::select(all_of(experts_2FA)), "Adequacy"),
  get_group_icc(data$experts_2F_z_fluency %>% dplyr::select(all_of(experts_2FA)), "Fluency"),
  get_group_icc(data$machine_1F_z_scores %>% dplyr::select(all_of(cometkiwi_wmt)), "2 Cometkiwi+Wmt22"),
  get_group_icc(data$machine_1F_z_scores %>% dplyr::select(all_of(xcomet_metricx24)), "Xcomet+Metricx24"),
  get_group_icc(data$machine_1F_z_scores %>% dplyr::select(-bicleaner_ai_score, -sd_norm, -average), "No bicleaner_ai_score")
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
  geom_errorbar(aes(ymin = ICC_lower_bound, ymax = ICC_upper_bound), width = 0.1) +
  coord_flip() +
  labs(x = "Group")

custom_ggsave("./results/Figure 5 - Updated ICC.png")

write.csv(
  irr,
  file = "./results/Table 3 - ICC Revised.csv",
  row.names = FALSE
)

rm(irr)

# Average scores

student_scores <- data$students_1F_z_scores %>%
  mutate(
    students = rowMeans(dplyr::pick(-hash, -sd_norm, -average))
  ) %>%
  dplyr::select(hash, students)

expert_scores <- data$experts_1F_z_scores %>%
  dplyr::select(all_of(c(experts, "hash"))) %>%
  mutate(
    experts = rowMeans(dplyr::pick(experts))
  ) %>%
  dplyr::select(hash, experts)

fluency_scores <- data$experts_2F_z_fluency %>%
  dplyr::select(all_of(c(experts_2FA, "hash"))) %>%
  mutate(
    fluency = rowMeans(dplyr::pick(all_of(experts_2FA)))
  ) %>%
  dplyr::select(hash, fluency)

adequacy_scores <- data$experts_2F_z_adequacy %>%
  dplyr::select(all_of(c(experts_2FA, "hash"))) %>%
  mutate(
    adequacy = rowMeans(dplyr::pick(all_of(experts_2FA)))
  ) %>%
  dplyr::select(hash, adequacy)

cometkiwi_wmt_scores <- data$machine_1F_z_scores %>%
  dplyr::select(all_of(c(cometkiwi_wmt, "hash"))) %>%
  mutate(
    cometkiwi_wmt = rowMeans(dplyr::pick(all_of(cometkiwi_wmt)))
  ) %>%
  dplyr::select(hash, cometkiwi_wmt)

xcomet_metricx24_scores <- data$machine_1F_z_scores %>%
  dplyr::select(all_of(c(xcomet_metricx24, "hash"))) %>%
  mutate(
    xcomet_metricx24 = rowMeans(dplyr::pick(all_of(xcomet_metricx24)))
  ) %>%
  dplyr::select(hash, xcomet_metricx24)

machine_scores <- data$machine_1F_z_scores %>%
  dplyr::select(-bicleaner_ai_score) %>%
  mutate(
    machines = rowMeans(dplyr::pick(-any_of(c("bicleaner_ai_scorea", "hash", "sd_norm", "average"))))
  ) %>%
  dplyr::select(hash, machines)

bicleaner_ai_score <- data$machine_1F_z_scores %>%
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
  left_join(scores, by = "hash")

# Correlation matrix between groups

cors <- cor(
  complete %>%
    dplyr::select(
      students,
      experts,
      fluency,
      adequacy,
      cometkiwi_wmt,
      xcomet_metricx24,
      machines,
      bicleaner_ai
    ),
  use = "pairwise.complete.obs"
)

plot_data <- melt(cors)
colnames(plot_data) <- c("x", "y", "value")

plot_data$label <- sprintf("%.3f", plot_data$value)

ggplot(plot_data, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colours = c("#0096FF", "white", "red"),
                       values = scales::rescale(c(0.1, 0.5, 1))) +
  geom_tile(color = "black") + coord_fixed() +
  guides(fill = guide_colourbar(title = "Correlation")) +
  geom_text(aes(label = label), color = "black", size = 3) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

custom_ggsave("./results/Figure 6 - Group correlation matrix.png", width = 180)
rm(
  student_scores,
  expert_scores,
  fluency_scores,
  adequacy_scores,
  machine_scores,
  xcomet_metricx24_scores,
  cometkiwi_wmt_scores,
  bicleaner_ai_score,
  cors,
  plot_data
)

# Scatter plot between scores

cor_scores <- scores %>%
  dplyr::select(-hash, -bicleaner_ai, -xcomet_metricx24, -cometkiwi_wmt)

levels <- c("machines", "students", "experts", "adequacy", "fluency")

df_long <- expand.grid(x = levels, y = levels, stringsAsFactors = FALSE) %>%
  left_join(cor_scores %>% mutate(id = row_number()), by = character()) %>%
  pivot_longer(cols = all_of(levels), names_to = "var", values_to = "value") %>%
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

custom_ggsave("./results/Figure 7 - Scatterplots for scores.png", width = 180)
rm(cor_scores, levels, df_long, plot_data)

# Standard deviations and complexity

student_scores <- data$students_1F_scores %>%
  mutate(
    students = apply(dplyr::select(., -hash, -sd_norm, -average), 1, sd)
  ) %>%
  dplyr::select(hash, students)

expert_scores <- data$experts_1F_scores %>%
  dplyr::select(all_of(c(experts, "hash"))) %>%
  mutate(
    experts = apply(dplyr::pick(experts), 1, sd)
  ) %>%
  dplyr::select(hash, experts)

fluency_scores <- data$experts_2F_fluency %>%
  dplyr::select(all_of(c(experts_2FA, "hash"))) %>%
  mutate(
    fluency = apply(dplyr::pick(experts_2FA), 1, sd)
  ) %>%
  dplyr::select(hash, fluency)

adequacy_scores <- data$experts_2F_z_adequacy %>%
  dplyr::select(all_of(c(experts_2FA, "hash"))) %>%
  mutate(
    adequacy = apply(dplyr::pick(experts_2FA), 1, sd)
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

plot_data <- melt(cors)
colnames(plot_data) <- c("x", "y", "value")
plot_data$label <- sprintf("%.3f", plot_data$value)

ggplot(plot_data, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colours = c("#0096FF", "white", "red"),
                       values = scales::rescale(c(0.1, 0.5, 1))) +
  geom_tile(color = "black") + coord_fixed() +
  guides(fill = guide_colourbar(title = "Correlation")) +
  geom_text(aes(label = label), color = "black", size = 3) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Expert learning

model <- lmer(
  duration ~ order + (1 | hash) + (1 | evaluator),
  data = data$enriched_expert_scores %>%
    filter(duration < 120)
)
summary(model)

# Expert shift

model <- lmer(
  answer ~ order + (1 | hash) + (1 | evaluator),
  data = data$enriched_expert_scores %>%
    filter(duration < 120)
)
summary(model)


model <- lmer(
  log(duration) ~ evaluator_type + (1 | hash) + (1 | evaluator),
  data = data$enriched_expert_scores %>%
    filter(duration < 120)
)

model <- lmer(
  duration ~ evaluator_type + (1 | hash) + (1 | evaluator),
  data = data$enriched_expert_scores
)

model <- lmer(
  z_score ~ evaluator_type + (1 | hash),
  data = data$enriched_expert_scores
)

model <- lmer(
  z_score ~ evaluator_type + (1 | hash) + (1 | evaluator),
  data = data$enriched_expert_scores
)







cors <- complete %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x))) %>%
  dplyr::select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs")

plot_data <- melt(cors)
colnames(plot_data) <- c("x", "y", "value")
plot_data$label <- sprintf("%.3f", plot_data$value)

ggplot(plot_data, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colours = c("#0096FF", "white", "red"),
                       values = scales::rescale(c(0.1, 0.5, 1))) +
  geom_tile(color = "black") + coord_fixed() +
  guides(fill = guide_colourbar(title = "Correlation")) +
  geom_text(aes(label = label), color = "black", size = 3) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

