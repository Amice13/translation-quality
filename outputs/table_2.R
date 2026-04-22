# Assessment of inter-rater reliability

duration_stats <- results[["duration_stats"]]

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

results[["irr_ira"]] <- irr_ira
results[["irr_results"]] <- irr_results

write.csv(
  irr_results,
  file = result_path("Table 2 - IRR Results.csv"),
  row.names = FALSE
)

rm(
  irr_ira_single,
  irr_ira_fluency,
  irr_ira_adequacy,
  irr_ira,
  irr_results,
  duration_stats
)
