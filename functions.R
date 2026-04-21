result_path <- function(...) file.path(RESULTS_FOLDER, ...)

# Provides basic stats about the defined variable
custom_describe <- function(data, variable) {
  data %>%
    summarise(
      n = n(),
      mean = mean({{ variable }}, na.rm = TRUE),
      sd = sd({{ variable }}, na.rm = TRUE),
      min = min({{ variable }}, na.rm = TRUE),
      q05 = quantile({{ variable }}, 0.05, na.rm = TRUE),
      q25 = quantile({{ variable }}, 0.25, na.rm = TRUE),
      median = quantile({{ variable }}, 0.50, na.rm = TRUE),
      q75 = quantile({{ variable }}, 0.75, na.rm = TRUE),
      q95 = quantile({{ variable }}, 0.95, na.rm = TRUE),
      max = max({{ variable }}, na.rm = TRUE),
      .groups = "drop"
    )
}

# Dataframe normalization
normalize_df <- function(df, digits = 2) {
  df %>%
    mutate(
      across(where(is.numeric), ~ round(.x, digits)),
      across(where(is.factor), as.character)
    )
}

is_primitive <- function(x) {
  is.atomic(x) && length(x) == 1 && !is.null(x)
}

# Provides the statistics about the interrater reliability and agreement
get_ira_irr <- function(data, n, type) {
  cols <- paste0("Expert ", n, c("_1", "_2"))
  scores <- data %>% dplyr::select(all_of(cols))

  icc_res <- irr::icc(
    as.matrix(scores),
    model = "twoway",
    type  = "consistency",
    unit  = "single"
  )

  score_diff <- scores[[2]] - scores[[1]]

  bias <- mean(score_diff, na.rm = TRUE)
  sd_diff <- sd(score_diff, na.rm = TRUE)

  upper <- bias + 1.96 * sd_diff
  lower <- bias - 1.96 * sd_diff

  t_res <- t.test(scores[[1]], scores[[2]], paired = TRUE)

  data.frame(
    evaluator = paste0("Expert ", n),
    type = type,
    ICC = icc_res$value,
    ICC_lower_bound = icc_res$lbound,
    ICC_upper_bound = icc_res$ubound,
    ICC_p = icc_res$p.value,
    ICC_F = icc_res$Fvalue,
    bias = bias,
    sd_diff = sd_diff,
    loa_lower = lower,
    loa_upper = upper,
    t_statistic = t_res$statistic,
    t_p_value = t_res$p.value
  )
}

# Get inter-rater ICC for a group of experts
get_group_icc <- function(data, group) {
  icc_res <- irr::icc(
    data %>% dplyr::select(-any_of(c("hash", "sd_norm", "average"))),
    model = "twoway",
    type = "agreement",
    unit = "single"
  )
  data.frame(
    group = group,
    ICC = icc_res$value,
    ICC_lower_bound = icc_res$lbound,
    ICC_upper_bound = icc_res$ubound,
    ICC_p = icc_res$p.value,
    ICC_F = icc_res$Fvalue
  )
}

get_expert_influence <- function(data) {
  scores <- data %>% dplyr::select(-any_of(c("hash", "sd_norm", "average")))
  experts <- colnames(scores)
  current_results <- lapply(experts, function(expert) {
    tmp <- scores %>% dplyr::select(-all_of(expert))

    icc_res <- irr::icc(
      tmp,
      model = "twoway",
      type = "agreement",
      unit = "single"
    )

    data.frame(
      removed_expert = expert,
      ICC = icc_res$value,
      lower = icc_res$lbound,
      upper = icc_res$ubound
    )
  })

  leave_one_out <- bind_rows(current_results)
  leave_one_out
}

# Function to plot correlation matrix
plot_cor <- function (cors) {
  plot_data <- melt(cors)
  colnames(plot_data) <- c("x", "y", "value")
  plot_data <- plot_data %>% mutate(label = sprintf("%.3f", value))
  
  ggplot(plot_data, aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(colours = c("#0096FF", "white", "red"),
                         values = scales::rescale(c(0.1, 0.5, 1))) +
    geom_tile(color = "black") + coord_fixed() +
    guides(fill = guide_colourbar(title = "Correlation")) +
    geom_text(aes(label = label), color = "black", size = 3) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )  
}
