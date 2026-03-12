# Helper functions
remove_duplicates <- function (dataset) {
  dataset %>%
    group_by(evaluator, hash) %>%
    slice(1) %>%
    ungroup()
}

create_wide_data <- function(dataset, score) {
  dataset %>%
    dplyr::select(hash, evaluator, {{ score }}) %>%
    tidyr::pivot_wider(names_from = evaluator, values_from = {{ score }}) %>%
    mutate(
      sd_norm = apply(dplyr::select(., -hash), 1, sd, na.rm = TRUE),
      average = rowMeans(dplyr::select(., -hash), na.rm = TRUE)
    )
}

create_retest_data <- function(dataset, score) {
  dataset %>%
    group_by(hash, evaluator) %>%
    filter(n() > 1) %>%
    arrange(start_time, .by_group = TRUE) %>%
    mutate(rep = row_number()) %>%
    ungroup() %>%
    mutate(rater = paste(evaluator, rep, sep = "_")) %>%
    dplyr::select(hash, rater, {{ score }}) %>%
    pivot_wider(names_from = rater, values_from = {{ score }}) %>%
    mutate(
      `Expert average_1` = rowMeans(dplyr::pick(dplyr::matches("_1$")), na.rm = TRUE),
      `Expert average_2` = rowMeans(dplyr::pick(dplyr::matches("_2$")), na.rm = TRUE)
    )
}

# Data sources

expert_scores <- read.csv("data/expert-scores.csv")

enriched_expert_scores <- expert_scores %>%
  mutate(
    evaluator = factor(evaluator, levels = paste0("Expert ", 1:12)),
    evaluator_type = factor(evaluator_type)
  ) %>%
  group_by(evaluator) %>%
  mutate(
    order = order(start_time),
    z_score = as.numeric(scale(answer)),
    percentile = (rank(answer, ties.method = "average") - 0.5) / n(),
    norm_score = qnorm(percentile),
    z_score_fluency = as.numeric(scale(fluency)),
    percentile_fluency = (rank(fluency, ties.method = "average") - 0.5) / n(),
    norm_score_fluency = qnorm(percentile_fluency),
    z_score_adequacy = as.numeric(scale(adequacy)),
    percentile_adequacy = (rank(adequacy, ties.method = "average") - 0.5) / n(),
    norm_score_adequacy = qnorm(percentile_adequacy)
  ) %>%
  ungroup()
  
machine_scores <- read.csv("data/machine-scores.csv")
enriched_machine_scores <- machine_scores %>%
  group_by(evaluator) %>%
  mutate(
    answer = score,
    answer = case_when(
      evaluator == "metricx23" ~ 1 - answer / 25,
      evaluator == "metricx24" ~ 1 - answer / 25,
      evaluator == "gemma3-27b" ~ answer / 100,
      TRUE ~ answer
    ),
    z_score = as.numeric(scale(answer)),
    percentile = (rank(answer, ties.method = "average") - 0.5) / n(),
    norm_score = qnorm(percentile)
  ) %>%
  ungroup()

source_pairs <- read.csv("data/pairs.csv")

# Split source data to sub-groups

students_scores <- enriched_expert_scores %>%
  filter(evaluator_type == "Student") %>%
  arrange(start_time)

experts_single_scores <- enriched_expert_scores %>%
  filter(evaluator_type == "Expert") %>%
  arrange(start_time)

experts_two_factors_scores <- enriched_expert_scores %>%
  filter(evaluator_type == "Expert-2FA") %>%
  arrange(start_time)

# Process machines' scores

machine_1F_scores <- create_wide_data(enriched_machine_scores, answer)
machine_1F_z_scores <- create_wide_data(enriched_machine_scores, z_score)
machine_1F_percentiles <- create_wide_data(enriched_machine_scores, percentile)
machine_1F_norm_scores <- create_wide_data(enriched_machine_scores, norm_score)

# Process students' scores

students_all_scores <- remove_duplicates(students_scores)
students_1F_scores <- create_wide_data(students_all_scores, answer)
students_1F_z_scores <- create_wide_data(students_all_scores, z_score)
students_1F_percentiles <- create_wide_data(students_all_scores, percentile)
students_1F_norm_scores <- create_wide_data(students_all_scores, norm_score)

# Process experts' single score
experts_single_icc <- create_retest_data(experts_single_scores, answer)

## Normalized general scores
experts_1F_all_scores <- remove_duplicates(experts_single_scores)
experts_1F_scores <- create_wide_data(experts_1F_all_scores, answer)
experts_1F_z_scores <- create_wide_data(experts_1F_all_scores, z_score)
experts_1F_percentiles <- create_wide_data(experts_1F_all_scores, percentile)
experts_1F_norm_scores <- create_wide_data(experts_1F_all_scores, norm_score)

# Process experts' fluency and adequacy scores

## Data for estimating the inter-rater reliability for fluency
experts_two_factors_fluency_icc <- create_retest_data(experts_two_factors_scores, fluency)

## Data for estimating the inter-rater reliability for adequacy
experts_two_factors_adequacy_icc <- create_retest_data(experts_two_factors_scores, adequacy)

## Wide data for fluency score
experts_2F_all_scores <- remove_duplicates(experts_two_factors_scores)
experts_2F_fluency <- create_wide_data(experts_2F_all_scores, fluency)
experts_2F_z_fluency <- create_wide_data(experts_2F_all_scores, z_score_fluency)
experts_2F_fluency_percentiles <- create_wide_data(experts_2F_all_scores, percentile_fluency)
experts_2F_fluency_norm_scores <- create_wide_data(experts_2F_all_scores, norm_score_fluency)

## Wide data for raw adequacy score
experts_2F_adequacy <- create_wide_data(experts_2F_all_scores, adequacy)
experts_2F_z_adequacy <- create_wide_data(experts_2F_all_scores, z_score_adequacy)
experts_2F_adequacy_percentiles <- create_wide_data(experts_2F_all_scores, percentile_adequacy)
experts_2F_adequacy_norm_scores <- create_wide_data(experts_2F_all_scores, norm_score_adequacy)

# Process texts

corpus <- corpus(source_pairs$en, docnames = source_pairs$hash)
diversities <- textstat_lexdiv(
  tokens(corpus),
  measure = c("U", "S")
)
readabilities <- textstat_readability(
  corpus,
  measure = c("ARI", "Coleman.Liau.short", "FORCAST", "nWS", "RIX")
)

pairs <- source_pairs %>%
  mutate(
    document = hash,
    syllable_en = nsyllable(en),
    syllable_uk = str_count(uk, regex("[уеіаоєяиюї]", ignore_case = TRUE)),
    length_en = nchar(en),
    length_uk = nchar(uk),
    size_diff = abs(length_en - length_uk) / max(length_en, length_uk),
    syllable_diff = abs(syllable_en - syllable_uk) / max(syllable_en, syllable_uk)
  ) %>%
  left_join(readabilities, by = "document") %>%
  left_join(diversities, by = "document")

data <- list(
  enriched_expert_scores = enriched_expert_scores,
  enriched_machine_scores = enriched_machine_scores,
  machine_1F_scores = machine_1F_scores,
  machine_1F_z_scores = machine_1F_z_scores,
  machine_1F_percentiles = machine_1F_percentiles,
  machine_1F_norm_scores = machine_1F_norm_scores,
  students_1F_scores = students_1F_scores,
  students_1F_z_scores = students_1F_z_scores,
  students_1F_percentiles = students_1F_percentiles,
  students_1F_norm_scores = students_1F_norm_scores,
  experts_single_icc = experts_single_icc,
  experts_1F_scores = experts_1F_scores,
  experts_1F_z_scores = experts_1F_z_scores,
  experts_1F_percentiles = experts_1F_percentiles,
  experts_1F_norm_scores = experts_1F_norm_scores,
  experts_two_factors_fluency_icc = experts_two_factors_fluency_icc,
  experts_two_factors_adequacy_icc = experts_two_factors_adequacy_icc,
  experts_2F_fluency = experts_2F_fluency,
  experts_2F_z_fluency = experts_2F_z_fluency,
  experts_2F_fluency_percentiles = experts_2F_fluency_percentiles,
  experts_2F_fluency_norm_scores = experts_2F_fluency_norm_scores,
  experts_2F_adequacy = experts_2F_adequacy,
  experts_2F_z_adequacy = experts_2F_z_adequacy,
  experts_2F_adequacy_percentiles = experts_2F_adequacy_percentiles,
  experts_2F_adequacy_norm_scores = experts_2F_adequacy_norm_scores,
  pairs = pairs  
)

rm(
  remove_duplicates,
  create_wide_data,
  create_retest_data,
  expert_scores,
  enriched_expert_scores,
  machine_scores,
  enriched_machine_scores,
  source_pairs,
  students_scores,
  experts_single_scores,
  experts_two_factors_scores,
  machine_1F_scores,
  machine_1F_z_scores,
  machine_1F_percentiles,
  machine_1F_norm_scores,
  students_all_scores,
  students_1F_scores,
  students_1F_z_scores,
  students_1F_percentiles,
  students_1F_norm_scores,
  experts_single_icc,
  experts_1F_all_scores,
  experts_1F_scores,
  experts_1F_z_scores,
  experts_1F_percentiles,
  experts_1F_norm_scores,
  experts_two_factors_fluency_icc,
  experts_two_factors_adequacy_icc,
  experts_2F_all_scores,
  experts_2F_fluency,
  experts_2F_z_fluency,
  experts_2F_fluency_percentiles,
  experts_2F_fluency_norm_scores,
  experts_2F_adequacy,
  experts_2F_z_adequacy,
  experts_2F_adequacy_percentiles,
  experts_2F_adequacy_norm_scores,
  corpus,
  diversities,
  readabilities,
  pairs
)
