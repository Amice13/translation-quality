# Data processing helper functions

remove_duplicates <- function(dataset) {
  dataset %>%
    group_by(.data$evaluator, .data$hash) %>%
    slice(1) %>%
    ungroup()
}

create_wide_data <- function(dataset, score) {
  dataset %>%
    dplyr::select(all_of(c("hash", "evaluator")), {{ score }}) %>%
    tidyr::pivot_wider(names_from = "evaluator", values_from = {{ score }})
}

create_retest_data <- function(dataset, score) {
  dataset %>%
    group_by(.data$evaluator, .data$hash) %>%
    filter(n() > 1) %>%
    arrange(.data$start_time, .by_group = TRUE) %>%
    mutate(rep = row_number()) %>%
    ungroup() %>%
    mutate(rater = paste(.data$evaluator, rep, sep = "_")) %>%
    dplyr::select(all_of(c("hash", "rater")), {{ score }}) %>%
    pivot_wider(names_from = .data$rater, values_from = {{ score }}) %>%
    mutate(
      `Expert average_1` = rowMeans(
        dplyr::pick(dplyr::matches("_1$")), na.rm = TRUE
      ),
      `Expert average_2` = rowMeans(
        dplyr::pick(dplyr::matches("_2$")), na.rm = TRUE
      )
    )
}

# Data sources

expert_scores <- read.csv("data/expert-scores.csv")
machine_scores <- read.csv("data/machine-scores.csv")
source_pairs <- read.csv("data/pairs.csv")
llm_scores <- read.csv("data/llm-scores.csv")

# Data enrichment

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

# Metricx23, Metricx24 and gemma3-27B were measured with the different scales
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

#LLM as a judge scores
enriched_llm_scores <- llm_scores %>%
  filter(!is.na(answer)) %>%
  mutate(
    z_score = as.numeric(scale(answer)),
    percentile = (rank(answer, ties.method = "average") - 0.5) / n(),
    norm_score = qnorm(percentile),
  )

enriched_llm_fluency_adequacy <- llm_scores %>%
  filter(is.na(answer)) %>%
  mutate(
    z_score_fluency = as.numeric(scale(fluency)),
    percentile_fluency = (rank(fluency, ties.method = "average") - 0.5) / n(),
    norm_score_fluency = qnorm(percentile_fluency),
    z_score_adequacy = as.numeric(scale(adequacy)),
    percentile_adequacy = (rank(adequacy, ties.method = "average") - 0.5) / n(),
    norm_score_adequacy = qnorm(percentile_adequacy)
  )
  
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
machine_1f_scores <- create_wide_data(enriched_machine_scores, answer)
machine_1f_z_scores <- create_wide_data(enriched_machine_scores, z_score)
machine_1f_perc <- create_wide_data(enriched_machine_scores, percentile)
machine_1f_norm <- create_wide_data(enriched_machine_scores, norm_score)

# Process LLMs' scores
llm_1f_scores <- create_wide_data(enriched_llm_scores, answer)
llm_1f_z_scores <- create_wide_data(enriched_llm_scores, z_score)
llm_1f_perc <- create_wide_data(enriched_llm_scores, percentile)
llm_1f_norm <- create_wide_data(enriched_llm_scores, norm_score)

llm_2f_fluency <- create_wide_data(enriched_llm_fluency_adequacy, fluency)
llm_2f_z_fluency <- create_wide_data(
  enriched_llm_fluency_adequacy,
  z_score_fluency
)
llm_2f_fluency_perc <- create_wide_data(
  enriched_llm_fluency_adequacy,
  percentile_fluency
)
llm_2f_fluency_norm <- create_wide_data(
  enriched_llm_fluency_adequacy,
  norm_score_fluency
)

llm_2f_adequacy <- create_wide_data(enriched_llm_fluency_adequacy, adequacy)
llm_2f_z_adequacy <- create_wide_data(
  enriched_llm_fluency_adequacy,
  z_score_adequacy
)
llm_2f_adequacy_perc <- create_wide_data(
  enriched_llm_fluency_adequacy,
  percentile_adequacy
)
llm_2f_adequacy_norm <- create_wide_data(
  enriched_llm_fluency_adequacy,
  norm_score_adequacy
)

# Process students' scores
students_all_scores <- remove_duplicates(students_scores)
students_1f_scores <- create_wide_data(students_all_scores, answer)
students_1f_z_scores <- create_wide_data(students_all_scores, z_score)
students_1f_perc <- create_wide_data(students_all_scores, percentile)
students_1f_norm <- create_wide_data(students_all_scores, norm_score)

# Process experts' single score
experts_single_icc <- create_retest_data(experts_single_scores, answer)

## Normalized general scores
experts_1f_all_scores <- remove_duplicates(experts_single_scores)
experts_1f_scores <- create_wide_data(experts_1f_all_scores, answer)
experts_1f_z_scores <- create_wide_data(experts_1f_all_scores, z_score)
experts_1f_perc <- create_wide_data(experts_1f_all_scores, percentile)
experts_1f_norm <- create_wide_data(experts_1f_all_scores, norm_score)

# Process experts' fluency and adequacy scores

## Data for estimating the inter-rater reliability for fluency
experts_2f_fluency_icc <- create_retest_data(
  experts_two_factors_scores, fluency
)

## Data for estimating the inter-rater reliability for adequacy
experts_2f_adequacy_icc <- create_retest_data(
  experts_two_factors_scores, adequacy
)

## Wide data for fluency score
experts_2f_all_scores <- remove_duplicates(experts_two_factors_scores)
experts_2f_fluency <- create_wide_data(experts_2f_all_scores, fluency)
experts_2f_z_fluency <- create_wide_data(experts_2f_all_scores, z_score_fluency)
experts_2f_fluency_perc <- create_wide_data(
  experts_2f_all_scores, percentile_fluency
)
experts_2f_fluency_norm <- create_wide_data(
  experts_2f_all_scores, norm_score_fluency
)

## Wide data for raw adequacy score
experts_2f_adequacy <- create_wide_data(
  experts_2f_all_scores, adequacy
)
experts_2f_z_adequacy <- create_wide_data(
  experts_2f_all_scores, z_score_adequacy
)
experts_2f_adequacy_perc <- create_wide_data(
  experts_2f_all_scores, percentile_adequacy
)
experts_2f_adequacy_norm <- create_wide_data(
  experts_2f_all_scores, norm_score_adequacy
)

# Calculate time between the first and the last batch
expert_batch_difference <- expert_scores %>%
  group_by(evaluator, hash) %>%
  filter(n() > 1) %>%
  mutate(rep = row_number()) %>%
  ungroup() %>%
  mutate(start_time = as.POSIXct(start_time, format = "%Y-%m-%dT%H:%M:%OS")) %>%
  group_by(evaluator, rep) %>%
  summarise(min_time = min(start_time), .groups = "drop") %>%
  arrange(evaluator, rep) %>%
  group_by(evaluator) %>%
  mutate(diff = min_time - lag(min_time)) %>%
  filter(!is.na(diff)) %>%
  dplyr::select(c("evaluator", "diff"))

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
    syllable_diff =
      abs(syllable_en - syllable_uk) / max(syllable_en, syllable_uk)
  ) %>%
  left_join(readabilities, by = "document") %>%
  left_join(diversities, by = "document")

data <- list(
  enriched_expert_scores = enriched_expert_scores,
  enriched_machine_scores = enriched_machine_scores,
  machine_1f_scores = machine_1f_scores,
  machine_1f_z_scores = machine_1f_z_scores,
  machine_1f_perc = machine_1f_perc,
  machine_1f_norm = machine_1f_norm,
  students_1f_scores = students_1f_scores,
  students_1f_z_scores = students_1f_z_scores,
  students_1f_perc = students_1f_perc,
  students_1f_norm = students_1f_norm,
  experts_single_icc = experts_single_icc,
  experts_1f_scores = experts_1f_scores,
  experts_1f_z_scores = experts_1f_z_scores,
  experts_1f_perc = experts_1f_perc,
  experts_1f_norm = experts_1f_norm,
  experts_2f_fluency_icc = experts_2f_fluency_icc,
  experts_2f_adequacy_icc = experts_2f_adequacy_icc,
  experts_2f_fluency = experts_2f_fluency,
  experts_2f_z_fluency = experts_2f_z_fluency,
  experts_2f_fluency_perc = experts_2f_fluency_perc,
  experts_2f_fluency_norm = experts_2f_fluency_norm,
  experts_2f_adequacy = experts_2f_adequacy,
  experts_2f_z_adequacy = experts_2f_z_adequacy,
  experts_2f_adequacy_perc = experts_2f_adequacy_perc,
  experts_2f_adequacy_norm = experts_2f_adequacy_norm,
  expert_batch_difference = expert_batch_difference,
  llm_1f_scores = llm_1f_scores,
  llm_1f_z_scores = llm_1f_z_scores,
  llm_1f_perc = llm_1f_perc,
  llm_1f_norm = llm_1f_norm,
  llm_2f_fluency = llm_2f_fluency,
  llm_2f_z_fluency = llm_2f_z_fluency,
  llm_2f_fluency_perc = llm_2f_fluency_perc,
  llm_2f_fluency_norm = llm_2f_fluency_norm,
  llm_2f_adequacy = llm_2f_adequacy,
  llm_2f_z_adequacy = llm_2f_z_adequacy,
  llm_2f_adequacy_perc = llm_2f_adequacy_perc,
  llm_2f_adequacy_norm = llm_2f_adequacy_norm,
  llm_1f_z_scores = llm_1f_z_scores,
  llm_2f_z_fluency = llm_2f_z_fluency,
  llm_2f_z_adequacy = llm_2f_z_adequacy,
  pairs = pairs
)

rm(
  remove_duplicates,
  create_wide_data,
  create_retest_data,
  expert_scores,
  enriched_expert_scores,
  machine_scores,
  llm_scores,
  enriched_machine_scores,
  enriched_llm_scores,
  enriched_llm_fluency_adequacy,
  source_pairs,
  students_scores,
  experts_single_scores,
  experts_two_factors_scores,
  machine_1f_scores,
  machine_1f_z_scores,
  machine_1f_perc,
  machine_1f_norm,
  students_all_scores,
  students_1f_scores,
  students_1f_z_scores,
  students_1f_perc,
  students_1f_norm,
  experts_single_icc,
  experts_1f_all_scores,
  experts_1f_scores,
  experts_1f_z_scores,
  experts_1f_perc,
  experts_1f_norm,
  experts_2f_fluency_icc,
  experts_2f_adequacy_icc,
  experts_2f_all_scores,
  experts_2f_fluency,
  experts_2f_z_fluency,
  experts_2f_fluency_perc,
  experts_2f_fluency_norm,
  experts_2f_adequacy,
  experts_2f_z_adequacy,
  experts_2f_adequacy_perc,
  experts_2f_adequacy_norm,
  llm_1f_scores,
  llm_1f_z_scores,
  llm_1f_perc,
  llm_1f_norm,
  llm_2f_fluency,
  llm_2f_z_fluency,
  llm_2f_fluency_perc,
  llm_2f_fluency_norm,
  llm_2f_adequacy,
  llm_2f_z_adequacy,
  llm_2f_adequacy_perc,
  llm_2f_adequacy_norm,
  expert_batch_difference,
  corpus,
  diversities,
  readabilities,
  pairs
)
