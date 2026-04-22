# Clustering of z-scores by LLM and QE models

scores <- data$machine_1f_z_scores %>%
  dplyr::select(-hash)

machine_cluster <- hclust(
  as.dist(1 - cor(scores, method = "spearman")),
  method = "average"
)
ggdendrogram(machine_cluster, rotate = TRUE, size = 2)
results[["machine_cluster"]] <- machine_cluster

custom_ggsave(result_path("Figure 4 - Machine models dendrogram.png"))

with_llm_scores <- data$machine_1f_z_scores %>%
  left_join(data$llm_1f_z_scores, by = "hash") %>%
  dplyr::select(-hash)

llm_machine_cluster <- hclust(
  as.dist(1 - cor(with_llm_scores,
                  method = "spearman",
                  use = "pairwise.complete.obs"
  )
  ),
  method = "average"
)

ggdendrogram(llm_machine_cluster, rotate = TRUE, size = 2)
custom_ggsave(result_path("Figure 4 - Machine models with LLMs dendrogram.png"))

results[["llm_machine_cluster"]] <- llm_machine_cluster

rm(
  scores,
  with_llm_scores,
  machine_cluster,
  llm_machine_cluster
)
