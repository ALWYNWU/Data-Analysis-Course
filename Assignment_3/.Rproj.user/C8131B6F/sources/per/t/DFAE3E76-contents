library(ggdendro)

q2_dist_vec <- c(
  0,      0.3,    0.4,    0.7,
  0.3,    0,      0.5,    0.8,
  0.4,    0.5,    0,      0.45,
  0.7,    0.8,    0.45,   0
)

matrix(q2_dist_vec, ncol = 4) %>%
  as.dist() %>%
  hclust(method='complete') %>%
  ggdendrogram()