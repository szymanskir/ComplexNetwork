library(dplyr)
library(igraph)
library(ggplot2)
library(patchwork)
library(tidyr)
library(visNetwork)

create_er_graph_adjacency_matrix <- function(n, p) {
  adjacency_matrix <- matrix(0, ncol = n, nrow = n)
  
  for (row in seq_len(n)) {
    cols <- seq_len(row - 1)
    are_connected <- rbinom(cols, 1, p)
    adjacency_matrix[row, cols] <- are_connected
    adjacency_matrix[cols, row] <- are_connected
  }
  
  adjacency_matrix
}

plot_ggraph <- function(graph) {
  visIgraph(graph, physics = FALSE)
}

plot_degree_hist <- function(graph) {
  degrees <- igraph::degree(graph)
  df <- data.frame(
    degree = degrees
  )
    ggplot(df, aes(x = degree)) +
      geom_bar(position = "dodge")
}

create_block_model_graph_adjacency_matrix <- function(n, k, prob_in, prob_out) {
  adjacency_matrix <- matrix(0, ncol = n, nrow = n)
  
  for (row in seq_len(n)) {
    for (col in seq_len(row - 1)) {
      same_community <- (row %/% (k + 1)) == (col %/% (k + 1))
      prob <- ifelse(same_community, prob_in, prob_out)
      edge_weight <- ifelse(same_community, 4, 1)
      
      are_connected <- rbinom(1, 1, prob)
      adjacency_matrix[row, col] <- are_connected * edge_weight
      adjacency_matrix[col, row] <- are_connected * edge_weight
    }
  }
  
  adjacency_matrix
}
