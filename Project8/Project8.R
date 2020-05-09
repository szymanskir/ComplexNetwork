library(dplyr)
library(ggplot2)
library(igraph)
source("Project8-ztm.R")

prepare_clustering_coeff_to_degree_relation_data <- function(graph) {
  data.frame(
    clustering_coeff = transitivity(graph,  type = "local"),
    degrees = degree(graph)
  )
}

plot_clustering_coeff_to_degree_relation <- function(data, include_theoretical_curve) {
  if (include_theoretical_curve) {
    data$type <- "real"
    theoretical_data <- data.frame(
      degrees = data$degrees,
      clustering_coeff = 1 / data$degrees,
      type = "theory"
    )
    
    data <- bind_rows(data, theoretical_data)
    
    ggplot(data, aes(x = degrees, y = clustering_coeff, color = type, group = type)) +
      geom_point()
  } else {
    ggplot(data, aes(x = degrees, y = clustering_coeff)) +
      geom_point()
  }
}
