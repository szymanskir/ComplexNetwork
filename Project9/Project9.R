library(ggplot2)
library(ggraph)
library(patchwork)
library(igraph)
library(Matrix)
library(pracma)
library(tidyr)

find_best_diffusion_graph <- function(vertex_count,
                                      edges_count,
                                      rewire_prob,
                                      start_temperature = 300, 
                                      temperature_lowering_factor = 0.9, 
                                      steps = 10) {
  graph <- erdos.renyi.game(vertex_count, edges_count, type = "gnm")
  start_graph <- graph
  current_temperature <- start_temperature
  stopifnot(temperature_lowering_factor < 1 && temperature_lowering_factor > 0)
  boltzman_constant <- 1.380649 * 1e-23
  energies <- c()
  
  for (step in seq_len(steps)) {
    start_energy <- igraph::mean_distance(graph, unconnected = FALSE)
    
    # Apply random change
    changed_graph <- rewire(graph, each_edge(prob = rewire_prob))
    
    new_energy <- igraph::mean_distance(changed_graph, unconnected = FALSE)
    energy_change <- new_energy - start_energy
    
    is_change_beneficial <- energy_change < 0
    current_temperature <- temperature_lowering_factor * current_temperature
    acceptance_ratio <- exp(-energy_change/(boltzman_constant * current_temperature))
    accept_bad_change <- runif(1) < acceptance_ratio
    
    if (is_change_beneficial || accept_bad_change) {
      graph <- changed_graph
      energies <- c(energies, new_energy)
    } else {
      energies <- c(energies, start_energy)
    }
  }
  
  list(
    start_graph = start_graph,
    end_graph = graph,
    energies = energies
  )
}

ggraph_plot <- function(graph) {
  ggraph(graph) +
    geom_edge_link() +
    geom_node_point(size = 5, color = "#d96411")
}

degree_dist_plot <- function(graph) {
  graph %>% 
    degree() %>% 
    qplot()
}

create_sim_result_plot <- function(simulation_results) {
  create_col_plot <- function(graph, title) {
    graph_plot <- ggraph_plot(graph) + ggtitle(title)
    degree_hist <- degree_dist_plot(graph)
    
    (graph_plot / degree_hist)
  }
  
  create_col_plot(simulation_results$start_graph, "Graf początkowy") | create_col_plot(simulation_results$end_graph, "Graf wynikowy")
}

