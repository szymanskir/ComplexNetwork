library(dplyr)
library(igraph)
library(ggplot2)

get_largest_component_size <- function(graph) {
  graph_vertex_count <- vcount(graph)
  if (graph_vertex_count == 0) {
    0
  } else {
    graph_components <- components(graph)
    graph_components$csize %>% max() / vcount(graph)
  }
}

break_random_vertices <- function(graph, probability) {
  vertices <- V(graph)
  vertices_count <- vcount(graph) * probability
  vertices_to_break <- sample(vertices, size = vertices_count)
  
  delete_vertices(graph, vertices_to_break)
}

attack_vertex <- function(graph, probability) {
  vertices_to_break_count <- vcount(graph) * probability
  vertices_to_break <- V(graph)[order(degree(graph), decreasing = TRUE)] %>% head(vertices_to_break_count)
  
  delete_vertices(graph, vertices_to_break)
}

run_single_breakdown_simulation <- function(graph, probabilities, breakdown_fun) {
  largest_component_sizes <- sapply(probabilities, function(prob) {
    breakdown_fun(graph, prob) %>% get_largest_component_size()
  })
  
  data.frame(
    breakdown_probability = probabilities,
    lcc_size = largest_component_sizes
  )
}

compare_er_and_ba_resilience <- function(graphs, graph_types, probabilities, breakdown_fun) {
  simulation_results <- mapply(function(graph, graph_type) {
    sim_result <- run_single_breakdown_simulation(graph, probabilities, breakdown_fun)
    sim_result$graph_type <- graph_type
    sim_result
  }, graph = graphs, graph_type = graph_types, USE.NAMES = FALSE, SIMPLIFY = FALSE) %>% bind_rows()
  
  simulation_results
}

run_comparison_simulation <- function(times, graphs, graph_types, probabilities, breakdown_fun) {
  
  lapply(
    seq_len(times), 
    function(x) compare_er_and_ba_resilience(graphs, graph_types, probabilities = probabilities, breakdown_fun = breakdown_fun)
  ) %>% 
    bind_rows() %>% 
    group_by(breakdown_probability, graph_type) %>% 
    summarise(lcc_size = mean(lcc_size))
}

plot_graph_resilience_comparison <- function(results) {
  ggplot(results, aes(x = breakdown_probability, y = lcc_size, color = graph_type)) +
    geom_point()
}
