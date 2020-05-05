library(ggplot2)
library(igraph)
library(parallel)

run_simulations <- function(counts, n, p) {
  num_cores <- detectCores()
  cl <- makeCluster(num_cores)
  simulation_results <- parLapply(cl = cl, seq_len(counts), function(x) {
    library(igraph)
    run_single_simulation <- function(n, p) {
      g <- make_empty_graph(n = n)
      vertex_pairs <- combn(V(g), 2)
      possible_edges_count <- ncol(vertex_pairs)
      shuffled_pairs <- vertex_pairs[, sample(possible_edges_count)]
      are_connected <- rbinom(possible_edges_count, 1, p)
      
      mean_degrees <- c()
      largest_connected_component_count <- c()
      for (k in seq_len(possible_edges_count)) {
        if (are_connected[k] == 1){
          g <- add_edges(g, shuffled_pairs[, k])
          mean_degrees <- c(mean_degrees, mean(degree(g)))
          lcc_count <- components(g)$csize %>% max() / n
          largest_connected_component_count <- c(largest_connected_component_count, lcc_count)
        }
      }
      
      data.frame(
        x = mean_degrees,
        y = largest_connected_component_count
      )
    }
    single_simulation_result <- run_single_simulation(n, p)
    single_simulation_result$run_id <- x
    single_simulation_result
  }) %>% bind_rows()
  
  stopCluster(cl)
  simulation_results
}

plot_simulation_results <- function(simulation_results) {
  simulation_results %>% 
    mutate(y = y) %>% 
    ggplot(aes(x = x, y = y, color = run_id)) +
    geom_point() +
    geom_smooth(color = "red") +
    labs(x = "Średni stopień wierzchołka", y = "Procent wierzchołków w największej spójnej składowej", color = "Nr symulacji") +
    viridis::scale_color_viridis()
}

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
  vertices_to_break_inds <- rbinom(length(vertices), size = 1, prob = probability) %>% as.logical()
  vertices_to_break <- vertices[vertices_to_break_inds]
  
  delete_vertices(graph, vertices_to_break)
}

break_random_edges <- function(graph, probability) {
  edges <- E(graph)
  edges_to_break_inds <- rbinom(length(vertices), size = 1, prob = probability) %>% as.logical()
  edges_to_break <- edges[edges_to_break_inds]
  
  delete_edges(graph, edges_to_break)
}

break_random_vertices_and_edges <- function(graph, probability) {
  graph %>% 
    break_random_vertices(probability) %>% 
    break_random_edges(probability)
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