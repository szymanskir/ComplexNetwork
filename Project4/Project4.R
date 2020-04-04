library(igraph)
library(purrr)

mean_nearest_vertex_degree <- function(graph) {
  nearest_neighbors_count <- degree(graph) %>% sum()
  
  nearest_neighbors_degrees <- sapply(V(graph), function(vertex_ind) {
    neighbors(graph, vertex_ind) %>% 
      degree(graph, .) %>% 
      sum()
  })
  
  sum(nearest_neighbors_degrees) / nearest_neighbors_count
}

mean_distance <- function(graph) {
  vertex_count <- vcount(graph)
  vertex_distances <- distances(graph)
  diag(vertex_distances) <- 0 # d(i,j) where i != j
  sum(vertex_distances) / (vertex_count * (vertex_count - 1))
}

capability <- function(graph) {
  vertex_count <- vcount(graph)
  vertex_distances_inverted <- 1 / distances(graph)
  diag(vertex_distances_inverted) <- 0 # d(i,j) where i != j
  sum(vertex_distances_inverted) / (vertex_count * (vertex_count - 1))
}

node_betweenness <- function(graph) {
  single_node_betweenness <- function(graph, v) {
    start_nodes <- setdiff(V(graph), v)
    val <- 0
    
    for (start_node in start_nodes) {
      end_nodes <- V(graph) %>% 
        keep(function(x) x > start_node) %>% 
        setdiff(v)
      
      for (end_node in end_nodes) {
        all_paths <- all_shortest_paths(graph = graph,
                                        from = start_node,
                                        to = end_node)
        all_paths_count <- length(all_paths$res)
        all_between_paths_count <- all_paths$res %>% 
          keep(function(path) v %in% path) %>% 
          length()
        
        val <- val + all_between_paths_count / all_paths_count
      }
    }
    
    val
  }
  
  vertex_count <- vcount(graph)
  node_betweenness <- sapply(V(graph), single_node_betweenness, graph = graph)
  (2 * node_betweenness) / ((vertex_count - 1) * (vertex_count - 2))
}
