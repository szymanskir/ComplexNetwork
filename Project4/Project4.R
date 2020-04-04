library(igraph)

mean_nearest_vertex_degree <- function(graph) {
  nearest_neighbors_count <- degree(graph) %>% sum()
  
  nearest_neighbors_degrees <- sapply(V(graph), function(vertex_ind) {
    neighbors(graph, vertex_ind) %>% 
      degree(graph, .) %>% 
      sum()
  })
  
  sum(nearest_neighbors_degrees) / nearest_neighbors_count
}
