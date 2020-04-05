library(igraph)

hist2df <- function(hist_vals) {
  data.frame(
    breaks = hist_vals$breaks[seq_along(hist_vals$counts)],
    counts = hist_vals$counts
  )
}

mean_nearest_neighbors_degree <- function(graph) {
  single_vertex_mean_nearest_neighbors_degree <- function(graph, v) {
    vertex_neighbors <- neighbors(graph, v, mode = "all") 
    mean(degree(graph = graph, v = vertex_neighbors))
  }
  
  sapply(V(graph), single_vertex_mean_nearest_neighbors_degree, graph = graph)
}

vertex_degree_and_mean_neares_neighbors_degree_relation <- function(graph) {
  data.frame(
    vertex_degree = degree(graph),
    mean_nearest_neighbors_degree = mean_nearest_neighbors_degree(graph)
  )
}

plot_relation <- function(df) {
  ggplot(df, aes(x = vertex_degree, y = mean_nearest_neighbors_degree)) +
    geom_point() +
    labs(x = "Vertex degree", y = "Mean degree of nearest neighbors")
}

###### PROJECT measures
mean_nearest_vertex_degree <- function(graph) {
  nearest_neighbors_count <- degree(graph) %>% sum()
  
  nearest_neighbors_degrees <- sapply(V(graph), function(vertex_ind) {
    neighbors(graph, vertex_ind) %>% 
      degree(graph, .) %>% 
      sum()
  })
  
  sum(nearest_neighbors_degrees) / nearest_neighbors_count
}


network_exponent <- function(vertex_degrees, xmin = 1) {
  vertex_count <- length(vertex_degrees)
  
  alpha <- 1 + vertex_count * sum(log(vertex_degrees / xmin))^(-1)
  sigma <- (alpha - 1) / sqrt(vertex_count)
  
  list(
    alpha = alpha,
    sigma = sigma
  )
}

network_correlation <- function(graph) {
  edges_count <- ecount(graph)
  edges <- E(graph)
  adjacent_vertices <- ends(graph, edges)
  
  adjacent_vertices_product <- apply(adjacent_vertices, 1, function(v) {
    degree(graph, v[1]) * degree(graph, v[2])
  })
  adjacent_vertices_sum <- apply(adjacent_vertices, 1, function(v) {
    degree(graph, v[1]) + degree(graph, v[2])
  })
  adjacent_vertices_sum_of_squares <- apply(adjacent_vertices, 1, function(v) {
    degree(graph, v[1])^2 + degree(graph, v[2])^2
  })
  
  nominator_left <- sum(adjacent_vertices_product) / edges_count
  nominator_right <- (sum(adjacent_vertices_sum) / (2 * edges_count))^2
  denominator_left <- sum(adjacent_vertices_sum_of_squares) / (2*edges_count)
  denominator_right <- (sum(adjacent_vertices_sum)/(2*edges_count))^2
  
  (nominator_left - nominator_right) / (denominator_left - denominator_right)
}

clustering_coefficient_1 <- function(graph) {
  node_clustering_coefficient <- function(graph, v) {
    v_degree <- degree(graph, v)
    v_neighbors <- neighbors(graph, v) %>% as.numeric()
    neighborhood_edges_count <- as_adjacency_matrix(graph)[v_neighbors, v_neighbors] %>% 
      sum() / 2
    possible_edges_count <- choose(v_degree, 2)
    
    clustering_coefficient <- neighborhood_edges_count / possible_edges_count
    if (is.nan(clustering_coefficient)) {
      NA
    } else {
      clustering_coefficient
    }
  }
  
  mean(sapply(V(graph), node_clustering_coefficient, graph = graph), na.rm = TRUE)
}

clustering_coefficient_2 <- function(graph) {
  triangles_count <- length(triangles(graph)) / 3
  all_simple_paths_of_legth_2_count <- do.call(c, lapply(V(graph), all_simple_paths, graph = graph)) %>% 
    keep(function(path) length(path) == 3) %>% 
    unique() %>% 
    length() / 2 # 1-2-3 is the same path as 3-2-1
  
  (3 * triangles_count) / all_simple_paths_of_legth_2_count
}


mean_distance <- function(graph) {
  vertex_count <- vcount(graph)
  vertex_distances <- distances(graph)
  diag(vertex_distances) <- 0 # d(i,j) where i != j
  sum(vertex_distances) / (vertex_count * (vertex_count - 1))
}

efficiency <- function(graph) {
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