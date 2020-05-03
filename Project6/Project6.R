library(animation)
library(ggraph)
library(igraph)
library(magrittr)

plot_graph_frame <- function(graph) {
  layout <- layout_with_fr(graph)
  ggraph(graph, layout = layout) +
    geom_edge_link() +
    geom_node_point()
} 

create_barabasi_albert_graph <- function(start_nodes_count, edges_per_time_step, time_steps, keep_steps = FALSE) {
  stopifnot(edges_per_time_step <= start_nodes_count)
  ba_graph <- make_full_graph(n = start_nodes_count, directed = FALSE)
  graph_steps <- list()
  
  for (time_step in seq_len(time_steps)) {
    vertex_degrees <- degree(ba_graph)
    vertex_connection_probability <- vertex_degrees / sum(vertex_degrees)
    
    vertices_to_connect_with <- sample(
      seq_along(vertex_degrees), 
      size = edges_per_time_step, 
      prob = vertex_connection_probability
    )
    
    ba_graph <- add_vertices(ba_graph, 1)
    added_vertex <- vcount(ba_graph)
    for (vertex_to_connect_with in vertices_to_connect_with) {
      ba_graph <- ba_graph + edge(added_vertex, vertex_to_connect_with)
    }
    
    if (keep_steps) {
      graph_steps[[length(graph_steps) + 1]] <- ba_graph
    }
  }
  
  list(
    graph = ba_graph,
    steps = graph_steps
  )
}

make_ba_animation <- function(ba_graph, output_file) {
  saveGIF({
    for (i in seq_along(ba_graph$steps)) {
      plot(ba_graph$steps[[i]])
    }
  }, movie.name = "tmp.gif")
  
  file.rename(from = "tmp.gif", output_file)
}

create_barabasi_albert_graph_model_a <- function(start_nodes_count, edges_per_time_step, time_steps) {
  stopifnot(edges_per_time_step <= start_nodes_count)
  ba_graph <- make_full_graph(n = start_nodes_count, directed = FALSE)
  graph_steps <- list()
  
  for (time_step in seq_len(time_steps)) {
    vertex_degrees <- degree(ba_graph)
    
    vertices_to_connect_with <- sample(
      seq_along(vertex_degrees), 
      size = edges_per_time_step
    )
    
    ba_graph <- add_vertices(ba_graph, 1)
    added_vertex <- vcount(ba_graph)
    for (vertex_to_connect_with in vertices_to_connect_with) {
      ba_graph <- ba_graph + edge(added_vertex, vertex_to_connect_with)
    }
  }
  
  ba_graph
}

create_barabasi_albert_graph_model_b <- function(nodes_count, edges_count, time_steps) {
  stopifnot(edges_count <= choose(nodes_count, 2))
  ba_graph <- random.graph.game(nodes_count, edges_count, type = "gnm")
  
  for (time_step in seq_len(time_steps)) {
    current_edge <- sample(E(ba_graph), size = 1)
    vertices <- ends(ba_graph, current_edge) %>% 
      as.vector()
    
    from_vertex <- vertices[1]
    
    vertex_degrees <- degree(ba_graph)
    vertex_connection_probability <- vertex_degrees / sum(vertex_degrees)
    
    vertex_to_connect_with <- sample(
      seq_along(vertex_degrees)[-from_vertex], 
      size = 1, 
      prob = vertex_connection_probability[-from_vertex]
    )
    
    ba_graph <- ba_graph %>% 
      delete_edges(current_edge) %>% 
      add_edges(c(from_vertex, vertex_to_connect_with))
  }
  
  ba_graph
}
