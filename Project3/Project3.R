library(igraph)
library(animation)

calculate_point_distance <- function(p1, p2) {
  sqrt(sum((p1 - p2) ^ 2))  
}

cross_product <- function(p1, p2) {
  p1_x <- p1[1]
  p1_y <- p1[2]
  p2_x <- p2[1]
  p2_y <- p2[2]
  
  p1_x * p2_y - p2_x * p1_y
}

calculate_cos_angle <- function(v1, v2) {
  dot_prod <- v1 %*% v2 
  norm_v1 <- norm(v1,type="2")
  norm_v2 <- norm(v2,type="2")
  dot_prod / (norm_v1 * norm_v2)
}

calculate_sin_angle <- function(v1, v2) {
  norm_v1 <- norm(v1,type="2")
  norm_v2 <- norm(v2,type="2")  
  cross_product(v1, v2) / (norm_v1 * norm_v2)
}

calculate_culomb_force <- function(v1, v2, electric_charge) {
  coulomb_constant <- 8.9875 * 1e9
  distance <- calculate_point_distance(v1, v2)
  force_value <- (coulomb_constant * electric_charge^2) / (distance^2)
  cos_angle <- calculate_cos_angle(v1 - v2, c(1, 0))
  sin_angle <- calculate_sin_angle(v1 - v2, c(1, 0))
  
  dx <- force_value * cos_angle
  dy <- force_value * sin_angle
  
  c(dx, dy)
}

calculate_spring_force <- function(v1, v2, spring_constant) {
  distance <- calculate_point_distance(v1, v2)
  force_value <- spring_constant * distance
  cos_angle <- calculate_cos_angle(v2 - v1, c(1, 0))
  sin_angle <- calculate_sin_angle(v1 - v2, c(1, 0))
  
  dx <- force_value * cos_angle
  dy <- force_value * sin_angle
  
  c(dx, dy)
}

plot_graph <- function(graph, spring_constant = 1e-5, electric_charge = 1e-6, mass = 1e-12, steps = 10) {
  current_layout <- matrix(runif(2 * vcount(graph), -1, 1), ncol = 2, byrow = TRUE)
  
  for (step in seq_len(steps)) {
    transition_matrix <- matrix(0, nrow = nrow(current_layout), ncol = ncol(current_layout))
    for (ind in seq_len(vcount(graph))) {
      # Calculate the net coulomb force applied to the `ind` vertex
      other_vertices <- current_layout[-ind, , drop = FALSE]
      net_coulomb_force <- apply(other_vertices, 1, function(other_vertex) {
        calculate_culomb_force(current_layout[ind, ], other_vertex, electric_charge)
      }) %>% t() %>% apply(2, sum)
      
      # Calculate the net spring force applied to the `ind` vertex
      vertex_neighbors <- neighbors(graph, ind)
      no_neighbors <- length(vertex_neighbors) == 0
      if (no_neighbors) {
        net_spring_force <- c(0, 0)
      } else {
        vertex_neighbors_coordinates <- current_layout[vertex_neighbors, , drop = FALSE]
        net_spring_force <- apply(vertex_neighbors_coordinates, 1, function(other_vertex) {
          calculate_spring_force(current_layout[ind, ], other_vertex, spring_constant)
        }) %>% t() %>% apply(2, sum)
      }

      transition_matrix[ind, ] <- 1e4 * (net_spring_force + net_coulomb_force)
    }
    current_layout <- current_layout + transition_matrix
    plot(graph, layout = current_layout)
  }
}

plot_igraph_builtin <- function(graph, steps = 10) {
  start_layout <- matrix(rnorm(2 * vcount(graph)), ncol = 2, byrow = TRUE)
  current_layout <- start_layout
  
  for (step in seq_len(steps)) {
    plot(graph, layout = current_layout)
    current_layout <- igraph::layout_with_fr(
      graph = graph,
      coords = start_layout,
      niter = step
    )
  }
}

create_animation <- function(output_file, plot_fun, graph, steps, interval) {
  saveGIF({
    plot_fun(graph = graph, steps = steps)
  }, movie.name = "tmp.gif", interval = 0.1)
  
  file.rename(from = "tmp.gif", output_file)
}
