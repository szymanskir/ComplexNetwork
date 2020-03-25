library(animation)
library(igraph)
library(logging)

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



calculate_total_energy <- function(graph, current_layout, constants = list(vector_electric_charge = 1e-3,
                                                                            spring_constant = 1e5,
                                                                            spring_length = 0.45)) {
  calculate_single_vertex_energy <- function(ind, layout, constants) {
    current_vertex <- layout[ind, ]
    other_vertices <- layout[-ind, , drop = FALSE]
    neighbor_vertices <- layout[neighbors(graph, ind), , drop = FALSE]
    
    coulomb_constant <- 8.9875 * 1e9
    electrostatic_energy <- apply(other_vertices, 1, function(other_vertex) {
      point_distance <- calculate_point_distance(current_vertex, other_vertex)
      (coulomb_constant * constants$vector_electric_charge ^2) / point_distance
    }) %>% sum()
      
    spring_energy <- apply(neighbor_vertices, 1, function(other_vertex) {
      point_distance <- calculate_point_distance(current_vertex, other_vertex)
      (constants$spring_constant / 2) * (constants$spring_length - point_distance)^2
    }) %>% sum()
    
    electrostatic_energy + spring_energy
  }
  
  sapply(seq_len(vcount(graph)), function(ind) calculate_single_vertex_energy(ind, current_layout, constants)) %>% 
    sum()
}

plot_simulated_annealing <- function(graph, start_temperature = 300, temperature_lowering_factor = 0.9, steps = 10) {
  stopifnot(temperature_lowering_factor < 1 && temperature_lowering_factor > 0)
  boltzman_constant <- 1.380649 * 1e-23
  vertices_count <- vcount(graph)
  current_layout <- matrix(runif(2 * vertices_count, -1, 1), ncol = 2, byrow = TRUE)
  current_temperature <- start_temperature
  
  for (step in seq_len(steps)) {
    start_energy <- calculate_total_energy(graph, current_layout)
    loginfo(sprintf("[Step %s] Current energy: %s", step, start_energy))
    
    # Apply random change
    random_vertex <- sample(seq_len(vertices_count), 1)
    vertex_movement <- runif(2, -1, 1)
    new_layout <- current_layout
    new_layout[random_vertex, ] <- new_layout[random_vertex, ] + c(vertex_movement)
    
    new_energy <- calculate_total_energy(graph, new_layout) 
    energy_change <- new_energy - start_energy
    
    is_change_beneficial <- energy_change < 0
    current_temperature <- temperature_lowering_factor * current_temperature
    acceptance_ratio <- exp(-energy_change/(boltzman_constant * current_temperature))
    accept_bad_change <- runif(1) < acceptance_ratio
    
    if (is_change_beneficial || accept_bad_change) {
      current_layout <- new_layout
      plot(graph, layout = current_layout)
    }
  }
  current_layout
}

create_animation <- function(output_file, plot_fun, graph, steps, interval) {
  saveGIF({
    plot_fun(graph = graph, steps = steps)
  }, movie.name = "tmp.gif", interval = 0.1)
  
  file.rename(from = "tmp.gif", output_file)
}
