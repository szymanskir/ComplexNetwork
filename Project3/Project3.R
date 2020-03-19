library(igraph)
source("Project3/utils.R")

### Functions
calculate_point_distance <- function(p1, p2) {
  sqrt(sum((p1 - p2) ^ 2))  
}

calculate_cos_angle <- function(v1, v2) {
  dot_prod <- v1 %*% v2 
  norm_v1 <- norm(v1,type="2")
  norm_v2 <- norm(v2,type="2")
  dot_prod / (norm_v1 * norm_v2)
}

calculate_culomb_force <- function(v1, v2, electric_charge) {
  coulomb_constant <- 8.9875 * 1e9
  v1_x <- v1[1]
  v1_y <- v1[2]
  v2_x <- v2[1]
  v2_y <- v2[2]
  
  distance <- calculate_point_distance(v1, v2)
  force_value <- (coulomb_constant * electric_charge^2) / (distance^2)
  cos_angle <- calculate_cos_angle(v1 - v2, c(1, 0))
  sin_angle <- sqrt(1 - cos_angle^2)
  
  dx <- force_value * cos_angle
  dy <- force_value * sin_angle
  
  c(dx, dy)
}

calculate_net_force <- function(v, other_vertices, spring_constant, electric_charge) {
 net_force <- apply(other_vertices, 1, function(other_vertex) {
   calculate_culomb_force(v, other_vertex, electric_charge)
 }) %>% t()
 
 browser()
 apply(net_force, 2, sum)
}

plot_graph <- function(graph, spring_constant = 1, electric_charge = 1e-6, mass = 1e-12, steps = 10) {
  current_layout <- matrix(runif(2 * vcount(graph), min = -2, 2), ncol = 2, byrow = TRUE)
  
  for (step in steps) {
    browser()
    transition_matrix <- matrix(0, nrow = nrow(current_layout), ncol = ncol(current_layout))
    for (ind in seq_len(vcount(graph))) {
      transition_matrix[ind, ] <- calculate_net_force(v = current_layout[ind, ],
                          other_vertices = matrix(current_layout[-ind, ], ncol = 2),
                          spring_constant = spring_constant,
                          electric_charge = electric_charge)      
    }
    current_layout <- current_layout + transition_matrix
    plot(graph, layout = current_layout)
  }
}


n <- 7
m <- 0.7
g <- erdos.renyi.game(n, m)

plot_graph(g)
 