library(drake)
source("Project3/Project3.R")

set.seed(44)

plan <- drake_plan(
  ############################
  ##############  Project 3
  ############################
  sample_graph = make_ring(20),
  own_animation = create_animation(
    output_file = file_out("Project3/animation-own_implementation.gif"),
    plot_fun = plot_graph,
    graph = sample_graph,
    steps = 150,
    interval = 0.1
  ),
  igraph_builtin_animation = create_animation(
    output_file = file_out("Project3/animation-igraph_builtin.gif"),
    plot_fun = plot_igraph_builtin,
    graph = sample_graph,
    steps = 50,
    interval = 1
  ),
  simmulated_annealing_animation = create_animation(
    output_file = file_out("Project3/animation-simulated_annealing.gif"),
    plot_fun = plot_simulated_annealing,
    graph = sample_graph,
    steps = 2500,
    interval = 1
  ),
  project3_report_html = rmarkdown::render(input = knitr_in("Project3/Ryszard.Szymanski-3.Rmd")),
  ############################
  ##############  Project 4
  ############################
  example_graph_project4 = erdos.renyi.game(8, 0.5),
  project4_report_html = rmarkdown::render(input = knitr_in("Project4/Ryszard.Szymanski-4.Rmd")),
)

make(plan)
