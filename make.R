library(drake)
source("Project3/Project3.R")

set.seed(44)

plan <- drake_plan(
  sample_graph = make_ring(14),
  own_animation = create_animation(
    output_file = file_out("Project3/animation-own_implementation.gif"),
    plot_fun = plot_graph,
    graph = sample_graph,
    steps = 50,
    interval = 0.1
  ),
  igraph_builtin_animation = create_animation(
    output_file = file_out("Project3/animation-igraph_builtin.gif"),
    plot_fun = plot_igraph_builtin,
    graph = sample_graph,
    steps = 50,
    interval = 1
  ),
  project3_report_html = rmarkdown::render(input = knitr_in("Project3/Ryszard.Szymanski-3.Rmd"))
)

make(plan)
