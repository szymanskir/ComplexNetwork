library(drake)
library(ggplot2)
source("Project3/Project3.R")
source("Project4/Project4.R")

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
  enron_graph = read.table("Project4/email-EU.edges") %>% 
    igraph::graph_from_data_frame(),
  example_graph_project4 = erdos.renyi.game(8, 0.5),
  
  ## Zadanie 12
  enron_graph_characteristic = vertex_degree_and_mean_neares_neighbors_degree_relation(enron_graph),
  enron_graph_characteristic_plot = plot_relation(enron_graph_characteristic),
  ## Zadanie 13.1
  enron_altered_each_edge = rewire(enron_graph, each_edge(prob = 0.5)),
  enron_altered_each_edge_characteristic = vertex_degree_and_mean_neares_neighbors_degree_relation(enron_altered_each_edge),
  enron_altered_each_edge_characteristic_plot = plot_relation(enron_altered_each_edge_characteristic),
  ## Zadanie 13.2
  enron_altered_degseq_kept = rewire(enron_graph, keeping_degseq(niter = vcount(enron_graph) * 10)),
  enron_altered_degseq_kept_characteristic = vertex_degree_and_mean_neares_neighbors_degree_relation(enron_altered_degseq_kept),
  enron_altered_degseq_kept_characteristic_plot = plot_relation(enron_altered_degseq_kept_characteristic),
  
  ## Zadanie 20
  nobel_prize_winners_erdos_number = read.table("Project4/erdos_number-nobel_prize_winners.txt", header = TRUE),
  nobel_prize_winners_erdos_number_histogram = ggplot(nobel_prize_winners_erdos_number, aes(x = as.numeric(ERDOS.NUMBER))) + geom_histogram(),
  fields_medal_winners_erdos_number = read.table("Project4/erdos_number-field_medal_winners.txt", header = TRUE),
  field_medal_winners_erdos_number_histogram = ggplot(fields_medal_winners_erdos_number, aes(x = as.numeric(ERDOS.NUMBER))) + geom_histogram(),
  
  ## Zadanie 21
  actors_bacon_number_data = data.frame(
    actor = c("Robert De Niro", "Michał Żebrowski", "Nikodem Rozbicki"),
    erdos_number = c(1, 3, 4),
    stringsAsFactors = FALSE
  ),
  project4_report_html = rmarkdown::render(input = knitr_in("Project4/Ryszard.Szymanski-4.Rmd")),
)

make(plan)
