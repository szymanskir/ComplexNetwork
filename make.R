library(drake)
library(ggplot2)
source("Project3/Project3.R")
source("Project4/Project4.R")
source("Project5/Project5.R")
source("Project6/Project6.R")
source("Project7/Project7.R")
source("Project8/Project8.R")

set.seed(44)
data(UKfaculty, package = "igraphdata")
data(USairports, package = "igraphdata")

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
  enron_altered_each_edge = rewire(enron_graph, each_edge(prob = 0.9)),
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
    bacon_number = c(1, 3, 4),
    stringsAsFactors = FALSE
  ),
  project4_report_html = rmarkdown::render(input = knitr_in("Project4/Ryszard.Szymanski-4.Rmd")),
  
  ############################
  ##############  Project 5
  ############################
  project5_report_html = callr::r(
    function(...) {
      library(drake)
      library(magrittr)
      library(visNetwork)
      source("Project5/Project5.R")
      rmarkdown::render(...)
    },
    args = list(
      knitr_in("Project5/Ryszard.Szymanski-5.Rmd"),
      quiet = TRUE
    )
  ),
  
  
  ############################
  ##############  Project 6
  ############################
  ba_graph = create_barabasi_albert_graph(3, 2, 100, keep_steps = TRUE),
  ba_graph_animation = make_ba_animation(ba_graph, "Project6/ba_graph_animation.gif"),
  ba_graph_large = create_barabasi_albert_graph(10, 4, 2000, keep_steps = TRUE),
  ba_graphs = lapply(1:10, function(x) create_barabasi_albert_graph(10, 4, 500, keep_steps = TRUE)$graph),
  ba_graphs_model_a = lapply(1:10, function(x) create_barabasi_albert_graph_model_a(10, 8, 1000)),
  ba_graphs_model_b = lapply(c(50, 500, 10000), function(x) create_barabasi_albert_graph_model_b(100, 1000, x)),
  project6_report_html = rmarkdown::render(input = knitr_in("Project6/Ryszard.Szymanski-6.Rmd")),
  
  ############################
  ##############  Project 7
  ############################
  mail_graph = read_graph("Project7/email.edgelist.txt"),
  simulation_vertex_count = 400,
  simulation_probability = 0.01,
  simulation_results = run_simulations(1000, simulation_vertex_count, simulation_probability),
  simulation_results_plot = plot_simulation_results(simulation_results),
  sim_vertex_count = 100,
  sim_ba_graph = barabasi.game(n = sim_vertex_count, power = 1, m = 2, directed = FALSE),
  sim_er_graph = erdos.renyi.game(n = vcount(sim_ba_graph), p.or.m = ecount(sim_ba_graph), type = "gnm", directed = FALSE),
  sim_er_graph_mail = erdos.renyi.game(n = vcount(mail_graph), p.or.m = ecount(mail_graph), type = "gnm", directed = FALSE),
  project_simulation_random_vertices = run_comparison_simulation(1000, list(sim_ba_graph, sim_er_graph), list("BA", "ER"), seq(0, 1, length.out = 100), break_random_vertices),
  project_simulation_random_vertices_plot = plot_graph_resilience_comparison(project_simulation_random_vertices),
  project_simulation_attack_vertices = run_comparison_simulation(1000, list(sim_ba_graph, sim_er_graph), list("BA", "ER"), seq(0, 1, length.out = 100), attack_vertex),
  project_simulation_attack_vertices_plot = plot_graph_resilience_comparison(project_simulation_attack_vertices),
  project_simulation_random_vertices_real = run_comparison_simulation(1000, list(mail_graph, sim_er_graph_mail), list("MAIL", "ER"), seq(0, 1, length.out = 100), break_random_vertices),
  project_simulation_random_vertices_real_plot = plot_graph_resilience_comparison(project_simulation_random_vertices_real),
  project_simulation_attack_vertices_real = run_comparison_simulation(1000, list(mail_graph, sim_er_graph_mail), list("MAIL", "ER"), seq(0, 1, length.out = 100), attack_vertex),
  project_simulation_attack_vertices_real_plot = plot_graph_resilience_comparison(project_simulation_attack_vertices_real),
  project7_report_html = rmarkdown::render(input = knitr_in("Project7/Ryszard.Szymanski-7.Rmd")),
  
  
  ############################
  ##############  Project 8
  ############################
  ant_colony_graph = read_graph("Project8/insecta-ant-colony1.edges", directed = FALSE),
  rec_amazon_graph = read_graph("Project8/rec-amazon.mtx", format = "edge", directed = FALSE),
  cit_dlbp_graph = read_graph("Project8/cit-DBLP.edges", directed = FALSE),
  
  data = target(
    prepare_clustering_coeff_to_degree_relation_data(graph),
    transform = map(graph = c(ant_colony_graph, rec_amazon_graph, cit_dlbp_graph))
  ),
  
  plot = target(
    plot_clustering_coeff_to_degree_relation(data, include_theoretical_curve = TRUE),
    transform = map(data)
  ),
  
  timetable_data = read_timetable_file("Project8/RA200516.TXT"),
  section_metadata = parse_sections(timetable_data),
  stop_metadata = retrieve_stop_metadata(section_metadata[type =="PR"], timetable_data),
  route_data = get_route_data(section_metadata, timetable_data),
  stop_time_metadata = get_stop_time_data(section_metadata, timetable_data),
  
  transport_graph_layer = target(
    create_transport_graph(stop_metadata, route_data, type),
    transform = map(type = c("bus", "tram", "train"))
  ),
  
  transport_graph_layer_plot = target(
    plot_transport_graph(transport_graph_layer),
    transform = combine(transport_graph_layer, .by = type)
  ),
  
  transport_graph_layer_summary = target(
    summarise_graph(transport_graph_layer),
    transform = combine(transport_graph_layer, .by = type)
  ),

  transport_graph = create_transport_graph(stop_metadata, route_data),
  transport_graph_plot = plot_transport_graph(transport_graph),
  transport_graph_layer_plottt = plot_clustering_coeff_to_degree_relation(prepare_clustering_coeff_to_degree_relation_data(transport_graph$graph), include_theoretical_curve = TRUE),
  transport_graph_summary = summarise_graph(transport_graph),
  transport_graph_animation = create_transport_graph_animation(stop_metadata, route_data, stop_time_metadata, "Project8/animation.gif"),
  transport_graph_summary_over_time = get_transport_graph_summary_over_time(stop_metadata, route_data, stop_time_metadata),
  transport_graph_summary_over_time_plot = plot_summary_over_time(transport_graph_summary_over_time),
  
  project8_report_html = rmarkdown::render(input = knitr_in("Project8/Ryszard.Szymanski-8.Rmd")),
  
  ############################
  ##############  Project 9
  ############################
  project9_report_html = rmarkdown::render(input = knitr_in("Project9/Ryszard.Szymanski-9.Rmd")),
)


make(plan)
