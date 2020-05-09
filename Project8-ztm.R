library(igraph)
library(ggraph)

read_timetable_file <- function(filepath) {
  file_lines <- readLines(filepath)
  stringi::stri_encode(file_lines, from = "windows-1250", to = "utf-8")
}

parse_pr_section <- function(section) {
  segment_parts <- function(section) {
    depth <- (nchar(section) - nchar(trimws(section, which = "left"))) / 3
    segment_index <- which(depth == min(depth))
    
    if (length(segment_index) == 1) {
      data.table::data.table(
        start_index = segment_index,
        end_index = length(section)
      )
    } else {
      data.table::data.table(
        start_index = segment_index,
        end_index = c(segment_index[2:length(segment_index)] - 1, length(section))
      )
    }
    
  }
  
  section_trimmed <- trimws(section)
  
  parse_single_stop <- function(row) {
    if (is.na(row$start_index) || is.na(row$end_index)) {
      browser()
    }
    content_to_parse <- section_trimmed[row$start_index:row$end_index]
    main_line_content <- strsplit(content_to_parse[1], split = "\\s{2,}")[[1]]
    sub_content <- content_to_parse[-1]
    sub_content <- gsub("L\\s+(\\d+)\\s+-\\s+(.+):\\s+(.+)", "\\1,\\2,\\3", sub_content)
    sub_content_data <- strsplit(sub_content, split = ",")
    lines <- lapply(sub_content_data, function(x) {
      stops <- strsplit(x[3], split = "\\s+")[[1]]
      gsub("\\^", "", stops)
    })
    lines <- do.call(c, lines)
    if (length(main_line_content) != 7) {
      stop("Inconsistent format!")
    }
    
    data.table::data.table(
      id = main_line_content[1],
      line_count = as.numeric(main_line_content[2]),
      street = main_line_content[3],
      direction = main_line_content[4],
      lat = as.numeric(gsub("Y=\\s+(\\d)", "\\1", main_line_content[5])),
      lon = as.numeric(gsub("X=\\s+(\\d)", "\\1", main_line_content[6])),
      pu = as.numeric(gsub("Pu=(\\d)", "\\1", main_line_content[7])),
      lines = paste(lines, collapse = ",")
    )
  }
  
  df_segments <- segment_parts(section)
  stop_data <- df_segments[, parse_single_stop(.SD), by = seq_len(nrow(df_segments)), .SDcols = 1:2]
  stop_data[, seq_len := NULL]
  stop_data
}

parse_sections <- function(timetable_file_lines, parent_section = NA) {
  section_marker_inds <- which(grepl("(^\\s+[*#]|^[*#])[^#]", timetable_file_lines))
  
  section_marker_lines <- timetable_file_lines[section_marker_inds]
  section_marker_lines_ltrimmed <- trimws(section_marker_lines, which = "left")
  section_marker_line_depth <- (nchar(section_marker_lines) - nchar(section_marker_lines_ltrimmed)) / 3
  section_type <- gsub("[*|#]([a-zA-z]+)\\s*(\\d*)", "\\1", section_marker_lines)
  section_nr_metadata <- gsub("[*|#]([a-zA-z]+)\\s*(\\d*)", "\\2", section_marker_lines)
  
  df <- data.table::data.table(
    line = section_marker_lines,
    depth = section_marker_line_depth,
    marker_id = section_marker_inds,
    type = trimws(section_type),
    number_metadata = section_nr_metadata
  )
  
  match_section_markers <- function(df_const_depth) {
    depth <- unique(df_const_depth$depth)
    start_marker_id <- seq(1, nrow(df_const_depth), 2)
    end_marker_id <- seq(2, nrow(df_const_depth), 2)
    
    start_index <- df_const_depth$marker_id[start_marker_id]
    end_index <- df_const_depth$marker_id[end_marker_id]
    start_section_type <- df_const_depth$type[start_marker_id]
    end_section_type <- df_const_depth$type[end_marker_id]
    number_metadata <- df_const_depth$number_metadata[start_marker_id]
    
    data.table::data.table(
      start_type = start_section_type,
      end_type = end_section_type,
      start_index = start_index,
      end_index = end_index,
      number_metadata = number_metadata
    )
  }
  section_metadata <- df[, match_section_markers(.SD), by = depth, .SDcols=seq_len(ncol(df))]
  
  if (any(section_metadata$start_type != section_metadata$end_type)) {
    stop ("Some section markers were paired incorrectly!")
  }
  section_metadata[, c("type", "start_type", "end_type") := .(start_type, NULL, NULL)]
  section_metadata
}

retrieve_stop_metadata <- function(sections_metadata, content) {
  stop_sections_metadata <- sections_metadata[type == "PR"]
  stop_metadata <- stop_sections_metadata[, parse_pr_section(content[(start_index+1):(end_index-1)]), by = seq_len(nrow(stop_sections_metadata))]
  stop_metadata[, seq_len := NULL]
  stop_metadata
}

get_route_data <- function(sections_metadata, content) {
  get_single_route_data <- function(content_to_parse) {
    route <- as.numeric(gsub("(.+)(\\d{6})(.+)", "\\2", content_to_parse))
    route <- route[!is.na(route)]
    
    df <- data.table::data.table(
      from = route[-length(route)],
      to = route[-1]
    )
    
  }
  route_sections_metadata <- sections_metadata[type == "LW"]
  route_data <- route_sections_metadata[, get_single_route_data(content[(start_index+1):(end_index-1)]), by = seq_len(nrow(route_sections_metadata))]
  route_data[, seq_len := NULL]
  unique(route_data)
}

get_stop_time_data <- function(sections_metadata, content) {
  get_single_time_data <- function(content_to_parse) {
    stop <- gsub("\\s+(.+)(\\d{6})\\s(.+)\\s(\\d+.\\d+).+", "\\2", content_to_parse)
    time <- gsub("\\s+(.+)(\\d{6})\\s(.+)\\s(\\d+.\\d+).+", "\\4", content_to_parse)
    
    data.table::data.table(
      stop = stop,
      time = time
    )
  }
  
  time_data_sections_metadata <- sections_metadata[type == "WK"]
  time_data <- time_data_sections_metadata[, get_single_time_data(content[(start_index+1):(end_index-1)]), by = seq_len(nrow(time_data_sections_metadata))]
  time_data[, seq_len := NULL]
  time_data
}

get_stop_type_based_on_line <- function(lines) {
  train_regex <- c("^(R\\d|WKD|RE\\d+|S\\d)$")
  bus_regex <- "^(\\d\\d\\d|L-\\d+|L\\d+|E-\\d+|N\\d+|Z\\d{1,2}|ZE\\d)$"
  tram_regex <- "^(\\d|\\d\\d)$"
  sapply(lines, function(line) {
    all_lines <- strsplit(line, ",")[[1]]
    is_bus <- any(grepl(bus_regex, all_lines))
    is_tram <- any(grepl(tram_regex, all_lines))
    is_train <- any(grepl(train_regex, all_lines))
    types <- c()
    if (is_train) {
      types <- c("train")
    } 
    if (is_tram) {
      types <- c(types, "tram")
    } 
    if (is_bus) {
      types <- c(types, "bus")
    }
    paste(types, collapse = ",")
  }, USE.NAMES = FALSE)
}

create_transport_graph <- function(stop_data, route_data, transport_types = c("bus", "tram", "train")) {
  stop_data[, id := as.numeric(id)]
  stop_data$type <- get_stop_type_based_on_line(stop_data$lines)
  vertex_data <- stop_data[!is.na(lat) & type %in% transport_types]
  
  edges_df <- route_data[from %in% vertex_data$id & to %in% vertex_data$id]
  edges_df[, c("id", "source", "target") := list(.I, from, to)]
  
  edges_final <- merge(edges_df, vertex_data[, .(id, type)], by.x = "from", by.y = "id", all.x = TRUE) %>% 
    merge(vertex_data, .(id, type), by.x = "to", by.y = "id", all.x = TRUE)
  edges_final$type <- mapply(function(x,y) intersect(x,y)[1], strsplit(edges_final$type.x, ","), strsplit(edges_final$type.y, ","))
  vertex_data[, c("x", "y") := list(lon, lat)]
  vertex_pos_df <- vertex_data[, .(x, y)]
  
  vertex_final <- vertex_data[, .(id, type, street, direction)]
  edges_final <- edges_final[, .(from, to, type)]
  
  
  g <- graph_from_data_frame(d = edges_final, directed = FALSE, vertices = vertex_final) 
  list(
    graph = g,
    vertex_pos = vertex_pos_df
  )
}

plot_transport_graph <- function(transport_graph) {
  ggraph(transport_graph$graph, layout = transport_graph$vertex_pos[, .(x, y)]) +
    geom_node_point(size = 0.2) +
    geom_edge_link(aes(color = factor(type)))
}

create_transport_graph_animation <- function(stop_data, route_data, stop_time_metadata, output_file) {
  stop_data[, id := as.numeric(id)]
  stop_data$type <- get_stop_type_based_on_line(stop_data$lines)
  vertex_data <- stop_data[!is.na(lat)]
  
  edges_df <- route_data[from %in% vertex_data$id & to %in% vertex_data$id]
  edges_df[, c("id", "source", "target") := list(.I, from, to)]
  
  edges_final <- merge(edges_df, vertex_data[, .(id, type)], by.x = "from", by.y = "id", all.x = TRUE) %>% 
    merge(vertex_data, .(id, type), by.x = "to", by.y = "id", all.x = TRUE)
  edges_final$type <- mapply(function(x,y) intersect(x,y)[1], strsplit(edges_final$type.x, ","), strsplit(edges_final$type.y, ","))
  vertex_data[, c("x", "y") := list(lon, lat)]
  vertex_pos_df <- vertex_data[, .(x, y)]
  
  vertex_final <- vertex_data[, .(id, type, street, direction)]
  edges_final <- edges_final[, .(from, to, type)]
  
  stop_time_metadata[, time := round(as.numeric(time)) %% 24]
  stop_time_metadata[, stop := as.numeric(stop)]
  stop_time_frame <- stop_time_metadata[, .N, by = .(stop, time)]
  frames <- unique(stop_time_frame$time) %>% sort()
  saveGIF({
    for (frame in frames) {
      stop_time_single_frame = stop_time_frame[time == frame]
      vertex_frame_df <- merge(vertex_data, stop_time_single_frame, by.x = "id", by.y = "stop")
      edges_frame_df <- edges_final[from %in% vertex_frame_df$id & to %in% vertex_frame_df$id]
      vertex_pos_frame_df <- vertex_frame_df[, .(x,y)]
      vertex_frame_df[, `:=`(x = NULL, y = NULL)]
      g <- graph_from_data_frame(d = edges_frame_df, directed = FALSE, vertices = vertex_frame_df) 
      
      p <- ggraph(g, layout = vertex_pos_frame_df) +
        geom_node_point(aes(color = N), size = 5) +
        geom_edge_link(aes(color = factor(type))) +
        labs(title = paste0("Frame ", frame)) +
        ggraph::scale_color_viridis()
      print(p)
    }
  }, movie.name = "tmp.gif", interval = 2)
  
  file.rename(from = "tmp.gif", output_file)
}

get_transport_graph_summary_over_time <- function(stop_data, route_data, stop_time_metadata) {
  stop_data[, id := as.numeric(id)]
  stop_data$type <- get_stop_type_based_on_line(stop_data$lines)
  vertex_data <- stop_data[!is.na(lat)]
  
  edges_df <- route_data[from %in% vertex_data$id & to %in% vertex_data$id]
  edges_df[, c("id", "source", "target") := list(.I, from, to)]
  
  edges_final <- merge(edges_df, vertex_data[, .(id, type)], by.x = "from", by.y = "id", all.x = TRUE) %>% 
    merge(vertex_data, .(id, type), by.x = "to", by.y = "id", all.x = TRUE)
  edges_final$type <- mapply(function(x,y) intersect(x,y)[1], strsplit(edges_final$type.x, ","), strsplit(edges_final$type.y, ","))
  vertex_data[, c("x", "y") := list(lon, lat)]
  vertex_pos_df <- vertex_data[, .(x, y)]
  
  vertex_final <- vertex_data[, .(id, type, street, direction)]
  edges_final <- edges_final[, .(from, to, type)]
  
  stop_time_metadata[, time := round(as.numeric(time)) %% 24]
  stop_time_metadata[, stop := as.numeric(stop)]
  stop_time_frame <- stop_time_metadata[, .N, by = .(stop, time)]
  frames <- unique(stop_time_frame$time) %>% sort()
  single_frame_processing <- function(frame) {
    stop_time_single_frame = stop_time_frame[time == frame]
    vertex_frame_df <- merge(vertex_data, stop_time_single_frame, by.x = "id", by.y = "stop")
    edges_frame_df <- edges_final[from %in% vertex_frame_df$id & to %in% vertex_frame_df$id]
    vertex_pos_frame_df <- vertex_frame_df[, .(x,y)]
    vertex_frame_df[, `:=`(x = NULL, y = NULL)]
    g <- graph_from_data_frame(d = edges_frame_df, directed = FALSE, vertices = vertex_frame_df) 
    summarise_graph(list(graph = g))
  }
  
  df <- data.table::rbindlist(lapply(frames, single_frame_processing))
  df$frame <- frames
  df
}

summarise_graph <- function(transport_graph) {
  graph <- transport_graph$graph
  data.frame(
    "Licza wierzchołków" = vcount(graph),
    "Liczba krawędzi" = ecount(graph),
    "Spójność" = is_connected(graph),
    "Średni stopień wierzchołka" = mean(degree(graph)),
    "Współczynnik gronowania" = transitivity(graph)
  )
}

plot_summary_over_time <- function(transport_graph_summary_over_time) {
 transport_graph_summary_over_time %>% 
    tidyr::pivot_longer(cols = -frame, names_to = "Variable", values_to = "Value") %>% 
    ggplot(aes(x = frame, y = Value, color = Variable)) +
    geom_point() +
    facet_wrap(. ~ Variable, scales = "free_y")
}
