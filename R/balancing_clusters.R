library(igraph)
library(dplyr)

strategic_network_clustering <- function(data, target_clusters = 100, min_network_distance = 5) {
  if(!("id" %in% colnames(data))){
    data$id <- data$parentTrafficLinkId
  }
  
  cat("Building link adjacency graph (links as vertices)...\n")
  
  # Step 1: Build graph where traffic links are vertices
  # Two links are connected if they share a traffic node
  
  n_missing_nodes <- sum(is.na(data$startTrafficNodeId))
  if(n_missing_nodes > 0){
    warning(paste("There are", n_missing_nodes, "traffic links that are missing nodes.\n"))
    data <- data %>% drop_na()
  }
  
  # Create a mapping of traffic nodes to the links that connect to them
  node_to_links <- data %>%
    select(id, startTrafficNodeId, endTrafficNodeId) %>%
    # Reshape to long format: each row is a (node, link) pair
    pivot_longer(cols = c(startTrafficNodeId, endTrafficNodeId), 
                 names_to = "endpoint", 
                 values_to = "traffic_node") %>%
    select(traffic_node, link_id = id) %>% 
    tidyr::drop_na()
  
  # Find pairs of links that share traffic nodes
  # link_connections <- node_to_links %>%
  #   inner_join(node_to_links, by = "traffic_node", suffix = c("_1", "_2")) %>%
  #   filter(link_id_1 != link_id_2) %>%  # Don't connect link to itself
  #   select(from = link_id_1, to = link_id_2) %>%
  #   distinct()  # Remove duplicate pairs
  
  # node_to_links: traffic_node, link_id
  
  link_connections <- node_to_links %>%
    # Self-join on traffic_node to find all links meeting at same node
    inner_join(node_to_links, by = "traffic_node", relationship = "many-to-many") %>%
    # Don't connect link to itself
    filter(link_id.x != link_id.y) %>%
    # Keep one direction only for undirected graph
    mutate(link_pair = pmap_chr(list(link_id.x, link_id.y), ~paste(sort(c(...)), collapse = "-"))) %>%
    distinct(link_pair, .keep_all = TRUE) %>%
    select(from = link_id.x, to = link_id.y)
  
  
  # Build undirected graph with links as vertices
  network_graph <- graph_from_data_frame(
    link_connections, 
    vertices = data.frame(name = data$id),
    directed = FALSE
  )
  
  cat(paste("Link adjacency graph built:", vcount(network_graph), "links,", 
            ecount(network_graph), "connections\n"))
  
  # Step 2: Strategic sampling of measurement points
  measurement_links <- data$id[data$child_link_has_data == TRUE]
  cat(paste("Total measurement links:", length(measurement_links), "\n"))
  
  # Use network distance to thin out measurement points
  selected_barriers <- strategic_sample_barriers(
    network_graph, 
    measurement_links, 
    min_distance = min_network_distance,
    target_count = target_clusters * 2  # Rough heuristic
  )
  
  # OR: randomly sample measurement points
  selected_barriers <- sample(measurement_links, size = 4000)
  
  # OR: Use all measurement links
  selected_barriers <- measurement_links
  
  cat(paste("Selected", length(selected_barriers), "strategic barrier points\n"))
  
  # Step 3: Create base clusters by removing barriers
  cat("Creating base clusters...\n")
  
  # Remove barrier links from network
  remaining_links <- setdiff(V(network_graph)$name, selected_barriers)
  cluster_graph <- induced_subgraph(network_graph, remaining_links)
  
  # Find connected components
  components <- components(cluster_graph)
  cat(paste("Found", components$no, "base clusters\n"))
  
  # Create base cluster assignments
  base_assignments <- data.frame(
    id = names(components$membership),
    cluster_id = components$membership,
    stringsAsFactors = FALSE
  )
  
  # Step 4: Assign barrier links to neighboring clusters
  cat("Assigning barrier links to neighboring clusters...\n")
  
  barrier_assignments <- assign_barriers_to_clusters(
    network_graph, 
    selected_barriers, 
    base_assignments
  )
  
  # Step 5: Handle non-barrier measurement links
  non_barrier_measurements <- setdiff(measurement_links, selected_barriers)
  measurement_assignments <- assign_measurements_to_clusters(
    base_assignments,
    non_barrier_measurements
  )
  
  # Step 6: Combine all assignments
  all_assignments <- rbind(
    base_assignments,
    barrier_assignments,
    measurement_assignments
  )
  
  # Handle any unassigned links (isolated components, etc.)
  unassigned_links <- setdiff(data$id, all_assignments$id)
  if(length(unassigned_links) > 0) {
    cat(paste("Creating singleton clusters for", length(unassigned_links), "unassigned links\n"))
    
    max_cluster <- max(all_assignments$cluster_id)
    singleton_assignments <- data.frame(
      id = unassigned_links,
      cluster_id = (max_cluster + 1):(max_cluster + length(unassigned_links)),
      stringsAsFactors = FALSE
    )
    
    all_assignments <- rbind(all_assignments, singleton_assignments)
  }
  
  # Summary
  cluster_sizes <- table(all_assignments$cluster_id)
  duplicate_count <- sum(duplicated(all_assignments$id))
  
  cat("\nClustering Summary:\n")
  cat(paste("Total clusters:", length(cluster_sizes), "\n"))
  cat(paste("Total assignments:", nrow(all_assignments), "\n"))
  cat(paste("Unique links:", length(unique(all_assignments$id)), "\n"))
  cat(paste("Duplicate assignments (boundary links):", duplicate_count, "\n"))
  cat("Cluster size distribution:\n")
  print(summary(as.numeric(cluster_sizes)))
  
  return(all_assignments)
}

# Helper function: Strategic sampling using network distance
strategic_sample_barriers <- function(graph, measurement_links, min_distance = 5, target_count = 200) {
  
  # Start with all measurement links that exist in the graph
  valid_measurements <- intersect(measurement_links, V(graph)$name)
  
  if(length(valid_measurements) == 0) {
    return(character(0))
  }
  
  selected <- character(0)
  candidates <- valid_measurements
  
  # Greedily select points that are far enough apart
  while(length(candidates) > 0 && length(selected) < target_count) {
    
    # Pick the first candidate for deterministic behavior
    new_barrier <- candidates[1]
    selected <- c(selected, new_barrier)
    
    # Remove candidates that are too close to this new barrier
    if(length(candidates) > 1) {
      # Calculate distances from new barrier to all remaining candidates
      distances <- distances(graph, v = new_barrier, to = candidates, mode = "all")
      
      # Keep only candidates that are far enough away
      far_enough <- candidates[distances[1,] >= min_distance | is.infinite(distances[1,])]
      candidates <- setdiff(far_enough, new_barrier)
    } else {
      candidates <- character(0)
    }
  }
  
  return(selected)
}

# Helper function: Assign barrier links to all neighboring clusters
assign_barriers_to_clusters <- function(network_graph, barriers, base_assignments) {
  
  barrier_assignments <- data.frame(
    id = character(0),
    cluster_id = integer(0),
    stringsAsFactors = FALSE
  )
  
  for(barrier in barriers) {
    if(barrier %in% V(network_graph)$name) {
      # Find links that are neighbors of this barrier
      neighbors <- neighbors(network_graph, barrier)
      neighbor_names <- neighbors$name
      
      # Find which clusters these neighbor links belong to
      neighbor_clusters <- base_assignments$cluster_id[base_assignments$id %in% neighbor_names]
      unique_clusters <- unique(neighbor_clusters)
      
      if(length(unique_clusters) > 0) {
        # Assign barrier to all neighboring clusters
        new_assignments <- data.frame(
          id = rep(barrier, length(unique_clusters)),
          cluster_id = unique_clusters,
          stringsAsFactors = FALSE
        )
        barrier_assignments <- rbind(barrier_assignments, new_assignments)
      }
    }
  }
  
  return(barrier_assignments)
}

# Helper function: Assign non-barrier measurement links to their containing cluster
assign_measurements_to_clusters <- function(base_assignments, measurement_links) {
  
  # These are measurement links that weren't selected as barriers
  # They should be assigned to whichever cluster they ended up in
  measurement_assignments <- base_assignments[base_assignments$id %in% measurement_links, ]
  
  return(measurement_assignments)
}

# Usage:
# cluster_mapping <- strategic_network_clustering(data, target_clusters = 100, min_network_distance = 5)
# 
# # The result will have duplicate rows for boundary measurement points
# # Each boundary point appears once for each cluster it borders