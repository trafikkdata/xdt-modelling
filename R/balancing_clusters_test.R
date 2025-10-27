library(igraph)
library(dplyr)

strategic_network_clustering <- function(data, target_clusters = 100, min_network_distance = 5) {
  undirected <- data %>% distinct(parentTrafficLinkId, .keep_all = TRUE) %>% 
    dplyr::select(parentTrafficLinkId, startTrafficNodeId, endTrafficNodeId)
  
  # Find parent traffic links where both children have data
  parent_links_with_data <- data %>% group_by(parentTrafficLinkId) %>% 
    summarise(child_link_has_data = all(!is.na(bestDataSourceAadt_trafficVolumeValue)))
    # summarise(child_link_has_data = all(bestDataSourceAadt_registrationFrequency == "CONTINUOUS"))
  
  undirected <- dplyr::full_join(undirected, parent_links_with_data)
  
  cat("Building link adjacency graph (links as vertices)...\n")
  
  # Step 1: Build graph where traffic links are vertices
  # Two links are connected if they share a traffic node
  
  n_missing_nodes <- sum(is.na(undirected$startTrafficNodeId))
  if(n_missing_nodes > 0){
    warning(paste("There are", n_missing_nodes, "traffic links that are missing nodes.\n"))
    undirected <- undirected %>% drop_na()
  }
  
  # Create a mapping of traffic nodes to the links that connect to them
  node_to_links <- undirected %>%
    select(parentTrafficLinkId, startTrafficNodeId, endTrafficNodeId) %>%
    # Reshape to long format: each row is a (node, link) pair
    tidyr::pivot_longer(cols = c(startTrafficNodeId, endTrafficNodeId), 
                        names_to = "endpoint", 
                        values_to = "traffic_node") %>%
    dplyr::select(traffic_node, link_id = parentTrafficLinkId) %>% 
    tidyr::drop_na()
  
  # Find pairs of links that share traffic nodes
  link_connections <- node_to_links %>%
    # Self-join on traffic_node to find all links meeting at same node
    dplyr::inner_join(node_to_links, by = "traffic_node", relationship = "many-to-many") %>%
    # Don't connect link to itself
    dplyr::filter(link_id.x != link_id.y) %>%
    # Keep one direction only for undirected graph
    dplyr::mutate(link_pair = purrr::pmap_chr(list(link_id.x, link_id.y), ~paste(sort(c(...)), collapse = "-"))) %>%
    dplyr::distinct(link_pair, .keep_all = TRUE) %>%
    dplyr::select(from = link_id.x, to = link_id.y)
  
  # Build undirected graph with links as vertices
  network_graph <- igraph::graph_from_data_frame(
    link_connections, 
    vertices = data.frame(name = undirected$parentTrafficLinkId),
    directed = FALSE
  )
  
  cat(paste("Link adjacency graph built:", vcount(network_graph), "links,", 
            ecount(network_graph), "connections\n"))
  
  # NEW: Identify mainland vs island components
  cat("Identifying mainland and island components...\n")
  all_components <- components(network_graph)
  
  # Find the largest component (mainland)
  component_sizes <- table(all_components$membership)
  mainland_component_id <- as.integer(names(which.max(component_sizes)))
  
  # Separate mainland and island links
  mainland_links <- names(all_components$membership[all_components$membership == mainland_component_id])
  island_components <- all_components$membership[all_components$membership != mainland_component_id]
  
  cat(paste("Mainland component:", length(mainland_links), "links\n"))
  cat(paste("Island components:", length(unique(island_components)), "separate islands\n"))
  cat(paste("Total island links:", length(island_components), "\n"))
  
  # Create subgraph for mainland only
  mainland_graph <- induced_subgraph(network_graph, mainland_links)
  
  # Step 2: Strategic sampling of measurement points (MAINLAND ONLY)
  measurement_links <- undirected$parentTrafficLinkId[undirected$child_link_has_data == TRUE]
  mainland_measurement_links <- intersect(measurement_links, mainland_links)
  
  cat(paste("Total measurement links:", length(measurement_links), "\n"))
  cat(paste("Mainland measurement links:", length(mainland_measurement_links), "\n"))
  
  # Use all measurement links as barriers (or apply your preferred sampling strategy)
  selected_barriers <- mainland_measurement_links
  
  cat(paste("Selected", length(selected_barriers), "barrier points on mainland\n"))
  
  # Step 3: Create base clusters by removing barriers (MAINLAND ONLY)
  cat("Creating base clusters on mainland...\n")
  
  # Remove barrier links from mainland network
  remaining_links <- setdiff(V(mainland_graph)$name, selected_barriers)
  cluster_graph <- induced_subgraph(mainland_graph, remaining_links)
  
  # Find connected components
  mainland_components <- components(cluster_graph)
  cat(paste("Found", mainland_components$no, "initial base clusters on mainland\n"))
  
  # Create base cluster assignments for mainland
  base_assignments <- data.frame(
    id = names(mainland_components$membership),
    cluster_id = mainland_components$membership,
    stringsAsFactors = FALSE
  )
  
  # Step 3.5: Merge small clusters with neighbors
  cat("Merging small clusters...\n")
  base_assignments <- merge_small_clusters(
    base_assignments,
    mainland_graph,  # Use original graph to find neighbors
    selected_barriers,
    min_size = 100,
    max_size = 1000
  )
  
  cat(paste("After merging:", length(unique(base_assignments$cluster_id)), "clusters remain\n"))
  
  # Step 4: Assign barrier links to neighboring clusters (MAINLAND ONLY)
  cat("Assigning barrier links to neighboring mainland clusters...\n")
  
  barrier_assignments <- assign_barriers_to_clusters(
    mainland_graph, 
    selected_barriers, 
    base_assignments
  )
  
  # Step 5: Handle non-barrier measurement links (MAINLAND ONLY)
  non_barrier_measurements <- setdiff(mainland_measurement_links, selected_barriers)
  measurement_assignments <- assign_measurements_to_clusters(
    base_assignments,
    non_barrier_measurements
  )
  
  # Step 6: Combine mainland assignments
  mainland_assignments <- rbind(
    base_assignments,
    barrier_assignments,
    measurement_assignments
  )
  
  # NEW: Handle island components - each island becomes one cluster
  cat("Assigning island components...\n")
  
  max_mainland_cluster <- max(mainland_assignments$cluster_id)
  
  # Create unique cluster ID for each island component
  island_cluster_map <- data.frame(
    original_component = unique(island_components),
    cluster_id = (max_mainland_cluster + 1):(max_mainland_cluster + length(unique(island_components)))
  )
  
  island_assignments <- data.frame(
    id = names(island_components),
    original_component = as.integer(island_components),
    stringsAsFactors = FALSE
  ) %>%
    left_join(island_cluster_map, by = "original_component") %>%
    select(id, cluster_id)
  
  cat(paste("Assigned", length(unique(island_components)), "island clusters\n"))
  
  # Step 7: Combine mainland and island assignments
  all_assignments <- rbind(
    mainland_assignments,
    island_assignments
  )
  
  # Handle any unassigned links
  unassigned_links <- setdiff(undirected$parentTrafficLinkId, all_assignments$id)
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
  cat(paste("Total clusters:", length(cluster_sizes), "(", mainland_components$no, "mainland +", 
            length(unique(island_components)), "islands)\n"))
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

# Helper function: Merge small clusters with neighboring clusters
merge_small_clusters <- function(base_assignments, network_graph, barrier_links, 
                                 min_size = 100, max_size = 1000) {
  
  # Calculate current cluster sizes
  cluster_sizes <- base_assignments %>%
    count(cluster_id, name = "size")
  
  # Identify small clusters that need merging
  small_clusters <- cluster_sizes %>%
    filter(size < min_size) %>%
    arrange(size) %>%  # Process smallest first
    pull(cluster_id)
  
  cat(paste("Found", length(small_clusters), "small clusters to merge\n"))
  
  # Track which clusters have been merged
  assignments <- base_assignments
  merged_count <- 0
  unable_to_merge <- 0
  
  for(small_cluster_id in small_clusters) {
    # Get current cluster assignment (might have changed due to previous merges)
    current_cluster_id <- assignments$cluster_id[assignments$cluster_id == small_cluster_id][1]
    
    # Skip if this cluster was already merged into another
    if(is.na(current_cluster_id)) next
    
    # Get all links in this small cluster
    cluster_links <- assignments$id[assignments$cluster_id == current_cluster_id]
    current_size <- length(cluster_links)
    
    # Skip if cluster has grown beyond min_size due to merges
    if(current_size >= min_size) next
    
    # Find neighboring clusters using the original network graph
    neighbor_clusters <- find_neighboring_clusters(
      cluster_links,
      network_graph,
      assignments,
      barrier_links
    )
    
    if(length(neighbor_clusters) == 0) {
      unable_to_merge <- unable_to_merge + 1
      next
    }
    
    # Calculate sizes of neighboring clusters
    neighbor_sizes <- cluster_sizes %>%
      filter(cluster_id %in% neighbor_clusters) %>%
      mutate(merged_size = size + current_size) %>%
      filter(merged_size <= max_size)
    
    if(nrow(neighbor_sizes) == 0) {
      # No valid merge targets (all neighbors too large)
      unable_to_merge <- unable_to_merge + 1
      next
    }
    
    # Pick the smallest valid neighbor to keep sizes balanced
    target_cluster <- neighbor_sizes %>%
      arrange(size) %>%
      slice(1) %>%
      pull(cluster_id)
    
    # Merge: reassign all links from small cluster to target cluster
    assignments$cluster_id[assignments$cluster_id == current_cluster_id] <- target_cluster
    
    # Update cluster sizes for future iterations
    cluster_sizes$size[cluster_sizes$cluster_id == target_cluster] <- 
      cluster_sizes$size[cluster_sizes$cluster_id == target_cluster] + current_size
    
    merged_count <- merged_count + 1
  }
  
  cat(paste("Successfully merged", merged_count, "small clusters\n"))
  if(unable_to_merge > 0) {
    cat(paste(unable_to_merge, "small clusters could not be merged (no valid neighbors)\n"))
  }
  
  return(assignments)
}

# Helper function: Find which clusters neighbor a given set of links
find_neighboring_clusters <- function(cluster_links, network_graph, 
                                      base_assignments, barrier_links) {
  
  neighbor_clusters <- c()
  
  for(link in cluster_links) {
    if(link %in% V(network_graph)$name) {
      # Find all neighbors in the original network
      neighbors <- neighbors(network_graph, link)
      neighbor_names <- neighbors$name
      
      # Check both direct non-barrier neighbors AND neighbors across barriers
      for(neighbor in neighbor_names) {
        if(neighbor %in% barrier_links) {
          # This is a barrier - look at its neighbors to find clusters on the other side
          barrier_neighbors <- neighbors(network_graph, neighbor)$name
          # Exclude other barriers and the current cluster's links
          non_barrier_neighbors <- setdiff(barrier_neighbors, barrier_links)
          non_barrier_neighbors <- setdiff(non_barrier_neighbors, cluster_links)
          
          # Find which clusters these belong to
          neighbor_cluster_ids <- base_assignments$cluster_id[base_assignments$id %in% non_barrier_neighbors]
          neighbor_clusters <- c(neighbor_clusters, neighbor_cluster_ids)
          
        } else if(neighbor %in% base_assignments$id) {
          # This is a direct non-barrier neighbor
          neighbor_cluster_id <- base_assignments$cluster_id[base_assignments$id == neighbor]
          neighbor_clusters <- c(neighbor_clusters, neighbor_cluster_id)
        }
      }
    }
  }
  
  # Return unique neighboring cluster IDs (excluding the cluster itself)
  current_cluster <- base_assignments$cluster_id[base_assignments$id == cluster_links[1]]
  unique_neighbors <- unique(neighbor_clusters)
  unique_neighbors <- unique_neighbors[unique_neighbors != current_cluster]
  
  return(unique_neighbors)
}

# Usage:
# cluster_mapping <- strategic_network_clustering(data, target_clusters = 100, min_network_distance = 5)