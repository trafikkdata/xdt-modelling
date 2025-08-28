# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Turning Movements Matrix Row Constructor ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(jsonlite)

process_turning_movements <- function(turning_movements_json, link_ids, node_id) {
  #' Process turning movements JSON string and create flow nodes
  #'
  #' @param turning_movements_json character string containing JSON data
  #' @param link_ids character vector of all possible link IDs  
  #' @param node_id character string identifying the physical node
  #'
  #' @return list containing flow node constraints and metadata
  
  # Parse JSON string
  if(is.na(turning_movements_json) || turning_movements_json == "" || is.null(turning_movements_json)) {
    return(list(
      flow_nodes = character(0),
      constraint_rows = matrix(nrow = 0, ncol = length(link_ids)),
      movements_data = data.frame()
    ))
  }
  
  # Clean and parse JSON
  movements <- fromJSON(turning_movements_json, simplifyVector = FALSE)
  
  if(length(movements) == 0) {
    return(list(
      flow_nodes = character(0),
      constraint_rows = matrix(nrow = 0, ncol = length(link_ids)),
      movements_data = data.frame()
    ))
  }
  
  # Convert to data frame for easier processing
  movements_df <- data.frame(
    incoming = character(length(movements)),
    outgoing = I(vector("list", length(movements))),  # Use I() to keep as list column
    stringsAsFactors = FALSE
  )
  
  for(i in seq_along(movements)) {
    movement <- movements[[i]]
    movements_df$incoming[i] <- movement$incomingId
    movements_df$outgoing[[i]] <- unlist(movement$outgoingIds)
  }
  
  # Create flow nodes by grouping movements
  flow_nodes <- create_flow_nodes(movements_df, node_id)
  
  # Build constraint matrix rows
  n_flow <- length(flow_nodes)
  n_links <- length(link_ids)
  constraint_rows <- matrix(0, nrow = n_flow, ncol = n_links)
  colnames(constraint_rows) <- link_ids
  
  flow_node_names <- character(n_flow)
  
  for(i in seq_along(flow_nodes)) {
    flow_node <- flow_nodes[[i]]
    flow_node_names[i] <- flow_node$name
    
    # Set -1 for incoming links
    for(incoming_link in flow_node$incoming_links) {
      if(incoming_link %in% link_ids) {
        col_idx <- which(link_ids == incoming_link)
        constraint_rows[i, col_idx] <- -1
      }
    }
    
    # Set +1 for outgoing links  
    for(outgoing_link in flow_node$outgoing_links) {
      if(outgoing_link %in% link_ids) {
        col_idx <- which(link_ids == outgoing_link)
        constraint_rows[i, col_idx] <- 1
      }
    }
  }
  
  rownames(constraint_rows) <- flow_node_names
  
  return(list(
    flow_nodes = flow_node_names,
    constraint_rows = constraint_rows,
    movements_data = movements_df
  ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Flow Node Creation Helper ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

create_flow_nodes <- function(movements_df, node_id) {
  #' Create flow nodes from turning movements using graph connectivity
  #'
  #' Strategy: 
  #' 1. Create a bipartite graph of incoming -> outgoing connections
  #' 2. Find connected components in this graph
  #' 3. Each connected component becomes one flow node
  #' 
  #' @param movements_df data frame with incoming and outgoing columns
  #' @param node_id character string for the physical node
  #'
  #' @return list of flow node objects
  
  if(nrow(movements_df) == 0) {
    return(list())
  }
  
  # Get all unique incoming and outgoing links
  all_incoming <- unique(movements_df$incoming)
  all_outgoing <- unique(unlist(movements_df$outgoing))
  
  # Create bipartite adjacency representation
  # We'll use a simple approach: create edges and find connected components
  edges <- data.frame(
    from = character(0),
    to = character(0),
    stringsAsFactors = FALSE
  )
  
  # Add edges for each turning movement
  for(i in seq_len(nrow(movements_df))) {
    incoming_link <- movements_df$incoming[i]
    outgoing_links <- movements_df$outgoing[[i]]
    
    for(outgoing_link in outgoing_links) {
      edges <- rbind(edges, data.frame(
        from = incoming_link,
        to = outgoing_link,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Find connected components using union-find approach
  components <- find_connected_components(edges, all_incoming, all_outgoing)
  
  # Create flow nodes from components
  flow_nodes <- list()
  
  for(i in seq_along(components)) {
    component <- components[[i]]
    
    # Separate incoming and outgoing links in this component
    component_incoming <- intersect(component, all_incoming)
    component_outgoing <- intersect(component, all_outgoing)
    
    # Determine flow type for naming
    flow_type <- determine_flow_type(component_incoming, component_outgoing)
    
    flow_node <- list(
      name = paste0(node_id, "_component_", i, "_", flow_type),
      incoming_links = component_incoming,
      outgoing_links = component_outgoing,
      physical_node = node_id,
      flow_type = flow_type
    )
    
    flow_nodes[[i]] <- flow_node
  }
  
  return(flow_nodes)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Connected Components Helper Functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

find_connected_components <- function(edges, all_incoming, all_outgoing) {
  #' Find connected components in bipartite graph using union-find
  #'
  #' @param edges data frame with from/to columns representing graph edges
  #' @param all_incoming character vector of incoming link IDs
  #' @param all_outgoing character vector of outgoing link IDs
  #'
  #' @return list of character vectors, each representing a connected component
  
  if(nrow(edges) == 0) {
    return(list())
  }
  
  # All nodes in the bipartite graph
  all_nodes <- unique(c(all_incoming, all_outgoing))
  
  # Initialize union-find structure
  parent <- setNames(all_nodes, all_nodes)  # Each node is its own parent initially
  
  # Union-find helper functions
  find_root <- function(node) {
    if(parent[node] != node) {
      parent[node] <<- find_root(parent[node])  # Path compression
    }
    return(parent[node])
  }
  
  union_nodes <- function(node1, node2) {
    root1 <- find_root(node1)
    root2 <- find_root(node2)
    if(root1 != root2) {
      parent[root2] <<- root1
    }
  }
  
  # Process all edges to build connected components
  for(i in seq_len(nrow(edges))) {
    union_nodes(edges$from[i], edges$to[i])
  }
  
  # Group nodes by their root
  components_map <- list()
  for(node in all_nodes) {
    root <- find_root(node)
    if(is.null(components_map[[root]])) {
      components_map[[root]] <- character(0)
    }
    components_map[[root]] <- c(components_map[[root]], node)
  }
  
  # Convert to list format
  components <- unname(components_map)
  
  # Filter out empty components
  components[lengths(components) > 0]
}

determine_flow_type <- function(incoming_links, outgoing_links) {
  #' Determine the flow type based on incoming/outgoing link counts
  #'
  #' @param incoming_links character vector of incoming links
  #' @param outgoing_links character vector of outgoing links
  #'
  #' @return character string describing flow type
  
  n_in <- length(incoming_links)
  n_out <- length(outgoing_links)
  
  if(n_in == 1 && n_out == 1) {
    return("passthrough")
  } else if(n_in > 1 && n_out == 1) {
    return("merge")
  } else if(n_in == 1 && n_out > 1) {
    return("split")  
  } else if(n_in > 1 && n_out > 1) {
    return("mixing")
  } else {
    return("unknown")
  }
}

build_incidence_matrix <- function(nodes, traffic_links){
  # Filter out the nodes that appear in the traffic link data
  relevant_nodes <- unique(c(traffic_links$startTrafficNodeId, traffic_links$endTrafficNodeId))
  
  # Get character vector of traffic link id's
  traffic_link_ids <- traffic_links$id
  
  # Initialize matrix
  A1 <- matrix(ncol = length(traffic_link_ids))
  
  # Iterate over the traffic nodes
  for(node in relevant_nodes){
    # Get legal turning movements for traffic node
    node_row <- dplyr::filter(nodes, id == node)
    turning_movements <- node_row$legalTurningMovements
    
    # Process turning movements to get flow nodes and the corresponding row(s)
    # for the incidence matrix.
    results <- process_turning_movements(turning_movements, 
                                         traffic_link_ids, 
                                         node)
    row_in_incidence_matrix <- results$constraint_rows
    A1 <- rbind(A1, row_in_incidence_matrix)
  }
  
  # Remove first row since this is just NA from initialization
  A1 <- A1[-1, ]
  
  return(A1)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example Usage ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Graph-based connected components examples:

# Case 1: Highway merging example
turning_movements_json1 <- '[
  { "incomingId": "C", "outgoingIds": [ "B_with" ] },
  { "incomingId": "B_against", "outgoingIds": [ "A_against" ] },
  { "incomingId": "A_with", "outgoingIds": [ "B_with" ] }
]'
# Expected: 2 components
# Component 1: {C, A_with} -> {B_with} (merge)
# Component 2: {B_against} -> {A_against} (passthrough)

# Case 2: Roundabout example
turning_movements_json2 <- '[
  { "incomingId": "G_with", "outgoingIds": [ "F_against" ] },
  { "incomingId": "F_with", "outgoingIds": [ "G_against" ] },
  { "incomingId": "E", "outgoingIds": [ "G_against", "F_against" ] }
]'
# Expected: 1 component (all connected through E)
# Component 1: {G_with, F_with, E} -> {F_against, G_against} (mixing)

link_ids <- c("A_with", "A_against", "B_with", "B_against", "C", "E", "F_with", "F_against", "G_with", "G_against")

result1 <- process_turning_movements(turning_movements_json1, link_ids, "n1")
result2 <- process_turning_movements(turning_movements_json2, link_ids, "n2")

print("Case 1 (highway merging):")
print(result1$flow_nodes)
print(result1$constraint_rows)
cat("\n")

print("Case 2 (roundabout mixing):")
print(result2$flow_nodes)
print(result2$constraint_rows)


# Trondheim
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")
A1 <- build_incidence_matrix(nodes = nodes, traffic_links = trondheim_data)

problem_node <- filter(nodes, id == "3889039")
res_test <- process_turning_movements(turning_movements_json = problem_node$legalTurningMovements, 
                                      link_ids = traffic_links$id, 
                                      node_id = "3889039")

