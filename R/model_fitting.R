# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Balance predictions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

balance_predictions <- function(data, model, constraint_matrix = NULL,
                                colname_aadt = "aadt", colname_sd = "aadt_sd", 
                                lambda = 1e-10){
  # Step 0: Set up data and constraint matrices
  # Flow constraints
  if(is.null(constraint_matrix)){
    A1 <- build_flow_constraints(data)
  }else{
    A1 <- constraint_matrix
  }

  n_p <- sum(!is.na(data[[colname_aadt]])) # No. of AADT values
  n_e <- nrow(data) # No. of traffic links (edges)
  n_n <- nrow(A1) # No. of traffic nodes (with entering and exiting traffic)
  
  A2 <- as.matrix(build_measurement_matrix(data, colname_aadt = colname_aadt))
  d <- na.omit(data[[colname_aadt]])
  Sigma_epsilon_mark <- diag(na.omit(data[[colname_sd]])^2)
  
  mu_v <- round(model$summary.fitted.values[, "0.5quant"])
  marginal_sds <- model$summary.fitted.values[, "sd"]
  
  Sigma_v <- diag(marginal_sds^2) 
  
  b <- c(rep(0, n_n), d)
  
  # Step 1: Handle extreme variances
  if (max(diag(Sigma_v)) > 1e10 || kappa(Sigma_v) > 1e12) {
    normal_vars <- diag(Sigma_v)[diag(Sigma_v) < 1e8]
    max_reasonable <- max(normal_vars) * 10
    capped_count <- sum(diag(Sigma_v) > max_reasonable)
    diag(Sigma_v)[diag(Sigma_v) > max_reasonable] <- max_reasonable
    warning(paste("Capped", capped_count, "extreme variances"))
  }
  
  # Step 2: Convert to dense matrices
  A1 <- as.matrix(A1)
  A2 <- as.matrix(A2)
  A <- rbind(A1, A2)
  
  Sigma_vb <- Sigma_v %*% t(A)
  
  
  # Step 3: Check system properties
  rank_A1 <- qr(A1)$rank
  rank_A2 <- qr(A2)$rank  
  rank_A <- qr(A)$rank
  rank_deficit <- nrow(A) - rank_A
  
  # Step 4: Build covariance matrix
  Sigma_epsilon <- as.matrix(bdiag(list(diag(rep(0, n_n)), Sigma_epsilon_mark)))
  Sigma_b <- A %*% Sigma_v %*% t(A) + Sigma_epsilon
  Sigma_b <- (Sigma_b + t(Sigma_b))/2  # Ensure symmetry
  
  # Step 5: Robust inversion
  if (rank_deficit > 0) {
    # Use pseudoinverse for rank-deficient systems
    Sigma_b_inv <- MASS::ginv(Sigma_b)
    method <- "pseudoinverse"
  } else if (kappa(Sigma_b) > 1e12) {
    # Use regularization for ill-conditioned systems
    Sigma_b_reg <- Sigma_b + lambda * diag(nrow(Sigma_b))
    Sigma_b_inv <- solve(Sigma_b_reg)
    method <- "regularized"
  } else {
    # Standard inversion
    Sigma_b_inv <- solve(Sigma_b)
    method <- "standard"
  }
  
  # Step 6: Calculate posterior mean and variance
  mu_v_given_b <- mu_v + Sigma_vb %*% Sigma_b_inv %*% (b - A %*% mu_v)
  Sigma_v_given_b <- Sigma_v - Sigma_vb %*% Sigma_b_inv %*% t(Sigma_vb)
  
  # Some elements may be less than 0, set them to small number
  mu_v_given_b[mu_v_given_b <= 0] <- 1
  
  
  # Step 7: Return results with diagnostics
  
  # Return the data frame with added columns for inla model and balanced results.
  result_data <- data %>% 
    mutate(inla_pred = mu_v, 
           inla_sd = marginal_sds,
           balanced_pred = mu_v_given_b,
           balanced_sd = Sigma_v_given_b)
  
  return(list(
    results = result_data,
    diagnostics = list(
      method_used = method,
      rank_deficit = rank_deficit,
      condition_number = kappa(Sigma_b),
      n_measurements = nrow(A2),
      n_links = ncol(A),
      n_constraints = nrow(A1),
      underdetermined = ncol(A2) > nrow(A2)
    ),
    matrices = list(A1 = A1, A2 = A2)
  ))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build adjacency matrix ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Build adjacency matrix
#'
#' @param link_data Data frame with columns "startTrafficNodeId" and "endTrafficNodeId".
#'
#' @returns Adjacency matrix. The number of rows and columns are both equal to the number of traffic links. An entry is 1 if the correspondng traffic links are connected, 0 otherwise.
#' @export
#'
build_adjacency_matrix <- function(link_data) {
  n_links <- nrow(link_data)
  
  # Create all possible pairs and check for shared nodes
  link_pairs <- expand.grid(i = 1:n_links, j = 1:n_links)
  link_pairs <- link_pairs[link_pairs$i != link_pairs$j, ]  # remove self-pairs
  
  # Check if pairs share nodes
  shared_node <- (
    (link_data$startTrafficNodeId[link_pairs$i] == link_data$startTrafficNodeId[link_pairs$j]) |
      (link_data$startTrafficNodeId[link_pairs$i] == link_data$endTrafficNodeId[link_pairs$j]) |
      (link_data$endTrafficNodeId[link_pairs$i] == link_data$startTrafficNodeId[link_pairs$j]) |
      (link_data$endTrafficNodeId[link_pairs$i] == link_data$endTrafficNodeId[link_pairs$j])
  )
  
  # Build sparse matrix WITHOUT symmetric constraint
  #library(Matrix)
  adj_sparse <- Matrix::sparseMatrix(
    i = link_pairs$i[shared_node],
    j = link_pairs$j[shared_node],
    x = 1,
    dims = c(n_links, n_links)
  )
  
  # Make it symmetric manually
  adj_sparse <- adj_sparse | Matrix::t(adj_sparse)
  
  return(adj_sparse)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build incidence matrix ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Build flow constraint matrix
#' 
#' This is old and balances at traffic nodes, not flow nodes!!!
#' 
#' @param link_data Data frame with columns "startTrafficNodeId" and "endTrafficNodeId".
#'
#' @returns A matrix with rows equal to the number of nodes (with incoming and outgoing traffic), and columns equal to the number of traffic links. In a given row, the links corresponding to incoming traffic get value 1, and the links corresponding to outgoing traffic get value -1.
#' @export
#'
build_flow_constraints <- function(link_data) {
  n_links <- nrow(link_data)
  constraints <- list()
  node_names <- character()  # Track which node each row represents
  
  # Get all unique nodes in the network
  all_nodes <- unique(c(link_data$startTrafficNodeId, link_data$endTrafficNodeId))
  
  for(node_id in all_nodes) {
    # Find links that END at this node (incoming traffic)
    incoming_links <- which(link_data$endTrafficNodeId == node_id)
    
    # Find links that START from this node (outgoing traffic)
    outgoing_links <- which(link_data$startTrafficNodeId == node_id)
    
    # Only create constraint if we have both incoming and outgoing traffic
    # (otherwise it's a network boundary node)
    if(length(incoming_links) > 0 & length(outgoing_links) > 0) {
      constraint_row <- rep(0, n_links)
      constraint_row[incoming_links] <- 1    # incoming = +1
      constraint_row[outgoing_links] <- -1   # outgoing = -1
      constraints <- append(constraints, list(constraint_row))
      node_names <- c(node_names, as.character(node_id))  # Store the node ID
    }
  }
  
  # Convert to matrix and add rownames and colnames
  if(length(constraints) > 0) {
    result_matrix <- do.call(rbind, constraints)
    rownames(result_matrix) <- node_names
    colnames(result_matrix) <- as.character(link_data$id)  # Add link IDs as column names
    return(result_matrix)
  } else {
    return(NULL)
  }
}

#' Process turning movements JSON string and create flow nodes
#'
#' @param turning_movements_json character string containing JSON data
#' @param link_ids character vector of all possible link IDs  
#' @param node_id character string identifying the physical node
#'
#' @return list containing flow node constraints and metadata
#'
process_turning_movements <- function(turning_movements_json, link_ids, node_id) {
  # Parse JSON string
  if(is.na(turning_movements_json) || turning_movements_json == "" || is.null(turning_movements_json)) {
    return(list(
      flow_nodes = character(0),
      constraint_rows = matrix(nrow = 0, ncol = length(link_ids)),
      movements_data = data.frame()
    ))
  }
  
  # Clean and parse JSON
  movements <- jsonlite::fromJSON(turning_movements_json, simplifyVector = FALSE)
  
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
#' 
create_flow_nodes <- function(movements_df, node_id) {

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


#' Find connected components in bipartite graph using union-find
#'
#' @param edges data frame with from/to columns representing graph edges
#' @param all_incoming character vector of incoming link IDs
#' @param all_outgoing character vector of outgoing link IDs
#'
#' @return list of character vectors, each representing a connected component
#' 
find_connected_components <- function(edges, all_incoming, all_outgoing) {
  
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


#' Determine the flow type based on incoming/outgoing link counts
#'
#' @param incoming_links character vector of incoming links
#' @param outgoing_links character vector of outgoing links
#'
#' @return character string describing flow type
#' 
determine_flow_type <- function(incoming_links, outgoing_links) {
  
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

#' Build incidence matrix from traffic links and constructed flow nodes
#'
#' @param nodes data frame containing legalTurningMovements column (which has JSON syntax)
#' @param traffic_links traffic link data set
#'
#' @return incidence matrix
#' 
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
# Build measurement matrix ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Build measurement matrix
#'
#' @param data Data frame containing the row "prelimAadt"
#'
#' @returns A matrix with each row corresponding to a traffic link with a measured traffic volume, and columns corresponding to all traffic links. For each row, the entry corresponding to the given traffic link has value 1.
#' @export
#'
build_measurement_matrix <- function(data, colname_aadt = "aadt") {
  n_e <- nrow(data)
  measured_links <- which(!is.na(data[[colname_aadt]]))
  n_p <- length(measured_links)
  
  # Create sparse matrix directly
  A_2 <- Matrix::sparseMatrix(
    i = 1:n_p,           # row indices
    j = measured_links,   # column indices
    x = 1,               # values (all 1s)
    dims = c(n_p, n_e)
  )
  
  return(A_2)
}


