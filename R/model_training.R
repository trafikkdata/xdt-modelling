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


#' Build flow constraint matrix
#'
#' @param link_data Data frame with columns "startTrafficNodeId" and "endTrafficNodeId".
#'
#' @returns A matrix with rows equal to the number of nodes (with incoming and outgoing traffic), and columns equal to the number of traffic links. In a given row, the links corresponding to incoming traffic get value 1, and the links corresponding to outgoing traffic get value -1.
#' @export
#'
build_flow_constraints <- function(link_data) {
  n_links <- nrow(link_data)
  constraints <- list()
  
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
    }
  }
  
  # Convert to matrix
  if(length(constraints) > 0) {
    return(do.call(rbind, constraints))
  } else {
    return(NULL)
  }
}

#' Build measurement matrix
#'
#' @param data Data frame containing the row "prelimAadt"
#'
#' @returns A matrix with each row corresponding to a traffic link with a measured traffic volume, and columns corresponding to all traffic links. For each row, the entry corresponding to the given traffic link has value 1.
#' @export
#'
build_measurement_matrix <- function(data) {
  n_e <- nrow(data)
  measured_links <- which(!is.na(data$prelimAadt))
  n_p <- length(measured_links)
  
  # Create sparse matrix directly
  A_2 <- sparseMatrix(
    i = 1:n_p,           # row indices
    j = measured_links,   # column indices
    x = 1,               # values (all 1s)
    dims = c(n_p, n_e)
  )
  
  return(A_2)
}
