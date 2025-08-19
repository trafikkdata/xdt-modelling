balance_predictions <- function(data, model, constraint_matrix = NULL,
                                colname_aadt = "aadt", colname_sd = "aadt_sd"){
  # Step 0: Set up data and constraint matrices
  # Flow constraints
  if(is.null(constraint_matrix)){
    A1 <- build_flow_constraints(data)
  }else{
    A1 <- constraint_matrix
    #A1 <- A1[-1, ]
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
  
  Sigma_vb <- Sigma_v %*% t(A)
  
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
    lambda <- 1e-10
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
    )
  ))
}


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


