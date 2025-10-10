

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
trondheim_data <- read_sf("data/processed/trondheim_data.geojson")

A1 <- build_incidence_matrix(nodes = nodes, traffic_links = trondheim_data, nodes_to_balance = "all")

examine_node_flow(A1, "3508236")



# We get a problem at this node since it has one incoming link that has no outgoing link.
# Need to figure out how to handle.
problem_node <- filter(nodes, id == "3889039")
res_test <- process_turning_movements(turning_movements_json = problem_node$legalTurningMovements, 
                                      link_ids = trondheim_data$id, 
                                      node_id = "3889039")

# What about the boundary case for clusters? 
# The node that is "ouside" the cluster should not be balanced. 

clustered_trd <- strategic_network_clustering(st_drop_geometry(trondheim_data))
cluster_16 <- trondheim_data %>% 
  right_join(filter(clustered_trd, cluster_id == 16), by = join_by(parentTrafficLinkId == id),
            relationship = "many-to-many")

print_turning_movements_for_link("0.0-0.39279228@72814-WITH", data = trondheim_data, nodes = nodes)


A1_old <- build_incidence_matrix(nodes = nodes, traffic_links = cluster_16, nodes_to_balance = "complete_nodes")
A1 <- build_incidence_matrix(nodes = nodes, traffic_links = cluster_16, nodes_to_balance = "complete_nodes")

# Node 92136 is the boundary node
boundary_node <- filter(nodes, id == "92136")
boundary_balancing <- process_turning_movements(
  turning_movements_json = boundary_node$legalTurningMovements, 
  link_ids = cluster_16$id, 
  node_id = "92136")

# Check if all outgoing and incoming are in group


# Identify nonzero elements
nonzero_idx <- boundary_balancing$constraint_rows != 0

# Show column names and corresponding values
data.frame(
  column = colnames(boundary_balancing$constraint_rows)[nonzero_idx],
  value = boundary_balancing$constraint_rows[nonzero_idx]
)


# Check the other boundary node
print_turning_movements_for_link("0.62832538-0.98872029@41927-WITH", data = trondheim_data, nodes = nodes)
boundary_node2 <- filter(nodes, id == "92200")
boundary_balancing2 <- process_turning_movements(
  turning_movements_json = boundary_node2$legalTurningMovements, 
  link_ids = cluster_16$id, 
  node_id = "92200")

boundary_balancing2$constraint_rows 
