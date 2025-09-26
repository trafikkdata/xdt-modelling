


data <- readRDS("data/processed/engineered_data.rds")

undirected <- data %>% distinct(parentTrafficLinkId, .keep_all = TRUE) %>% 
  select(parentTrafficLinkId, startTrafficNodeId, endTrafficNodeId)

# Create a mapping of traffic nodes to the links that connect to them
node_to_links <- undirected %>%
  select(parentTrafficLinkId, startTrafficNodeId, endTrafficNodeId) %>%
  # Reshape to long format: each row is a (node, link) pair
  pivot_longer(cols = c(startTrafficNodeId, endTrafficNodeId), 
               names_to = "endpoint", 
               values_to = "traffic_node") %>%
  select(traffic_node, link_id = parentTrafficLinkId) %>% 
  tidyr::drop_na()

link_connections <- node_to_links %>%
  # Self-join on traffic_node to find all links meeting at same node
  inner_join(node_to_links, by = "traffic_node", relationship = "many-to-many") %>%
  # Don't connect link to itself
  filter(link_id.x != link_id.y) %>%
  # Keep one direction only for undirected graph
  mutate(link_pair = pmap_chr(list(link_id.x, link_id.y), ~paste(sort(c(...)), collapse = "-"))) %>%
  distinct(link_pair, .keep_all = TRUE) %>%
  select(from = link_id.x, to = link_id.y, traffic_node)


# Build undirected graph with links as vertices
network_graph <- graph_from_data_frame(
  link_connections, 
  vertices = data.frame(name = data$parentTrafficLinkId),
  directed = FALSE
)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Connected Components Analysis ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get connected components
components <- components(network_graph)

# Basic component statistics
cat("Number of connected components:", components$no, "\n")
cat("Total vertices in graph:", vcount(network_graph), "\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Component Size Distribution ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get size of each component
component_sizes <- table(components$membership)
component_sizes_df <- data.frame(
  component_id = as.numeric(names(component_sizes)),
  size = as.numeric(component_sizes)
) %>%
  arrange(desc(size))

# Print component size summary
cat("\nComponent size summary:\n")
print(summary(component_sizes_df$size))

# Show largest components
cat("\nTop 10 largest components:\n")
print(head(component_sizes_df, 10))

# Show smallest components  
cat("\nSmallest components:\n")
print(tail(component_sizes_df, 10))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Detailed Component Analysis ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Percentage of vertices in largest component
largest_component_size <- max(component_sizes_df$size)
pct_in_largest <- round(100 * largest_component_size / vcount(network_graph), 2)
cat("\nLargest component contains:", largest_component_size, "vertices")
cat(" (", pct_in_largest, "% of all vertices)\n", sep = "")

# Count components by size categories
size_categories <- component_sizes_df %>%
  mutate(
    category = case_when(
      size == 1 ~ "Isolated vertices",
      size == 2 ~ "Pairs",
      size <= 5 ~ "Small (3-5)",
      size <= 20 ~ "Medium (6-20)",
      size <= 100 ~ "Large (21-100)",
      TRUE ~ "Very large (100+)"
    )
  ) %>%
  count(category, name = "num_components") %>%
  mutate(total_vertices = component_sizes_df %>% 
           mutate(category = case_when(
             size == 1 ~ "Isolated vertices",
             size == 2 ~ "Pairs", 
             size <= 5 ~ "Small (3-5)",
             size <= 20 ~ "Medium (6-20)",
             size <= 100 ~ "Large (21-100)",
             TRUE ~ "Very large (100+)"
           )) %>%
           group_by(category) %>%
           summarise(vertices = sum(size), .groups = "drop") %>%
           arrange(match(category, c("Isolated vertices", "Pairs", "Small (3-5)", 
                                     "Medium (6-20)", "Large (21-100)", "Very large (100+)"))) %>%
           pull(vertices)
  )

cat("\nComponent categories:\n")
print(size_categories)
