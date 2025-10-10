
# Misc. tests ------------------------------------------------------------------

print_turning_movements_for_link_at_node(node_id = "347086", 
                                         link_id = "0.0-1.0@319629-WITH", 
                                         nodes)

turns <- get_turning_movements(nodes, node_id = "271925")

print_turning_movements_for_link("0.0@3112222-1.0@3112230-WITH", data, nodes)

# Find number of traffic links per county
data %>% group_by(county) %>% count() %>% arrange(desc(n))
