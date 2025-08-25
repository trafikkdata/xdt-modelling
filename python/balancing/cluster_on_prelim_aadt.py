from __future__ import annotations

from collections.abc import Iterable

import pandas as pd
import networkx as nx
from utilities import build_graph_from_geojson_as_dictionary


def add_cluster_id_to_df(df: pd.DataFrame, geojson_data: dict) -> pd.DataFrame:
    """
    Add a cluster ID to the DataFrame based on the cluster ID map.

    Args:
        df (pd.DataFrame): The input DataFrame.

    Returns:
        pd.DataFrame: The updated DataFrame with added cluster IDs.
    """
    cluster_id_map = create_map_for_cluster_to_id(df, geojson_data)

    return df.merge(cluster_id_map[['id', 'cluster_id']], on='id', how='left')


def create_map_for_cluster_to_id(df: pd.DataFrame, geojson_data: dict) -> pd.DataFrame:
    """
    This function takes a DataFrame and GeoJSON data as input to first update a graph's nodes with a 'hasPrelimAadt' attribute based on the 'prelimAadt' column in the DataFrame. It then identifies weakly connected components within the graph, considering the 'hasPrelimAadt' attribute to form clusters. Each unique cluster of nodes is assigned a unique ID. The function returns a DataFrame mapping each node to its corresponding cluster ID.

    The function operates in several steps:
    1. It builds a graph from the provided GeoJSON data.
    2. Updates the graph's nodes with a 'hasPrelimAadt' attribute based on the presence of a 'prelimAadt' value in the input DataFrame.
    3. Identifies weakly connected components in the graph and optionally modifies these components based on the 'hasPrelimAadt' attribute.
    4. Assigns a unique ID to each unique set (cluster) of connected components.
    5. Creates and returns a new DataFrame where each row contains a node and its corresponding cluster ID.

    Parameters:
        df (pd.DataFrame): A pandas DataFrame with at least two columns: 'id' and 'prelimAadt'. The 'id' column should correspond to node identifiers in the graph built from `geojson_data`.
        geojson_data (dict): GeoJSON data used to construct a graph. Nodes and edges in this graph represent geographic features and their connections, respectively.

    Returns:
        pd.DataFrame: A DataFrame where each row represents a node, identified by 'id', and its assigned cluster ID under 'cluster_id'. This mapping facilitates the identification of clusters formed based on the connectivity and 'hasPrelimAadt' attribute of nodes.

    """  # noqa: E501
    G = build_graph_from_geojson_as_dictionary(geojson_data)

    df = df[['id', 'prelimAadt']].copy()
    df['hasPrelimAadt'] = df['prelimAadt'].notnull()

    # Update graph nodes with `hasPrelimAadt`
    for _, row in df.iterrows():
        if row['id'] in G.nodes:
            G.nodes[row['id']]['hasPrelimAadt'] = row['hasPrelimAadt']

    components_with_prelim_aadt_included = cluster_weakly_connected_components(G)

    # Assign unique cluster IDs to each component
    unique_id_to_set_with_prelim_aadt: dict[int, frozenset[str]] = {}
    current_id = 1

    for list_of_sets_of_updated_components in components_with_prelim_aadt_included:
        for set_of_updated_components in list_of_sets_of_updated_components:
            frozen_set = frozenset(set_of_updated_components)
            if frozen_set not in unique_id_to_set_with_prelim_aadt.values():
                unique_id_to_set_with_prelim_aadt[current_id] = frozen_set
                current_id += 1

    # Collect rows for DataFrame
    rows = []
    for key, values in unique_id_to_set_with_prelim_aadt.items():
        rows.extend([{'cluster_id': key, 'id': value} for value in values])

    # Create DataFrame and confirm columns
    map_for_cluster_to_id = pd.DataFrame(rows)

    return map_for_cluster_to_id


def get_subgraph_without_prelim_aadt(G: pd.DataFrame) -> list[pd.DataFrame]:
    """Filter nodes in the subgraph of G without 'hasPrelimAadt' property set to True."""
    return [node for node in G.nodes if not G.nodes[node].get('hasPrelimAadt')]


def add_neighboring_prelimAadt_nodes_recursive(components: Iterable, digraph: nx.DiGraph) -> list:  # noqa: C901 TODO
    def add_neighbors(component: set) -> None:
        # This set will track nodes added in this iteration to avoid modifying
        # the set being iterated over
        nodes_added = set()
        for node in component:
            # Check both neighbors (for directed edges) and predecessors (for
            # incoming edges)
            for neighbor in digraph.neighbors(node):
                if digraph.nodes[neighbor].get('hasPrelimAadt', False) and neighbor not in component:
                    nodes_added.add(neighbor)
            for predecessor in digraph.predecessors(node):
                if digraph.nodes[predecessor].get('hasPrelimAadt', False) and predecessor not in component:
                    nodes_added.add(predecessor)
        # Update the original component with newly added nodes
        component.update(nodes_added)
        # If new nodes were added, recurse to explore their neighbors as well
        if nodes_added:
            add_neighbors(component)

    updated_components = []
    for component in components:
        # Make a copy of the current component to modify
        updated_component = set(component)
        # Use the helper function to add neighbors recursively
        add_neighbors(updated_component)
        updated_components.append(updated_component)

    return updated_components


def add_neighboring_prelimAadt_nodes_recursive_greedy(components: Iterable, digraph: nx.DiGraph) -> list:  # noqa: C901 TODO
    def add_neighbors_greedy(component: set) -> None:  # noqa: C901 TODO
        # This set will track nodes added in this iteration to avoid modifying
        # the set being iterated over
        nodes_added = set()
        for node in component:
            # Check both neighbors (for directed edges) and predecessors (for
            # incoming edges)
            added = False  # Flag to indicate if a neighbor has been added

            for neighbor in digraph.neighbors(node):
                if digraph.nodes[neighbor].get('hasPrelimAadt', False) and neighbor not in component:
                    nodes_added.add(neighbor)
                    added = True  # Set the flag when a neighbor is added
                    break  # Break the loop after adding the first valid neighbor

            if not added:  # Only check predecessors if no neighbor was added
                for predecessor in digraph.predecessors(node):
                    if digraph.nodes[predecessor].get('hasPrelimAadt', False) and predecessor not in component:
                        nodes_added.add(predecessor)
                        break  # Break the loop after adding the first valid predecessor

        # Update the original component with newly added nodes
        component.update(nodes_added)
        # If new nodes were added, recurse to explore their neighbors as well
        if nodes_added:
            add_neighbors_greedy(component)

    updated_components = []
    for component in components:
        # Make a copy of the current component to modify
        updated_component = set(component)
        # Use the helper function to add neighbors recursively
        add_neighbors_greedy(updated_component)
        updated_components.append(updated_component)

    return updated_components


def cluster_weakly_connected_components(G: nx.DiGraph) -> list[list[str]]:
    """Process weakly connected components to categorize and expand them based on 'hasPrelimAadt' property."""
    components_with_prelim_aadt_included = []
    components_without_prelim_aadt = []

    # Iterate over all weakly connected components of G
    for components in nx.weakly_connected_components(G):
        component_subgraph = G.subgraph(components)

        # Filter nodes within the component subgraph without 'hasPrelimAadt'
        nodes_without_prelim_aadt = get_subgraph_without_prelim_aadt(component_subgraph)
        subgraph_without_prelim_aadt = component_subgraph.subgraph(nodes_without_prelim_aadt)

        # Convert to undirected graph to find connected components
        undirected_subgraph = subgraph_without_prelim_aadt.to_undirected()
        components = list(nx.connected_components(undirected_subgraph))
        components_without_prelim_aadt.append(components)

        # Expand components by adding neighboring nodes with 'hasPrelimAadt'
        updated_components = add_neighboring_prelimAadt_nodes_recursive_greedy(components, component_subgraph)
        components_with_prelim_aadt_included.append(updated_components)

    return components_with_prelim_aadt_included
