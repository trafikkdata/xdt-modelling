from __future__ import annotations

from collections import deque

import pandas as pd
import networkx as nx
from utils import build_graph_from_geojson_as_dictionary


def filter_df_to_largest_n_components(df: pd.DataFrame, geojson_data: dict, n: int = 2) -> pd.DataFrame:
    G = build_graph_from_geojson_as_dictionary(geojson_data)
    components = list(nx.strongly_connected_components(G))
    largest_components = sorted(components, key=len, reverse=True)[:n]
    nodes_to_keep = set().union(*largest_components)
    filtered_df = df[df['id'].isin(nodes_to_keep)]
    return filtered_df


def add_min_distance_to_prelimAadt(geojson_data: dict, df: pd.DataFrame, n: int = 2) -> pd.DataFrame:
    G = build_graph_from_geojson_as_dictionary(geojson_data)
    components = list(nx.strongly_connected_components(G))
    largest_components = sorted(components, key=len, reverse=True)[:n]
    nodes_to_keep = set().union(*largest_components)
    G = G.subgraph(nodes_to_keep).copy()

    df_prelim = df[df['prelimAadt'].notna()]
    prelimAadt_map = dict(zip(df_prelim['id'], df_prelim['prelimAadt'], strict=False))

    for node in G.nodes():
        prelimAadt_value = prelimAadt_map.get(f'{node}')
        if prelimAadt_value is not None:
            G.nodes[node]['prelimAadt'] = prelimAadt_value

    G_undirected = G.to_undirected()
    min_distance_to_prelimAadt = {node: float('inf') for node in G_undirected.nodes}
    queue = deque([(node, 0) for node, data in G_undirected.nodes(data=True) if 'prelimAadt' in data])
    visited = set()

    while queue:
        current_node, distance = queue.popleft()
        if current_node in visited and min_distance_to_prelimAadt[current_node] <= distance:
            continue
        visited.add(current_node)
        min_distance_to_prelimAadt[current_node] = distance
        for neighbor in G_undirected.neighbors(current_node):
            if neighbor not in visited or distance + 1 < min_distance_to_prelimAadt[neighbor]:
                queue.append((neighbor, distance + 1))

    df = df.copy()  # Ensure df is a copy, not a slice
    df.loc[:, 'numLinksFromPrelimAadt'] = df['id'].map(min_distance_to_prelimAadt)

    return df
