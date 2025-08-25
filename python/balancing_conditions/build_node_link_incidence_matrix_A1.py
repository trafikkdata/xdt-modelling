from __future__ import annotations

import numpy as np
import pandas as pd


def build_node_link_incidence_matrix_A1(traffic_node_network: pd.DataFrame, link_ids: list[str]) -> np.ndarray:
    """
    Build a balancing matrix A1 for a traffic network.

    A1: Node-link incidence matrix (Balancing matrix)
        This part of the matrix represents the relationship between traffic nodes
        (intersections) and traffic links (roads). It ensures the flow conservation at each node, meaning the total incoming traffic equals the total outgoing traffic at each intersection.

    Parameters:
    -----------
    traffic_node_network : pd.DataFrame
        A dataframe containing information about the traffic nodes in the network.
    link_ids : List[str]
        A list of unique identifiers for traffic links.

    Returns:
    --------
    np.ndarray
        The balancing matrix A1 with dimensions `len(node_ids) x len(link_ids)`.

    Raises:
    -------
    ValueError
        If a link ID is not found in the link_ids list.
    """  # noqa: E501

    node_ids = traffic_node_network['id'].tolist()

    n_nodes = len(node_ids)
    n_links = len(link_ids)

    A1 = np.zeros((n_nodes, n_links), dtype=int)

    # Populate the balancing matrix
    for _, traffic_node in traffic_node_network.iterrows():
        node_row_index = node_ids.index(traffic_node['id'])
        turning_movements = traffic_node.get('legalTurningMovements', [])
        if turning_movements is None:
            print(f'No legal turning movements found for node {traffic_node["id"]}')
        else:
            for movement in turning_movements:
                if 'incomingId' in movement:
                    column_link_index = link_ids.index(movement['incomingId'])
                    A1[node_row_index, column_link_index] = -1

                for outgoing_id in movement.get('outgoingIds', []):
                    column_link_index = link_ids.index(outgoing_id)
                    A1[node_row_index, column_link_index] = 1
    return A1


def observation_from_A1_for_b(A1: pd.DataFrame) -> np.ndarray:
    return np.zeros(len(A1)).reshape(-1, 1)


def uncertainty_from_A1_for_Sigma_epsilon(A1: pd.DataFrame) -> np.ndarray:
    return np.zeros(len(A1)).reshape(-1, 1)
