from __future__ import annotations

import scipy as sp
import pandas as pd


def extract_legal_turning_movements(geojson_data: dict) -> list[dict]:
    """
    Extracts legal turning movements from the GeoJSON data.

    Iterates over the features in the GeoJSON data and collects all legal turning movements
    from the `legalTurningMovements` property.

    Args:
        geojson_data (dict): A dictionary containing GeoJSON data with features.

    Returns:
        List[dict]: A list of dictionaries representing legal turning movements.
    """
    legal_turning_movements = []
    for feature in geojson_data['features']:
        properties = feature.get('properties', {})
        legal_turning_movements_list = properties.get('legalTurningMovements')
        if isinstance(legal_turning_movements_list, list):
            legal_turning_movements.extend(legal_turning_movements_list)
    return legal_turning_movements


def get_unique_directed_traffic_link_ids_from_traffic_node_network(legal_turning_movements: list[dict]) -> list[str]:
    """
    Retrieves unique directed traffic link IDs from a list of legal turning movements.

    Extracts incoming and outgoing IDs from the turning movements and returns a unique list
    of directed traffic link IDs.

    Args:
        legal_turning_movements (List[dict]): A list of dictionaries representing legal turning movements.

    Returns:
        List[str]: A list of unique directed traffic link IDs.
    """
    directed_traffic_links_id = set()
    for turning_movement in legal_turning_movements:
        incoming_id = turning_movement['incomingId']
        outgoing_ids = turning_movement['outgoingIds']
        directed_traffic_links_id.add(incoming_id)
        directed_traffic_links_id.update(outgoing_ids)
    return list(directed_traffic_links_id)


def get_unique_directed_traffic_link_ids_from_preprocessed_directed_links(traffic_data: pd.DataFrame) -> list[str]:
    """
    Retrieves unique directed traffic link IDs from preprocessed traffic data.

    Extracts the unique IDs from the "id" column of a DataFrame containing traffic data.

    Args:
        traffic_data (pd.DataFrame): A pandas DataFrame containing preprocessed traffic data.

    Returns:
        List[str]: A list of unique directed traffic link IDs.
    """
    return list(set(traffic_data['id']))


def get_intersection_of_directed_traffic_link_ids(ids_from_network: list[str], ids_from_data: list[str]) -> list[str]:
    """
    Finds the intersection of two lists of directed traffic link IDs.

    Computes the intersection of IDs found in a traffic node network and IDs found in preprocessed traffic data.

    Args:
        ids_from_network (List[str]): A list of directed traffic link IDs from the traffic node network.
        ids_from_data (List[str]): A list of directed traffic link IDs from preprocessed traffic data.

    Returns:
        List[str]: A list of IDs present in both input lists.
    """
    return list(set(ids_from_network).intersection(set(ids_from_data)))


def create_adjacency_matrix(
    legal_turning_movements: list[dict], directed_traffic_links_id: list[str], link_id_to_index: dict[str, int]
) -> sp.sparse.csr_matrix:
    """
    Creates an adjacency matrix representing the connectivity between directed traffic links.

    The adjacency matrix is constructed based on legal turning movements, where a 1 indicates
    a legal movement from one link to another.

    Args:
        legal_turning_movements (List[dict]): A list of legal turning movements.
        directed_traffic_links_id (List[str]): A list of unique directed traffic link IDs.
        link_id_to_index (Dict[str, int]): A mapping from link IDs to their indices.

    Returns:
        scipy.sparse.csr_matrix: An adjacency matrix representing the connectivity between traffic links.
    """

    # Initialize lists to store data for creating sparse matrix
    rows = []
    cols = []
    data = []

    # Populate the rows, cols, and data lists based on legal turning movements
    for turning_movement in legal_turning_movements:
        incoming_id = turning_movement['incomingId']
        outgoing_ids = turning_movement['outgoingIds']
        if incoming_id in link_id_to_index:
            incoming_index = link_id_to_index[incoming_id]
            for outgoing_id in outgoing_ids:
                if outgoing_id in link_id_to_index:
                    outgoing_index = link_id_to_index[outgoing_id]
                    # Add links for both directions
                    rows.append(incoming_index)
                    cols.append(outgoing_index)
                    data.append(1)
                    # Add reverse link
                    rows.append(outgoing_index)
                    cols.append(incoming_index)
                    data.append(1)

    num_directed_traffic_links = len(directed_traffic_links_id)
    # Create sparse adjacency matrix (using CSR format)
    adjacency_matrix = sp.sparse.csr_matrix((data, (rows, cols)), shape=(num_directed_traffic_links, num_directed_traffic_links))

    return adjacency_matrix
