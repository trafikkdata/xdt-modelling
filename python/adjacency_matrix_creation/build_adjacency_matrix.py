from __future__ import annotations

import pandas as pd

from .graph_utils import (
    create_adjacency_matrix,
    extract_legal_turning_movements,
    get_intersection_of_directed_traffic_link_ids,
    get_unique_directed_traffic_link_ids_from_traffic_node_network,
    get_unique_directed_traffic_link_ids_from_preprocessed_directed_links,
)
from .dataframe_utils import add_min_distance_to_prelimAadt


def create_adjacency_matrix_from_nodes_and_df(geojson_data: dict, df: pd.DataFrame) -> tuple[pd.DataFrame, list[str]]:
    """
    Create an adjacency matrix based on the geojson data and the DataFrame.

    Args:
    geojson_data (dict): The geojson data containing legal turning movements.
    df (pd.DataFrame): The input DataFrame.

    Returns:
    Tuple[pd.DataFrame, List[str]]: The adjacency matrix and the list of directed traffic link IDs.
    """

    legal_turning_movements = extract_legal_turning_movements(geojson_data)

    directed_traffic_links_id_from_network = get_unique_directed_traffic_link_ids_from_traffic_node_network(legal_turning_movements)

    directed_traffic_links_id_from_preprocessed_links = get_unique_directed_traffic_link_ids_from_preprocessed_directed_links(df)

    directed_traffic_link_ids = get_intersection_of_directed_traffic_link_ids(
        directed_traffic_links_id_from_network, directed_traffic_links_id_from_preprocessed_links
    )

    link_id_to_index = {link_id: index for index, link_id in enumerate(directed_traffic_link_ids)}

    adj_matrix = create_adjacency_matrix(legal_turning_movements, directed_traffic_link_ids, link_id_to_index)

    index_to_link_id = {index: link_id for link_id, index in link_id_to_index.items()}
    # Convert to COO format
    adj_coo_matrix = adj_matrix.tocoo()
    adj_coo_matrix_dataframe = pd.DataFrame(
        {
            'row': adj_coo_matrix.row,
            'col': adj_coo_matrix.col,
            'data': adj_coo_matrix.data,
            'row_id': [index_to_link_id.get(row, row) for row in adj_coo_matrix.row],
            'col_id': [index_to_link_id.get(col, col) for col in adj_coo_matrix.col],
        }
    )

    return adj_coo_matrix_dataframe, directed_traffic_link_ids


def update_dataframe(df: pd.DataFrame, geojson_data: dict, directed_traffic_link_ids: list[str]) -> pd.DataFrame:
    """
    Update the DataFrame based on the geojson data and directed traffic link IDs.

    Args:
    df (pd.DataFrame): The input DataFrame.
    geojson_data (dict): The geojson data containing network information.
    directed_traffic_link_ids (List[str]): The list of directed traffic link IDs.

    Returns:
    pd.DataFrame: The updated DataFrame.
    """
    df = add_min_distance_to_prelimAadt(geojson_data, df)

    updated_df = df[df['id'].isin(directed_traffic_link_ids)]

    return updated_df


def build_adjacency_matrix(df: pd.DataFrame, geojson_data: dict) -> tuple[pd.DataFrame, pd.DataFrame]:
    """
    Build the adjacency matrix and update the DataFrame.

    Args:
    df (pd.DataFrame): The input DataFrame.
    geojson_data (dict): The geojson data containing network information.

    Returns:
    Tuple[pd.DataFrame, pd.DataFrame]: The adjacency matrix and the updated DataFrame.
    """

    # df = filter_df_to_largest_n_components(df, geojson_data, n=1)

    adj_matrix, directed_traffic_link_ids = create_adjacency_matrix_from_nodes_and_df(geojson_data, df)
    updated_df = update_dataframe(df, geojson_data, directed_traffic_link_ids)

    return adj_matrix, updated_df
