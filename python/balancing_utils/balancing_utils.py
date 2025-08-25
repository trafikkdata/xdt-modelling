from __future__ import annotations

from typing import Any
from collections import defaultdict
from collections.abc import Iterable

import numpy as np
import pandas as pd


def filter_gdf(gdf: pd.DataFrame, link_ids: list[str]) -> pd.DataFrame:
    def check_legal_turning_movements(movements: Any, link_ids: Any) -> Any:
        if movements is not None:
            for movement in movements:
                incoming_id = movement.get('incomingId')
                outgoing_ids = movement.get('outgoingIds')

                # Check if incomingId is in link_ids
                if incoming_id not in link_ids:
                    return False

                # Check if all outgoingIds are in link_ids
                if isinstance(outgoing_ids, list) and not all(outgoing_id in link_ids for outgoing_id in outgoing_ids):
                    return False

            # If all conditions pass, return True
            return True

        return False

    # Filter the rows based on the condition
    filtered_gdf = gdf[gdf['legalTurningMovements'].apply(lambda x: check_legal_turning_movements(x, link_ids))]

    return filtered_gdf


def get_opposite_direction_link_id(link_id: str) -> str | None:
    """
    Helper function to get the link ID in the opposite direction.

    This function takes a link ID and returns its opposite direction equivalent by replacing
    the directional suffix. If the link ID ends with '-WITH', it is replaced with '-AGAINST',
    and vice versa. If the link ID does not contain either suffix, the function returns None.

    Parameters:
    -----------
    link_id : str
        A link ID with a directional suffix ('-WITH' or '-AGAINST').

    Returns:
        link_id : str
        A link ID with the opposite directional suffix ('-WITH' or '-AGAINST'), or None

    """

    if '-WITH' in link_id:
        return link_id.replace('-WITH', '-AGAINST')
    if '-AGAINST' in link_id:
        return link_id.replace('-AGAINST', '-WITH')
    return None


def invert_legal_turning_movements_df(gdf: pd.DataFrame) -> pd.DataFrame:
    """
    Invert the legal turning movements for the entire DataFrame.

    This function processes the 'legalTurningMovements' column for each row in the
    DataFrame and creates a new column 'invertedLegalTurningMovements' that stores
    the inverted turning movements for each entry. The inversion swaps the incoming
    and outgoing link IDs.

    Parameters:
    -----------
    gdf : pd.DataFrame
        The DataFrame containing traffic node network data with a column
        'legalTurningMovements' to be inverted.

    Returns:
    --------
    pd.DataFrame
        The DataFrame with an additional column 'invertedLegalTurningMovements'.
    """

    def invert_row_legal_turning_movements(legal_turning_movements: Iterable) -> list[dict]:
        """Invert legal turning movements for a single row."""
        inverted_movements = defaultdict(list)

        if legal_turning_movements:
            for movement in legal_turning_movements:
                incoming_id = movement.get('incomingId')
                outgoing_ids = movement.get('outgoingIds', [])

                for outgoing_id in outgoing_ids:
                    inverted_movements[outgoing_id].append(incoming_id)

        # Convert the defaultdict to the required format
        inverted_movements_list = [{'outgoingId': outgoing_id, 'incomingIds': incoming_ids} for outgoing_id, incoming_ids in inverted_movements.items()]
        return inverted_movements_list

    # Apply the inversion for the entire DataFrame
    gdf['invertedLegalTurningMovements'] = gdf['legalTurningMovements'].apply(invert_row_legal_turning_movements)

    return gdf


def extract_incoming_and_outgoing_traffic_link_ids_from_gdf(gdf: pd.DataFrame) -> tuple:  # noqa: C901 TODO
    """
    Extract incoming and outgoing traffic link IDs from a traffic node network within a DataFrame.

    This function iterates through the 'legalTurningMovements' column in the given GeoDataFrame (gdf).
    For each movement, it collects the 'incomingId' and unique 'outgoingIds' to form two separate lists
    of incoming and outgoing traffic link IDs. These lists are then returned as a tuple.

    Parameters:
    -----------
    gdf : GeoDataFrame
        A GeoDataFrame representing the traffic node network, containing 'legalTurningMovements'
        which includes movement data with 'incomingId' and 'outgoingIds'.

    Returns:
    --------
    tuple of list
        A tuple containing two lists: the first list contains unique incoming traffic link IDs,
        and the second list contains unique outgoing traffic link IDs.
    """
    incoming_traffic_link_ids = []
    outgoing_traffic_link_ids = []

    def process_movements(movements: Iterable | None) -> None:
        """Process the legalTurningMovements column to extract incoming and outgoing link IDs."""
        if not movements:
            return

        for movement in movements:
            incoming_id = movement.get('incomingId')
            if incoming_id and incoming_id not in incoming_traffic_link_ids:
                incoming_traffic_link_ids.append(incoming_id)

            outgoing_ids = movement.get('outgoingIds')
            if isinstance(outgoing_ids, list):
                for outgoing_id in outgoing_ids:
                    if outgoing_id and outgoing_id not in outgoing_traffic_link_ids:
                        outgoing_traffic_link_ids.append(outgoing_id)

    # Apply the function to each row in the GeoDataFrame
    gdf['legalTurningMovements'].apply(process_movements)

    return incoming_traffic_link_ids, outgoing_traffic_link_ids


def calculate_c_for_residuals(A_w: np.ndarray, A_tau: np.ndarray, predicted_aadt: np.ndarray) -> float:
    """
    Calculate the constant 'c' for residuals in turning movement traffic data.

    This function computes a constant 'c' that represents the root mean square of
    the residuals from predicted Annual Average Daily Traffic (AADT) values. It
    uses the weight matrices A_w and A_tau for turning movements to compute the
    residuals of incoming and outgoing traffic. The function then calculates the
    root mean square of these residuals, which is used as a measure of the
    deviation of predicted traffic volumes from observed volumes in a traffic network,
    for non-I-intersections.

    Parameters:
    -----------
    A_w : np.ndarray
        The weight matrix for incoming traffic distribution across different turning movements.
    A_tau : np.ndarray
        The weight matrix for outgoing traffic distribution across different turning movements.
    predicted_aadt : np.ndarray
        An array or matrix of predicted Annual Average Daily Traffic (AADT) values for each traffic link.

    Returns:
    --------
    float
        The constant 'c', representing the root mean square of residuals for turning movement traffic data.
    """

    incoming_residuals = A_w.dot(predicted_aadt).flatten()
    outgoing_residuals = A_tau.dot(predicted_aadt).flatten()
    # Ensure not empty slice
    if incoming_residuals.size == 0:
        incoming_residuals = np.zeros(1)
    if outgoing_residuals.size == 0:
        outgoing_residuals = np.zeros(1)

    c_residuals_epsilon = np.sqrt(np.mean(np.concatenate((incoming_residuals**2, outgoing_residuals**2))))
    return c_residuals_epsilon


def remove_I_intersection_nodes(traffic_node_network: pd.DataFrame) -> pd.DataFrame:
    """
    Remove nodes that are I-intersections from a traffic node network DataFrame and return the modified DataFrame
    along with a list of removed node IDs.

    Parameters:
    -----------
    traffic_node_network : DataFrame
        A DataFrame representing the traffic node network with each row as a node and its attributes.

    Returns:
    --------
    tuple
        The modified traffic node network DataFrame
    """
    # Identifying I-intersection nodes with mutual exclusivity check
    i_intersection_condition = ((traffic_node_network['numberOfIncomingLinks'] == 1) & (traffic_node_network['numberOfOutgoingLinks'] == 1)) | (
        (traffic_node_network['numberOfIncomingLinks'] == 2) & (traffic_node_network['numberOfOutgoingLinks'] == 2)
    )

    # Additional mutual exclusivity check for nodes with 2 incoming and 2
    # outgoing links
    additional_i_intersection_nodes = []
    for _, row in traffic_node_network[i_intersection_condition].iterrows():
        if row['numberOfIncomingLinks'] == 2 and row['numberOfOutgoingLinks'] == 2:
            outgoing_from_incoming = {}
            for movement in row['legalTurningMovements']:
                incoming_id = movement['incomingId']
                if incoming_id not in outgoing_from_incoming:
                    outgoing_from_incoming[incoming_id] = set(movement['outgoingIds'])
                else:
                    outgoing_from_incoming[incoming_id].update(movement['outgoingIds'])

            outgoing_lists = list(outgoing_from_incoming.values())

            # Ensure there are at least two sets to compare
            if len(outgoing_lists) >= 2 and len(outgoing_lists[0].intersection(outgoing_lists[1])) == 0:
                additional_i_intersection_nodes.append(row['id'])

    # Combine initial condition with additional check
    i_intersection_node_ids = set(traffic_node_network[i_intersection_condition]['id'].tolist())
    i_intersection_node_ids.update(additional_i_intersection_nodes)

    # Removing I-intersection nodes from the DataFrame
    modified_network = traffic_node_network[~traffic_node_network['id'].isin(i_intersection_node_ids)]

    return modified_network
