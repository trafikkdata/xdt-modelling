from __future__ import annotations

from typing import Any
from functools import cache

import numpy as np
import pandas as pd
from balancing_utils.balancing_utils import get_opposite_direction_link_id


def fill_A_omega(  # noqa: C901 TODO
    v: np.ndarray,
    gdf: pd.DataFrame,
    link_ids: list,
    incoming_link_ids: list,
) -> np.ndarray:
    """
    Build the matrix A_w for traffic distribution in a network using a DataFrame.

    Optimizations:
    - Replaced list.index() with dictionary lookups for O(1) access.
    - Cached the get_opposite_direction_link_id function using lru_cache for repeated calls.
    """
    v = v.flatten()  # V has shape n_e x 1
    n_i = len(incoming_link_ids)
    n_e = len(link_ids)
    A_w = np.zeros((n_i, n_e))

    # Build mappings for O(1) lookups
    link_id_to_index = {link_id: idx for idx, link_id in enumerate(link_ids)}
    incoming_link_id_to_index = {link_id: idx for idx, link_id in enumerate(incoming_link_ids)}

    # Cache the get_opposite_direction_link_id function
    @cache
    def get_opposite_direction_link_id_cached(link_id: int) -> None:
        return get_opposite_direction_link_id(link_id)

    def process_node_movements(node: Any) -> Any:
        total_incoming_traffic = sum(v[link_id_to_index[movement['incomingId']]] for movement in node)

        for movement in node:
            jl_index = link_id_to_index[movement['incomingId']]
            incoming_traffic = v[jl_index]
            row_index = incoming_link_id_to_index[movement['incomingId']]

            if total_incoming_traffic != 0:
                A_w[row_index, jl_index] = -1 / total_incoming_traffic
            else:
                A_w[row_index, jl_index] = 0

            for outgoing_id in movement['outgoingIds']:
                if outgoing_id not in link_id_to_index:
                    continue  # Skip if outgoing_id not in link_ids
                li_index = link_id_to_index[outgoing_id]
                incoming_id = get_opposite_direction_link_id_cached(outgoing_id)

                if incoming_id in link_id_to_index:
                    il_index = link_id_to_index[incoming_id]
                    traffic_that_cannot_contribute = v[il_index]
                else:
                    traffic_that_cannot_contribute = 0

                # Check if the only incoming link is the twin of the outgoing
                # link
                if traffic_that_cannot_contribute == total_incoming_traffic:
                    traffic_that_cannot_contribute = 0

                sum_v_kl_except_i = total_incoming_traffic - traffic_that_cannot_contribute
                denominator = sum_v_kl_except_i * total_incoming_traffic
                omega_li_jl = incoming_traffic / denominator if denominator != 0 else 0

                A_w[row_index, li_index] = omega_li_jl

    # Apply the function to each node in the DataFrame
    gdf['legalTurningMovements'].apply(process_node_movements)

    return A_w


def observation_from_A_omega_for_b(num_incoming_links: int) -> np.ndarray:
    return np.zeros(num_incoming_links).reshape(-1, 1)


def create_Sigma_epsilon_incoming_flattened(incoming_link_ids: list, incoming_links_I_intersections: list, c: float, c_I: float = 0.01) -> np.ndarray:
    """
    Create a diagonal matrix representing the variances of incoming traffic links.

    This function generates a diagonal matrix where each diagonal element corresponds to
    the variance of an incoming traffic link. The variance for each link is determined based
    on its presence in specific intersections. For links part of 'incoming_links_I_intersections',
    a fixed, smaller variance (c_not_I) is assigned. For other links, a user-defined variance (c) is used.

    Parameters:
    -----------
    incoming_link_ids : list
        A list of IDs representing the incoming traffic links.

    incoming_links_I_intersections : list
        A list of link IDs that are part of specific intersections, which are assigned a lower variance.

    c : float
        The variance to be applied to incoming links not part of 'incoming_links_I_intersections'.

    c_not_I : float, optional (default 0.01)
        The smaller variance to be applied to links that are part of 'incoming_links_I_intersections'.

    Returns:
    --------
    np.ndarray
        A np array with variances,, representing the uncertainty in incoming traffic links.
    """
    Sigma_e_I_vector = np.zeros(len(incoming_link_ids))
    for index, link_id in enumerate(incoming_link_ids):
        if link_id in incoming_links_I_intersections:
            Sigma_e_I_vector[index] = c_I**2
        else:
            Sigma_e_I_vector[index] = c
    return Sigma_e_I_vector.reshape(-1, 1)


def identify_incoming_I_intersection_links(traffic_node_network: pd.DataFrame) -> list:
    """
    Identify incoming links at I-intersections within a traffic node network.

    Parameters:
    -----------
    traffic_node_network : dict
        A dictionary representing the traffic node network with legal turning movements.

    Returns:
    --------
    list
        A list of unique incoming link IDs at I-intersections.
    """
    I_intersection_links: list[Any] = []
    for movements in traffic_node_network['legalTurningMovements']:
        incoming = {movement['incomingId'] for movement in movements}
        outgoing = {oid for movement in movements for oid in movement['outgoingIds']}

        if len(incoming) == 1 and len(outgoing) == 1:
            I_intersection_links.extend(incoming)
        elif len(incoming) == 2 and len(outgoing) == 2:
            outgoing_from_incoming = {}
            for movement in movements:
                incoming_id = movement['incomingId']
                if incoming_id not in outgoing_from_incoming:
                    outgoing_from_incoming[incoming_id] = set(movement['outgoingIds'])
                else:
                    outgoing_from_incoming[incoming_id].update(movement['outgoingIds'])

            outgoing_lists = list(outgoing_from_incoming.values())
            if len(outgoing_lists[0].intersection(outgoing_lists[1])) == 0:
                I_intersection_links.extend(incoming)

    return list(set(I_intersection_links))
