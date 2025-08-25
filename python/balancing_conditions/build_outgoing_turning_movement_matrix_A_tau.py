from __future__ import annotations

from typing import Any
from functools import cache

import numpy as np
import pandas as pd
from balancing_utils.balancing_utils import get_opposite_direction_link_id


def fill_A_tau(  # noqa: C901 TODO
    v: np.ndarray,
    gdf: pd.DataFrame,
    link_ids: list,
    outgoing_link_ids: list,
) -> np.ndarray:
    """
    Build the matrix A_tau for traffic distribution in a network based on outgoing traffic using a DataFrame.

    Optimizations:
    - Replaced list.index() with dictionary lookups for O(1) access.
    - Cached the get_opposite_direction_link_id function using lru_cache for repeated calls.
    """
    v = v.flatten()  # V has shape n_e x 1
    n_u = len(outgoing_link_ids)
    n_e = len(link_ids)
    A_tau = np.zeros((n_u, n_e))

    # Build mappings for O(1) lookups
    link_id_to_index = {link_id: idx for idx, link_id in enumerate(link_ids)}
    outgoing_link_id_to_index = {link_id: idx for idx, link_id in enumerate(outgoing_link_ids)}

    # Cache the get_opposite_direction_link_id function
    @cache
    def get_opposite_direction_link_id_cached(link_id: Any) -> Any:
        return get_opposite_direction_link_id(link_id)

    def process_node_movements(node: Any) -> None:
        """Process the invertedLegalTurningMovements to compute matrix A_tau."""
        total_outgoing_traffic = sum(v[link_id_to_index[movement['outgoingId']]] for movement in node)

        for movement in node:
            lj_index = link_id_to_index[movement['outgoingId']]
            outgoing_traffic = v[lj_index]
            row_index = outgoing_link_id_to_index[movement['outgoingId']]

            if total_outgoing_traffic != 0:
                A_tau[row_index, lj_index] = -1 / total_outgoing_traffic
            else:
                A_tau[row_index, lj_index] = 0
            for incoming_id in movement['incomingIds']:
                if incoming_id in link_id_to_index:
                    il_index = link_id_to_index[incoming_id]
                    outgoing_id = get_opposite_direction_link_id_cached(incoming_id)

                    if outgoing_id in link_id_to_index:  # The incoming traffic link has an existing outgoing twin link
                        li_index = link_id_to_index[outgoing_id]
                        traffic_that_cannot_contribute = v[li_index]
                    else:
                        traffic_that_cannot_contribute = 0

                    # Check if the only outgoing link is the twin of the
                    # incoming link
                    if traffic_that_cannot_contribute == total_outgoing_traffic:
                        traffic_that_cannot_contribute = 0

                    sum_v_lk_except_i = total_outgoing_traffic - traffic_that_cannot_contribute
                    denominator = sum_v_lk_except_i * total_outgoing_traffic
                    tau_il_lj = outgoing_traffic / denominator if denominator != 0 else 0

                    A_tau[row_index, il_index] = tau_il_lj

    # Apply the function to each node in the DataFrame
    gdf['invertedLegalTurningMovements'].apply(process_node_movements)

    return A_tau


def observation_from_A_tau_for_b(num_outgoing_links: int) -> np.ndarray:
    return np.zeros(num_outgoing_links).reshape(-1, 1)


def create_Sigma_epsilon_outgoing_flattened(outgoing_link_ids: list, outgoing_links_I_intersections: list, c: float, c_I: float = 0.01) -> np.ndarray:
    """
    Create a diagonal matrix representing the variances of outgoing traffic links.

    This function generates a diagonal matrix where each diagonal element corresponds to
    the variance of an outgoing traffic link. The variance for each link is determined based
    on its presence in specific intersections. For links part of 'outgoing_links_I_intersections',
    a fixed, smaller variance (c_not_I) is assigned. For other links, a user-defined variance (c) is used.

    Parameters:
    -----------
    outgoing_link_ids : list
        A list of IDs representing the outgoing traffic links.

    outgoing_links_I_intersections : list
        A list of link IDs that are part of specific intersections, which are assigned a lower variance.

    c : float
        The variance to be applied to outgoing links not part of 'outgoing_links_I_intersections'.

    c_not_I : float, optional (default 0.01)
        The smaller variance to be applied to links that are part of 'outgoing_links_I_intersections'.

    Returns:
    --------
    np.ndarray
        A np array with variances, representing the uncertainty in outgoing traffic links.
    """
    Sigma_e_O_vector = np.zeros(len(outgoing_link_ids))
    for index, link_id in enumerate(outgoing_link_ids):
        index = outgoing_link_ids.index(link_id)
        if link_id in outgoing_links_I_intersections:
            Sigma_e_O_vector[index] = c_I**2
        else:
            Sigma_e_O_vector[index] = c
    return Sigma_e_O_vector.reshape(-1, 1)


def identify_outgoing_I_intersection_links(traffic_node_network: pd.DataFrame) -> list:
    """
    Identify outgoing links at I-intersections within a traffic node network.

    Parameters:
    -----------
    traffic_node_network : dict
        A dictionary representing the traffic node network with legal turning movements.

    Returns:
    --------
    list
        A list of unique outgoing link IDs at I-intersections.
    """
    I_intersection_links: list[Any] = []
    for movements in traffic_node_network['legalTurningMovements']:
        incoming = {movement['incomingId'] for movement in movements}
        outgoing = {oid for movement in movements for oid in movement['outgoingIds']}

        if len(incoming) == 1 and len(outgoing) == 1:
            I_intersection_links.extend(outgoing)
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
                I_intersection_links.extend(outgoing)

    return list(set(I_intersection_links))
