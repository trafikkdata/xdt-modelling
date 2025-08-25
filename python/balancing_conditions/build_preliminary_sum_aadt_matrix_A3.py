from __future__ import annotations

from typing import Any

import numpy as np


def build_preliminary_sum_aadt_matrix_A3(link_ids: list, ids_with_prelim_aadt: list) -> np.ndarray:
    """
    Build a sum matrix A3 based on preliminary Annual Average Daily Traffic (AADT).

    This matrix is a single-row matrix where each column corresponds to a traffic link in 'link_ids'.
    The matrix elements are set to 1 for each column where the column's link ID is present
    in 'ids_with_prelim_aadt', and 0 otherwise.

    Parameters:
    -----------
    link_ids : list
        A list of unique identifiers for all traffic links in the network.
    ids_with_prelim_aadt : list
        A list of unique identifiers for selected traffic links with preliminary AADT data.

    Returns:
    --------
    np.ndarray
        The sum matrix A3 with dimensions `1 x len(link_ids)`.
    """
    A3 = np.zeros((1, len(link_ids)))
    link_id_index = {link_id: index for index, link_id in enumerate(link_ids)}
    for link_id in ids_with_prelim_aadt:
        if link_id in link_id_index:
            A3[0, link_id_index[link_id]] = 1
    return A3


def observation_from_A3_for_b(prelimAadt: np.ndarray) -> np.ndarray:
    return np.array(np.sum(prelimAadt)).reshape(-1, 1)


def uncertainty_prelim_sum_aadt_matrix_A3(prelim_standard_error: Any) -> np.ndarray:
    return np.array(np.sum(prelim_standard_error * np.sqrt(365) ** 2)).reshape(-1, 1)
