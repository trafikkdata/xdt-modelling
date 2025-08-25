from __future__ import annotations

import numpy as np


def build_preliminary_aadt_matrix_A2(link_ids: list, ids_with_prelim_aadt: list) -> np.ndarray:
    """
    Build a preliminary matrix A2 based on Annual Average Daily Traffic (AADT).

    This matrix maps the given traffic link IDs to their corresponding preliminary AADT values.
    Each row in the matrix corresponds to an ID in 'ids_with_prelim_aadt', and each column
    corresponds to a traffic link in 'link_ids'. The matrix elements are set to 1 where the
    row's traffic link ID matches the column's link ID, and 0 otherwise.

    Parameters:
    -----------
    ids_with_prelim_aadt : list
        A list of unique identifiers for selected traffic links with preliminary AADT data.
    link_ids : list
        A list of unique identifiers for all traffic links in the network.

    Returns:
    --------
    np.ndarray
        The preliminary matrix A2 with dimensions `len(ids_with_prelim_aadt) x len(link_ids)`.
    """
    n_p = len(ids_with_prelim_aadt)
    n_e = len(link_ids)
    A2 = np.zeros((n_p, n_e))
    for i, link_id in enumerate(ids_with_prelim_aadt):
        if link_id in link_ids:
            A2[i, link_ids.index(link_id)] = 1
    return A2


def observation_from_A2_for_b(prelimAadt: np.ndarray) -> np.ndarray:
    return prelimAadt.reshape(-1, 1)


def preliminary_aadt_standard_error(prelimAadtStandardError: np.ndarray) -> np.ndarray:
    """
    Compute the standard deviation vector for preliminary AADT.

    This function computes the standard deviation vector for preliminary AADT values.
    The standard error is calculated as the square root of the squared standard error values.

    Parameters:
    -----------
    prelimAadtStandardError : np.ndarray
        An array of standard error values for preliminary AADT.

    Returns:
    --------
    np.ndarray
        The standard deviation vector for preliminary AADT.
    """
    return (prelimAadtStandardError**2).reshape(-1, 1)
