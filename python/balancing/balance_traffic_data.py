from __future__ import annotations

import time
import logging

import numpy as np
import pandas as pd
import geopandas as gpd
from balancing_utils.balancing_utils import (
    filter_gdf,
    calculate_c_for_residuals,
    remove_I_intersection_nodes,
    invert_legal_turning_movements_df,
    extract_incoming_and_outgoing_traffic_link_ids_from_gdf,
)
from balancing.cluster_on_prelim_aadt import add_cluster_id_to_df
from balancing.update_gaussian_parameters import update_gaussian_parameters
from balancing_conditions.build_preliminary_aadt_matrix_A2 import (
    observation_from_A2_for_b,
    preliminary_aadt_standard_error,
    build_preliminary_aadt_matrix_A2,
)
from balancing_conditions.build_node_link_incidence_matrix_A1 import (
    observation_from_A1_for_b,
    build_node_link_incidence_matrix_A1,
    uncertainty_from_A1_for_Sigma_epsilon,
)
from balancing_conditions.build_preliminary_sum_aadt_matrix_A3 import (
    observation_from_A3_for_b,
    build_preliminary_sum_aadt_matrix_A3,
    uncertainty_prelim_sum_aadt_matrix_A3,
)
from balancing_conditions.build_incoming_turning_movement_matrix_A_w import (
    fill_A_omega,
    observation_from_A_omega_for_b,
    identify_incoming_I_intersection_links,
    create_Sigma_epsilon_incoming_flattened,
)
from balancing_conditions.build_outgoing_turning_movement_matrix_A_tau import (
    fill_A_tau,
    observation_from_A_tau_for_b,
    identify_outgoing_I_intersection_links,
    create_Sigma_epsilon_outgoing_flattened,
)

DAYS_IN_YEAR = 365.25


def balance_traffic_data(df: pd.DataFrame, traffic_nodes: dict, num_iterations: int) -> pd.DataFrame:
    # group_name = "county"
    group_name = 'cluster_id'
    if group_name == 'cluster_id':
        df = add_cluster_id_to_df(df, traffic_nodes)

    balanced_df = pd.DataFrame()

    # Get the total number of groups
    total_groups = len(df[group_name].unique())
    progress_threshold = total_groups // 10  # Calculate the 10% threshold

    traffic_node_network = gpd.GeoDataFrame.from_features(traffic_nodes['features'])
    traffic_node_network = invert_legal_turning_movements_df(traffic_node_network)

    # Iterate through each group in the DataFrame
    start_time = time.time()
    for i, (_, group) in enumerate(df.groupby(group_name)):
        traffic_nodes_for_group = filter_gdf(traffic_node_network, group['id'].tolist())
        balanced_group = balance_group(group, traffic_nodes_for_group, num_iterations)

        # Remove duplicates based on id
        balanced_df = pd.concat([balanced_df, balanced_group], ignore_index=True)
        balanced_df = balanced_df.drop_duplicates(subset='id', keep='first')

        # Log progress every 10% of total groups
        if (i + 1) % progress_threshold == 0:
            elapsed_time = round(time.time() - start_time, 2)
            logging.info(f'Processed {i + 1} out of {total_groups} groups in {elapsed_time} secondes ({(i + 1) / total_groups * 100:.1f}%)')
            start_time = time.time()

    logging.info(f'Finished balancing traffic data with {balanced_df.shape} rows')

    return balanced_df


def balance_group(group: pd.DataFrame, traffic_nodes: pd.DataFrame, num_iterations: int) -> pd.DataFrame:
    """
    Balance the traffic data for a group of traffic links.

    Args:
    group (pd.DataFrame): The input DataFrame group.
    traffic_nodes (pd.DataFrame): The traffic nodes data.
    num_iterations (int): The number of iterations to balance the data.

    Returns:
    pd.DataFrame: The balanced DataFrame group.
    """
    # Initialize variables
    inla_predictions = np.asarray(group['inlaPrediction'].values)
    inla_std_dev = np.asarray(group['inlaStandardDeviation'].values)
    inla_variance_matrix = np.diag(inla_std_dev**2)
    traffic_link_ids = group['id'].tolist()
    traffic_link_ids_with_prelim_aadt = group['id'][group['prelimAadt'].notna()].tolist()

    # Extract traffic link IDs and filter I intersection nodes
    incoming_link_ids, outgoing_link_ids = extract_incoming_and_outgoing_traffic_link_ids_from_gdf(traffic_nodes)
    incoming_links_I, outgoing_links_I = identify_incoming_I_intersection_links(traffic_nodes), identify_outgoing_I_intersection_links(traffic_nodes)
    
    # Uncomment line below to balance only non-I intersection nodes
    traffic_nodes = remove_I_intersection_nodes(traffic_nodes)

    # Build matrices
    A1 = build_node_link_incidence_matrix_A1(traffic_nodes, traffic_link_ids)
    A2 = build_preliminary_aadt_matrix_A2(traffic_link_ids, traffic_link_ids_with_prelim_aadt)
    A3 = build_preliminary_sum_aadt_matrix_A3(traffic_link_ids, traffic_link_ids_with_prelim_aadt)
    A_original = np.concatenate((A1, A2, A3))

    # Prepare observations and uncertainties
    prelimAadt = group['prelimAadt'].dropna().values
    prelimAadtStandardError = group['prelimAadtStandardError'].dropna().values
    b = np.concatenate(
        (
            observation_from_A1_for_b(A1),
            observation_from_A2_for_b(prelimAadt),
            observation_from_A3_for_b(prelimAadt),
            observation_from_A_omega_for_b(len(incoming_link_ids)),
            observation_from_A_tau_for_b(len(outgoing_link_ids)),
        )
    )
    Sigma_epsilon_original = np.concatenate(
        (uncertainty_from_A1_for_Sigma_epsilon(A1), preliminary_aadt_standard_error(prelimAadt), uncertainty_prelim_sum_aadt_matrix_A3(prelimAadtStandardError))
    )

    # Iterate to balance the data
    for _ in range(num_iterations):
        A_w = fill_A_omega(inla_predictions, traffic_nodes, traffic_link_ids, incoming_link_ids)
        A_tau = fill_A_tau(inla_predictions, traffic_nodes, traffic_link_ids, outgoing_link_ids)
        A = np.concatenate((A_original, A_w, A_tau))

        c = calculate_c_for_residuals(A_w, A_tau, inla_predictions)
        Sigma_epsilon = np.concatenate(
            (
                Sigma_epsilon_original,
                create_Sigma_epsilon_incoming_flattened(incoming_link_ids, incoming_links_I, c),
                create_Sigma_epsilon_outgoing_flattened(outgoing_link_ids, outgoing_links_I, c),
            )
        )

        inla_predictions, inla_variance_matrix = update_gaussian_parameters(
            Sigma_v=inla_variance_matrix, A=A, b=b, Sigma_epsilon=Sigma_epsilon, mu_v=inla_predictions
        )

    # Update the group with balanced results
    group['balancedAadt'] = inla_predictions.flatten()
    group['balancedAadtStandardDeviation'] = np.sqrt(np.diag(inla_variance_matrix)).flatten()
    group['balancedAadtStandardError'] = group['balancedAadtStandardDeviation'] / np.sqrt(DAYS_IN_YEAR)
    return group
