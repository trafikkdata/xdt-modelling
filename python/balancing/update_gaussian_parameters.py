from __future__ import annotations

import numpy as np


def update_gaussian_parameters(
    Sigma_v: np.ndarray,
    A: np.ndarray,
    Sigma_epsilon: np.ndarray,
    b: np.ndarray,
    mu_v: np.ndarray,
) -> tuple[np.ndarray, np.ndarray]:
    """
    Update parameters for Bayesian inference in linear models.

    This function updates the parameters mu_v and Sigma_v in the context of Bayesian inference for linear models.
    It performs calculations using the provided covariance matrix (Sigma_v), transformation matrix (A),
    noise covariance (Sigma_epsilon), observation vector (b), and the mean vector (mu_v).
    The update is based on the Bayesian formulae for posterior mean and covariance in a linear Gaussian context.

    Parameters:
    -----------
    Sigma_v : np.ndarray
        Covariance matrix of the prior distribution.

    A : np.ndarray
        Transformation matrix relating the latent variables to the observations.

    Sigma_epsilon : np.ndarray
        Covariance matrix of the observational noise.

    b : np.ndarray
        Observation vector.

    mu_v : np.ndarray
        Mean vector of the prior distribution.

    Returns:
    --------
    mu_v_new : np.ndarray
        Updated mean vector of the posterior distribution.

    Sigma_v_new : np.ndarray
        Updated covariance matrix of the posterior distribution.
    """

    mu_v = mu_v.reshape(-1, 1)
    # Calculating Sigma_vb
    Sigma_vb = Sigma_v @ A.T

    # Calculating Sigma_b
    Sigma_b = A @ Sigma_vb + Sigma_epsilon

    Sigma_b = (Sigma_b + Sigma_b.T) / 2  # Ensure symmetric
    Sigma_b += np.eye(Sigma_b.shape[0]) * 1e-3  # Ensure no singular matrix

    # Calculating discrepancy
    assert not np.isnan(Sigma_b).any(), 'NaN detected in Sigma_b'
    assert not np.isinf(Sigma_b).any(), 'Infinity detected in Sigma_b'

    # Ensures that A @ mu_v is of shape (89, 1)
    discrepancy = b - A @ mu_v.reshape(-1, 1)

    # Updating Sigma_v
    Sigma_v_new = Sigma_v - Sigma_vb @ np.linalg.solve(Sigma_b, Sigma_vb.T)

    Sigma_v_new = np.maximum(Sigma_v_new, 0.01 * Sigma_v)

    mu_v_new = mu_v + Sigma_vb @ np.linalg.solve(Sigma_b, discrepancy)
    mu_v_new = np.maximum(mu_v_new, 0)

    return mu_v_new, Sigma_v
