from __future__ import annotations

from typing import Any

import numpy as np
import pandas as pd

Z_SCORE_99 = 2.58


def estimate_standard_error(aadt: float) -> Any:
    thresholds = [500, 1000, 5000, 10000, 30000]
    errors = [40, 100, 200, 400, 1000, 2000]
    return next((err for thresh, err in zip(thresholds, errors, strict=False) if aadt < thresh), errors[-1])


def calculate_confidence_interval(mean: float, std_dev: float, z_score: float = Z_SCORE_99) -> tuple[float, float]:
    margin = std_dev * z_score
    return mean - margin, mean + margin


def eale_threshold(aadt: float) -> float:
    if aadt <= 1000:
        return 2 - (1.6 / (1 + np.exp(-0.01 * (aadt - 500))))
    if aadt <= 10000:
        sigmoid_1000 = 2 - (1.6 / (1 + np.exp(-0.01 * 500)))
        return sigmoid_1000 - 0.15 * ((aadt - 1000) / 9000) ** 0.5
    if aadt <= 50000:
        value_10000 = eale_threshold(10000)
        return value_10000 - 0.06 * ((aadt - 10000) / 40000) ** 0.5
    return 0.20


def calculate_eale(actual: float, predicted: float) -> float:
    return np.exp(np.abs(np.log(predicted) - np.log(actual))) - 1


def custom_round(value: float) -> float:
    thresholds = [500, 1000, 5000, 10000, 30000]
    roundings = [20, 50, 100, 200, 500, 1000]
    for thresh, rounding in zip(thresholds, roundings, strict=False):
        if value <= thresh:
            return max(round(value / rounding) * rounding, 1)
    return round(value / roundings[-1]) * roundings[-1]


def update_dataframe(df: pd.DataFrame) -> pd.DataFrame:
    df['balancedAadt'] = df['balancedAadt'].apply(custom_round)
    df['eALE'] = df.apply(lambda row: calculate_eale(row['balancedAadt'], row['guesstimatedAadt']), axis=1)

    for col in ['balancedAadt', 'guesstimatedAadt']:
        lower, upper = zip(
            *df.apply(lambda row: calculate_confidence_interval(row[col], row.get(f'{col}StandardDeviation', estimate_standard_error(row[col]))), axis=1),  # noqa: B023 TODO check thiS.
            strict=False,
        )
        df[f'lower_bound_{col}'] = lower
        df[f'upper_bound_{col}'] = upper

    df['acceptedAadt'] = df.apply(
        lambda row: pd.notna(row['prelimAadt'])
        or (
            row['lower_bound_balancedAadt'] <= row['upper_bound_guesstimatedAadt']
            and row['lower_bound_guesstimatedAadt'] <= row['upper_bound_balancedAadt']
            and row['eALE'] <= eale_threshold(row['balancedAadt'])
        ),
        axis=1,
    )

    return df
