from __future__ import annotations

import logging

import pandas as pd


def remove_columns(df: pd.DataFrame) -> pd.DataFrame:
    """Removes specific columns from a DataFrame if they exist."""
    columns_to_remove = [
        'parentLinkId',
        'lastModified',
        'contractAreaNames',
        'hasOnlyPublicTransportLanes',
        'startTrafficNodeId',
        'endTrafficNodeId',
        'roadLinkIds',
        'roadNetNodeIds',
        'isBlocked',
        'associatedTrpIds',
        'primaryTrpId',
        'isInvalid',
        'isTrafficWithMetering',
        'associatedTollStationIds',
    ]

    existing_columns = [col for col in columns_to_remove if col in df.columns]
    return df.drop(columns=existing_columns)


def fill_mode_columns(df: pd.DataFrame) -> None:
    """Fills missing values in specified columns with their mode."""
    columns_to_fill = [
        'highestSpeedLimit',
        'lowestSpeedLimit',
        'roadCategory',
        'functionClass',
        'minLanes',
        'functionalRoadClass',
        'maxLanes',
    ]

    for column in columns_to_fill:
        mode_value = df[column].mode(dropna=True)
        if mode_value.empty:
            logging.warning(f"All values in '{column}' are NaN. Consider defining a default value or removing this column.")
            continue

        mode_value = mode_value[0]
        df[column] = df[column].astype(type(mode_value)).fillna(mode_value)
