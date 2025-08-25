from __future__ import annotations

from typing import Any, TypedDict

import pandas as pd

from .column_operations import fill_mode_columns
from .utility_functions import extract_smallest_element, safely_extract_first_element


def extract_and_fill_values(df: pd.DataFrame) -> pd.DataFrame:
    """Extracts elements from lists and fills missing values for various columns."""
    list_columns = {
        'functionalRoadClass': extract_smallest_element,
        'functionClass': extract_smallest_element,
        'municipalities': safely_extract_first_element,
        'counties': safely_extract_first_element,
        'roadCategory': safely_extract_first_element,
    }

    # Apply transformations from list_columns to the relevant columns
    for col, func in list_columns.items():
        df[col] = df[col].apply(func)

    # First extract the first element from 'roadSystemReferences' then split
    df['roadSystemReference'] = df['roadSystemReferences'].apply(safely_extract_first_element)
    df['roadSystemReference'] = df['roadSystemReference'].apply(lambda x: x.split(' ')[0] if x else None)

    # Fill mode-based columns
    fill_mode_columns(df)

    # Extract traffic volume related data
    df['guesstimatedAadt'] = df['trafficVolumes'].apply(extract_guesstimated_aadt)
    df['prelimAadt'] = df['trafficVolumes'].apply(lambda v: extract_most_recent_measured_or_derived_value(v, 'trafficVolumeValue'))
    df['prelimAadtStandardError'] = df['trafficVolumes'].apply(lambda v: extract_most_recent_measured_or_derived_value(v, 'correctedStandardError'))
    df['heavyRatio'] = df['trafficVolumes'].apply(extract_heavy_ratio)
    # Extract most recent volume data and drop unnecessary columns
    df['result'] = df['trafficVolumes'].apply(extract_most_recent_volume_data)
    df['isDerived'] = df['result'].apply(lambda x: x['isDerived'])
    df["isContinuous"] = df["result"].apply(lambda x: x["isContinuous"])
    df['measuredVolumeYear'] = df['result'].apply(lambda x: x.get('volumeYear', None))
    df.drop(columns=['result', 'trafficVolumes'], inplace=True)

    return df


def add_guesstimated_aadt_as_prelim_for_ferry_links(df: pd.DataFrame) -> pd.DataFrame:
    """Adds guesstimated AADT as preliminary AADT for ferry traffic links."""
    ferry_links = df[df['isFerryTrafficLink']]['id']
    df.loc[df['id'].isin(ferry_links), 'prelimAadt'] = df['guesstimatedAadt'] + 1.0
    df.loc[df['id'].isin(ferry_links), 'prelimAadtStandardError'] = 0.0
    return df


def extract_guesstimated_aadt(volumes: Any) -> Any | None:
    """Extracts the 'GUESSTIMATED' traffic volume value from a list of traffic volumes."""
    if isinstance(volumes, list):
        for volume in volumes:
            if isinstance(volume, dict) and volume.get('trafficVolumeType') == 'GUESSTIMATED':
                return volume['trafficVolumeValue']
    return None


def extract_heavy_ratio(volumes: Any) -> Any | None:
    """
    Extracts the heavy ratio (heavyRatio) for a 'MEASURED' traffic volume from
    a list of traffic volumes. Returns None if not found.
    """
    if isinstance(volumes, list):
        for volume in volumes:
            if isinstance(volume, dict) and volume.get('trafficVolumeType') == 'MEASURED':
                return volume.get('heavyRatio')
    return None


def extract_most_recent_measured_or_derived_value(volumes: Any, attribute: Any) -> Any:
    """Extracts the newest traffic volume attribute from a list of measured or derived traffic volumes."""
    try:
        valid_volumes = [v for v in volumes if v.get('trafficVolumeType') in ['MEASURED', 'DERIVED']]
        newest_volume = max(valid_volumes, key=lambda x: x['year'], default=None)
        return newest_volume.get(attribute) if newest_volume else None
    except (TypeError, ValueError, AttributeError):
        return None


class RecentVolumeData(TypedDict):
    value: Any | None
    isDerived: bool
    isContinuous: bool
    volumeYear: Any | None


def extract_most_recent_volume_data(volumes: Any) -> RecentVolumeData:
    """Extracts the newest traffic volume data from a list of measured or derived traffic volumes."""
    try:
        valid_volumes = [v for v in volumes if v.get('trafficVolumeType') in ['MEASURED', 'DERIVED']]
        newest_volume = max(valid_volumes, key=lambda x: x['year'], default=None)
        if newest_volume:
            return {
                'value': newest_volume.get('trafficVolumeValue'),
                'isDerived': newest_volume.get('trafficVolumeType') == 'DERIVED',
                'isContinuous': newest_volume.get('registrationFrequency') == 'CONTINUOUS',
                'volumeYear': newest_volume.get('year'),
            }
        return {'value': None, 'isDerived': False, 'isContinuous': False, 'volumeYear': None}
    except (TypeError, ValueError, AttributeError):
        return {'value': None, 'isDerived': False, 'isContinuous':False, 'volumeYear': None}
