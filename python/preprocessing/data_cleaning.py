from __future__ import annotations

import pandas as pd


def round_and_clean_aadt_values(df: pd.DataFrame) -> pd.DataFrame:
    """Rounds AADT values and handles negative values."""

    for col in ['prelimAadt', 'prelimAadtStandardError']:
        df[col] = df[col].apply(lambda x: round(x) if pd.notna(x) else None)
        df.loc[df[col] < 0, [col]] = None
    return df


def add_guesstimated_aadt_as_prelim_for_ferry_links(df: pd.DataFrame) -> pd.DataFrame:
    """Adds guesstimated AADT as preliminary AADT for ferry traffic links."""

    ferry_links = df[df['isFerryTrafficLink']]['id']
    df.loc[df['id'].isin(ferry_links), 'prelimAadt'] = df['guesstimatedAadt'] + 1.0
    df.loc[df['id'].isin(ferry_links), 'prelimAadtStandardError'] = 0.0
    return df


def add_county(df: pd.DataFrame) -> pd.DataFrame:
    """Add column county with name of county based on county number."""

    county_mapping = {
        3: 'Oslo',
        11: 'Rogaland',
        15: 'Møre og Romsdal',
        18: 'Nordland',
        31: 'Østfold',
        32: 'Akershus',
        33: 'Buskerud',
        34: 'Innlandet',
        39: 'Vestfold',
        40: 'Telemark',
        42: 'Agder',
        46: 'Vestland',
        50: 'Trøndelag',
        55: 'Troms',
        56: 'Finnmark',
    }

    df['county'] = df['counties'].map(county_mapping)

    return df
