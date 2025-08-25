from __future__ import annotations

import logging

import pandas as pd
from preprocessing.data_cleaning import add_county, round_and_clean_aadt_values
from preprocessing.data_extraction import extract_and_fill_values, add_guesstimated_aadt_as_prelim_for_ferry_links
from preprocessing.column_operations import remove_columns


def preprocess_dataframe(df: pd.DataFrame) -> pd.DataFrame:
    """Main function to preprocess the DataFrame."""
    logging.info('Preprocessing dataframe...')
    df = remove_columns(df)
    df = extract_and_fill_values(df)
    df = df.where(pd.notna(df), None)

    df = round_and_clean_aadt_values(df)

    df = df.rename(columns={'functionalRoadClasses': 'functionalRoadClass', 'functionClasses': 'functionClass'})

    df = add_county(df)

    return df
