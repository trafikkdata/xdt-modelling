from __future__ import annotations

import os
from typing import TYPE_CHECKING

import numpy as np
from result_processing.upload_to_github_release import upload_df_to_github_release

if TYPE_CHECKING:
    from pandas import DataFrame


def save_json_file_and_upload_to_github_release(df: DataFrame) -> DataFrame:
    df = df.copy()  # Ensure we modify a separate copy

    df.loc[:, 'balancedAadt'] = np.round(df['balancedAadt'])
    df.loc[:, 'balancedAadtStandardError'] = np.round(df['balancedAadtStandardError'])
    

    df['estimatedAdt'] = df['balancedAadt']
    
    # For aadt in county Oslo, set estimtatedAdt to inlaPrediction
    mask_oslo = df['county'] == 'Oslo'
    df.loc[mask_oslo, 'estimatedAdt'] = df.loc[mask_oslo, 'inlaPrediction']
    
    
    df['estimatedAdtStandardError'] = df['balancedAadtStandardError']

    df['heavyRatio'] = df['heavyRatio'].combine_first(df['heavyRatioPred'])

            
    df_json = df[['id', 'estimatedAdt', 'estimatedAdtStandardError', 'heavyRatio']]
    



    print("Uploading JSON to GitHub Release")
    upload_df_to_github_release(df_json)

    return df
