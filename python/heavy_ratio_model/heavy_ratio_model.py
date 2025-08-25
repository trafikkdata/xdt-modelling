from __future__ import annotations

import logging

import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor


def predict_heavy_ratio(df: pd.DataFrame) -> pd.DataFrame:
    """
    Predicts missing heavy vehicle ratio values using an optimized Random Forest model.
    
    Uses pre-optimized parameters and features determined through cross-validation:
    - Selected 32 features including road characteristics, traffic volumes, and geographical indicators
    - Optimized hyperparameters: max_depth=10, min_samples_leaf=4, min_samples_split=10, n_estimators=200
    
    Parameters:
        df (pd.DataFrame): Input DataFrame containing road network features
            
    Returns:
        pd.DataFrame: Original DataFrame with predicted heavyRatio values filled in where
                     they were previously null.
    """
    logger = logging.getLogger(__name__)

    # Define categorical columns that need encoding
    factor_columns = [
        'roadCategory',
        'highestSpeedLimit',
        'functionalRoadClass',
        'isFerryTrafficLink',
        'hasTouristRoad',
        'maxLanes',
        'minLanes',
        'functionClass',
        'roadSystemReference',
    ]

    # Convert categorical columns
    for col in factor_columns:
        if col in df.columns:
            df[col] = df[col].astype('category')

    # Create log length feature
    if 'length' in df.columns:
        df['logLength'] = np.log(df['length'] + 1)
    else:
        df['logLength'] = np.nan

    # Split into train and predict sets
    df_train = df[~df['heavyRatio'].isnull()].copy()
    df_predict = df[df['heavyRatio'].isnull()].copy()

    if df_train.empty or df_predict.empty:
        logger.info('No data to train on or predict. Returning original DataFrame.')
        return df

    # Pre-selected features from optimization
    selected_features = [
        'municipalities', 'counties', 'length', 'lowestSpeedLimit', 'guesstimatedAadt',
        'prelimAadt', 'prelimAadtStandardError', 'index', 'link', 'logLength',
        'inlaPrediction', 'inlaStandardDeviation', 'functionalRoadClass_1',
        'functionalRoadClass_2', 'functionalRoadClass_3', 'functionalRoadClass_4',
        'functionalRoadClass_5', 'maxLanes_2', 'maxLanes_3', 'minLanes_2',
        'highestSpeedLimit_40', 'highestSpeedLimit_50', 'highestSpeedLimit_60',
        'highestSpeedLimit_80', 'highestSpeedLimit_110', 'roadCategory_FYLKESVEG',
        'roadCategory_RIKSVEG', 'functionClass_B', 'functionClass_C', 'functionClass_D',
        'functionClass_E', 'hasTouristRoad_TRUE'
    ]

    # Prepare data for modeling
    combined = pd.concat([df_train, df_predict])
    combined = pd.get_dummies(
        combined, 
        columns=[c for c in factor_columns if c in df.columns], 
        drop_first=True
    )

    df_train_encoded = combined.loc[df_train.index]
    df_predict_encoded = combined.loc[df_predict.index]

    # Create final model with optimized parameters
    final_model = RandomForestRegressor(
        n_estimators=200,
        max_depth=10,
        min_samples_split=10,
        min_samples_leaf=4,
        random_state=42,
        n_jobs=-1
    )

    # Filter features that exist in the current dataset
    available_features = [f for f in selected_features if f in df_train_encoded.columns]
    logger.info(f'Using {len(available_features)} features from the optimized feature set')

    # Train model and make predictions
    X_train = df_train_encoded[available_features]
    y_train = df_train_encoded['heavyRatio']
    X_predict = df_predict_encoded[available_features]

    final_model.fit(X_train, y_train)
    predictions = final_model.predict(X_predict)
    df.loc[df_predict.index, 'heavyRatio'] = predictions
    
    logger.info(f'Model trained on {len(df_train)} rows; predicted heavyRatio for {len(df_predict)} rows.')
    
    return df