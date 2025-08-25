from __future__ import annotations

import os
import time
import logging
import argparse
from typing import Any
from pathlib import Path
from functools import wraps
from collections.abc import Callable

import numpy as np
import pandas as pd
from rpy2 import robjects
from utils import open_file
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr
from rpy2.robjects.conversion import localconverter
from preprocessing.preprocessing import preprocess_dataframe
from rpy2.rinterface_lib.embedded import RRuntimeError, endr
from balancing.balance_traffic_data import balance_traffic_data
from heavy_ratio_model.heavy_ratio_model import predict_heavy_ratio
from result_processing.process_and_upload_result import save_json_file_and_upload_to_github_release
from adjacency_matrix_creation.build_adjacency_matrix import build_adjacency_matrix

# Suppress INFO level logs from rpy2
logging.getLogger('rpy2.rinterface_lib').setLevel(logging.ERROR)

PROJECT_ROOT = Path(os.path.dirname(os.path.abspath(__file__))).parent.parent
DATA_PATH = PROJECT_ROOT / 'data'
SRC_PATH = PROJECT_ROOT / 'src'
R_PATH = SRC_PATH / 'R'

SKIP_R = bool(os.environ.get('SKIP_R'))


class DataPipelineError(Exception):
    """Custom exception for errors in the data pipeline."""


def setup_logging(log_level: str = 'INFO') -> None:
    """
    Set up logging configuration.

    Args:
        log_level (str): Logging level (default: "INFO")
    """
    logging.basicConfig(level=getattr(logging, log_level), format='%(asctime)s - %(name)s - %(levelname)s - %(message)s', datefmt='%Y-%m-%d %H:%M:%S')


def log_function_call(func: Callable) -> Callable:
    @wraps(func)
    def wrapper(*args: Any, **kwargs: Any) -> None:
        logger = logging.getLogger(func.__module__)
        logger.info(f'Entering function: {func.__name__}')
        start_time = time.time()
        try:
            result = func(*args, **kwargs)
            logger.info(f'Exiting function: {func.__name__}')
            return result
        except Exception as e:
            logger.exception(f'Exception in {func.__name__}: {str(e)}')
            raise
        finally:
            end_time = time.time()
            logger.info(f'Function {func.__name__} execution time: {end_time - start_time:.2f} seconds')

    return wrapper


def parse_arguments() -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(description='Data Pipeline Script')
    parser.add_argument('--year', type=str, required=True, help='Year in YYYY format')
    parser.add_argument(
        '--num_iterations',
        type=int,
        choices=range(1, 16),
        required=True,
        help='Number of balancing iterations (1-15)',
    )

    parser.add_argument(
        '--log-level',
        type=str,
        choices=['DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'],
        default='INFO',
        help='Set the logging level',
    )
    return parser.parse_args()


def validate_inputs(args: argparse.Namespace) -> None:
    """
    Validate input arguments.

    Args:
        args (argparse.Namespace): Parsed command-line arguments

    Raises:
        ValueError: If the year format is invalid
        FileNotFoundError: If required data files are missing
    """
    # Check if the year is valid
    if not args.year.isdigit() or len(args.year) != 4:
        raise ValueError('Year should be in YYYY format')

    # Define the correct file paths relative to the project root
    data_files = [
        DATA_PATH / f'directed-traffic-links-{args.year}.json',
        DATA_PATH / f'traffic-nodes-{args.year}.geojson',
    ]

    # Check if the required data files exist
    missing_files = [file for file in data_files if not file.exists()]
    if missing_files:
        raise FileNotFoundError(f'Required file(s) not found: {", ".join(str(f) for f in missing_files)}')


@log_function_call
def load_and_preprocess_data(year: str) -> pd.DataFrame:
    """
    Load and preprocess data for the given year.

    Args:
        year (str): Year in YYYY format

    Returns:
        pd.DataFrame: Preprocessed data
    """

    input_file_name = DATA_PATH / f'directed-traffic-links-{year}.json'

    input_file_path = Path(input_file_name)

    # Load the data from JSON file
    df = open_file(input_file_path)

    if df is not None:
        # Preprocess the data
        df = preprocess_dataframe(df)
        logging.info(f'Data preprocessed successfully for year {year} with {len(df)} records.')
        return df
    raise DataPipelineError(f'Failed to load data for year {year}.')


@log_function_call
def create_adjacency_matrix(df: pd.DataFrame, year: int) -> tuple[np.ndarray, pd.DataFrame]:
    """
    Build adjacency matrix from the dataframe.

    Args:
        df (pd.DataFrame): Input dataframe

    Returns:
        tuple[np.ndarray, pd.DataFrame]: Adjacency matrix and updated dataframe

    Raises:
        DataPipelineError: If there's an error during adjacency matrix construction
    """
    try:
        input_file_path = DATA_PATH / f'./traffic-nodes-{year}.geojson'

        # Load the data from JSON file
        traffic_nodes = open_file(input_file_path)

        adjacency_matrix, updated_df = build_adjacency_matrix(df, traffic_nodes)

    except Exception as e:
        raise DataPipelineError(f'Error in building adjacency matrix: {e}') from e

    return adjacency_matrix, updated_df


@log_function_call
def run_inla_prediction(df: pd.DataFrame, adj_matrix_df: pd.DataFrame) -> pd.DataFrame:
    """Run INLA prediction by calling the R script using rpy2."""
    try:
        # Import required R packages
        base = importr('base')  # noqa: F841
        logging.info('R session has been successfully started.')

        # Handle list columns
        list_columns = ['roadSystemReferences', 'roadNodeIds']
        for col in list_columns:
            if col in df.columns:
                df[col] = df[col].apply(lambda x: ','.join(map(str, x)) if isinstance(x, list) else str(x))

        if SKIP_R:
            return df

        # Convert Python objects to R objects
        with localconverter(robjects.default_converter + pandas2ri.converter):
            r_df = robjects.conversion.py2rpy(df)
            r_adj_matrix_df = robjects.conversion.py2rpy(adj_matrix_df)

        # Construct the path to the R script
        r_script_path = R_PATH / 'predict_inla.R'

        # Load the R script
        robjects.r['source'](str(r_script_path))

        # Get the prediction function from the R script
        predict_inla = robjects.r['predict_inla']

        # Call the R function
        result = predict_inla(r_df, r_adj_matrix_df)

        # Convert R dataframe back to Python
        with localconverter(robjects.default_converter + pandas2ri.converter):
            predicted_df = robjects.conversion.rpy2py(result)

        return predicted_df

    except Exception as e:
        if hasattr(e, '__traceback__'):
            import traceback

            traceback.print_tb(e.__traceback__)
        raise Exception(f'Error in INLA prediction: {e}') from e

    finally:
        # Explicitly end the R session after the prediction is done
        try:
            # if SKIP_R:
            endr(fatal=False)  # Shut down the R session explicitly
            logging.info('R session has been successfully ended.')
        except RRuntimeError as e:
            print(f'Error ending R session: {e}')


@log_function_call
def balance_results(df: pd.DataFrame, num_iterations: int, year: int) -> pd.DataFrame:
    """
    Balance results for the given number of iterations.

    Args:
        df (pd.DataFrame): Input dataframe
        num_iterations (int): Number of balancing iterations

    Returns:
        pd.DataFrame: Balanced dataframe

    Raises:
        DataPipelineError: If there's an error during result balancing
    """
    try:
        if SKIP_R:
            return df

        input_file_path = DATA_PATH / f'./traffic-nodes-{year}.geojson'

        # Load the data from JSON file
        traffic_nodes = open_file(input_file_path)

        balanced_df = balance_traffic_data(df, traffic_nodes, num_iterations)

        return balanced_df
    except Exception as e:
        raise DataPipelineError(f'Error in balancing results: {e}') from e


def main() -> None:
    """Main function to run the data pipeline."""

    start_time = time.time()
    args = parse_arguments()
    setup_logging(args.log_level)
    logger = logging.getLogger(__name__)

    logger.debug(f'{SKIP_R=}')

    logger.info('Starting data pipeline')
    validate_inputs(args)

    df = load_and_preprocess_data(args.year)
    logger.info('Data loaded and preprocessed')

    adj_matrix, df = create_adjacency_matrix(df, args.year)
    logger.info('Adjacency matrix built')
    df = run_inla_prediction(df, adj_matrix)

    logger.info('INLA prediction completed')
    df = balance_results(df, args.num_iterations, args.year)
    logger.info(f'Results balanced with {args.num_iterations} iterations')

    df = save_json_file_and_upload_to_github_release(df)
    logging.info('Balanced results uploaded to GitHub release')
    logger.info(f'Data pipeline completed successfully in {time.time() - start_time:.2f} seconds')

    # Save balanced df as a CSV file in model/data
    output_file = DATA_PATH / f'adt_model_results_data_{args.year}.csv'
    df.to_csv(output_file, index=False)


if __name__ == '__main__':
    main()
