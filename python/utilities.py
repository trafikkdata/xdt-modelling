from __future__ import annotations

import json
import logging
from pathlib import Path

import pandas as pd
import networkx as nx


def open_file(file_path: Path) -> pd.DataFrame:
    """
    Open a file and load its content into a pandas DataFrame.

    Args:
        file_path (Path): Path to the input file

    Returns:
        pd.DataFrame: Data loaded into a pandas DataFrame

    Raises:
        DataPipelineError: If the file format is not supported or loading fails
    """
    try:
        if file_path.suffix == '.json':
            # Load the JSON file into a pandas DataFrame
            data = pd.read_json(file_path)
            logging.info(f'Successfully loaded data from {file_path}')
            return data

        if file_path.suffix == '.geojson':
            with open(file_path) as f:
                return json.load(f)
        elif file_path.suffix == '.csv':
            # Load CSV if needed (flexibility for future use)
            data = pd.read_csv(file_path)
            logging.info(f'Successfully loaded data from {file_path}')
            return data
        else:
            raise ImportError(f'Unsupported file format: {file_path.suffix}')
    except Exception as e:
        raise ImportError(f'Failed to load data from {file_path}: {e}') from e


def build_graph_from_geojson_as_dictionary(geojson_data: dict) -> nx.DiGraph:
    """
    Constructs a directed graph from GeoJSON data representing a road network.

    The function iterates through the GeoJSON features and extracts turning movements,
    creating directed edges between nodes based on the `legalTurningMovements` property.

    Args:
        geojson_data (dict): A dictionary containing GeoJSON data with features.

    Returns:
        nx.DiGraph: A directed graph where edges represent legal traffic movements.
    """
    G = nx.DiGraph()  # Directed graph

    for feature in geojson_data['features']:
        legal_turns = feature['properties'].get('legalTurningMovements', None)

        if legal_turns is not None:
            for turn in legal_turns:
                incoming_id = turn['incomingId']
                outgoing_ids = turn['outgoingIds']

                if outgoing_ids is None:
                    continue

                for outgoing_id in outgoing_ids:
                    G.add_edge(incoming_id, outgoing_id)
    return G
