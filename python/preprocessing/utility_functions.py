from __future__ import annotations

import logging
from typing import Any


def safely_extract_first_element(value: Any) -> Any:
    """Safely extracts the first element from a list or returns the value directly."""
    if isinstance(value, list) and value:
        return value[0]
    if isinstance(value, str | int | float | type(None)):
        return value
    logging.warning(f'Skipping field: invalid type {type(value).__name__}')
    return None


def extract_smallest_element(x: Any) -> Any:
    """Extracts the smallest element from a list or returns the input value."""
    return min(x) if isinstance(x, list) else x
