from __future__ import annotations

import os
import json
import logging

import requests
from pandas import DataFrame


def upload_df_to_github_release(df: DataFrame) -> None:  # noqa: C901
    """
    Uploads a DataFrame to a GitHub release as a JSON file.

    Parameters:
        df (pd.DataFrame): The DataFrame to upload.
    """
    # Load GitHub token from environment variable
    GITHUB_TOKEN = os.getenv('TRAFIKKDATA_GH_PKG_TOKEN')
    if not GITHUB_TOKEN:
        raise ValueError('GitHub token not found. Make sure TRAFIKKDATA_GH_PKG_TOKEN is set in your environment.')

    # Define the repository and release details
    REPO_OWNER = 'trafikkdata'
    REPO_NAME = 'adt-model-results'
    RELEASE_TAG = 'adt-model'
    RELEASE_NAME = 'Newest release'
    RELEASE_BODY = 'Newest release of the ADT model results.'
    FILE_PATH = 'newest_adt_model_results.json'

    # Save the DataFrame to a JSON file
    df.to_json(FILE_PATH, orient='records')

    # Define headers
    headers = {'Authorization': f'token {GITHUB_TOKEN}', 'Accept': 'application/vnd.github.v3+json'}

    # Check if the release by tag exists
    release_url = f'https://api.github.com/repos/{REPO_OWNER}/{REPO_NAME}/releases/tags/{RELEASE_TAG}'
    release_response = requests.get(release_url, headers=headers, timeout=60)

    if release_response.status_code == 200:
        release = release_response.json()
        release_id = release['id']

        # Delete the release
        delete_release_url = f'https://api.github.com/repos/{REPO_OWNER}/{REPO_NAME}/releases/{release_id}'
        delete_release_response = requests.delete(delete_release_url, headers=headers, timeout=60)

        if delete_release_response.status_code != 204:
            logging.error('Failed to delete release.')
            logging.error(delete_release_response.text)
    else:
        logging.info('Release not found, proceeding to create a new release.')

    # Check if the tag exists and delete it
    delete_tag_url = f'https://api.github.com/repos/{REPO_OWNER}/{REPO_NAME}/git/refs/tags/{RELEASE_TAG}'
    delete_tag_response = requests.delete(delete_tag_url, headers=headers, timeout=60)

    if delete_tag_response.status_code != 204:
        logging.error('Failed to delete tag.')
        logging.error(delete_tag_response.text)

    # Create the release
    release_data = {
        'tag_name': RELEASE_TAG,
        'name': RELEASE_NAME,
        'body': RELEASE_BODY,
        'draft': False,
        'prerelease': False,
    }

    create_release_response = requests.post(
        f'https://api.github.com/repos/{REPO_OWNER}/{REPO_NAME}/releases',
        headers=headers,
        data=json.dumps(release_data),
        timeout=60,
    )

    if create_release_response.status_code == 201:
        release = create_release_response.json()
        upload_url = release['upload_url'].replace('{?name,label}', '')

        # Upload the file
        with open(FILE_PATH, 'rb') as file:
            file_name = FILE_PATH.split('/')[-1]
            upload_headers = headers.copy()
            upload_headers.update({'Content-Type': 'application/octet-stream'})
            upload_params = {'name': file_name}
            upload_response = requests.post(
                upload_url,
                headers=upload_headers,
                params=upload_params,
                data=file,
                timeout=60,
            )
            if upload_response.status_code == 201:
                # Verify and update release status if necessary
                release_id = release['id']
                release_url = f'https://api.github.com/repos/{REPO_OWNER}/{REPO_NAME}/releases/{release_id}'
                release_check_response = requests.get(release_url, headers=headers, timeout=60)
                if release_check_response.status_code == 200:
                    release_check_data = release_check_response.json()
                    if release_check_data['draft']:
                        # Update release to remove draft status
                        update_release_data = {'tag_name': RELEASE_TAG, 'name': RELEASE_NAME, 'body': RELEASE_BODY, 'draft': False, 'prerelease': False}
                        update_release_response = requests.patch(
                            release_url,
                            headers=headers,
                            data=json.dumps(update_release_data),
                            timeout=60,
                        )
                        if update_release_response.status_code != 200:
                            logging.error('Failed to update release status.')
                            logging.error(update_release_response.text)
            else:
                logging.error('Failed to upload file.')
                logging.error(upload_response.text)
    else:
        logging.error('Failed to create release.')
        logging.error(create_release_response.text)
