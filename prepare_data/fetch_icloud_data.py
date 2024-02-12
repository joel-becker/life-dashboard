import os
from dotenv import load_dotenv
from pyicloud import PyiCloudService
from pyicloud.exceptions import (
    PyiCloudFailedLoginException,
    PyiCloud2SARequiredException,
    PyiCloudAPIResponseException,
)
import zipfile
import xml.etree.ElementTree as ET
from io import BytesIO
import pandas as pd
from xml.etree.ElementTree import iterparse
from datetime import datetime


class iCloudDataFetcher:
    """
    iCloudDataFetcher is a class for fetching health-related data from iCloud.

    Attributes:
    - api (PyiCloudService): The API service object for iCloud.
    - is_authenticated (bool): A flag indicating whether the user is authenticated.
    - icloud_folder_name (str): The name of the iCloud folder where health data is stored.
    - verbose (bool): A flag indicating whether to print verbose messages.

    Methods:
    - __init__: Initializes iCloudDataFetcher with default settings.
    - login: Logs into iCloud using environment variables.
    - handle_2fa: Handles 2-Factor Authentication if required.
    - fetch_zip_data: Fetches ZIP data from iCloud's 'health_data' folder.
    - unpack_zip: Unpacks fetched ZIP data.
    - read_unzipped_xml: Reads unzipped XML data and converts it to a DataFrame.
    - fetch_apple_health_data: Fetches and unpacks health data from iCloud into a DataFrame.
    """

    def __init__(self):
        """
        Initialize iCloudDataFetcher with default settings.

        Attributes:
        - api (PyiCloudService): The API service object for iCloud.
        - is_authenticated (bool): A flag indicating whether the user is authenticated.
        - icloud_folder_name (str): The name of the iCloud folder where health data is stored.
        - verbose (bool): A flag indicating whether to print verbose messages.
        """
        self.api = None
        self.is_authenticated = False
        self.icloud_folder_name = "health_data"
        self.verbose = True

    def login(self):
        """
        Logs in to iCloud using environment variables.

        Returns:
        - bool: True if login successful or already logged in, False otherwise.

        Side-effects:
        - Prints messages indicating success or failure of login.
        """
        load_dotenv()
        email = os.environ.get("ICLOUD_EMAIL")
        password = os.environ.get("ICLOUD_PASSWORD")

        if not email or not password:
            if self.verbose:
                print("[Error] iCloud credentials not found in environment variables.")
            return False

        try:
            self.api = PyiCloudService(email, password)
            if self.verbose:
                print("[Info] Login successful.")
            if not self.api.requires_2fa:
                self.is_authenticated = True  # Already authenticated
            return True
        except PyiCloudFailedLoginException:
            if self.verbose:
                print("[Error] Failed iCloud login.")
            return False

    def handle_2fa(self):
        """
        Handles 2-Factor Authentication if required.

        Returns:
        - bool: True if 2FA successful, False otherwise.

        Side-effects:
        - Prints messages indicating success or failure of 2FA.
        """
        if not self.api or not self.api.requires_2fa:
            if self.verbose:
                print("[Error] Either not logged in or 2FA not required.")
            return False

        try:
            import click

            code = click.prompt("Enter the code sent to your device")
            result = self.api.validate_2fa_code(code)
            if result:
                if self.verbose:
                    print("[Info] 2FA successful.")
                self.is_authenticated = True  # Authenticated now
                return True
            else:
                if self.verbose:
                    print("[Error] 2FA failed.")
                return False
        except Exception as e:
            if self.verbose:
                print(f"[Error] 2FA handling failed: {e}")
            return False

    def fetch_zip_data(self, zip_file_name):
        """
        Fetches zip data from iCloud Drive.

        Returns:
        - BytesIO or None: BytesIO object containing zip data if successful, None otherwise.

        Side-effects:
        - Prints messages indicating success or failure of data fetch.
        """
        try:
            drive = self.api.drive
            folder = drive["dashboard_data"]
        except KeyError:
            if self.verbose:
                print("[Error] Folder not found.")
            return None
        except Exception as e:
            if self.verbose:
                print(f"[Error] An unexpected error occurred: {e}")
            return None

        zip_data = None
        try:
            if zip_file_name in folder.dir():
                drive_file = folder[zip_file_name]
                with drive_file.open(stream=True) as response:
                    if self.verbose:
                        print(f"[Info] Fetching {zip_file_name}.")
                    zip_data = BytesIO(response.raw.read())
                    if self.verbose:
                        print(f"[Info] Fetched {zip_file_name} successfully.")
            else:
                if self.verbose:
                    print(f"[Error] {zip_file_name} not found.")
                return None
        except Exception as e:
            if self.verbose:
                print(f"[Error] An unexpected error occurred: {e}")
            return None

        if zip_data is None:
            if self.verbose:
                print("[Error] ZIP file not found.")
        else:
            if self.verbose:
                print("[Info] ZIP file fetched successfully.")
        return zip_data

    def unpack_zip(self, zip_data):
        """
        Unpacks fetched ZIP data.

        Parameters:
        - zip_data (BytesIO): The ZIP data as a BytesIO object.

        Returns:
        - dict or None: Dictionary of BytesIO objects if unzipping successful, None otherwise.

        Side-effects:
        - Prints messages indicating success or failure of unzipping.
        """
        if zip_data is None:
            if self.verbose:
                print("[Error] No ZIP data to unpack.")
            return None

        files = {}
        try:
            with zipfile.ZipFile(zip_data) as z:
                for filename in z.namelist():
                    with z.open(filename) as f:
                        files[filename] = BytesIO(f.read())
            if self.verbose:
                print("[Info] Successfully unzipped.")
            return files
        except Exception as e:
            if self.verbose:
                print(f"[Error] An exception occurred during unzipping: {e}")
            return None

    def read_unzipped_xml(self, xml_data, data_type='record'):
        """
        Reads unzipped XML data and converts it to a DataFrame.

        Parameters:
        - xml_data (BytesIO): The XML data as a BytesIO object.
        - data_type (str): The type of data to extract ('record' or 'workout').

        Returns:
        - pd.DataFrame or None: DataFrame containing XML data if successful, None otherwise.

        Side-effects:
        - Prints messages indicating success or failure of XML data reading.
        """
        if xml_data is None:
            if self.verbose:
                print("[Error] No XML data to read.")
            return None

        data_list = []  # Initialize an empty list to collect data

        try:
            xml_data.seek(0)  # Reset pointer to the start of the file
            for event, elem in ET.iterparse(xml_data):
                if event == "end" and elem.tag == data_type.capitalize():
                    # Extract data
                    record = {}
                    for name, value in elem.items():
                        record[name] = value

                    # Handling nested elements
                    for child in elem:
                        print(child.tag)
                        #if child.tag in ['totalDistance', 'totalEnergyBurned', ...]:  # Add relevant tags
                        #    record[child.tag] = child.text  # or child.attrib if data is in attribute

                    data_list.append(record)

                    # Clear the element from the tree to free memory
                    elem.clear()

            if self.verbose:
                print(f"[Info] Successfully read XML {data_type} data.")

            # Convert list of dictionaries to DataFrame
            df = pd.DataFrame(data_list)
            return df
        except ET.ParseError as e:
            if self.verbose:
                print(f"[Error] XML parsing failed: {e}")
            return None
        except Exception as e:
            if self.verbose:
                print(f"[Error] An exception occurred during XML reading: {e}")
            return None

    def fetch_csv_data(self, csv_file_name="strong.csv"):
        """
        Fetches a CSV file from the 'health_data' folder on iCloud Drive and converts it into a DataFrame.

        Parameters:
        - csv_file_name (str): The name of the CSV file to fetch. Default is "strong.csv".

        Returns:
        - pd.DataFrame or None: A DataFrame containing the CSV data if successful, None otherwise.

        Side-effects:
        - Prints informational messages indicating the success or failure of the operation.
        """
        try:
            drive = self.api.drive
            health_data_folder = drive["dashboard_data"]
        except KeyError:
            if self.verbose:
                print("[Error] Folder not found.")
            return None

        csv_data = None
        try:
            if csv_file_name in health_data_folder.dir():
                drive_file = health_data_folder[csv_file_name]
                with drive_file.open(stream=True) as response:
                    csv_data = pd.read_csv(BytesIO(response.raw.read()))
            else:
                if self.verbose:
                    print(f"[Error] {csv_file_name} not found.")
                return None
        except Exception as e:
            if self.verbose:
                print(f"[Error] An unexpected error occurred: {e}")
            return None

        if csv_data is not None:
            if self.verbose:
                print(f"[Info] {csv_file_name} fetched successfully.")
            return csv_data

    def fetch_apple_health_data(self):
        """
        Fetches and unpacks health data from iCloud into a DataFrame.

        Returns:
        - pd.DataFrame or None: DataFrame containing health data if successful, None otherwise.

        Side-effects:
        - Prints messages indicating success or failure of the entire data fetch process.
        """
        zip_data = self.fetch_zip_data("export.zip")
        if zip_data is not None:
            try:
                unzipped_data = self.unpack_zip(zip_data)
                unzipped_export = unzipped_data["apple_health_export/export.xml"]
            except:
                if self.verbose:
                    print("[Error] Data unzipping failed.")
                return None

        unzipped_export_record = self.read_unzipped_xml(unzipped_export, data_type='record')
        unzipped_export_workout = self.read_unzipped_xml(unzipped_export, data_type='workout')
        if (
            unzipped_export_record is not None and not unzipped_export_record.empty
        ) and (
            unzipped_export_workout is not None and not unzipped_export_workout.empty
        ):
            if self.verbose:
                print("[Info] Data fetched successfully.")
            return unzipped_export_record, unzipped_export_workout
        else:
            if self.verbose:
                print("[Error] Data fetching failed.")
            return None

    def fetch_emoods_data(self):
        """
        Fetches and processes the latest eMoods data from iCloud Drive.

        Returns:
        - Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame] or None:
          A tuple containing DataFrames for main symptoms, custom entries, and custom symptoms if successful, None otherwise.
        """
        zip_data = self.fetch_zip_data("eMoods-2024-Feb.zip")
        if zip_data is not None:
            try:
                unzipped_data = self.unpack_zip(zip_data)
            except:
                if self.verbose:
                    print("[Error] Data unzipping failed.")
                return None

        # Reading the CSV files from the extracted data
        main_symptoms = pd.read_csv(unzipped_data["entry.csv"])
        custom_entries = pd.read_csv(unzipped_data["entry_custom_symptom.csv"])
        custom_symptoms = pd.read_csv(unzipped_data["custom_symptom.csv"])

        return main_symptoms, custom_entries, custom_symptoms

    def fetch_icloud_data(self):
        """
        Executes the entire data fetching pipeline, including login, 2FA, fetching Apple Health data,
        and fetching a specific CSV file from 'health_data' iCloud folder.

        Returns:
        - tuple: A tuple containing two DataFrames (health_data, strong_data) if successful.
                 Both or either could be None in case of failure.

        Side-effects:
        - Prints informational messages indicating the success or failure of each operation.
        """
        if self.login():
            if not self.is_authenticated:
                if not self.handle_2fa():
                    if self.verbose:
                        print("[Error] 2FA failed. Exiting.")
                    return None
            apple_health_data_record, apple_health_data_workout = self.fetch_apple_health_data()
            strong_data = self.fetch_csv_data()
            main_symptoms, custom_entries, custom_symptoms = self.fetch_emoods_data()
            return (
                apple_health_data_record,
                apple_health_data_workout,
                strong_data,
                main_symptoms,
                custom_entries,
                custom_symptoms,
            )
