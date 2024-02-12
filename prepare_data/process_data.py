import pandas as pd
from pandas.api.types import CategoricalDtype
from datetime import datetime


def clean_dataframe(data: pd.DataFrame) -> pd.DataFrame:
    """Utility function to clean DataFrame."""
    data.columns = data.columns.str.lower().str.replace(" ", "_")
    return data


class HealthDataProcessor:
    """
    Class for processing Apple Health data.
    """

    def process_record_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """Cleans raw health data."""

        # Rename columns to clean format
        data = clean_dataframe(data)

        # Convert 'value' to numeric type
        data["value"] = pd.to_numeric(data["value"], errors="coerce")

        # Convert 'enddate' to datetime and extract components
        data["enddate"] = pd.to_datetime(data["enddate"])
        data["date"] = data["enddate"].dt.date
        data["year"] = data["enddate"].dt.year
        data["month"] = data["enddate"].dt.month
        data["day"] = data["enddate"].dt.day
        data["yday"] = data["enddate"].dt.dayofyear
        data["wday"] = data["enddate"].dt.weekday
        data["hour"] = data["enddate"].dt.hour
        data["minute"] = data["enddate"].dt.minute

        # Clean 'type' and 'device' columns
        data["type"] = data["type"].str.replace("HKQuantityTypeIdentifier", "")
        data["device"] = data["device"].str.extract(r"name:(.*?),", expand=False)

        # Drop unnecessary columns
        data.drop(["sourceversion", "creationdate", "startdate"], axis=1, inplace=True)

        return data

    def join_dates(
        self,
        data: pd.DataFrame,
        multiple_metrics: bool = False,
        metric_type: str = "energy",
        energy_metrics: list = None,
        nutrition_metrics: list = None,
        volume_metrics: list = None,
    ) -> pd.DataFrame:
        """Fills gaps in data by joining with a complete date sequence."""
        if energy_metrics is None:
            energy_metrics = [
                "Calorie expenditure",
                "Calorie intake",
                "Calorie deficit (absolute)",
                "Calorie deficit (relative)",
            ]
        if nutrition_metrics is None:
            nutrition_metrics = ["Protein", "Sugar", "Water", "Carbohydrates", "Fat"]
        if volume_metrics is None:
            volume_metrics = ["hypertrophy_adjusted_volume", "volume"]

        if multiple_metrics:
            if metric_type == "energy":
                metrics = energy_metrics
            elif metric_type == "nutrition":
                metrics = nutrition_metrics
            elif metric_type == "volume":
                metrics = volume_metrics

        # Create a date sequence from min date to today
        min_date = data["date"].min()
        max_date = datetime.now().date()
        date_seq = pd.date_range(min_date, max_date, freq="D").to_frame(
            index=False, name="date"
        )

        # Handle multiple metrics by creating a Cartesian product with dates
        if multiple_metrics:
            if metric_type == "energy":
                metrics = [
                    "Calorie expenditure",
                    "Calorie intake",
                    "Calorie deficit (absolute)",
                    "Calorie deficit (relative)",
                ]
            elif metric_type == "nutrition":
                metrics = ["Protein", "Sugar", "Water", "Carbohydrates", "Fat"]
            elif metric_type == "volume":
                metrics = ["hypertrophy_adjusted_volume", "volume"]

            date_seq["key"] = 0
            metrics_df = pd.DataFrame({"metric": metrics})
            metrics_df["key"] = 0

            date_seq = date_seq.merge(metrics_df, on="key").drop("key", axis=1)

        # Join the data with the complete date sequence
        data["date"] = pd.to_datetime(data["date"])
        if multiple_metrics:
            data = pd.merge(date_seq, data, how="left", on=["date", "metric"])
        else:
            data = pd.merge(date_seq, data, how="left", on="date")

        return data

    def extract_workout_data(self, workout_data: pd.DataFrame) -> pd.DataFrame:
        """
        Extracts and processes workout data for soccer, non-soccer cardio, and cooldown activities.

        Parameters:
        - workout_data (pd.DataFrame): DataFrame containing workout data.

        Returns:
        - pd.DataFrame: Processed workout data with type, date, and duration.
        """
        # Rename columns to clean format
        workout_data = clean_dataframe(workout_data)

        # Clean and prepare workout data
        workout_data['startdate'] = pd.to_datetime(workout_data['startdate'])
        workout_data['enddate'] = pd.to_datetime(workout_data['enddate'])

        workout_data['workoutactivitytype'] = workout_data['workoutactivitytype'].str.replace('HKWorkoutActivityType', '')
        workout_data = workout_data.rename({"workoutactivitytype": "type"}, axis=1)

        workout_data['duration'] = pd.to_numeric(workout_data['duration'], errors='coerce')

        # Filter for soccer and non-soccer cardio workouts
        soccer_data = workout_data[workout_data['type'] == 'Soccer']
        non_soccer_cardio = workout_data[workout_data['type'].isin([
            'Running', 'Swimming', 'Cycling',
            'Hiking', 'StairClimbing', 'CardioDance',
            'Rowing', 'Other', 'Cooldown', 'Yoga'])]

        # TODO: Define criteria for cooldown sessions

        # Combine data
        combined_data = pd.concat([soccer_data, non_soccer_cardio])

        # Select relevant columns
        result = combined_data[['type', 'startdate', 'duration']]
        result.rename(columns={'startdate': 'date'}, inplace=True)

        return result
    
    def get_data_for_workout(self, data, workout):
        """
        Extracts data for a specific workout period from a given dataset.

        Parameters:
        - data (pd.DataFrame): DataFrame containing health data.
        - workout (pd.DataFrame): DataFrame containing a single workout record.

        Returns:
        - pd.DataFrame: Data for the specified workout period.
        """
        start = workout['startDate'].item()
        end = workout['endDate'].item()
        return data[(data['startDate'] >= start) & (data['endDate'] <= end)]

    def convert_to_minute_proportion(self, number):
        return int(number) + ((number % 1) / 100 * 60)

    def get_pace_for_workout(self, workout):
        if workout['totalDistance'] == 0.0:
            return 0.0
        pace = workout['duration'] / workout['totalDistance']
        return self.convert_to_minute_proportion(pace)

    def process_workouts(self, workouts, heartrate_data, energy_data, distance_data):
        """
        Processes workout data to include heart rate, energy burned, distance, and pace.

        Parameters:
        - workouts (pd.DataFrame): DataFrame containing workouts.
        - heartrate_data (pd.DataFrame): DataFrame containing heart rate data.
        - energy_data (pd.DataFrame): DataFrame containing energy burned data.
        - distance_data (pd.DataFrame): DataFrame containing distance data.

        Returns:
        - pd.DataFrame: Enhanced workout data.
        """
        workouts['heartrate_data'] = workouts.apply(lambda row: self.get_data_for_workout(heartrate_data, row), axis=1)
        workouts['energy_data'] = workouts.apply(lambda row: self.get_data_for_workout(energy_data, row), axis=1)
        workouts['distance_data'] = workouts.apply(lambda row: self.get_data_for_workout(distance_data, row), axis=1)

        workouts['hr_mean'] = workouts['heartrate_data'].apply(lambda x: x['value'].mean() if not x.empty else None)
        workouts['total_energy_burned'] = workouts['energy_data'].apply(lambda x: x['value'].sum() if not x.empty else None)
        workouts['total_distance'] = workouts['distance_data'].apply(lambda x: x['value'].sum() if not x.empty else None)
        workouts['pace'] = workouts.apply(self.get_pace_for_workout, axis=1)

        # Drop the temporary data columns
        workouts.drop(['heartrate_data', 'energy_data', 'distance_data'], axis=1, inplace=True)

        return workouts
    
    def process_workout_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        A wrapper function that chains cleaning and filtering operations.

        Parameters:
        - data (pd.DataFrame): Raw workout data.

        Returns:
        - pd.DataFrame: Processed workout data.
        """
        cleaned_data = self.extract_workout_data(data)
        #filtered_data = self.filter_workout_data(cleaned_data)

        #return filtered_data
        return cleaned_data

class WeightDataProcessor:
    def __init__(
        self,
        health_data_processor: HealthDataProcessor,
    ):
        self.hdp = health_data_processor

    def wrangle_weight_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """Wrangles weight data."""
        data = data[data["type"] == "BodyMass"].loc[:, ["date", "value"]]
        data = self.hdp.join_dates(data)
        data["value"].interpolate(inplace=True)  # Approximate NAs
        data["body_mass"] = data["value"] * 2.20462  # Convert to pounds
        data = (
            data.groupby("date").agg({"body_mass": "min"}).reset_index()
        )  # Min body mass per date
        data["date"] = pd.to_datetime(data["date"]).dt.date
        return data

    def wrangle_nonweightlifting_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        Wrangles non-weightlifting data.

        1. Filters data to include only specific activities.
        2. Cleans 'workout_activity_type' and 'creation_date'.
        3. Selects relevant columns.

        Parameters:
        - data (pd.DataFrame): Raw non-weightlifting data.

        Returns:
        - pd.DataFrame: Wrangled non-weightlifting data.
        """
        # Define non-weightlifting activities
        nonweightlifting_activities = [
            "HKWorkoutActivityTypeSoccer",
            "HKWorkoutActivityTypeRunning",
            "HKWorkoutActivityTypeHiking",
            "HKWorkoutActivityTypeStairClimbing",
        ]

        # Filter data and clean columns
        data = data[data["workout_activity_type"].isin(nonweightlifting_activities)]
        data["date"] = pd.to_datetime(data["creation_date"]).dt.date

        # Select relevant columns
        data = data[["date", "total_energy_burned"]]

        return data


class ExerciseDataProcessor:
    """
    Class for processing Exercise data.
    """

    def clean_exercise_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """Cleans raw exercise data."""

        # Rename columns to clean format
        data = clean_dataframe(data)

        # Convert 'date' to datetime type
        data["date"] = pd.to_datetime(data["date"]).dt.date

        # Select relevant columns
        data = data[["date", "exercise_name", "set_order", "weight", "reps"]]

        return data


class WeightliftingDataProcessor:
    def calculate_derivative_metrics(self, data: pd.DataFrame) -> pd.DataFrame:
        # Convert 'date' to datetime format
        data["date"] = pd.to_datetime(data["date"])

        # Sort data
        data.sort_values(by=["exercise_name", "date"], inplace=True)

        # Group by 'exercise_name' and 'date', then find the daily max of 'one_rep_max'
        daily_max = (
            data.groupby(["exercise_name", "date"])["one_rep_max"].max().reset_index()
        )

        # Calculate cumulative max
        daily_max["cummax_one_rep_max"] = daily_max.groupby("exercise_name")[
            "one_rep_max"
        ].cummax()

        # Set 'date' as the index
        daily_max.set_index("date", inplace=True)

        # Calculate rolling 90-day max
        daily_max["rolling_90d_max"] = (
            daily_max.groupby("exercise_name")["one_rep_max"]
            .rolling(window="90D")
            .max()
            .reset_index(level=0, drop=True)
        )

        # Reset index for merging
        daily_max.reset_index(inplace=True)

        # Merge calculated metrics back into original data
        data = pd.merge(
            data,
            daily_max,
            on=["exercise_name", "date"],
            how="left",
            suffixes=("", "_y"),
        )

        # Drop duplicated columns from merge
        data.drop(
            [col for col in data.columns if col.endswith("_y")], axis=1, inplace=True
        )

        # Sort data
        data.sort_values(by=["date", "exercise_name"], inplace=True)

        return data

    def wrangle_weightlifting_data(
        self, exercise_data: pd.DataFrame, weight_data: pd.DataFrame
    ) -> pd.DataFrame:
        # Join with weight data
        data = pd.merge(exercise_data, weight_data, on="date", how="left")

        # Calculate 1RM metrics
        data = self.calculate_1RM(data)

        # Calculate derivative metrics
        data = self.calculate_derivative_metrics(data)

        # Sort by date
        data.sort_values(by="date", inplace=True)

        # Filter data
        data = self.filter_exercise_data(data)

        return data

    def calculate_1RM(self, data: pd.DataFrame) -> pd.DataFrame:
        # Initialize columns for calculated metrics
        data["one_rep_max"] = 0.0

        # List of unweighted exercises
        unweighted_exercises = [
            "Chin Up",
            "Pull Up",
            "Wide Pull Up",
            "Back Extension",
            "Ab Coaster",
            "Battle Ropes",
            "Press Up",
            "Chest Dip",
            "Triceps Dip",
            "Curved Hack Squat",
            "Bulgarian Split Squat (Plate-Loaded)",
            "Knee Raise (Captain's Chair)",
            "Half-Hack Squat",
            "Goblet Squat (Kettlebell)",
            "Lunge (Dumbbell)",
        ]

        # Get barbell exercises
        data["exercise_name"] = data["exercise_name"].fillna("Unknown").astype(str)
        all_exercises = data["exercise_name"].unique()
        barbell_exercises = [
            exercise for exercise in all_exercises if "(Barbell)" in exercise
        ]

        # Calculate 1RM and hypertrophy-adjusted 1RM
        for idx, row in data.iterrows():
            exercise_name = row["exercise_name"]
            weight = row["weight"]
            reps = row["reps"]
            body_mass = row["body_mass"]

            if exercise_name in barbell_exercises:
                weight += 44

            if exercise_name in unweighted_exercises:
                one_rep_max = body_mass * (1 + (reps / 30))
            else:
                one_rep_max = weight * (1 + (reps / 30))

            data.at[idx, "one_rep_max"] = one_rep_max

        # Compute cumulative maxima
        data_grouped = data.groupby("exercise_name")
        for name, group in data_grouped:
            group["cummax_one_rep_max"] = group["one_rep_max"].cummax()
            data.update(group)

        return data

    def filter_exercise_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """Filters exercise data to remove probable dropsets and easier versions of machines."""

        # Filter out rows with missing exercise names
        data = data[data["exercise_name"].notna()]

        # Filter out zero 1RM values
        data = data[data["one_rep_max"] > 0]

        # Drop unnecessary columns
        data.drop(["set_order", "weight", "body_mass"], axis=1, inplace=True)

        return data

    def process_exercise_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """
        A wrapper function that chains cleaning and filtering operations.

        Parameters:
        - data (pd.DataFrame): Raw exercise data.

        Returns:
        - pd.DataFrame: Processed exercise data.
        """
        cleaned_data = self.clean_exercise_data(data)
        filtered_data = self.filter_exercise_data(cleaned_data)

        return filtered_data


class MergedDataProcessor:
    """Class for processing data that involves or is downstream of merging datasets."""

    def __init__(
        self,
        health_data_processor: HealthDataProcessor,
        exercise_data_processor: ExerciseDataProcessor,
    ):
        self.health_data_processor = health_data_processor
        self.exercise_data_processor = exercise_data_processor

    def wrangle_volume_data(
        self,
        weightlifting_data: pd.DataFrame,
        nonweightlifting_data: pd.DataFrame,
        weight_data: pd.DataFrame,
    ) -> pd.DataFrame:
        """Wrangles exercise volume data."""
        nonweightlifting_data_wrangled = (
            self.health_data_processor.wrangle_nonweightlifting_data(
                nonweightlifting_data
            )
        )

        # Group by 'date' and calculate volume metrics
        volume_data = (
            weightlifting_data.groupby("date")
            .agg({"one_rep_max": "sum", "hypertrophy_adjusted_one_rep_max": "sum"})
            .reset_index()
        )

        # Merge weightlifting and nonweightlifting data
        merged_data = pd.merge(
            volume_data, nonweightlifting_data_wrangled, on="date", how="outer"
        )

        # Fill NA values
        merged_data.fillna({"total_energy_burned": 0}, inplace=True)

        # Sort by 'date'
        merged_data.sort_values("date", inplace=True)

        return merged_data
