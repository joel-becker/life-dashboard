from prepare_data.fetch_icloud_data import iCloudDataFetcher
from prepare_data.process_data import (
    HealthDataProcessor,
    WeightDataProcessor,
    ExerciseDataProcessor,
    WeightliftingDataProcessor,
    MergedDataProcessor,
)
from prepare_data.process_mental_health import MentalHealthDataProcessor
from prepare_data.perform_calculations import VolumeDataProcessor, PerformCalculations
from prepare_data.save_for_app import save_to_csv
from importlib import reload
import pandas as pd


def main():
    # Fetch data from iCloud
    fetcher = iCloudDataFetcher()
    (
        apple_health_data_records,
        apple_health_data_workouts,
        strong_data,
        main_symptoms,
        custom_entries,
        custom_symptoms,
    ) = fetcher.fetch_icloud_data()

    if apple_health_data_records or apple_health_data_workouts is None:
        print("Failed to fetch iCloud data. Exiting.")
        return

    # Process raw data
    hdp = HealthDataProcessor()
    edp = ExerciseDataProcessor()
    wdp = WeightDataProcessor(hdp)
    wldp = WeightliftingDataProcessor()
    mhp = MentalHealthDataProcessor(main_symptoms, custom_entries, custom_symptoms)

    apple_data_records = hdp.process_record_data(apple_health_data_records)
    apple_data_workouts = hdp.process_workout_data(apple_health_data_workouts)
    exercise_data = edp.clean_exercise_data(strong_data)
    mental_health_data = mhp.wrangle_mental_health_data()
    non_exercise_goal_data = mhp.wrangle_non_exercise_goal_data()
    weight_data = wdp.wrangle_weight_data(apple_data_records)
    weightlifting_data = wldp.wrangle_weightlifting_data(exercise_data, weight_data)

    vdp = VolumeDataProcessor(weightlifting_data)
    volume_data = vdp.process_volume_data()

    # Save or update data for RShiny to use
    save_to_csv(apple_data_workouts, filename="non_weightlifting_exercise", folder="./data/")
    save_to_csv(weight_data, filename="weight", folder="./data/")
    save_to_csv(weightlifting_data, filename="weightlifting", folder="./data/")
    save_to_csv(mental_health_data, filename="mental_health", folder="./data/")
    save_to_csv(non_exercise_goal_data, filename="non_exercise_goal", folder="./data/")
    save_to_csv(volume_data, filename="volume", folder="./data/")


if __name__ == "__main__":
    main()
