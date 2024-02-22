import pandas as pd
import numpy as np
from dateutil.parser import parse
from sklearn.preprocessing import MinMaxScaler
import re


class MentalHealthDataProcessor:
    def __init__(self, main_symptoms, custom_entries, custom_symptoms, verbose=False):
        self.main_symptoms = main_symptoms
        self.custom_entries = custom_entries
        self.custom_symptoms = custom_symptoms
        self.verbose = verbose

    def merge_data(self):
        # Find the maximum date in custom_entries
        max_date = self.custom_entries["ENTRY"].max()

        # Merge custom_data
        custom_data = (
            self.custom_symptoms.rename(columns={"ID": "SYMPTOM"})
            .merge(self.custom_entries, on="SYMPTOM")
            .assign(
                days_since_last=lambda df: (max_date - df["ENTRY"]),
                date=lambda df: (
                    pd.Timestamp("today")
                    - pd.to_timedelta(df["days_since_last"], unit="d")
                )
                .dt.strftime("%Y-%m-%d")
                .astype(str),
            )
            .pivot_table(index="date", columns="NAME", values="VALUE", aggfunc="first")
            .reset_index()
        )

        # Merge with main symptoms data
        self.main_symptoms["date"] = pd.to_datetime(
            self.main_symptoms["DATE (YYYY-MM-DD)"]
        ).astype(str)
        combined_data = self.main_symptoms.merge(custom_data, on="date", how="left")
        return combined_data

    @staticmethod
    def clean_merged_data(df):
        # Convert column names to snake case
        df.columns = [re.sub(r"\s+", "_", col).lower() for col in df.columns]

        # Combine liberty
        df["combined_liberty"] = (df["positive_liberty"] + df["negative_liberty"]) / 2
        df["liberty"] = df["liberty"].fillna(df["combined_liberty"])

        # Remove irrelevant columns
        columns_to_drop = [
            "id",
            "date_(yyyy-mm-dd)",
            "positive_liberty",
            "negative_liberty",
            "combined_liberty",
        ]
        df.drop(columns=columns_to_drop, inplace=True, errors="ignore")

        # Move 'date' column to the first position
        date_column = df.pop("date")
        df.insert(0, "date", date_column)

        # Set numeric columns to 1 if NA, excluding 'date' and 'note', after first non-NA date
        numeric_columns = df.select_dtypes(include=[np.number]).columns.tolist()
        numeric_columns = [col for col in numeric_columns if col not in ["date"]]

        for col in numeric_columns:
            first_non_na_date = df[df[col].notna()]["date"].min()
            df.loc[df["date"] > first_non_na_date, col] = df.loc[
                df["date"] > first_non_na_date, col
            ].fillna(1)

        # Set note to None if NA
        df["note"] = df["note"].fillna("")

        return df

    def add_scores_to_exceptional_days(self, data):
        exceptional_days_conditions = [
            ("private tour of Aruba’s national park", 4.5),
            ("performed Part of Your World", 5.0),
            ("Day after Duelling Pianos", 4.0),
            ("magical island off of Vieques", 5.0),
            ("jam session at Linden", 4.5),
            ("hotel Halloween party", 5.0),
            ("Won FTX fellowship", 4.0),
            ("Did well in important exam", 4.5),
            ("art gallery rationalist party", 4.0),
            ("last full day in nyc", 4.5),
            ("Received great grades", 4.0),
            ("first day living in Nassau", 5.0),
            ("day two in the Bahamas", 4.0),
            ("Met up with FTX staff", 4.0),
            ("big welcome dinner for FTX fellows", 4.0),
            ("going to war with FTX fellows", 4.0),
            ("amazing opportunity in the Bahamas", 4.0),
            ("opulent party", 4.0),
            ("blissful meditation experience", 4.5),
            ("Received great professional offer", 4.0),
            ("workday; EA dinner", 4.0),
            ("Excursion to the Exumas", 4.5),
            ("Miti's dream job offer", 4.5),
            ("excited for Miti", 4.0),
            ("Back in Nassau", 4.0),
            ("Atlantis water park", 4.5),
            ("Relaxing day with Miti", 4.0),
            ("Miti's fancy birthday dinner", 4.5),
            ("Dinner with Ben, Aria", 4.0),
            ("Misha and Joel's", 4.0),
            ("call with immigration lawyer", 4.0),
            ("excited for conferences", 4.0),
            ("seeing Greta", 4.5),
            ("friends back-to-back, through EAGxBoston", 4.5),
            ("First full day of EAGx", 4.5),
            ("Last day of EAGxBoston", 4.5),
            ("job contract meeting", 4.0),
            ("Booba and Miti", 4.5),
            ("Walk around Kenwood", 4.5),
            ("First day of EAG London", 4.0),
            ("Second day of EAG London", 4.0),
            ("FTX agree to guarantee", 4.0),
            ("Day after great immigration news", 4.5),
            ("coffee tasting in art gallery", 4.0),
            ("singalong with Chi", 4.0),
            ("Mum's arrival in Nassau", 4.0),
            ("Russian table songs with iulia", 4.5),
            ("lunch with Mum and Miti on Paradise Island", 4.5),
            ("Breakfast with iulya", 5.0),
            ("albeit with a very painful shoulder", 4.0),
            ("Given responsibility to run Wytham event", 4.0),
            ("Met my new puppy", 4.5),
            ("Bonding with Anemone", 4.5),
            ("getting over angst around normative uncertainty", 4.5),
            ("27th birthday", 4.0),
            ("arriving in SF for Constellation", 4.0),
            ("start in-person work on SHELTER Weekend", 4.0),
            ("welcoming participants to SHELTER Weekend", 4.5),
            ("First full day of SHELTER Weekend", 4.5),
            ("Meet up with London-based friends", 4.0),
            ("FTX bonus announcement", 4.0),
            ("Sending Misha off", 4.0),
            ("First full day in Boston", 4.0),
            ("Returning to Getaway cabins", 4.0),
            ("Leaving Getaway cabins", 4.5),
            ("renewed creativity and tolerance", 4.0),
            ("first solo thinking trip", 4.5),
            ("experiencing long-kindness glow on top of the lake", 4.5),
            ("Alexey, Linch, Basil, Sam, Greta, and Eric", 4.5),
            ("Cedar point with Miti", 4.0),
            ("Reminded of how great I feel", 4.0),
            ("Clarifying chat with Alexey", 4.0),
            ("coffee, lunch, heat, cold, and dinner with Misha", 4.0),
            ("AI concerns started to click intuitively", 4.0),
            ("Last day in Prague", 4.5),
            ("Beautiful trip with Miti", 4.0),
            ("wrote nice new years messages", 4.0),
            ("Job offer from CAIS", 4.5),
            ("tacos with EA NYC friends, Harry, and Sophie", 4.0),
            ("Exploring Roma Norte and Ian's food suggestions", 4.0),
            ("First day at CAIS", 4.0),
            ("Feeling grateful for my recent luck", 4.0),
            ("Abba-St. Patrick's day club night", 4.0),
            ("Happy with green card", 4.5),
            ("Enjoying beautiful CDMX", 4.0),
            ("exploring Paris for hours", 4.5),
            ("party with Arden", 4.0),
            ("Harry and Sophie's wedding", 4.5),
            ("ESPR house party in north London", 4.0),
            ("pro-social start-up ideas", 4.0),
            ("energized at Ryan's disney character party", 4.5),
            ("Momentum hot tub party", 4.0),
            ("dancing at Constance", 4.5),
            ("coffee with Richard", 4.0),
            ("bizarre, hilarious, and deeply aspirational theatre", 5.0)
            # Add more conditions as needed
        ]

        for note, score in exceptional_days_conditions:
            data.loc[data["note"].str.contains(note, na=False), "elevated"] = score

        return data

    def anxiety_depression_interaction(self, data):
        """Calculate the interaction between anxiety and depression."""
        anxiety = data["anxiety"].apply(lambda x: x**6.6582 if x != 0 else np.nan)
        depression = data["depressed"].apply(
            lambda x: x**6.6582 if x != 0 else np.nan
        )
        data["anxiety_depression_interaction"] = 0.5 * (anxiety + depression) ** 0.5

        return data["anxiety_depression_interaction"]

    def calculate_score(
        self,
        data,
        vars_info,
        cutoff_proportion=1,
        exclude_dates=[("2000-01-01", "2021-12-01"), ("2023-06-16", "2023-09-20")],
        rescale=True,
    ):
        """
        Vectorized function to calculate scores accounting for missing data with a cutoff.

        Parameters:
        - data (DataFrame): The DataFrame containing the data.
        - vars_info (dict): A dictionary with variable names as keys and tuples (min_val, max_val, weight, transformation) as values.
        - cutoff_proportion (float): The minimum proportion of non-NA factors required to calculate the score.
        - exclude_dates (list of tuples): List of date ranges (as tuples) to exclude. Each tuple is (start_date, end_date).

        Returns:
        - DataFrame: The DataFrame with new adjusted scores columns.
        """
        # Convert date column to datetime
        data["date"] = pd.to_datetime(data["date"])

        # Initialize valid_rows with all True
        valid_rows = pd.Series([True] * len(data), index=data.index)

        # Exclude specified date ranges
        if exclude_dates:
            for start_date, end_date in exclude_dates:
                start_date = pd.to_datetime(start_date)
                end_date = pd.to_datetime(end_date)
                exclusion = data["date"].between(start_date, end_date)
                if self.verbose:
                    print(f"Excluding dates between {start_date} and {end_date}:")
                    print(data.loc[exclusion, "date"])
                valid_rows &= ~exclusion

        # Check for cutoff proportion
        available_factors = data[[var for var in vars_info]].notna().sum(axis=1)
        total_factors = len(vars_info)
        if self.verbose:
            print("Data with above cutoff (but below 100%) proportion:")
            print(data.loc[available_factors / total_factors >= cutoff_proportion])

        # Revised uniform data check
        main_vars = {"elevated", "depressed", "anxiety"}
        main_zero_check = (
            data[[var for var in vars_info if var in main_vars]].eq(0).all(axis=1)
        )
        custom_one_check = (
            data[[var for var in vars_info if var not in main_vars]].eq(1).all(axis=1)
        )
        if self.verbose:
            print("Main zero check:")
            print(
                data.loc[
                    main_zero_check, [var for var in vars_info if var in main_vars]
                ]
            )
            print("Custom one check:")
            print(
                data.loc[
                    custom_one_check, [var for var in vars_info if var not in main_vars]
                ]
            )

        # uniform_data = main_check & main_zero_check & custom_check & custom_one_check
        uniform_data = main_zero_check & custom_one_check
        if self.verbose:
            print("Uniform data:")
            print(data.loc[uniform_data, [var for var in vars_info]])
            print("Non-uniform data:")
            print(data.loc[~uniform_data, [var for var in vars_info]])
        valid_rows = (available_factors / total_factors >= cutoff_proportion) & (
            ~uniform_data
        )

        # Apply transformations and weight, preserving NAs
        for var, (min_val, max_val, weight, transformation) in vars_info.items():
            transformed_col = f"transformed_{var}"
            data[transformed_col] = data[var].apply(
                lambda x: transformation(x) * weight if pd.notna(x) else np.nan
            )

        # Replace NAs with 0s for score calculation
        for transformed_col in [f"transformed_{var}" for var in vars_info]:
            data[transformed_col].fillna(0, inplace=True)

        # Calculate the actual score
        data["score_sum"] = data[[f"transformed_{var}" for var in vars_info]].sum(
            axis=1
        )

        # Rescale the score
        if rescale:
            # Calculate min and max possible scores
            min_scores = np.array(
                [
                    transformation(min_val) * weight
                    if weight > 0
                    else transformation(max_val) * weight
                    for min_val, max_val, weight, transformation in vars_info.values()
                ]
            )
            max_scores = np.array(
                [
                    transformation(max_val) * weight
                    if weight > 0
                    else transformation(min_val) * weight
                    for min_val, max_val, weight, transformation in vars_info.values()
                ]
            )
            min_possible_score = min_scores.sum()
            max_possible_score = max_scores.sum()

            # Normalize the score
            score = (data["score_sum"] - min_possible_score) / (
                max_possible_score - min_possible_score
            ) * 200 - 100
        else:
            score = data["score_sum"]

        # Assign np.nan to invalid rows
        score = np.where(valid_rows, score, np.nan)

        # Clean up temporary columns
        data.drop(
            columns=[f"transformed_{var}" for var in vars_info] + ["score_sum"],
            inplace=True,
        )

        return score

    def calculate_mental_health_metrics(self, data):
        # Define variables info with transformations
        data["anxiety_depression_interaction"] = self.anxiety_depression_interaction(
            data
        )
        mental_health_vars = {
            "elevated": (1, 4, 1, lambda x: x**2.8675),
            "anxiety_depression_interaction": (
                1,
                4,
                -1,
                lambda x: x,
            ),
        }
        subjective_well_being_vars = {
            "elevated": (1, 4, 1, lambda x: x**2),
            "energy": (1, 4, 1, lambda x: x**2),
            "fun": (1, 4, 0.5, lambda x: x**2),
            "dog_interaction": (1, 2, 4, lambda x: x),
            "anxiety": (1, 4, -1, lambda x: (x**2)),
            "conflict": (1, 2, -4, lambda x: x),
            "depressed": (1, 4, -1, lambda x: (x**2)),
        }
        life_satisfaction_vars = {
            "self-acceptance": (1, 4, 1, lambda x: x),
            "liberty": (1, 4, 2, lambda x: x),
            "value_alignment": (1, 4, 2, lambda x: x),
            "health": (1, 4, 2, lambda x: x),
            "security": (1, 4, 2, lambda x: x),
            "relationship_satisfaction": (1, 4, 2, lambda x: x),
            "integrity": (1, 4, 2, lambda x: x),
            "optimism": (1, 4, 1, lambda x: x),
            "shame": (1, 4, -1, lambda x: (x**2)),
            "suicidality": (1, 2, -4, lambda x: x),
        }
        work_satisfaction_vars = {
            "energy": (1, 4, 1, lambda x: x),
            "mental_clarity": (1, 4, 4 / 2, lambda x: (x**2)),
            "value_alignment": (1, 4, 2 / 2, lambda x: (x**2)),
            "security": (1, 4, 1, lambda x: x),
            "relationship_satisfaction": (1, 4, 1, lambda x: x),
            "liberty": (1, 4, 2, lambda x: x),
            "achievement": (1, 4, 4, lambda x: x),
            "learning": (1, 4, 0.5, lambda x: x),
            "work_depth": (1, 4, 1 / 2, lambda x: (x**2)),
            "professional_mastery": (1, 4, 2, lambda x: x),
        }

        data["mental_health"] = self.calculate_score(
            data, mental_health_vars, rescale=False
        )
        data["subjective_well_being"] = self.calculate_score(
            data, subjective_well_being_vars
        )
        data["life_satisfaction"] = self.calculate_score(data, life_satisfaction_vars)
        data["work_satisfaction"] = self.calculate_score(data, work_satisfaction_vars)

        data["mental_health_partial"] = self.calculate_score(
            data, mental_health_vars, 0.5, rescale=False
        )
        data["subjective_well_being_partial"] = self.calculate_score(
            data, subjective_well_being_vars, 0.5
        )
        data["life_satisfaction_partial"] = self.calculate_score(
            data, life_satisfaction_vars, 0.5
        )
        data["work_satisfaction_partial"] = self.calculate_score(
            data, work_satisfaction_vars, 0.5
        )

        return data

    def reshape_to_tidy_format(self, data):
        """
        Reshape the data into a tidy format with columns for date, metric, value, and value_partial.

        Parameters:
        - data (DataFrame): The DataFrame containing the mental health data.

        Returns:
        - DataFrame: The reshaped DataFrame in tidy format.
        """
        # Selecting relevant columns for reshaping
        value_cols = [
            "mental_health",
            "subjective_well_being",
            "life_satisfaction",
            "work_satisfaction",
        ]
        partial_value_cols = [col + "_partial" for col in value_cols]

        # Reshape the full value columns
        tidy_full = pd.melt(
            data,
            id_vars=["date", "note"],
            value_vars=value_cols,
            var_name="metric",
            value_name="value",
        )

        # Reshape the partial value columns
        tidy_partial = pd.melt(
            data,
            id_vars=["date", "note"],
            value_vars=partial_value_cols,
            var_name="metric",
            value_name="value_partial",
        )

        # Extracting metric names for partial values
        tidy_partial["metric"] = tidy_partial["metric"].str.replace("_partial", "")

        # Merging the full and partial values
        tidy_data = pd.merge(
            tidy_full, tidy_partial, on=["date", "metric", "note"], how="left"
        )

        return tidy_data

    def wrangle_mental_health_data(self):
        data = self.merge_data()
        data = self.clean_merged_data(data)
        data = self.add_scores_to_exceptional_days(data)
        data = self.calculate_mental_health_metrics(data)
        data = self.reshape_to_tidy_format(data)
        return data
    
    def wrangle_non_exercise_goal_data(self):
        data = self.merge_data()
        data = self.clean_merged_data(data)
        goal_columns = [
            'date', 
            'asked_how_friends_are',
            'energizing_personal_interactions',
            'energizing_professional_interactions', 
            'floss'
            ]
        data = data[goal_columns]
        for col in data.columns[1:]:
            data[col] = data[col] - 1.0
        return data
