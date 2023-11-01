import csv
import pandas as pd

# Load the CSV file into a pandas DataFrame
df = pd.read_csv("AirDelay2018.csv")

# Remove the "Column to remove" column
df.drop(columns=["WHEELS_ON", "TAXI_IN", "CANCELLED", "CANCELLATION_CODE",
                 "DIVERTED", "CRS_ELAPSED_TIME", "ACTUAL_ELAPSED_TIME",
                 "AIR_TIME", "CARRIER_DELAY", "WEATHER_DELAY",
                 "NAS_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY",
                 "Unnamed: 27", "TAXI_OUT", "WHEELS_OFF","CRS_ARR_TIME",
                 "ARR_TIME","ARR_DELAY", "DISTANCE", "CRS_DEP_TIME"], inplace=True)

df = df[df["ORIGIN"] == "DAL"]

# Save the modified DataFrame back to a CSV file
df.to_csv("ModAirDelay2018V2.csv", index=False, quoting=csv.QUOTE_NONNUMERIC)
