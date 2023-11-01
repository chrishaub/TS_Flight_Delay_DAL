import pandas as pd
import csv

# Read weather data
wdf = pd.read_csv("D:/SMUMSDS/TimeSeries/Final Project/weather.csv")

# Calculate daily average temperature and humidity
weather = wdf.groupby(['month', 'day', 'hour']).agg(avg_temp=('temp', 'mean'), avg_humidity=('humidity', 'mean')).reset_index()

# Read flight data
airdf = pd.read_csv("D:/SMUMSDS/TimeSeries/Final Project/ModAirDelay2018V2.csv")

# Extract year, month, and day from the FL_DATE column
airdf2 = airdf['FL_DATE'].str.split('-', expand=True)
airdf2.columns = ['year', 'month', 'day']
airdf2 = pd.concat([airdf2, airdf.iloc[:,1:]], axis=1)

# Convert year, month, and day to integers
airdf2['year'] = airdf2['year'].astype(int)
airdf2['month'] = airdf2['month'].astype(int)
airdf2['day'] = airdf2['day'].astype(int)

# Merge weather and flight data
df = pd.merge(airdf2, weather, how='left', on=['month', 'day'])

# Save the modified DataFrame back to a CSV file
df.to_csv("D:/SMUMSDS/TimeSeries/Final Project/ModAirDelay2018V3.csv", index=False, quoting=csv.QUOTE_NONNUMERIC)

# Display the first 5 rows of the modified DataFrame
print(df.head())

# Display the data types of the modified DataFrame
print(df.dtypes)


# # Display the data types of the modified DataFrame
# print(df.dtypes)

# Display the column names of the modified DataFrame
print(df.columns)

# Display the number of missing values in each column of the modified DataFrame
print(df.isnull().sum())

