import pandas as pd
import csv

# Read weather data
wdf = pd.read_csv("D:/SMUMSDS/TimeSeries/Final Project/weather.csv")

# Calculate daily average temperature and humidity
weather = wdf.groupby(['month', 'day', 'hour']).agg(avg_temp=('temp', 'mean'), avg_humidity=('humidity', 'mean')).reset_index()
# print(weather.head(3))

# Read flight data
airdf = pd.read_csv("D:/SMUMSDS/TimeSeries/Final Project/ModAirDelay2018V2.csv")
# print(airdf.head(3))


# Remove rows with missing values in DEP_TIME, DEP_DELAY column
airdf = airdf.dropna(subset=['DEP_TIME'])
airdf = airdf.dropna(subset=['DEP_DELAY'])
# print(airdf.head(30))

# Convert DEP_TIME to hour
airdf['DEP_TIME'] = airdf['DEP_TIME'].apply(lambda x: int(x/100))

# print(airdf.head(20))


# Bin DEP_TIME into intervals of one hour
airdf['DEP_TIME_BIN'] = pd.cut(airdf['DEP_TIME'], range(0, 25))

# Print the first 5 rows to check the result
# print(airdf.head(10))

# Extract month, day, from the FL_DATE column
airdf2 = airdf['FL_DATE'].str.split('-', expand=True)
airdf2.columns = ['year', 'month', 'day'] 
airdf2 = pd.concat([airdf2, airdf.iloc[:,1:]], axis=1)
# print(airdf2.head(5))

# Convert year, month, and day to integers
airdf2['month'] = airdf2['month'].astype(int)
airdf2['day'] = airdf2['day'].astype(int)
# print(airdf2.head(5))

# Group by month, day, and hour, keep original columns
airdf2 = airdf2.groupby(['year', 'month', 'day', 'DEP_TIME']).agg(avg_dep_delay=('DEP_DELAY', 'mean')).reset_index()
airdf2['avg_dep_delay'] = round(airdf2['avg_dep_delay'],2)
# print(airdf2.head(10))

print(weather.head(3))
print(airdf2.shape)

# Merge weather and flight data
df = pd.merge(airdf2, weather, how='left', left_on=['month', 'day', 'DEP_TIME'], right_on=['month', 'day', 'hour'])
df.drop(columns=['hour'], inplace=True)
# print(df.head(5))
# print(df.shape)

# Remove rows with missing values in avg_temp, avg_humidity column
df = df.dropna(subset=['avg_temp'])
df = df.dropna(subset=['avg_humidity'])

# Count the number of missing values in each column
missing_values_count = df.isnull().sum()

# Print the result, no missing values
# print(missing_values_count)

# Save the modified DataFrame back to a CSV file
df.to_csv("D:/SMUMSDS/TimeSeries/Final Project/ModAirDelay2018V3.csv", index=False, quoting=csv.QUOTE_NONNUMERIC)

# Display the first 5 rows
# print(df.head())
