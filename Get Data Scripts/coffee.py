# NYC restaurants inspection dataset
# From NY Open data site

%matplotlib inline
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# Dataframe # 1: base dataframe
###############################
# 1) Inspection dates parsed and set as index
# 2) Data with '1900-01-01' inspection dates removed

df = pd.read_csv('DOHMH_NYCrestaurants.csv', parse_dates = ['INSPECTION DATE'])

df_dated = df.copy()
df_dated.set_index('INSPECTION DATE', inplace=True)
df_dated = df_dated[df_dated.index != '1900-01-01']

print("Length before removing 1900-01-01:" ,len(df),
"\nLength after removing 1900-01-01:", len(df_dated))


# Dataframes # 2 and #3
#######################
# De-duplication of dates (primarily to prevent issues due to duplicate grades/scores when answering certain questions)
# Note: inspection details for each score are lost thus, for details, use df_dated
# Source: http://pandas.pydata.org/pandas-docs/stable/merging.html

frames2 = []  # Separate frames for DF2 will be added here
frames3 = []  # Separate frames for DF3 will be added here

for identifier in df_dated['CAMIS'].unique():

    # To create DF2
    curr_df = df_dated[df_dated['CAMIS'] == identifier]  # New dataframe for each identifier
    curr_df = curr_df[~curr_df.index.duplicated(keep='first')]  # Magic method for removing duplicate index dates
    curr_df = curr_df[::-1]  # Date sorted. Note: in initial dataset, dates are sorted in descending order
    frames2.append(curr_df)  # Save dataframe to list

    # To create DF3
    curr_df2 = curr_df[curr_df.index == curr_df.index.min()] # Keep ID with only the oldest date
                                                             # Check if initial inspection date
    frames3.append(curr_df2)  # Save dataframe to list

# Combine dataframes to create DF2, DF3
df_dedupe = pd.concat(frames2)
df_dedupeID = pd.concat(frames3)

print("Length before removing inspection dates duplicates (DF1):", len(df_dated))
print("Length after removing inspection dates duplicates (DF2):", len(df_dedupe))
print("Length when only keeping the OLDEST inspection results per restaurant (DF3):", len(df_dedupeID))


# Create a list of all inspection types
from itertools import chain
types_raw = set(chain.from_iterable(df_dedupeID['INSPECTION TYPE']\
                                    .map(str)\
                                    .map(lambda x: x.split("/")).tolist()))
types = [item.strip(" ") for item in types_raw if item != '']
print(types)

### Notes from groupby count by inspection types:
# 1. All groups > 100 are wither initial inspection or re-inspection
# 2. **Pre-permit Initial Inspection** - proxy for restaurant opening date

# Create column 'year' to group counts by year for the analysis
df_dedupeID['year'] = df_dedupeID.index.year

######################
# COFFEE SHOPS - ALL #
######################

# Purpose: Coffee shops count per zipcode per year

coffee_by_zip = df_dedupeID[df_dedupeID['CUISINE DESCRIPTION'] == "CafÃ©/Coffee/Tea"]\
                            .groupby(['ZIPCODE', 'DBA']).year.value_counts().unstack(fill_value=0)
coffee_by_zip.reset_index(inplace=True)
coffee_by_zip2 = coffee_by_zip.groupby('ZIPCODE', as_index=False)\
                [coffee_by_zip.filter(regex='201.*').columns].sum()
# Cumulative coffees count for each year since only first inspection year in data
coffee_by_zip2[2014] = coffee_by_zip2[2014] + coffee_by_zip2[2013] # 2014 now includes those inspected in 2013 as well
coffee_by_zip2[2015] = coffee_by_zip2[2015] + coffee_by_zip2[2014]
coffee_by_zip2[2016] = coffee_by_zip2[2016] + coffee_by_zip2[2015]
coffee_by_zip2[2017] = coffee_by_zip2[2017] + coffee_by_zip2[2016]

coffee_by_zip2['ZIPCODE'] = coffee_by_zip2['ZIPCODE'].astype('int')

# Save dataframe as CSV file
coffee_by_zip2.to_csv("count_coffee.csv")


######################
# COFFEE SHOPS - NEW #
######################

# Purpose: New coffee shops count per zipcode per year

#Coffee shops - only 'Pre-permit (Operational) / Initial Inspection'
newcoffee_by_zip = df_dedupeID[(df_dedupeID['CUISINE DESCRIPTION'] == "CafÃ©/Coffee/Tea") &
                            (df_dedupeID['INSPECTION TYPE'] == "Pre-permit (Operational) / Initial Inspection")]\
                            .groupby(['ZIPCODE', 'DBA']).year.value_counts().unstack(fill_value=0)
newcoffee_by_zip.reset_index(inplace=True)
newcoffee_by_zip2 = newcoffee_by_zip.groupby('ZIPCODE', as_index=False)\
                    [newcoffee_by_zip.filter(regex='201.*').columns].sum()

newcoffee_by_zip2['ZIPCODE'] = newcoffee_by_zip2['ZIPCODE'].astype('int')

# Save dataframe as CSV file
newcoffee_by_zip2.to_csv('count_coffee_new.csv')
