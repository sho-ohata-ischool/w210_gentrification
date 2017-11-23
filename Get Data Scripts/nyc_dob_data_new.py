import pandas as pd
import numpy as np
import os.path

pd.options.display.max_rows = 100

job_type = ['A1', 'A2', 'DM', 'NB']

def date_lookup(s):
    """
    This is an extremely fast approach to datetime parsing.
    For large data, the same dates are often repeated. Rather than
    re-parse these, we store all unique dates, parse them, and
    use a lookup to convert all dates.
    """
    dates = {date:pd.to_datetime(date) for date in s.unique()}
    return s.map(dates)

##Load main dataset file
df_income = pd.read_csv('Income_Home_Prices_ZIP_v2.csv')
zip_list = df_income['ZIP'].unique().tolist() ##full zip code list
column_names = df_income.columns.tolist() ##column names
column_names = [name for name in column_names if name not in job_type]
df_income = df_income[column_names]

## Load DOB dataset
if os.path.exists('DOB_Permit_cleaned.csv'):
    df = pd.read_csv('DOB_Permit_cleaned.csv')
else: ## For new dob data: https://data.cityofnewyork.us/Housing-Development/DOB-Permit-Issuance/ipu4-2q9a
    df = pd.read_csv('DOB_Permit_Issuance.csv')
    df = df[["BOROUGH", "Bin #", "House #", "Street Name", "Job #", "Job doc. #",
       "Job Type", "Self_Cert", "Block", "Lot", "Community Board", "Zip Code",
       "Bldg Type", "Residential", "Special District 1", "Special District 2",
       "Work Type", "Permit Status", "Filing Status", "Permit Type",
       "Permit Sequence #", "Permit Subtype", "Oil Gas", "Site Fill",
       "Filing Date", "Issuance Date", "Expiration Date", "Job Start Date",
       "PERMIT_SI_NO", "LATITUDE", "LONGITUDE",
       "COUNCIL_DISTRICT", "CENSUS_TRACT", "NTA_NAME"]]
    df.to_csv('DOB_Permit_cleaned.csv', index=False)
    
## filter for interested job type and doc version 1 as many of them are re-filing/add-on to initial job
df = df[(df['Job Type'].isin(job_type)) & (df['Job doc. #']==1) & (df['Zip Code'].isin(zip_list))] 
df['Filing Date'] = date_lookup(df['Filing Date']) ##convert str to datetime
df['Filing Year'] = df['Filing Date'].dt.year ##get year

## Multiple filing per Job # so take minimum year based on job #
df_Job = df[['Job #', 'Job Type', 'Zip Code', 'Filing Year']].groupby(['Job #', 'Job Type', 'Zip Code']).min().reset_index()
## Then do a count based on Zip Code year and job type
df_Job = df_Job.groupby(['Zip Code', 'Filing Year', 'Job Type']).size().reset_index()
df_Job['Zip Code'] = df_Job['Zip Code'].map(lambda x: int(x))
df_Job['Filing Year'] = df_Job['Filing Year'].map(lambda x: int(x))
df_Job=df_Job.rename(columns = {0:'Count'})

## Fill in zero value for Zip/Year/Job combination where there are none
df_Job_col_names = ['Zip Code', 'Filing Year', 'Job Type']
all_years = df_Job['Filing Year'].unique().tolist()
full = pd.MultiIndex.from_product([zip_list, all_years, job_type], names=df_Job_col_names)
df_Job = df_Job.set_index(df_Job_col_names).reindex(full, fill_value=0).reset_index()

df_Job.to_csv('nyc_permit_yearly_count.csv', index=False)

##Pivot melted data to append to main dataset file
permits = df_Job.pivot_table(index=['Zip Code', 'Filing Year'], columns='Job Type')
permits.columns = permits.columns.droplevel().rename(None)
permits = permits.reset_index()
column_names.extend(['A1','A2','DM','NB'])

df2 = pd.merge(df_income, permits, how='left', left_on=['ZIP', 'Year'], right_on=['Zip Code', 'Filing Year'])
df2 = df2[column_names]

assert(df_income.shape[0]==df2.shape[0])
df2.to_csv('Income_Home_Prices_ZIP_v2.csv', index=False)