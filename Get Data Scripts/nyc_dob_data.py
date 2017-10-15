import pandas as pd
import re
import json
import os.path
from google_places_wrapper import google_places_call

## Get Google Places API Key https://developers.google.com/places/web-service/get-api-key
api_key = ''

## Data resides at 'https://data.cityofnewyork.us/Housing-Development/DOB-Job-Application-Filings/ic3t-wcy2'

df = pd.read_csv('DOB_Job_Application_Filings.csv')
df = df[['Job #', 'Doc #', 'Borough', 'House #', 'Street Name', 'Block', 'Lot',
       'Bin #', 'Job Type', 'Job Status', 'Job Status Descrp',
       'Latest Action Date', 'Building Type', 'Community - Board',
       'Landmarked', 'City Owned', 'Plumbing', 'Mechanical', 'Boiler',
       'Fuel Burning', 'Fuel Storage', 'Standpipe', 'Sprinkler', 'Fire Alarm',
       'Equipment', 'Fire Suppression', 'Curb Cut', 'Other',
       'Other Description', 'Pre- Filing Date', 'Paid', 'Fully Paid',
       'Assigned', 'Approved', 'Fully Permitted', 'Initial Cost',
       'Total Est. Fee', 'Fee Status', 'Existing Zoning Sqft',
       'Proposed Zoning Sqft', 'Horizontal Enlrgmt', 'Vertical Enlrgmt',
       'Enlargement SQ Footage', 'Street Frontage', 'ExistingNo. of Stories',
       'Proposed No. of Stories', 'Existing Height', 'Proposed Height',
       'Existing Dwelling Units', 'Proposed Dwelling Units',
       'Existing Occupancy', 'Proposed Occupancy', 'Site Fill', 'Zoning Dist1',
       'Zoning Dist2', 'Zoning Dist3', 'Special District 1',
       'Special District 2', 'Non-Profit', 'Job Description']]

df['Existing Occupancy'] = df['Existing Occupancy'].str.strip()
df['Proposed Occupancy'] = df['Proposed Occupancy'].str.strip()
df['House #'] = df['House #'].map(lambda x: re.sub('\D', '',x))

if os.path.exists('google_output.json'):
	with open("google_output.json", 'r') as f:
	    google_data = json.load(f)
else:
	google_data = {}

## Combine the columns to get full address
df['Full Address'] = df[['House #',  'Street Name', 'Borough']].apply(lambda x : '{} {} {}'.format(x[0].strip(),x[1].strip(),x[2].strip()), axis=1)
df['formatted_address'] = None
df['name'] = None
df['loc_type'] = None
df['lat'] = None
df['lon'] = None

for index, row in df[df['formatted_address'].isnull()].iterrows():
    address = df.iloc[index]['Full Address']
    if address not in google_data.keys():
        try:
            formatted_address, name, loc_type, lat, lon = google_places_call(address)
            df = df.set_value(index=index, col='formatted_address', value=formatted_address)
            df = df.set_value(index=index, col='name', value=name)
            df = df.set_value(index=index, col='loc_type', value=loc_type)
            df = df.set_value(index=index, col='lat', value=lat)
            df = df.set_value(index=index, col='lon', value=lon)
            google_data[address] = {'formatted_address': formatted_address, 'name': name, 
                                      'loc_type': loc_type, 'lat': lat, 'lon': lon}
        except:
            df = df.set_value(index=index, col='formatted_address', value='Not Found')
            next
    else:
        df = df.set_value(index=index, col='formatted_address', value=google_data[address]['formatted_address'])
        df = df.set_value(index=index, col='name', value=google_data[address]['name'])
        df = df.set_value(index=index, col='loc_type', value=google_data[address]['loc_type'])
        df = df.set_value(index=index, col='lat', value=google_data[address]['lat'])
        df = df.set_value(index=index, col='lon', value=google_data[address]['lon'])


df['Zip Code'] = df['formatted_address'].map(lambda x: str(re.findall('(?<=NY )(.*?)(?=, U)', x)[0]) if len(re.findall('(?<=NY )(.*?)(?=, U)', x)) > 0 else None)

## Some manual fixes of Zip Code data
df.loc[df['Full Address']=='4 NEW YORK PLAZA MANHATTAN', 'Zip Code'] = '10004'
df.loc[df['Full Address']=='4 NY PLAZA MANHATTAN', 'Zip Code'] = '10004'
df.loc[df['Full Address']=='2727 UNIVERSITY AVENUE BRONX', 'Zip Code'] = '10468'
df.loc[df['Full Address']=='2535 86 STREET BROOKLYN', 'Zip Code'] = '11224'

df[df['formatted_address']!='Not Found'].to_csv('NYC_DOB_data_processed.csv',index=False) ##output the processed data with zip code

with open("google_output.json", 'w') as f: ##output miscellaneous data as JSON
    json.dump(google_data, f, indent=2)

## Start code for the yearly aggregate data
df['Permit Year'] = pd.to_datetime(df['Pre- Filing Date']).dt.year
df_Job = df[df['Job Type'].isin(['A1', 'A2', 'DM', 'NB'])].groupby(['Zip Code', 'Permit Year', 'Job Type']).size().reset_index()
df_income = pd.read_csv('Income_Home_Prices_ZIP.csv') ## Get the income home prices by zip data

df_Job=df_Job[df_Job['Zip Code']!='nan']
df_Job['Zip Code']=df_Job['Zip Code'].map(lambda x: int(x))

## Join the two datasets
df_Job = pd.merge(df_Job, df_income[['LandSqMile', 'ZIP Code', 'Median Home Value ($/sq. foot)', 'Year']], how='inner', left_on=['Zip Code', 'Permit Year'], right_on=['ZIP Code', 'Year'])
df.columns = ['Zip Code','Permit Year','Job Type','Count','LandSqMile', 'ZIP Code','Median Home Value','Year', 'Count per SqMile']
df_Job.rename({'Median Home Value ($/sq. foot)': 'Median Home Value', 0: 'Count'}, inplace=True)
df_Job['Count per SqMile']=df_Job['Count']/df_Job['LandSqMile']
df_Job[df_Job['Median Home Value']].replace('[\$,]', '', regex=True).astype(float) ## Remove dollar symbol

df_Job.to_csv('NYC_DOB_data_yearly.csv', index=False)