import json
import pandas as pd
from google_places_wrapper import google_places_nearby_rev_geocode

## Get Google Places API Key https://developers.google.com/places/web-service/get-api-key
api_key = ''

## File that contains the list of lat/lon and zip code
if os.path.exists('nyc_crime_violent_dict.json'):
    with open("nyc_crime_violent_dict.json", 'r') as f:
        nyc_crime_violent_dict = json.load(f)
else:
    nyc_crime_violent_dict = {}


df = pd.read_csv('nyc_crime_violent_trim.csv') ## This is a trimmed data processed in Big Query for violent crimes
df_trim = df[['LAT_TRIM', 'LON_TRIM']].drop_duplicates() ## Get unique lat/lon
df_trim['Zip Code'] = None

for index, row in df_trim.iterrows(): ## Begin loop
    lat, lon = df_trim.loc[index][0:2] ## Get lat/lon
    r = google_places_nearby_rev_geocode(lat,lon,api_key)['results'] ## use reverse geocode to get nearest address
    for x in r: ## response is a list of locations
        if 'formatted_address' in x.keys(): ## each item in the list is a JSON of address components
            try:
                zip_code = re.findall('(?<=NY )(.*?)(?=, U)', x['formatted_address'])[0]  ## process zip code from formatted address
                df_trim = df_trim.set_value(index=index, col='Zip Code', value=zip_code)
                nyc_crime_violent_dict[(lat,lon)] = zip_code
                break
            except:
                next
        else:
            next

df_merge = pd.merge(df, df_trim, how='inner', left_on=['LAT_TRIM', 'LON_TRIM'], right_on=['LAT_TRIM', 'LON_TRIM']) ## Join the data
df_merge.to_csv('nyc_crime_violent_trim_with_zip.csv', index=False)

####### Do the same process for the non violent crime
df_non = pd.read_csv('nyc_crime_nonviolent_trim.csv') ## This is a trimmed data processed in Big Query for non-violent crimes
df_non_trim = df_non[['LAT_TRIM', 'LON_TRIM']].drop_duplicates()
df_non_trim['Zip Code'] = None

for index, row in df_non_trim.iterrows():
    lat, lon = df_non_trim.loc[index][0:2]
    if (lat, lon) not in nyc_crime_violent_dict.keys():
        r = google_places_nearby_rev_geocode(lat,lon,api_key)['results']
        for x in r:
            if 'formatted_address' in x.keys():
                try:
                    zip_code = re.findall('(?<=NY )(.*?)(?=, U)', x['formatted_address'])[0]
                    df_non_trim = df_non_trim.set_value(index=df_non_trim.iloc[iloc].name, col='Zip Code', value=zip_code)
                    nyc_crime_violent_dict[(lat,lon)] = zip_code
                    break
                except:
                    next
            else:
                print('Not Found')
    else:
        df_non_trim = df_non_trim.set_value(index=index, col='Zip Code', value=nyc_crime_violent_dict[(lat,lon)])

df_non_merge = pd.merge(df_non, df_non_trim, how='inner', left_on=['LAT_TRIM', 'LON_TRIM'], right_on=['LAT_TRIM', 'LON_TRIM'])
df_non_merge.to_csv('nyc_crime_nonviolent_trim_with_zip.csv', index=False)

nyc_crime_violent_dict_str = {str(key): values for key, values in nyc_crime_violent_dict.items()} ## store data for future use
with open('nyc_crime_violent_dict.json', 'w') as f:
    json.dump(nyc_crime_violent_dict_str, f, indent= 2)