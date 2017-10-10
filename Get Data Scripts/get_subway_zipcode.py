import re
import pandas as pd
from google_places_wrapper import google_places_keyword

## Get Google Places API Key https://developers.google.com/places/web-service/get-api-key
api_key = ''

## Subway location data http://web.mta.info/developers/data/nyct/subway/Stations.csv
subway_data = pd.read_csv('Stations.csv')
subway_data['formatted_address'] = None
subway_data['Zip Code'] = None

for index, row in subway_data[subway_data['formatted_address'].isnull()].iterrows():
    lat, lon = subway_data.iloc[index]['GTFS Latitude'], subway_data.iloc[index]['GTFS Longitude']
    output = google_places_keyword('restaurant', lat, lon, api_key)
    formatted_address = output[list(output.keys())[0]]['formatted_address']
    subway_data = subway_data.set_value(index=index, col='formatted_address', value=formatted_address)
    
subway_data = subway_data.set_value(index=105, col='formatted_address', value='97 Nassau St, New York, NY 10038, United States') ##fix one row
subway_data['Zip Code'] = subway_data['formatted_address'].map(lambda x: re.findall('(?<=NY )(.*?)(?=, U)', x)[0]) ## Find zipcode via regex
subway_data = subway_data[['Station ID','Complex ID','GTFS Stop ID','Division','Line','Stop Name','Borough','Daytime Routes','Structure','GTFS Latitude','GTFS Longitude', 'Zip Code']]
subway_data.to_csv('subway_data.csv', index=False)