import requests
from time import sleep

class StopAssignments(Exception): pass

def get_google_results(r):
    output = {}
    if r.status_code == 200:
        if r.json()['status'] == 'OK':
            for x in range(len(r.json()['results'])):
                results = r.json()['results'][x]
                output[results['place_id']] = {
                    'formatted_address': results['formatted_address'],
                    'name': results['name'],
                    'loc_type': results['types'],
                    'lat': results['geometry']['location']['lat'],
                    'lon': results['geometry']['location']['lng']                                      
                }
            return output

        elif r.json()['status'] == 'ZERO_RESULTS':
            return output
            print (r.json()['status'])
            raise StopAssignments
        elif r.json()['status'] == 'OVER_QUERY_LIMIT':
            return output
            print (r.json()['status'])
            raise StopAssignments
        else:
            print (r.json()['status'])
            raise StopAssignments
    else:
        raise StopAssignments

## Helper function to go to next page of google places search results
def google_places_next_page(next_page_token, api_key):
    web_call_string = 'https://maps.googleapis.com/maps/api/place/textsearch/json?pagetoken=%s&key=%s' %(next_page_token, api_key)
    sleep(3) # Time in seconds. There is a time lag between when the next page token is issued and when it is ready
    r = requests.get(web_call_string)
    return r

## Helper function to get data from Google Places
def google_places_call(zip_code, city, keyword, api_key): ## city should be passed as "New York, NY" for NYC
    
    text_string = '%s+near+%s+%s' %(re.sub(' ', '+', keyword), str(zip_code), re.sub(' ', '+', city))
    web_call_string = 'https://maps.googleapis.com/maps/api/place/textsearch/json?query=%s&key=%s' %(text_string,api_key)
    r = requests.get(web_call_string)
    output = get_google_results(r)

    while 'next_page_token' in r.json().keys():
        r = google_places_next_page(r.json()['next_page_token'], api_key)
        new_output = get_google_results(r)
        output = dict(list(output.items()) + list(new_output.items()))
    
    return output

def google_places_keyword(keyword, lat, lon, api_key):
    text_string = '%ss' %(re.sub(' ', '+', keyword))
    web_call_string = 'https://maps.googleapis.com/maps/api/place/textsearch/json?query=%s&location=%s,%s&radius=50&key=%s' %(text_string,lat,lon,api_key)
    r = requests.get(web_call_string)
    output = get_google_results(r)
    return output

def google_places_nearby_call(name_or_address, lat, lon):
    text_string = re.sub(' ' , '+', name_or_address)
    web_call_string = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=%s,%s&radius=50&keyword=%s&key=%s' \
    %(lat, lon, text_string, api_key)
    return requests.get(web_call_string)

def google_places_nearby_call_lat_lon_for_zip(lat, lon, api_key):
    web_call_string = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=%s,%s&radius=10000&type=postal_code&key=%s' \
    %(lat, lon, api_key)
    return requests.get(web_call_string)

def google_places_nearby_rev_geocode(lat, lon, api_key):
    web_call_string = 'https://maps.googleapis.com/maps/api/geocode/json?latlng=%s,%s&key=%s' %(lat, lon, api_key)
    return requests.get(web_call_string).json()

## Usage - pass zip code, city and key word along with api key
## test = google_places_call(11104, 'New York, NY', 'coffee shops', api_key)
## The results will be a JSON with the key being the Google Places unique place id