# -*- coding: utf-8 -*-
"""
Created on Wed Jan 22 21:02:39 2020

@author: kevin
"""

import requests_html
import pandas as pd



def redfin(url=None):
    if url == None: url = 'https://www.redfin.com/stingray/api/gis-csv?al=1&market=dc&max_price=500000&min_stories=1&num_homes=350&ord=redfin-recommended-asc&page_number=1&region_id=20065&region_type=6&sf=1,2,3,5,6,7&status=9&uipt=1,2,3,4,5,6&v=8'
    #url = 'https://www.redfin.com/city/20065/MD/Upper-Marlboro/filter/max-price=500k'
    with requests_html.HTMLSession() as session:
        r = session.get(url)
        #r.html.render()
        df = pd.read_csv(url)
        df.columns = df.columns.str.strip().str.lower().str.replace(' ', '_').str.replace('(', '').str.replace(')', '')

        #data = pd.read_html(r.html.html)
    return df

def redfin2():
    url = 'https://www.redfin.com/zipcode/20772/filter/property-type=house,max-price=500k,min-sqft=2.5k-sqft,has-garage,basement-type=finished+unfinished,min-stories=2,viewport=39.00739:38.7725:-76.64966:-76.95007,no-outline'
    url = '/stingray/api/gis-csv?al=1&basement_types=0,1,3&gar=true&max_price=500000&min_listing_approx_size=2500&min_stories=2&num_homes=350&ord=redfin-recommended-asc&page_number=1&poly=-76.95007%2038.7725%2C-76.64966%2038.7725%2C-76.64966%2039.00739%2C-76.95007%2039.00739%2C-76.95007%2038.7725&sf=1,2,3,5,6,7&status=9&uipt=1&v=8'
    with requests_html.HTMLSession() as session:
        r = session.get(url)
        print(r.html.links)
        #r.html.render()
        #data = pd.read_csv(url)
        #data = pd.read_html(r.html.html)

def auction_hw():
    #url ='https://www.hwestauctions.com/assets/php4/tabbedWebsite.php'
    url = 'https://www.hwestauctions.com/schedule.v4.php'
    session = requests_html.HTMLSession()
    r = session.get(url)
    #r.html.render()
    data = pd.read_html(r.html.html)
    r.close()
    df = data[2]
    df.columns = df.columns.str.strip().str.lower().str.replace(' ', '_').str.replace('(', '').str.replace(')', '')
    #pd.DataFrame()
    
    #for i in range(5):
    #    df[data[2][i][0]] = data[0][i][1:]
    #df[['Address','Post_Date']] = df['Address, City (First Pub)'].str.split('(',expand=True)
    #df['Post_Date'] = df['Post_Date'].str.replace(")", "")

    return df

from math import radians, sin, cos, acos
def distance(lat1, lon1, lat2 = 38.870393, lon2=-76.878019):
    lat1 = radians(lat1)
    lon1 = radians(lon1)
    lat2 = radians(lat2)
    lon2 = radians(lon2)
    dist = 6371.01 * acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(lon1 - lon2))
    #convert from km to m
    dist = dist * 0.621371
    return dist

redfin = redfin()
hw = auction_hw()
homes = pd.concat([redfin,hw], sort =False)
    