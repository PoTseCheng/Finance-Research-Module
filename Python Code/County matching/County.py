import pandas as pd
from geopy.geocoders import Nominatim
import numpy as np
import os
import time

def county(name):
    '''
    This county function will add an additional column called "County" to the original data frame
    
    Input:
        name (str): User defined token to access OPEN STREET MAP, any random string is fine. Please
        change it every time you run the code, as a token may become invalid.

    output:targetname.csv
          
    '''
    #Start the timing
    start = time.time()

    #Define the user agent to access geo-locating service
    geolocator = Nominatim(user_agent=name)

    #define directory
    path_parent = os.path.dirname(os.getcwd())
    path_parent2 = os.path.dirname(path_parent)
    path = os.path.join(path_parent2, "Storage\\")
    

    #Read in the targeted csv file
    df = pd.read_csv(path+ r"GPS Locations.csv", encoding = "ISO-8859-1")

    #We need to split the df into non-air and air in order to avoid time out
    df_nonair = df.loc[df["Airport"] == False]
    tuple_prepare = df_nonair[["Lat", "Lon"]]
    tuple_prepare = tuple_prepare.fillna(0.)
    tuples = [tuple(x) for x in tuple_prepare.to_numpy()]

    #construct container
    output = []

    #Get the county for non-air

    for i in range(len(tuples)):
        output.append(geolocator.reverse(tuples[i]).raw.get("address").get("county"))

    #Append the result to original non-air df
    nonairdf = df_nonair.copy()
    nonairdf["County"] = output

    #Focus also on the airports now
    df_air = df.loc[df["Airport"] == True]
    tuple_prepare2 = df_air[["Lat", "Lon"]]
    tuple_prepare2 = tuple_prepare2.fillna(0.)
    tuples2 = [tuple(x) for x in tuple_prepare2.to_numpy()]

    #Construct container
    output2 = []

    #Similar as before
    for i in range(len(tuples2)):
        output2.append(geolocator.reverse(tuples2[i]).raw.get("address").get("county"))
    airdf = df_air.copy()
    airdf["County"] = output2

    #merge the results
    result = pd.concat([nonairdf, airdf])

    #Save the File
    result.to_csv(path + r"Full Location Data.csv", encoding='utf-8', index=False)
    
    #finish the timing
    end = time.time()
    elapsed_time = str(end-start)
    print("This function takes approximately "+ elapsed_time+ " seconds to run.")
    return
    