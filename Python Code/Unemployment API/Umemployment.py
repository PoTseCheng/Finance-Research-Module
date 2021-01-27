import requests
import pandas as pd 
import ast
import time
import os

def get_umemployment():
    '''
    This function grabs the data from BLS for umemployment, will print the elapsed time.
    '''
    #time the function
    start = time.time()

    #Define directory
    path_parent = os.path.dirname(os.getcwd())
    path_parent2 = os.path.dirname(path_parent)
    path = os.path.join(path_parent2, "Storage\\")

    #Request data from 2010 to 2020, we need to break the time up because there is a limit on the server for requesting data
    numlist = []
    for i in range(1, 73):
        numlist.append(str(i).zfill(2))
        #more preparations
        weblist= []
        namelist = []
    for i in numlist:
        weblist.append("https://api.bls.gov/publicAPI/v2/timeseries/data/LASST"+str(i)+"0000000000003?startyear=2010&endyear=2020&registrationkey=414ce447d0cc4c3a91c94b9f4fc64004")
        namelist.append("LASST"+str(i)+"2020")
    for i, j in zip(weblist, namelist):
        response = requests.get(i)
        response.encoding = 'utf-8'
        test= response.json()
        test2= str(test["Results"]["series"])[1:-1]
        test3 = ast.literal_eval(test2)
        if test3["data"] != []:
            df = pd.DataFrame.from_dict(test3["data"])
            df = df.drop(columns=["footnotes", "periodName"])
            df = df.replace(['M12'],12)
            df = df.replace("M11", 11)
            df = df.replace(['M10'],10)
            df = df.replace("M09", 9)
            df = df.replace(['M08'],8)
            df = df.replace("M07", 7)
            df = df.replace(['M06'],6)
            df = df.replace("M05", 5)
            df = df.replace(['M04'],4)
            df = df.replace("M03", 3)
            df = df.replace(['M02'],2)
            df = df.replace("M01", 1)
            df.to_csv(str(j)+".csv", index=False)

    #For year 1990 to 2010
    #Similar Procedures
    numlist = []
    for i in range(1, 73):
        numlist.append(str(i).zfill(2))
        weblist= []
        namelist = []
    for i in numlist:
        weblist.append("https://api.bls.gov/publicAPI/v2/timeseries/data/LASST"+str(i)+"0000000000003?startyear=1990&endyear=2010&registrationkey=414ce447d0cc4c3a91c94b9f4fc64004")
        namelist.append("LASST"+str(i)+"2010")
    for i, j in zip(weblist, namelist):
        response = requests.get(i)
        response.encoding = 'utf-8'
        test= response.json()
        test2= str(test["Results"]["series"])[1:-1] #need to drop the bracket because some genius in the bls thought it was super smart to make a list within the dictionaries
        test3 = ast.literal_eval(test2)
        if test3["data"] != []:
            df = pd.DataFrame.from_dict(test3["data"])
            df = df.drop(columns=["footnotes", "periodName"])
            df = df.replace(['M12'],12)
            df = df.replace("M11", 11)
            df = df.replace(['M10'],10)
            df = df.replace("M09", 9)
            df = df.replace(['M08'],8)
            df = df.replace("M07", 7)
            df = df.replace(['M06'],6)
            df = df.replace("M05", 5)
            df = df.replace(['M04'],4)
            df = df.replace("M03", 3)
            df = df.replace(['M02'],2)
            df = df.replace("M01", 1)
            df.to_csv(str(j)+".csv", index=False)
    #Merging the files
    #initial first df
    df1 = pd.read_csv("LASST012020.csv")
    df2 = pd.read_csv("LASST012010.csv")
    df = pd.concat([df1, df2])
    df["StateID"] = "LASST010000000000003"
    df["StateCode"] = "AL"
    #Preparation for other dfs
    real = ["02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "72"]
    statecode2 = ["AK", "AZ", "AR", "CA", "CO","CT","DE","District of Columbia","FL","GA","HI","ID","IL","IN","IA", "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY", "Puerto Rico"]
    outputname2 = ["LASST"+ i + "0000000000003" for i in real]
    for i, j in zip(real, range(len(statecode2))):
        df1 = pd.read_csv("LASST"+i+"2020.csv")
        df2 = pd.read_csv("LASST"+i+"2010.csv")
        dftemp = pd.concat([df1, df2])
        dftemp["StateID"] = outputname2[j]
        dftemp["StateCode"] = statecode2[j]
        df = pd.concat([df, dftemp])
    df = df.drop(columns=["latest"])
    df.to_csv(path+r'State Characteristics.csv', index=False)
    
    real.append("01")
    #Remove temporary files
    for i in real:
        os.remove("LASST"+i+"2020.csv")
        os.remove("LASST"+i+"2010.csv")
    end=time.time()
    elapsed = end-start
    print("Total Time for running this function is "+str(elapsed)+ " seconds")
    return
