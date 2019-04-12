## saves timestamp every XX minutes


import re
import datetime
import os
import csv
import sys
import time

def main():
    
    while True:
        tmp = [str(datetime.datetime.now())]

        print(tmp)
        
        filePath = r"D:\Dropbox\AcademiaDropbox\ComputerTimestamps.csv"
        exists = os.path.isfile(filePath)
        
        if sys.version[0:3] == '3.7':
            openType =  'a+'
        elif sys.version[0:3] == '3.5':
            openType = 'a+'
        else:
            openType = 'a+b'

        with open(filePath, openType) as myfile:
            wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
            if not exists:
                wr.writerows([["Datetime"]])
            wr.writerows([tmp])

        time.sleep(60*15) # sleep 15 minutes

if __name__== "__main__":
    main()
