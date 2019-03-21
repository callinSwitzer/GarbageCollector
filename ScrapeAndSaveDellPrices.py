## gets sale price and regular price for dell xps

from bs4 import BeautifulSoup
import re
import requests
import datetime
import os
import csv
import sys

def main():
    
    non_decimal = re.compile(r'[^\d.]+')

    URL = "https://www.dell.com/en-us/shop/dell-laptops/new-xps-13-touch/spd/xps-13-9380-laptop/xnita3ws707h"

    page = requests.get(URL)

    soup = BeautifulSoup(page.content, 'html.parser')
    dellValue = soup.find_all(class_='cf-rr-total cf-rr-price-display')[0].find_all("div")[0].get_text()

    salePrice = float(non_decimal.sub('', dellValue))
    print("sale price", salePrice)

    estimatedValue = soup.find_all(class_='cf-i')

    try:
        evalString = estimatedValue[0].find_all("div", class_ = "strikethrough cf-price")[0].get_text()
        regPrice = float(non_decimal.sub('', evalString))
    except:
        regPrice = str(salePrice)
    print("regular price", regPrice)

    tmp = [str(regPrice), str(salePrice), str(datetime.datetime.now())]
    
    filePath = r"D:\Dropbox\AcademiaDropbox\dellXpsPrices.csv"
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
            wr.writerows([["RegPrice", "SalePrice", "Datetime"]])
        wr.writerows([tmp])

if __name__== "__main__":
    main()
