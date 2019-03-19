## gets sale price and regular price for dell xps

from bs4 import BeautifulSoup
import re
import requests
import datetime
import os
import csv


def main(): 
    URL = "https://www.dell.com/en-us/shop/dell-laptops/new-xps-13-touch/spd/xps-13-9380-laptop/xnita3ws707h"

    page = requests.get(URL)

    soup = BeautifulSoup(page.content, 'html.parser')

    estimatedValue = soup.find_all(class_='cf-i')

    evalString = estimatedValue[0].find_all("div", class_ = "strikethrough cf-price")[0].get_text()

    non_decimal = re.compile(r'[^\d.]+')
    regPrice = float(non_decimal.sub('', evalString))
    print("regular price", regPrice)

    dellValue = soup.find_all(class_='cf-rr-total cf-rr-price-display')[0].find_all("div")[0].get_text()

    salePrice = float(non_decimal.sub('', dellValue))
    print("sale price", salePrice)

    tmp = [str(regPrice), str(salePrice), str(datetime.datetime.now())]

    filePath = r"D:\Dropbox\AcademiaDropbox\dellXpsPrices.csv"
    exists = os.path.isfile(filePath)

    if sys.version[0:3] == '3.7':
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