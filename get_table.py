__author__ = 'Ricky'

import urllib2
from bs4 import BeautifulSoup
import csv
from collections import defaultdict
import re

def findTable():
    d=defaultdict(list)
    d['year']=[]


    headings=['rk','tm','g','19a','19m','20a','20m','30a','30m','40a','40m','50a','50m','fga','fgm','fgpct','xpa','xpm','xpct']
    year=1960
    while year<=2015:
        print year
        response = urllib2.urlopen('http://www.pro-football-reference.com/years/'+str(year)+'/')
        html = response.read()
        soup = BeautifulSoup(html)
        table = soup.find("table", attrs={"id":"kicking"})

        #headings = [th.get_text() for th in table.find("tr").find_all("th")]


        datasets = []
        for row in table.find_all("tr")[1:]:

            dataset = zip(headings, (td.get_text() for td in row.find_all("td")))
            datasets.append(dataset)
            for field in dataset:
                d[field[0]].append(field[1])

            d['year'].append(year)
        d['year'].remove(year)
        year+=1
    return d



def export(filename):
    d=findTable()
    with open(filename, "wb") as outfile:
        writer = csv.writer(outfile)
        writer.writerow(d.keys())
        writer.writerows(zip(*d.values()))

#findTable()


export("test.csv")
