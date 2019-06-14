# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

from urllib.request import urlopen
from urllib.error import HTTPError
from bs4 import BeautifulSoup as bs

def getTitle(url):
    try:
        html = urlopen(url)
    except HTTPError as e:
        return None
        
    try:
        bsObj = bs(html.read(), "lxml")
        title = bsObj.body.h1
    except AttributeError as e:
        return None
    return title


title = getTitle("http://pythonscraping.com/pages/page1.html")
if title ==None:
    print("Title could not be found")
else:
    print(title)


