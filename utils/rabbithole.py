#!/usr/bin/env python

import requests
import re
from BeautifulSoup import BeautifulSoup

ROOT = "http://www.thefremontproject.com"
START = "http://www.thefremontproject.com/rabbithole/next/9332a8013468d85de3d528551fc49a4cca383d98.html"

resp = requests.get(START)

soup = BeautifulSoup(resp.text)

next_link = soup.find('a', id='next-link').get('href')

while next_link:
	print next_link
	resp = requests.get(ROOT + next_link)
	soup = BeautifulSoup(resp.text)
	next_link = soup.find('a', id='next-link').get('href')