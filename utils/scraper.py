#!/usr/bin/env python

import requests
import re
from bs4 import BeautifulSoup

import os
from subprocess import Popen, PIPE

ROOT = "http://www.bodybuilding.com/fun/"
START = "http://www.bodybuilding.com/fun/lee-labrada-12-week-lean-body-trainer.html"
LINK_PATTERN = r'.*lee-labrada-12-week-lean-body-trainer-week-\d+-day-\d+\.html'

abspath = lambda *p: os.path.abspath(os.path.join(*p))
SCRIPT_ROOT = abspath(os.path.dirname(__file__))


def execute_command(command):
	print "Executing: " + command
	result = Popen(command, shell=True, stdout=PIPE).stdout.read()
	if len(result) > 0 and not result.isspace():
		#raise Exception(result)
		print(result)

def do_screen_capturing(url, screen_path, width=None, height=None):
    print "Capturing screen (" + url + ")..."
    execute_command("webkit2png --ignore-ssl-check --selector=main -D ./leanbody -F " + url)

resp = requests.get(START)

soup = BeautifulSoup(resp.text, "html.parser")

pages = soup.find("div", class_ = "Article-body").find_all('a')
pages = [a.get('href') for a in pages]
pages = sorted(list(set([l for l in pages if l and re.match(LINK_PATTERN, l)])))

for idx, page in enumerate(pages):
	print "Page " + str(idx) + " out of " + str(len(pages))

	if not page.startswith("http"):
		page = ROOT + page
	do_screen_capturing(page, page.split(".")[0])


