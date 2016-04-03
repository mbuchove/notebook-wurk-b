
# coding: utf-8


from os import path, system
import re
import requests
from bs4 import BeautifulSoup
from getpass import getpass

un = 'veritas'
pw = getpass(prompt="enter pword")
page_url = "https://veritas.sao.arizona.edu/wiki/index.php/Advanced_Analysis_Test_Samples"
response = requests.get(page_url, auth=(un,pw))
pass # do nothing 
print(response.text)

rxpage = re.compile(r'wiki')
match = rxpage.search(page_url)
wiki_base = page_url[:match.start()]
print(wiki_base)

soup = BeautifulSoup(response.text, 'lxml')

urls = []
names = []
for i, link in enumerate(soup.findAll('a')):
    file_addr = link.get('href')
    if file_addr:
        _FULLURL = wiki_base + file_addr
        if _FULLURL.endswith('.txt'):
            urls.append(_FULLURL)
            slct = soup.select('a')[i].attrs['href']
            names.append(path.split(slct)[1])

names_urls = zip(names, urls)
save_dir = path.expanduser('~')+"VERITAS/VEGAS-v2_5_5/validation/standard_runlists"
# directory not working 

for name, url in names_urls:
    print('Downloading %s' % url)
    cmd = 'wget --user {0} --password {1} --directory-prefix={2} {3}'.format(un, pw, save_dir, url)
    print(system('wget --user {0} --password {1} {2}'.format(un, pw, url)))
    
