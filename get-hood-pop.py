import pandas as pd
import numpy as np
from bs4 import BeautifulSoup
import requests 


cluster_id=range(1,40)

pop10=[]

for num in cluster_id:
    r = requests.get('http://neighborhoodinfodc.org/nclusters/Nbr_prof_clus' + str(num) + '.html')
    b = BeautifulSoup(r.text)
    pop10.append(b('table')[2].find_all('tr')[6].find_all('td')[1].text)  

    
neighborhood_data = pd.DataFrame({id:cluster_id, 'pop10':pop10})

neighborhood_data.to_csv('popHood.csv')

