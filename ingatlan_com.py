import requests
baseurl='https://ingatlan.com'
hID=31892542

hirdetesurl=str(baseurl)+"/"+str(hID)
x = requests.get(hirdetesurl)
print(x.status_code)