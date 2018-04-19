# RealEstimator

The RealEstimator is webscraper written in R for the purposes of scraping real estate postings from ingatlan.jofogas-dot-hu.

The script was written with generalisation in mind.

Tested on: OpenSUSE Leap 42.3 and OpenSUSE Tumbleweed 20180410

Please note that the rJava package is the distro packaged version because the CRAN package has issues with the java on these distros.

written by: Juhasz Istvan 

contact: [https://github.com/JuhaszIstvan](https://github.com/JuhaszIstvan )

# Data Structure


## General files
1.The script uses a master project table to record the various file names and access paths associated with each project.  

2.It also uses a coordinate table that stores the parsed results of the returns of the reverse geocoding of the latitude and longitude data found on the ad pages.

3.The script uses a sessionstable where it records key characteristics of each scan. The table is also used to pull the records found during the last scan for the purposes of calculating the closing date for them.

## Project specific files

Each project has at least 2 tables
1) the ADDetailList,
  that contains the ad IDs, ask prices and scan dates recorded during each scan. This table is used to query the scanning of the ad pages
2) AdDetailList
  A table that contains the additional columns of each record.
  
# Execution
## Realestimator script 
The script was originally written in several files as an R Studio markdown workbook, which guaranteed a clean environment with all outputs dumped into the output/backup directories.

Correspondingly, the code has 3 phases:

1) It runs a query and produces a list of ads currently returned.
2) It compares that list to the data frame of ad details. The new records are then set aside for additional queries. Records are considered identical if and only if the Price and jofogas ID matches. This means that ads whose prices have changed are queried again.
3) New ads are then collected by calling the ads' URLs in rvest. Ads with identical jofogas IDs(SiteID) are added and not updated.
4) Ads that were not found in the current scan but were present in the last one (last scan is determined by checking the last COMPLETED session of the project listed in the SessionTable file) are considered closed and have their PublishEndTime attribute filled with the midpoint of the last and current scan's execution time.
5) Ads that are present in the current scan but have a non-NULL PublishEndTime get their PublishEndTime cleared.
6) While the detailquery performs some mangling, the idea is to keep that function generalised and move all real estate specific data mangling to a separate function.
7) Location aggregation:Coordinates
The ads have latitude and longitude parameters that are generated by the Site if the Location1 and Location2 entries were recognised or used ZIP codes if they were  not. This location data is compared to the CoordinateTable and the unrecognised numbers are reverse geocoded into post addresses using Google Maps. 

8) Location aggregation:parsing 
The ads have two free-text fields (LocStreet1 and LocStreet2) that are occasionally filled correctly. Common misspellings are corrected and the update locations are marked into the Location1 and Location2 fields preserving the raw data.

## Analysis.rmd
This script is written in an R markdown workbook format. It is purpose is reproducibly process the various data cleaning operations. It contains multiple mutually exclusive Machine learning fittings, Record Linkage attempts that makes full execution impossible!

