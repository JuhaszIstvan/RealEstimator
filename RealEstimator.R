# impressum ---------------------------------------------------------------
#"Jófogás ingatlan webscraping process"
#written by: Istvan Juhasz
#contact: https://github.com/JuhaszIstvan  
#to be executed daily.


# setup -------------------------------------------------------------------

#In preparation to running the code on a (virtual) server, the file format needs to be changed to simple.R script. .R scripts do not automatically generate a new environment like .rmd-s, thus:
rm(list=ls())
closeAllConnections()
Projectfolder<-"~/Documents/Code/R/Realestatimator"
setwd(Projectfolder)

libraries<-c("rvest","stringi","data.table","sqldf","lubridate","dplyr","ggmap","mapproj","XML")

for(index in 1:length(libraries)){
  #cat(libraries[index])
  require(libraries[index], character.only = TRUE)
       #lapply(, require, character.only = TRUE)
}

# Functions ---------------------------------------------------------------

GenerateURL<-function(siteurl='https://ingatlan.jofogas.hu/',Sellbuy='s',Commodity='lakas',City="budapest"){
  if (exists("Minsize") & exists("Maxsize")){
    if(Maxsize<Minsize)
    {
      temp<-Maxsize
      Maxsize<-Minsize
      Minsiz<-temp
    }
  }
  #City<-"budapest"

  #Kerület<-"vii-kerulet"
  Commodity<-"lakas"
  Sellbuy<-"s"
  if (exists("City")){citytag<-paste(City,"/",sep='')}else{citytag<-NULL}
  if (exists("Kerület")){citytag<-paste(citytag,Kerület,"/",sep='')}
  if (exists("Minsize")){Mintag<-paste("min_size=",Minsize,"&",sep='')}else{Mintag<-NULL}
  if (exists("Maxsize")){Maxtag<-paste("max_size=",Maxsize,"&",sep='')}else{Maxtag<-NULL}
  if (exists("Sellbuy")){directiontag<-paste("st=",Sellbuy,"&",sep='')}else{Sellbuy<-NULL}
    
  CheckUrl<-paste(siteurl,citytag,Commodity,"?",Maxtag,Mintag,"st=s", sep='')

return(CheckUrl)
}  

hirdetescrawler<-function(adurl,adAr=0){

  #add SiteID
  SiteID<-substring(adurl,regexpr("\\_[^\\_]*$", adurl)[1]+1,regexpr("\\.[^\\.]*$", adurl)[1]-1)
  names(SiteID)<-"SiteID"
  ValaszRecord<-as.data.frame(SiteID)
  
  #add SiteUrl
  SiteUrl<-adurl
  names(SiteUrl)<-"SiteUrl"
  ValaszRecord<-cbind(ValaszRecord,SiteUrl)
  
  #add Price 
  #price is a bit tricky to mine from this site because it has 3 different values with 2 of them being being hidden after rendering. 
  #BUT! I could get the data from the search results page so until i find the will to do it properly, i will pass the argument on like this  all is good.
  Ar<-adAr
  names(Ar)<-"Ar"
  ValaszRecord<-cbind(ValaszRecord,Ar)
  
  #add execution time
  QueryTime<-Sys.time()
  ValaszRecord<-cbind(ValaszRecord,QueryTime)

  #add QueryState
  QueryState="No errors recorded"
  names(QueryState)<-"QueryState"
  ValaszRecord<-cbind(ValaszRecord,QueryState)

  #collect parameters
  weblap = tryCatch({weblap <- read_html(adurl)},
  warning = function(warningcondition) {#stop(print("warning-handler-code"))
    },
  error = function(errorcondition) {
    closeAllConnections()
  ValaszRecord$QueryState<<-paste("ERROR: ",errorcondition$message)})
  if (ValaszRecord$QueryState!="No errors recorded")
    {return(ValaszRecord)}
  
  Paraméterérték<-weblap %>%
    html_nodes(".parameters") %>%
    html_nodes(".reParamValue") %>%
    html_text()
  Paramétercímke<-weblap %>%
    html_nodes(".parameters") %>%
    html_nodes(".reParamLabel") %>%
    html_text()
  
  names(Paraméterérték)<-Paramétercímke
  #Paraméterérték<-as.data.frame(t(Paraméterérték))
  ValaszRecord<-cbind(ValaszRecord,t(Paraméterérték))

  #add description
  Description<-weblap %>%
  html_nodes(".description") %>%
  html_text()
  if(length(Description>0)){
  names(Description)<-"Description"
  ValaszRecord<-cbind(ValaszRecord,Description)
  }
  
  #add the number of pictures
  PicNums<-weblap %>%
  html_nodes(".number") %>%
  html_text()
  if(length(PicNums==1)){
  names(PicNums)<-"NumberOfPictures"
  ValaszRecord<-cbind(ValaszRecord,PicNums)
  }
  
  
  #add street(it can have multiple values)
  Loc_Street<-weblap %>%
    html_nodes(".street") %>%
    html_text()
  if(length(Loc_Street>0)){
  names(Loc_Street)<-paste(rep("Loc_Street",length(Loc_Street)),1:length(Loc_Street),sep="")
  Loc_Street<-t(Loc_Street)
  ValaszRecord<-cbind(ValaszRecord,Loc_Street)
  }

  #add ad placement time
  PublishedTime<-weblap %>%
    html_nodes(".time") %>%
    html_text()
  if(length(PublishedTime>0)){
    names(PublishedTime)<-"Feladás ideje"
    ValaszRecord<-cbind(ValaszRecord,PublishedTime)
  }
  
  #add hírdető neve
  Hirdeto<-weblap %>%
    html_nodes(".name") %>%
    html_text()
  if(length(Hirdeto)){
    names(Hirdeto)<-"Hirdeto"
    ValaszRecord<-cbind(ValaszRecord,Hirdeto)
  }
  
  #add coordinates
  Latitude<-weblap %>%
    html_nodes(".re_vi_map") %>%
    html_attr("data-latitude")
  if(length(Latitude)){
  names(Latitude)<-"Latitude"
  ValaszRecord<-cbind(ValaszRecord,Latitude)
  }
  
  Longitude<-weblap %>%
    html_nodes(".re_vi_map") %>%
    html_attr("data-longitude")
  if(length(Longitude)){
  names(Longitude)<-"Longitude"
  ValaszRecord<-cbind(ValaszRecord,Longitude)
  }
  return(ValaszRecord)
}

CleanUpLakasData<-function(hirdetéstábla){

  #méret
  hirdetéstábla[,"Méret_Cleared"]<-as.numeric(as.character(gsub("[^0-9]","",hirdetéstábla[,"Méret:"])))
  
  #rezsi
  hirdetéstábla[,"Közköltség_Fthó"]<-as.numeric(as.character(gsub("[^0-9]","",hirdetéstábla[,"Havi közösköltség:"])))
  
  #szobák
  hirdetéstábla[,"SzobaSzám_Cleared"]<-as.numeric(as.character(gsub("[^0-9]","",hirdetéstábla[,"Szobák száma:"])))
  
  # Dealing with naughty columns
  colnames(hirdetéstábla)<-tstsor2<-gsub("[^a-z^A-Z^0-9]","",iconv(colnames(hirdetéstábla), to="ASCII//TRANSLIT"))
  hirdetéstábla$konyha<-grepl("Beépített konyha",hirdetéstábla$Ingatlanfelszereltsege)
  hirdetéstábla$KulonWC<-grepl("Fürdő és WC külön",hirdetéstábla$Ingatlanfelszereltsege)
  hirdetéstábla$Legkondicinalo<-grepl("Légkondicíonáló",hirdetéstábla$Ingatlanfelszereltsege)
  hirdetéstábla$Gepesitett<-grepl("Gépesített",hirdetéstábla$Ingatlanfelszereltsege)
  hirdetéstábla$Riaszto<-grepl("Riasztó",hirdetéstábla$Ingatlanfelszereltsege) 
  hirdetéstábla$Akadalymentesitett<-grepl("Akadálymentesített",hirdetéstábla$Ingatlanfelszereltsege)

 
  #parsing the publish date. Unfortunately, the monthnames are in Hungarian locale, so we have to do an ol' R-switch-a-roo
  # the year is not listed, I wonder how the conversion work post jan.01. If it fails, I will have to grab the year from the querydate or pipe it though a is.month < current.month gate. We shall see soon enough! 
  #update: If the  date is larger(later) than the query time, the script decreases it  by one (year). This will produce odd numbers for ads who had been active for more than a year.
  #nota bene, according to documentation of as.date, the locale format is OS dependent, this script uses  the format for linux (openSuse leap 42.3) 
  lct<-Sys.setlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "hu_HU.UTF-8")
  
     hirdetéstábla$PublishedTimeCleared<-as.POSIXct(ifelse(as.POSIXct(hirdetéstábla$PublishedTime,format='%B %d. %H:%M')>hirdetéstábla$QueryTime,as.POSIXct(hirdetéstábla$PublishedTime,format='%B %d. %H:%M')-dyears(1),as.POSIXct(hirdetéstábla$PublishedTime,format='%B %d. %H:%M')),origin='1970-01-01')
  
  
  #hirdetéstábla$PublishedTimeCleared<-as.Date(hirdetéstábla$PublishedTime,format='%B %d. %H:%M')
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  return(hirdetéstábla)
}

hirdeteslista <- function(weblap){
  require(rvest)
  #első proba wrapperrel
  htmltempdata <- html_nodes(weblap,'section.reLiSection.subjectWrapper')
  #Converting the ranking data to text
  TitleData<- html_text(htmltempdata)
  head(TitleData)
  #masodik proba a.subjecttel
  htmltempdata <- html_nodes(weblap,'a.subject')
  szubject<- html_text(htmltempdata)
  titles<-weblap %>%
    html_nodes("div.jfg-item") %>%
    html_nodes("div.contentArea") %>%
    html_nodes("a.subject") %>%
    html_text()
  links<-weblap %>%
    html_nodes("div.jfg-item") %>%
    html_nodes("div.contentArea") %>%
    html_nodes("a.subject") %>%
    html_attr("href")
  Reference<-weblap %>%
    html_nodes("div.jfg-item") %>%
    html_nodes("div.contentArea") %>%
    html_nodes("a.subject") %>%
    html_attr("href")

  Penznem<-weblap %>%
    html_nodes("div.jfg-item") %>%
    html_nodes("div.contentArea") %>%
    html_nodes(".currency") %>%
    html_text()

    ArRaw<-weblap %>%
    html_nodes("div.jfg-item") %>%
    html_nodes("div.contentArea") %>%
    html_nodes(".priceBox") %>%
    html_text() 

  kistábla<-as.data.table(cbind(titles,links,ArRaw,Penznem))
  #kistábla[,Ar:= stri_replace_all_fixed(ArRaw,Penznem,"")][]
  kistábla[,Ar:= as.numeric(gsub(" ","",gsub(Penznem,"",ArRaw))), by=Penznem]
  kistábla[,SiteID:=substring(links,regexpr("\\_[^\\_]*$", links)[1]+1,regexpr("\\.[^\\.]*$", links)[1]-1), by=links]
  
  return(kistábla)
}

MapAds<-function(CheckUrl,SessionID,ProjectName){
    webpage <- read_html(CheckUrl)
  
  ##retrieving the number of total hits
  htmltempdata <- html_nodes(webpage,'div.re-all-count')
  totaltalálat<- html_text(htmltempdata)
  
  if (totaltalálat!=""){
    nyitunk<-nchar('Összes hirdetés: ')+1
    vége<-gregexpr(pattern =' db',totaltalálat)[[1]][1]-1  
    totáltalálat_kesz<-as.numeric(gsub(" ","",substring(totaltalálat,nyitunk,vége)))
    ResultNumber<<-totáltalálat_kesz
  } else {totáltalálat_kesz<- -9999}
    SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
    SessionTable[SessionTable$SessionID==SessionID,"ResultNumber"]<-totáltalálat_kesz
    saveRDS(SessionTable, file="SessionTable.Rda")
  
  rm(vége)          #cleanup
  rm(nyitunk)       #cleanup
  rm(totaltalálat)  #cleanup
  
  #Define the threshold in respect of the number of results in order to be able to prevent/interrupt executions that might spam the website.
  
  TooMany<-29000
  
  #initial sanity check on the result number using the values defined above
  
  if (totáltalálat_kesz>TooMany){
  print("ERROR: Execution skipped due to results exceeding threshold")
  }else if(totáltalálat_kesz>0)
    {
      #Beginning to browsing throughthe result pages
      print(paste("Nonezero no. of results:", totáltalálat_kesz))
      i<-0
      #the repeat sections turns the pages until the new page variable is empty. 
      repeat
        {
        z<-0
        i<-i+1
        z<-z+1
        max.attempts=3
        #In case the page fails to load, we attempt to fetch the site max.attempts number of times
        for (z in 1:max.attempts)
          {
          x <- try(eval(webpage<-read_html(CheckUrl)))
          if (inherits(x, "try-error"))
             {
            print (paste("WARNING:","Failed to open:",CheckUrl))
             Sys.sleep(runif(1, 8, 15))
             }
           else
             {
             break
             }
          }
        #Provided that the website variable didn't inherit an error, we may poceed, otherwise, it is dead, Jim
        if (inherits(x, "try-error"))
           {
            print(paste("ERROR resolving link",CheckUrl,"Breaking execution"))
            break}
            TempWebPage<-NULL
      TempWebPage = tryCatch({hirdeteslista(webpage)
        }, warning = function(warning_message) {
        #print(warning_message)
      }, error = function(error_message) {
        #print(paste("error",error_message))
        SaveTheWebPage<-TempWebPage
      } 
      )
      if(exists("TempWebPage")){
          if(!exists("masszlista")){
          masszlista<-TempWebPage
          }else{
            masszlista<-rbind(masszlista,TempWebPage)    
          }
         }
#this is where we zone in on the link to the last page. this isnt even being used for anything yet.
        htmltempdata <- NULL
        htmltempdata <- html_nodes(webpage,'.ad-list-pager-item-last')
        if (!is.null(htmltempdata)){ #if it doesnt exist, conclude that we are at the last page
          utolsó_oldal_link<-html_attr(htmltempdata,"href")
        } else {
          print("ERROR hibás utolsó oldal link")
          break
        }

        #this is where we zone in on the link to the next page
        htmltempdata <- html_nodes(webpage,'.jofogasicon-right')
        következő_oldal_link<-html_attr(htmltempdata,"href")
        #Verify if we have  an URL ready 
        CheckUrl<-következő_oldal_link
        if(length(CheckUrl)==0) #if it doesnt exist, conclude that we are at the last page
        {
          cat("No new URL available")
          break
        }
        #randomised delay to avoid triggering spam protection 

        maszkido<-runif(1, 0.1, 2.23)
        cat(paste('CL: ',CheckUrl,"w8ing ", format(round(maszkido,3),nsmall=3), " second(s)\n"))
        Sys.sleep(maszkido)
        #next cycle!
        }
    
      if(nrow(masszlista)>0)
        {
        #drop duplicate items (due to reranking during execution)
        masszlista<-masszlista[!duplicated(masszlista[,2]),]
        masszlista$SessionID<-SessionID
        masszlista$ProjectName<-ProjectName
        
        return(masszlista)
        }
      else
      {cat("ERROR: the return table isempty")
        return("ERROR: The Return table is empty")}
    }
}

# Initial_Scan ------------------------------------------------------------

#Projects
UseExistingProject<-TRUE #non-predefined projects will be missing necessary filenames and FAIL!
#ProjectName<-"JofogasLakasokBpTest" 

ProjectName<-"JofogasLakasokBPFull"

#the below NULL-ifications became technically redundant when resetting the environment was added to the top.
TableOfProjectsFile<-"rest_TableOfProjects.Rda"
TableOfProjects<-NULL
ProjectData<-NULL
ProjectAdListFile<-NULL
ProjectAdDetailListFile<-NULL
ProjectCurrentAdList<-NULL
ProjectAdDetailList<-NULL
CheckUrl<-NULL

if (!file.exists(TableOfProjectsFile)){
  print("ERROR: ProjectDataFile not found, exiting!")
} else {
  print("SUCCESS: ProjectDataFile found")
  TableOfProjects<-readRDS(file=TableOfProjectsFile)
}

if (!exists("TableOfProjects")){
  print("ERROR: Failed to load the Project Data File!")
} else {
  if (!exists("ProjectName")){
  print("ERROR: No Project Name available!")
  } else {
    print("Project Name available!")
    ProjectData<-TableOfProjects[TableOfProjects$ProjectName==ProjectName,]
    if (!exists("ProjectData")){
      print("ERROR: The Project record could not be retrieved!")
    } else{
      if(nrow(ProjectData)!=1){ 
        print("ERROR: Project name is not unambigous!")
      }
    }
  }
}

if (exists("ProjectData")){
  ProjectAdListFile<-ProjectData$ProjectAdListFile
  ProjectAdDetailListFile<-ProjectData$ProjectAdDetailListFile
  CheckUrl<-ProjectData$ProjectURL
  print(paste("Project Name:",ProjectName,sep=''))
  print(paste("ProjectAdListFile:", ProjectAdListFile,sep=''))
  print(paste("ProjectAdDetailListFile:", ProjectAdDetailListFile,sep=''))
  print(paste("CheckUrl:", CheckUrl,sep=''))
}
print('Overture')

if(UseExistingProject==FALSE){
  CheckUrl<-GenerateURL(Commodity='lakas', City ='budapest')
  print(paste("Calculated url:",CheckUrl))
}
#getting a SessionID

 #Open sessionTable. I am loading and saving the sessiontable all the time because a I don't know how to be thead-safe in R  yet. 
  SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
  #generate the new Session
  start.time<-Sys.time()
  SessionID<-paste(ProjectName,"_",format(Sys.time(),"%Y%m%d_%H%M"),stri_rand_strings(n=1, length=2, pattern="[A-Z0-9]"),sep='')
  NewSession<-as.data.frame(list(ProjectName,SessionID,start.time,start.time,0,0,"Not completed"))
  names(NewSession)<-c("ProjectName","SessionID","StartTime","EndTime","RunTime","ResultNumber","Status")
  SessionTable<-merge(SessionTable,NewSession,all=TRUE) 
  saveRDS(SessionTable, file="SessionTable.Rda")

#if we have an URL available, we should scratch it into our tracker then proceed to interpret it.
if(!exists("CheckUrl")){
  print("ERROR: NO URL Available!") 
} else {
  SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
  SessionTable[SessionTable$SessionID==SessionID,"URL"]<-CheckUrl
  saveRDS(SessionTable, file="SessionTable.Rda")
#collecting the ad listings.
  ProjectCurrentAdList<-MapAds(CheckUrl = CheckUrl,SessionID = SessionID,ProjectName=ProjectName)
#The script urrently uses the sorted filelist of the project directory. this is not robust an requies adherence to a naming pattern. A proper solution is using the SessionTable to track the files as it was originally intended to be done. 
#Update:The file names are stored and retrieved from the SessionTables instead
  ProjectCurrentAdListFile<-paste(getwd(),"/BackUp/",SessionID,"_SUMM.Rda",sep='')
  saveRDS(ProjectCurrentAdList, file=ProjectCurrentAdListFile)

  #in case something goes wonky, a backup is  generated prior to  touching the files. Since they are uniquely dated, a cleanup process should be deployed to avoid excessive disk usage. This, however, had not been automated yet.
  ProjectAdList<-readRDS(file=ProjectAdListFile, refhook =NULL)
  file.copy(ProjectAdListFile, paste(getwd(),"/BackUp/",ProjectAdListFile,"_",format(Sys.time(),"%Y%m%d_%H%M"),"_backup.Rda",sep=''))
  
  ProjectAdList<-merge(ProjectAdList,ProjectCurrentAdList,all=TRUE)
  saveRDS(ProjectAdList, file=ProjectAdListFile)

  SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
  SessionTable[SessionTable$SessionID==SessionID,"QueryAdList"]<-ProjectCurrentAdListFile
  saveRDS(SessionTable, file="SessionTable.Rda")

#updating the sessiontable file with the execution time data
  end.time <- Sys.time()
  time.taken<-as.numeric(difftime(end.time,start.time, units="mins"))
  SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
  SessionTable[SessionTable$SessionID==SessionID,"EndTime"]<-end.time
  SessionTable[SessionTable$SessionID==SessionID,"RunTime"]<-time.taken
}
if (!exists("ProjectCurrentAdList")){  
  SessionTable[SessionTable$SessionID==SessionID,"Status"]<-"No Data received"
}else{
  SessionTable[SessionTable$SessionID==SessionID,"Status"]<-"Completed"  
  SessionTable[SessionTable$SessionID==SessionID,"ReceivedAds"]<-nrow(ProjectCurrentAdList)
}
print(paste("Execution time:",time.taken))
saveRDS(SessionTable, file="SessionTable.Rda")
  
print('The AD mappings have been finished.')

# Collecting_the_new_or_changed_ads ---------------------------------------

#merging the data with the basic data table. The new definitions should be moved to the top. 
#Load the cumulated dataset

SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
ProjectCurrentAdListFile<-SessionTable[SessionTable$SessionID==SessionID,"QueryAdList"]
saveRDS(SessionTable, file="SessionTable.Rda")
ProjectCurrentAdList<-readRDS(file=ProjectCurrentAdListFile, refhook =NULL)
ProjectAdList<-readRDS(file=ProjectAdListFile, refhook =NULL)
ProjectAdDetailList<-readRDS(file=ProjectAdDetailListFile, refhook =NULL)

if(exists("ProjectCurrentAdDetailList")){rm(ProjectCurrentAdDetailList)}
#check the ads that have disappeared  
#I am using the SQL query instead of normal data.frame operations because this kind of data is exactly the kind that is best stored in a proper RDBMS. To my greatest chagrim, sqlite can't handle outer joins. 
#it is time to close these ads:( #these are the queries from the current project that are completed ( we dont want to have an incomplete query to prematurely close ads, now do we? )


AdsThatHaveDisappeared <- sqldf('SELECT DISTINCT SiteID FROM ProjectAdDetailList WHERE PublishEndTime IS NULL EXCEPT SELECT DISTINCT SiteID FROM ProjectCurrentAdList')
AdsCurrentThatHaveDisappearedFile<-paste(getwd(),"/BackUp/",SessionID,"_AdsClosed.Rda",sep='')
saveRDS(AdsThatHaveDisappeared, file=AdsCurrentThatHaveDisappearedFile)
  SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
  SessionTable[SessionTable$SessionID==SessionID,"ClosedAdNumber"]<-nrow(AdsThatHaveDisappeared)
  SessionTable[SessionTable$SessionID==SessionID,"ClosedAdFile"]<-AdsCurrentThatHaveDisappearedFile
  saveRDS(SessionTable, file="SessionTable.Rda")

if(nrow(AdsThatHaveDisappeared)>0){
  SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
  #We are selecting the penultimate (previous) session of this Project
  ProjectSessions<- sqldf(sprintf('SELECT SessionID,ProjectName,StartTime,EndTime,Status FROM SessionTable WHERE ProjectName="%s" AND Status="Completed" order by StartTime DESC',ProjectName))
  saveRDS(SessionTable, file="SessionTable.Rda")
  
  #Check if the first in the line is our current session, it kinda has to.  
  CurrentSessionRecord<-ProjectSessions[1,]
  PreviousSessionRecord<-ProjectSessions[2,]
  
  if(CurrentSessionRecord$SessionID==SessionID)
  {
    PublishedBecsultEndtTime<-(CurrentSessionRecord$StartTime -     PreviousSessionRecord$StartTime)/2+PreviousSessionRecord$StartTime
    ProjectAdDetailList<-sqldf(c(sprintf('UPDATE ProjectAdDetailList SET PublishEndTime="%f" WHERE SiteID IN (SELECT SiteID FROM AdsThatHaveDisappeared)',PublishedBecsultEndtTime),'select * from main.ProjectAdDetailList'))
 
    file.copy(ProjectAdDetailListFile, paste(getwd(),"/BackUp/",ProjectAdDetailListFile,"_",format(Sys.time(),"%Y%m%d_%H%M"),"_backup.Rda",sep=''))
    saveRDS(ProjectAdDetailList, file=ProjectAdDetailListFile)
  
  } else {print("ERROR: SessionTAble has an issue with the SessionID")}
} else {print("WARNING: No records to be closed")}
  
#Checking if there are ads that need to be reopened. The entries which are scanned in the current session, BUT have non-null end time in the detailtable. 
AdsThatHaveBeenReopened <- sqldf('SELECT DISTINCT SiteID FROM ProjectAdDetailList WHERE PublishEndTime IS NOT NULL INTERSECT SELECT DISTINCT SiteID FROM ProjectCurrentAdList')



AdsThatHaveBeenReopenedFile<-paste(getwd(),"/BackUp/",SessionID,"_AdsOpened.Rda",sep='')
saveRDS(AdsThatHaveBeenReopened, file=AdsThatHaveBeenReopenedFile)


SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
SessionTable[SessionTable$SessionID==SessionID,"NewAdfile"]<-AdsThatHaveBeenReopenedFile
saveRDS(SessionTable, file="SessionTable.Rda")

ProjectAdDetailList[ProjectAdDetailList$SiteID %in% AdsThatHaveBeenReopened$SiteID,"PublishEndTime"]<-NA

#producing the list of previously unseen or changed ads.
if(exists("AdsToBeCollected")){rm(AdsToBeCollected)}
AdsToBeCollected <- sqldf('SELECT SiteID, Ar FROM ProjectCurrentAdList EXCEPT SELECT SiteID, Ar FROM ProjectAdDetailList')

SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
SessionTable[SessionTable$SessionID==SessionID,"NewAdNumber"]<-nrow(AdsToBeCollected)
saveRDS(SessionTable, file="SessionTable.Rda")
ProjectCurrentAdDetailListFile<-paste(getwd(),"/BackUp/","Detailedhirdetes_",format(Sys.time(),"%Y_%m_%d_%H%M%S"),".Rda",sep='')

if(nrow(AdsToBeCollected)>0){
  #Fetching the links to these Ads
  AdsToBeCollected<-sqldf('SELECT AdsToBeCollected.*,SUM.links
  from AdsToBeCollected
  INNER JOIN 
  ProjectCurrentAdList SUM
  ON 
  AdsToBeCollected.SiteID = SUM.SiteID')
  start.time<-Sys.time()
  SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
  SessionTable[SessionTable$SessionID==SessionID,"QueryAdDetailStatus"]<-"Started"
  SessionTable[SessionTable$SessionID==SessionID,"QueryAdDetailStartTime"]<-start.time
  saveRDS(SessionTable, file="SessionTable.Rda")
  
  i<-0
  lenfetch<-nrow(AdsToBeCollected)
  for(index in 1:lenfetch)
    {
      link = as.character(AdsToBeCollected[index,'links'])
      Ar = as.numeric(as.character(AdsToBeCollected[index,'Ar']));
      i<-i+1
      print(paste('CL:',i,'/',lenfetch,' ',link,sep=''))
      hirdetesdata<-hirdetescrawler(adurl=link,adAr=Ar)
    
      #sanity check for the collected 1-row data.frame
      if(!exists("ProjectCurrentAdDetailList")){
        print('ProjectCurrentAdDetailList nem létezik, creating')
        ProjectCurrentAdDetailList<-hirdetesdata
      }else{
        ProjectCurrentAdDetailList<-merge(ProjectCurrentAdDetailList,hirdetesdata,all=TRUE) 
        maszkido<-runif(1, 0.1, 2.23)
        cat('várunk: ',maszkido, ' másodpercet',"\n")
        Sys.sleep(maszkido)
      }
  }
  #check how many ads we failed to pull and run them again. Most error cases in the past were due to outages in the internet connection (VPN *cough cough*)
  
  redotable<-ProjectCurrentAdDetailList[ProjectCurrentAdDetailList$QueryState!="No errors recorded" & ProjectCurrentAdDetailList$QueryState!="ERROR:  HTTP error 410.",]
  if (nrow(redotable)>0){
    AdsToBeCollected<-redotable
    i<-0
    lenfetch<-nrow(AdsToBeCollected)
    for(index in 1:lenfetch){
      link = as.character(AdsToBeCollected[index,'SiteUrl'])
      Ar = as.numeric(as.character(AdsToBeCollected[index,'Ar']));
      i<-i+1
      print(paste('CL:',i,'/',lenfetch,' ',link,sep=''))
      hirdetesdata<-hirdetescrawler(adurl=link,adAr=Ar)
    
      #sanity check for the collected data.frame
      if(!exists("ProjectCurrentAdDetailList")){
        ProjectCurrentAdDetailList<-hirdetesdata
      }else{
        ProjectCurrentAdDetailList<-merge(ProjectCurrentAdDetailList,hirdetesdata,all=TRUE) 
        maszkido<-runif(1, 0.1, 3.23)
        cat('várunk: ',maszkido, ' másodpercet',"\n")
        Sys.sleep(maszkido)
      }
    }
    saveRDS(ProjectCurrentAdDetailList, file=ProjectCurrentAdDetailListFile)
  }
  ProjectCurrentAdDetailList<-CleanUpLakasData(ProjectCurrentAdDetailList)
  saveRDS(ProjectCurrentAdDetailList, file=ProjectCurrentAdDetailListFile)
}

if(exists('ProjectCurrentAdDetailList')){
    print("Merging newly added detailtables")
    ProjectAdDetailList<-merge(ProjectAdDetailList,ProjectCurrentAdDetailList,all=TRUE)
}else{
    print ("WARNING No new Table to add")
}

end.time<-Sys.time()
SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
SessionTable[SessionTable$SessionID==SessionID,"QueryAdDetailList"]<-ProjectCurrentAdDetailListFile
SessionTable[SessionTable$SessionID==SessionID,"QueryAdDetailEndTime"]<-end.time
SessionTable[SessionTable$SessionID==SessionID,"QueryAdDetailStatus"]<-"Completed"
saveRDS(SessionTable, file="SessionTable.Rda")
saveRDS(ProjectAdDetailList, file=ProjectAdDetailListFile)

print("End of the main function")


# Post-processing ---------------------------------------------------------
# Updating the Adlist table with the specific timestamp of the addetail record. This information allows the creation of foreign composite keys to the AdDetailList table.
SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)

ProjectAdDetailList<-readRDS(file=ProjectAdDetailListFile, refhook =NULL)
ProjectAdList<-readRDS(file=ProjectAdListFile, refhook =NULL)


ProSessionID="JofogasLakasokBPFull_20180107_20561I"
ProSessionID="JofogasLakasokBPFull_20180309_1855JO"
ProSessionID="JofogasLakasokBPFull_20180109_22058X"
ProSessionID<-SessionID

TestAdDetailList<-ProjectAdDetailList
QueryEndTime<-SessionTable[SessionTable$SessionID==ProSessionID,"QueryAdDetailEndTime"]

SessionTempAdList<-ProjectAdList[ProjectAdList$SessionID==ProSessionID,c('SiteID','SessionID')]
TestAdDetailList<-TestAdDetailList[TestAdDetailList$SiteID %in% SessionTempAdList$SiteID,]
TestAdDetailList$Deviance<-ifelse(QueryEndTime-TestAdDetailList$QueryTime>=0,QueryEndTime-TestAdDetailList$QueryTime,99999999999999999)
TestAdDetailList<-TestAdDetailList[with(TestAdDetailList, order(Deviance)),]
TestAdDetailList2<-TestAdDetailList[!duplicated(TestAdDetailList$SiteID,fromLast=FALSE),]

SessionTempAdList3<-merge.data.frame(SessionTempAdList,TestAdDetailList2[,c("SiteID","QueryTime")],by="SiteID",all.x = TRUE, all.y = FALSE)

library(sqldf)
#ProjectAdList$QueryTime<-NA
ProjectAdList2<-sqldf(c(paste("UPDATE ProjectAdList"
              ," SET QueryTime = (SELECT SessionTempAdList3.QueryTime FROM SessionTempAdList3 WHERE ProjectAdList.SiteID = SessionTempAdList3.SiteID AND ProjectAdList.SessionID = SessionTempAdList3.SessionID)"
              ," WHERE SessionID IN (SELECT SessionID FROM SessionTempAdList3)"
)
, " SELECT * FROM main.ProjectAdList"
)
)

ProjectAdList<-ProjectAdList2

saveRDS(ProjectAdList, file=ProjectAdListFile)



# LocationWrangling -------------------------------------------------------
# This section is responsible for correcting common typos misspellings and alternate names. It was a necessary to do SOMETHING in order to be able to begin actually fitting regressions on the data. 


replaceMe<-function(df,from,to,level='LocStreet1'){
  df[tolower(trimws(df$LocStreet1))==tolower(from),'LocStreet1']<-to
  df[tolower(trimws(df$LocStreet2))==tolower(from),'LocStreet2']<-to
  return(df)
}
 
ProjectName="JofogasLakasokBPFull"
SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
TableOfProjectsFile<-"rest_TableOfProjects.Rda"
TableOfProjects<-readRDS(file=TableOfProjectsFile)

ProjectData<-TableOfProjects[TableOfProjects$ProjectName==ProjectName,]
ProjectAdDetailListFile<-ProjectData$ProjectAdDetailListFile
ProjectAdDetailList<-readRDS(file=ProjectAdDetailListFile, refhook =NULL)

cols<-grep("^Location", names(ProjectAdDetailList),value=TRUE)
ProjectAdDetailList<- ProjectAdDetailList[,!(names(ProjectAdDetailList) %in% cols)]
ProjectAdListFile<-ProjectData$ProjectAdListFile
ProjectAdList<-readRDS(file=ProjectAdListFile, refhook =NULL)

TestAdDetailList<-ProjectAdDetailList

TestAdDetailList<-TestAdDetailList[with(TestAdDetailList, order(QueryTime,decreasing = T)),]
TestAdDetailList<-TestAdDetailList[!duplicated(TestAdDetailList$SiteID),]
TestAdDetailList<-TestAdDetailList[!duplicated(TestAdDetailList[c("MeretCleared","Description","Kerulet")]),]


TestAdDetailList$LocStreet1<-gsub("\\d", "", TestAdDetailList$LocStreet1)
TestAdDetailList$LocStreet2<-gsub("\\d", "", TestAdDetailList$LocStreet2) 

TestAdDetailList$LocStreet1<-trimws(TestAdDetailList$LocStreet1)
TestAdDetailList$LocStreet2<-trimws(TestAdDetailList$LocStreet2)

TestAdDetailList$LocStreet1<-stri_trans_general(TestAdDetailList$LocStreet1, id = "Title")
TestAdDetailList$LocStreet2<-stri_trans_general(TestAdDetailList$LocStreet2, id = "Title")

TestAdDetailList$LocStreet1<-trimws(gsub(" Környéke$", "", TestAdDetailList$LocStreet1))
TestAdDetailList$LocStreet2<-trimws(gsub(" Környéke$", "", TestAdDetailList$LocStreet2)) 

TestAdDetailList$LocStreet1<-trimws(gsub(" St$", " Sétány", TestAdDetailList$LocStreet1)) 
TestAdDetailList$LocStreet2<-trimws(gsub(" St$", " Sétány", TestAdDetailList$LocStreet2))

TestAdDetailList$LocStreet1<-trimws(gsub("\\(", "", TestAdDetailList$LocStreet1)) 
TestAdDetailList$LocStreet2<-trimws(gsub("\\(", "", TestAdDetailList$LocStreet2)) 

TestAdDetailList$LocStreet1<-trimws(gsub(")", "", TestAdDetailList$LocStreet1)) 
TestAdDetailList$LocStreet2<-trimws(gsub(")", "", TestAdDetailList$LocStreet2)) 


TestAdDetailList$LocStreet1<-trimws(gsub(" Krt[.]{0,1}$", " Körút", TestAdDetailList$LocStreet1) )
TestAdDetailList$LocStreet2<-trimws(gsub(" Krt[.]{0,1}$", " Körút", TestAdDetailList$LocStreet2) )

TestAdDetailList$LocStreet1<-trimws(gsub("  ", " ", TestAdDetailList$LocStreet1) )
TestAdDetailList$LocStreet2<-trimws(gsub("  ", " ", TestAdDetailList$LocStreet2) )

TestAdDetailList$LocStreet1<-trimws(gsub("Belső ", "Belső-", TestAdDetailList$LocStreet1) )
TestAdDetailList$LocStreet2<-trimws(gsub("Belső ", "Belső-", TestAdDetailList$LocStreet2) )

TestAdDetailList$LocStreet1<-trimws(gsub("Középső ", "Középső-", TestAdDetailList$LocStreet1) )
TestAdDetailList$LocStreet2<-trimws(gsub("Középső ", "Középső-", TestAdDetailList$LocStreet2) )

TestAdDetailList$LocStreet1<-trimws(gsub("Külső ", "Külső-", TestAdDetailList$LocStreet1) )
TestAdDetailList$LocStreet2<-trimws(gsub("Külső ", "Külső-", TestAdDetailList$LocStreet2) )

TestAdDetailList$LocStreet1<-trimws(gsub(" Környékén$", "", TestAdDetailList$LocStreet1))
TestAdDetailList$LocStreet2<-trimws(gsub(" Környékén$", "", TestAdDetailList$LocStreet2)) 

TestAdDetailList$LocStreet1<-gsub(" Közelében$", "", TestAdDetailList$LocStreet1)
TestAdDetailList$LocStreet2<-gsub(" Közelében$", "", TestAdDetailList$LocStreet2)

TestAdDetailList$LocStreet1<-gsub(" Közepe$", "", TestAdDetailList$LocStreet1)
TestAdDetailList$LocStreet2<-gsub(" Közepe$", "", TestAdDetailList$LocStreet2)

TestAdDetailList$LocStreet2<-gsub(" Ltp", " Lakótelep", TestAdDetailList$LocStreet2) 
TestAdDetailList$LocStreet1<-gsub(" Ltp", " Lakótelep", TestAdDetailList$LocStreet1) 

TestAdDetailList$LocStreet1<-gsub("Bartok", "Bartók", TestAdDetailList$LocStreet1) 
TestAdDetailList$LocStreet2<-gsub("Bartok", "Bartók", TestAdDetailList$LocStreet2) 

TestAdDetailList$LocStreet1<-gsub("Karoly", "Károly", TestAdDetailList$LocStreet1) 
TestAdDetailList$LocStreet2<-gsub("Karoly", "Károly", TestAdDetailList$LocStreet2) 

TestAdDetailList$LocStreet1<-gsub("Adrássy", "Andrássy", TestAdDetailList$LocStreet1) 
TestAdDetailList$LocStreet2<-gsub("Adrássy", "Andrássy", TestAdDetailList$LocStreet2)

TestAdDetailList$LocStreet1<-gsub(" Ut$"," Út", TestAdDetailList$LocStreet1,ignore.case = TRUE) 
TestAdDetailList$LocStreet2<-gsub(" Ut$"," Út",TestAdDetailList$LocStreet2,ignore.case = TRUE)

TestAdDetailList$LocStreet2<-gsub(" U$[.]{0,1}$", " Utca", TestAdDetailList$LocStreet2,ignore.case = TRUE) 
TestAdDetailList$LocStreet1<-gsub(" U$[.]{0,1}$", " Utca", TestAdDetailList$LocStreet1,ignore.case = TRUE) 

TestAdDetailList$LocStreet1<-gsub(" Úton", " Út", TestAdDetailList$LocStreet1,ignore.case = TRUE) 
TestAdDetailList$LocStreet2<-gsub(" Úton", " Út", TestAdDetailList$LocStreet2,ignore.case = TRUE) 

TestAdDetailList$LocStreet1<-gsub(" Útnál", " Út", TestAdDetailList$LocStreet1,ignore.case = TRUE) 
TestAdDetailList$LocStreet2<-gsub(" Útnál", " Út", TestAdDetailList$LocStreet2,ignore.case = TRUE) 


TestAdDetailList$LocStreet1<-trimws(gsub("[VIX]{0,5}[.]{0,1}[ ]{0,1}kerület[i]{0,1}","",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("[VIX]{0,5}[.]{0,1}[ ]{0,1}kerület[i]{0,1}","",TestAdDetailList$LocStreet2,ignore.case = TRUE))
TestAdDetailList$LocStreet1<-trimws(gsub("[0-9]{0,2}[.]{0,1}[ ]{0,1}kerület[i]{0,1}","",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("[0-9]{0,2}[.]{0,1}[ ]{0,1}kerület[i]{0,1}","",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("- Ii/A","",TestAdDetailList$LocStreet1,ignore.case = TRUE))

TestAdDetailList$LocStreet2<-gsub("- Ii/A","",TestAdDetailList$LocStreet2,ignore.case = TRUE)
TestAdDetailList$LocStreet2<-gsub("/","",TestAdDetailList$LocStreet2,ignore.case = TRUE)

TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Corvinnegyed","Corvinnegyed",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Corvinnegyed","Corvinnegyed",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Csarnoknegyed","Csarnoknegyed",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Csarnoknegyed","Csarnoknegyed",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Orczynegyed","Csarnoknegyed",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Orczynegyed","Csarnoknegyed",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Ganznegyed","Ganznegyed",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Ganznegyed","Ganznegyed",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Magdolnanegyed","Magdolnanegyed",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Magdolnanegyed","Magdolnanegyed",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Magdolnanegyed","Magdolnanegyed",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Magdolnanegyed","Magdolnanegyed",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Népszínháznegyed","Népszínháznegyed",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Népszínháznegyed","Népszínháznegyed",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Losoncinegyed","Losoncinegyed",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Losoncinegyed","Losoncinegyed",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Palotanegyed","Palotanegyed",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Palotanegyed","Palotanegyed",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("(Erzsébetváros|Józsefváros|Terézváros)?(\\, )?Nagykörúton Belül(i)?( Terület)?"," Nagykörúton Belüli Terület",TestAdDetailList$LocStreet1,ignore.case = TRUE,perl=TRUE))
TestAdDetailList$LocStreet1<-trimws(gsub("(Erzsébetváros|Józsefváros|Terézváros)?(\\, )?Nagykörúton Kívül(i)?( Terület)?"," Nagykörúton Kívüli Terület",TestAdDetailList$LocStreet1,ignore.case = TRUE,perl=TRUE))

TestAdDetailList$LocStreet2<-trimws(gsub("(Erzsébetváros|Józsefváros|Terézváros)?(\\, )?Nagykörúton Belül(i)?( Terület)?"," Nagykörúton Belüli Terület",TestAdDetailList$LocStreet2,ignore.case = TRUE,perl=TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("(Erzsébetváros|Józsefváros|Terézváros)?(\\, )?Nagykörúton Kívül(i)?( Terület)?"," Nagykörúton Kívüli Terület",TestAdDetailList$LocStreet2,ignore.case = TRUE,perl=TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("Középső-Ferencváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Rehabilitációs Terület","Középső Ferencváros - Rehabilitációs Terület",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Középső-Ferencváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Rehabilitációs Terület","Középső Ferencváros - Rehabilitációs Terület",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("Középső-Ferencváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Malmok","Középső Ferencváros - Malmok",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Középső-Ferencváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Malmok","Középső Ferencváros - Malmok",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub(" Negyed","negyed",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub(" Negyed","negyed",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-gsub("Utcza","Utca",TestAdDetailList$LocStreet1,ignore.case = TRUE)
TestAdDetailList$LocStreet2<-gsub("Utcza","Utca",TestAdDetailList$LocStreet2,ignore.case = TRUE)

TestAdDetailList$LocStreet1<-trimws(gsub("Budapest[ ]{0,1}[,]{0,1}","",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Budapest[ ]{0,1}[,]{0,1}","",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("--","-",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("--","-",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("  "," ",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("  "," ",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList[is.na(TestAdDetailList$LocStreet1),'LocStreet1']<-"NotSpecified"
TestAdDetailList[is.na(TestAdDetailList$LocStreet2),'LocStreet2']<-"NotSpecified"

TestAdDetailList[TestAdDetailList$LocStreet1=="Nagykörúton Belüli Terület" & TestAdDetailList$Kerulet=="VII.",'LocStreet1']<-"Erzsébetváros Nagykörúton Belüli Terület"
TestAdDetailList[TestAdDetailList$LocStreet2=="Nagykörúton Belüli Terület" & TestAdDetailList$Kerulet=="VII.",'LocStreet2']<-"Erzsébetváros Nagykörúton Belüli Terület"

TestAdDetailList[TestAdDetailList$LocStreet1=="Nagykörúton Belüli Terület"&TestAdDetailList$Kerulet=="VI.",'LocStreet1']<-"Terézváros Nagykörúton Belüli Terület"
TestAdDetailList[TestAdDetailList$LocStreet2=="Nagykörúton Belüli Terület"&TestAdDetailList$Kerulet=="VI.",'LocStreet2']<-"Terézváros Nagykörúton Belüli Terület"

TestAdDetailList[TestAdDetailList$LocStreet1=="Nagykörúton Belüli Terület"&TestAdDetailList$Kerulet=="VIII.",'LocStreet1']<-"Józsefváros Nagykörúton Belüli Terület"
TestAdDetailList[TestAdDetailList$LocStreet2=="Nagykörúton Belüli Terület"&TestAdDetailList$Kerulet=="VIII.",'LocStreet2']<-"Józsefváros Nagykörúton Belüli Terület"

TestAdDetailList[TestAdDetailList$LocStreet1=="Nagykörúton Kívüli Terület" & TestAdDetailList$Kerulet=="VII.",'LocStreet1']<-"Erzsébetváros Nagykörúton Kívüli Terület"
TestAdDetailList[TestAdDetailList$LocStreet2=="Nagykörúton Kívüli Terület" & TestAdDetailList$Kerulet=="VII.",'LocStreet2']<-"Erzsébetváros Nagykörúton Kívüli Terület"

TestAdDetailList[TestAdDetailList$LocStreet1=="Nagykörúton Kívüli Terület"&TestAdDetailList$Kerulet=="VI.",'LocStreet1']<-"Terézváros Nagykörúton Kívüli Terület"
TestAdDetailList[TestAdDetailList$LocStreet2=="Nagykörúton Kívüli Terület"&TestAdDetailList$Kerulet=="VI.",'LocStreet2']<-"Terézváros Nagykörúton Kívüli Terület"

TestAdDetailList[TestAdDetailList$LocStreet1=="Nagykörúton Kívüli Terület"&TestAdDetailList$Kerulet=="VIII.",'LocStreet1']<-"Józsefváros Nagykörúton Kívüli Terület"
TestAdDetailList[TestAdDetailList$LocStreet2=="Nagykörúton Kívüli Terület"&TestAdDetailList$Kerulet=="VIII.",'LocStreet2']<-"Józsefváros Nagykörúton Kívüli Terület"


TestAdDetailList<-replaceMe(TestAdDetailList,"Környéke","")
TestAdDetailList<-replaceMe(TestAdDetailList,"Losonczi Negyed","Losoncinegyed")
TestAdDetailList<-replaceMe(TestAdDetailList,"Mátyás-hegy","Mátyáshegy")
TestAdDetailList<-replaceMe(TestAdDetailList,"Orbán hegy","Orbánhegy")
TestAdDetailList<-replaceMe(TestAdDetailList,"Terézváros, Nagykörúton Kívül","Terézváros Nagykörúton Kívül")
TestAdDetailList<-replaceMe(TestAdDetailList,"Terézváros, Nagykörúton Belül","Terézváros Nagykörúton Kívül")
TestAdDetailList<-replaceMe(TestAdDetailList,"Tisztviselő Telep","Tisztviselőtelep")

TestAdDetailList<-replaceMe(TestAdDetailList,"Bezerédi Pál Utca","Bezerédi Utca")

TestAdDetailList<-replaceMe(TestAdDetailList,"Ajtósi Dűrer Sor","Ajtósi Dürer Sor")
TestAdDetailList<-replaceMe(TestAdDetailList,"Alsórákos Frekventált, Mégis Csendes Utcájában","Alsórákos")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bajcsy- Zsilinszky Út","Bajcsy-Zsilinszky Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bajcsy Zsilinszky","Bajcsy-Zsilinszky Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bajcsy-Zsilinszky Utca","Bajcsy-Zsilinszky Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bajcsy-Zsilinszky","Bajcsy-Zsilinszky Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bajcsy Zsilinszky Út","Bajcsy-Zsilinszky Út")


TestAdDetailList<-replaceMe(TestAdDetailList,"Apáczai-Csere János Utca","Apáczai Csere János Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Viziváros","Víziváros")
TestAdDetailList<-replaceMe(TestAdDetailList,"Újpest-kertváros","Újpest-kertváros")
TestAdDetailList<-replaceMe(TestAdDetailList,"Budapest Vi. Izabella Utca","Izabella Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Újpest-kertváros","Újpest-kertváros")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bem Rkp","Bem Rakpart")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bem Utca","Bem József Utca")

TestAdDetailList<-replaceMe(TestAdDetailList,"Bartók B. Út","Bartók Béla Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Belgrád Rakpart, Erzsébet-Híd","Belgrád Rakpart")
TestAdDetailList<-replaceMe(TestAdDetailList,"Belgrád Rkp","Belgrád Rakpart")
TestAdDetailList<-replaceMe(TestAdDetailList,"Belváros Szívében Remek Befektetés","Belváros")
TestAdDetailList<-replaceMe(TestAdDetailList,"Benczur","Benczúr Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Andrassy Út","Andrássy Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Andrássy Út Eleje, Nm Terasz","Andrássy Út")

TestAdDetailList<-replaceMe(TestAdDetailList,"Andrássy Út Közelében, Székely Bertalan Utca","Székely Bertalan Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Arany Janos","Arany János")
TestAdDetailList<-replaceMe(TestAdDetailList,"Andrássy Út Eleje, Nm Terasz","Andrássy Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Andrássy Utca, Nm Terasz","Andrássy Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Benyovszky Móricz Utca","Benyovszky Móric Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Berzenczey Utca","Berzenczey Gergely Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Berzsenyi Utca","Berzsenyi Dániel Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bebó K. Utca","Bebó Károly Utca")


TestAdDetailList<-replaceMe(TestAdDetailList,"Blaha","Blaha Lujza Tér")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bank Centernél Design Lakás","Banknegyed")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bank Negyedben","Banknegyed")
TestAdDetailList<-replaceMe(TestAdDetailList,"Baross Gábor-Telep","Baross Gábor Telep")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bartok Béla","Bartok Béla Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Berzenczey Gergely Utca","Berzeviczy Gergely Utca")

TestAdDetailList<-replaceMe(TestAdDetailList,"Bogdáni","Bogdáni Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bókay Telep","Bókaytelep")


TestAdDetailList<-replaceMe(TestAdDetailList,"Bp. Orczy Út","Orczy Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Budafoki","Budafoki Út")

TestAdDetailList<-replaceMe(TestAdDetailList,"Budaörsi","Budaörsi Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bulcsú","Bulcsu Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Centenárium Lakótelep","Centenáriumi Lakótelep")
TestAdDetailList<-replaceMe(TestAdDetailList,"Colombus Utca","Columbus Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Corvin Krt","Corvin Körút")
TestAdDetailList<-replaceMe(TestAdDetailList,"Corvin-Negyed","Corvinnegyed")
TestAdDetailList<-replaceMe(TestAdDetailList,"Csepel-Belváros","Csepel Belváros")
TestAdDetailList<-replaceMe(TestAdDetailList,"Csikihegyek Utca","Csíkihegyek Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Csillaghegyi Kertkapcsolatos","Csillaghegy")
TestAdDetailList<-replaceMe(TestAdDetailList,"Csontváry Utca","Csontváry Kosztka Tivadar Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Csontváry K. Tivadar","Csontváry Kosztka Tivadar Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Csontváry Utca","Csontváry Kosztka Tivadar Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Csontvary.k.t","Csontváry Kosztka Tivadar Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Csut Utca","Csút Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Budaliget Ii/A","Budaliget")
TestAdDetailList<-replaceMe(TestAdDetailList,"Alacskai Út","Alacskai Úti Lakótelep")
TestAdDetailList<-replaceMe(TestAdDetailList,"Alacskai Lakótelep","Alacskai Úti Lakótelep")
TestAdDetailList<-replaceMe(TestAdDetailList,"Alacskai Út, Nm Terasz","Alacskai Úti Lakótelep")
TestAdDetailList<-replaceMe(TestAdDetailList,"Alső Erdősor","Alsó Erdősor")
TestAdDetailList<-replaceMe(TestAdDetailList,"Alsórákos  Egressy Téri Lakótelep","Alsórákos")
TestAdDetailList<-replaceMe(TestAdDetailList,"Alsórákos - Kacsóh Pongrác Úti Lakótelep","Alsórákos")
TestAdDetailList<-replaceMe(TestAdDetailList,"Batthány","Batthyány Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Battyány Utca","Batthyány Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Batthyányi","Batthyány Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Batthyányi Utca","Batthyány Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Battyhány Utca","Batthyány Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Batthyány Lajos Utca","Batthyány Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Battyány","Batthyány Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Battyhány","Batthyány Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bebo Károly Utca","Bebó Karoly Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Déri M","Déri Miksa Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Dessewffy Arisztid Utca","Dessewffy Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Diósárok","Diós Árok")
TestAdDetailList<-replaceMe(TestAdDetailList,"Diósárok Út","Diós Árok")
TestAdDetailList<-replaceMe(TestAdDetailList,"Diós Árok Út","Diós Árok")
TestAdDetailList<-replaceMe(TestAdDetailList,"Dioszeghi Samuel Utca","Diószeghy Sámuel Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Diószeghy","Diószeghy Sámuel Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Diószeghy S","Diószeghy Sámuel Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Dioszegi Sámuel","Diószeghy Sámuel Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Diószegi Sámuel Utca","Diószeghy Sámuel Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Diplomatanegyedben","Diplomatanegyed")
TestAdDetailList<-replaceMe(TestAdDetailList,"Dob Utca Legszebb Része","Dob Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,". Virág U. Utca","Virág Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,", Nyírpalota Út","Nyírpalota Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Vi. Izabella Utca","Izabella Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Xxii. Galga Utca","Galga Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Zugló Tallér Utca","Tallér Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Zugligeti Út","Xii.ker Zugligeti Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Árpádfejedelem Útja","Árpád Fejedelem Útja")

TestAdDetailList<-replaceMe(TestAdDetailList,", Teleki Utca","Teleki Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Arany János Utcai Lakótelep","Arany János Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Csarnoknegyed","Csarnoknegyed")
TestAdDetailList<-replaceMe(TestAdDetailList,"Józsefváros  Csarnoknegyed","Csarnoknegyed")
TestAdDetailList<-replaceMe(TestAdDetailList,"Józsefváros - Csarnoknegyed","Csarnoknegyed")
TestAdDetailList<-replaceMe(TestAdDetailList,"Belső-Erzsébetváros - Dohány Utca","Dohány Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Berda József U Utca","Berda József Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Belső- Ferencváros","Belső-Ferencváros")
TestAdDetailList<-replaceMe(TestAdDetailList,"Rózsa U. Utca","Rózsa Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Bauer Sandor","Bauer Sándor Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Angyalföldi Utca","Angyalföldi Út")
TestAdDetailList<-replaceMe(TestAdDetailList,"Angyalföld - Lőportárdűlő","Angyalföld Lőportárdűlő")
TestAdDetailList<-replaceMe(TestAdDetailList,"Angyalföld Lőportárdűlő","Angyalföld Lőportárdűlő")
TestAdDetailList<-replaceMe(TestAdDetailList,"Angyalföld  Lőportárdűlő","Angyalföld Lőportárdűlő")
TestAdDetailList<-replaceMe(TestAdDetailList,".ker Nagytétény Tenkes Utca","Tenkes Utca")
TestAdDetailList<-replaceMe(TestAdDetailList,"Józsefváros  Losoncinegyed","Losoncinegyed")
TestAdDetailList<-replaceMe(TestAdDetailList,"Józsefváros Losoncinegyed","Losoncinegyed")
TestAdDetailList<-replaceMe(TestAdDetailList,"Losonczy Ter","Losonci Tér")
TestAdDetailList<-replaceMe(TestAdDetailList,"Diplomata Negyed","Diplomatanegyed")

TestAdDetailList[TestAdDetailList$LocStreet1=="",'LocStreet1']<-"NotSpecified"
TestAdDetailList[TestAdDetailList$LocStreet2=="",'LocStreet2']<-"NotSpecified"


LocationDF<-TestAdDetailList[,c('Kerulet','LocStreet1',"LocStreet2")]
LocColFirst<-LocationDF %>%count(LocStreet1)
LocColSecond<-LocationDF%>%count(LocStreet2)
colnames(LocColFirst)<-c("Location","Col1Freq")
colnames(LocColSecond)<-c("Location","Col2Freq")

LocationFreqs<-merge(LocColFirst,LocColSecond,by="Location",all=T)
LocationFreqs[is.na(LocationFreqs$Col1Freq),'Col1Freq']<-0
LocationFreqs[is.na(LocationFreqs$Col2Freq),'Col2Freq']<-0

LocationFreqs$Cat<-ifelse(LocationFreqs$Col1Freq>LocationFreqs$Col2Freq,1,2)
LocationFreqs$Total<-LocationFreqs$Col1Freq+LocationFreqs$Col2Freq
LocationFreqs$utca<-paste(LocationFreqs$Location," Utca",sep="")
LocationFreqs$ut<-paste(LocationFreqs$Location," Út",sep="")

for(i in 1:length(LocationFreqs$utca)){
  hello<-LocationFreqs$utca[i]
  út<-LocationFreqs$ut[i]
  Location<-LocationFreqs[LocationFreqs$utca==hello,'Location']
  hellonum<-LocationFreqs[LocationFreqs$Location==hello,'Total']
  utnum<-LocationFreqs[LocationFreqs$Location==út,'Total']
  if(length(LocationFreqs[LocationFreqs$Location==hello,'Total'])>0){
    TestAdDetailList<-replaceMe(TestAdDetailList,Location,hello)
    LocationFreqs[i,'UtcaNum']<-LocationFreqs[LocationFreqs$Location==hello,'Total']}
}

LocationDF<-TestAdDetailList[,c('Kerulet','LocStreet1',"LocStreet2")]
LocColFirst<-LocationDF %>%count(LocStreet1)
LocColSecond<-LocationDF%>%count(LocStreet2)
colnames(LocColFirst)<-c("Location","Col1Freq")
colnames(LocColSecond)<-c("Location","Col2Freq")


TestAdDetailList$LocStreet1<-ifelse(is.na(TestAdDetailList$LocStreet1)&!is.na(TestAdDetailList$LocStreet2),TestAdDetailList$LocStreet2,TestAdDetailList$LocStreet1)
TestAdDetailList$LocStreet2<-ifelse(is.na(TestAdDetailList$LocStreet1)&!is.na(TestAdDetailList$LocStreet2),NA,TestAdDetailList$LocStreet2)
LocationFreqs<-merge(LocColFirst,LocColSecond,by="Location",all=T)
LocationFreqs[is.na(LocationFreqs$Col1Freq),'Col1Freq']<-0
LocationFreqs[is.na(LocationFreqs$Col2Freq),'Col2Freq']<-0
LocationFreqs<-LocationFreqs[!LocationFreqs$Location=="",]


LocationFreqs$Cat<-ifelse(LocationFreqs$Col1Freq>LocationFreqs$Col2Freq,1,2)
LocationFreqs$Total<-LocationFreqs$Col1Freq+LocationFreqs$Col2Freq

LocationDF<-TestAdDetailList[,c('Kerulet','LocStreet1',"LocStreet2")]

#We want to drop the location labels that do not have enough occurences to classify it with enough confidernce.
LocationFreqs$drop<-ifelse(LocationFreqs$Total<7,TRUE,FALSE)
LocationFreqsDrop<-LocationFreqs[!LocationFreqs$drop,]


TestAdDetailList<-merge(TestAdDetailList,LocationFreqsDrop[,c('Location','Cat')],by.x="LocStreet1",by.y="Location",all.x=TRUE)
TestAdDetailList$Cat1<-TestAdDetailList$Cat
TestAdDetailList$Cat<-NULL # we are reusing this variable

TestAdDetailList<-merge(TestAdDetailList,LocationFreqsDrop[,c('Location','Cat')],by.x="LocStreet2",by.y="Location",all.x=TRUE)
TestAdDetailList$Cat2<-TestAdDetailList$Cat
TestAdDetailList$Cat<-NULL
TestAdDetailList[is.na(TestAdDetailList$Cat1),'Cat1']<-0 #NAs are incomparable
TestAdDetailList[is.na(TestAdDetailList$Cat2),'Cat2']<-0

TestAdDetailList$Location1<-ifelse(TestAdDetailList$Cat1==1,TestAdDetailList$LocStreet1,ifelse(TestAdDetailList$Cat2==1,TestAdDetailList$LocStreet2,"NotSpecified"))
TestAdDetailList$Location2<-ifelse(TestAdDetailList$Cat1==2,TestAdDetailList$LocStreet1,ifelse(TestAdDetailList$Cat2==2,TestAdDetailList$LocStreet2,"NotSpecified"))

LocationTable<-TestAdDetailList[,c('SiteID','Location1','Location2')]

LocationTable[LocationTable$Location1=="NotSpecified",'Location1']<-NA
LocationTable[LocationTable$Location2=="NotSpecified",'Location2']<-NA

#We are adding the consolidated location labels to the original record set.
ProjectAdDetailList<-merge(ProjectAdDetailList,LocationTable,by.x="SiteID",by.y="SiteID",all.x=TRUE)

saveRDS(ProjectAdDetailList,file = ProjectAdDetailListFile)


# Reverse_Geocoding -------------------------------------------------------
#Geocoding is using information to get geolocation data (latitudinal and longitudinal), reverse geocoding is - its reverse. Jofogas.hu stores the coordinates  on a minimap that we have scrapped in the earlier phase.
#reverse geocoding will give us the postal names of these location. A major issue here is that the coordinates are generated by looking up the provided location data on google map. Unfortunately, as of the writing of this comment - 2018-04-28 - the site uses freetext instead of a closed list meaning that it sometimes recognises the wrong street or nothing at all producing bogus coordinates.
#Truth to be told, if jofogas limited the acceptable values, the whole coordinate thing wouldn't even be necessary.

cat("Beginning the execution of the chunk: Reverse_Geocoding")

CoordinateTableFile<-"CoordinateTable.Rda"
file.copy(CoordinateTableFile, paste(getwd(),"/BackUp/",CoordinateTableFile,"_",format(Sys.time(),"%Y%m%d_%H%M"),"_backup.Rda",sep=''))

CoordinateTable<-readRDS(CoordinateTableFile)

ProjectName="JofogasLakasokBPFull"
SessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
TableOfProjectsFile<-"rest_TableOfProjects.Rda"
TableOfProjects<-readRDS(file=TableOfProjectsFile)

ProjectData<-TableOfProjects[TableOfProjects$ProjectName==ProjectName,]
ProjectAdDetailListFile<-ProjectData$ProjectAdDetailListFile
ProjectAdDetailList<-readRDS(file=ProjectAdDetailListFile, refhook =NULL)

cols<-grep(("^hazszam|^Zip|^District|^neighborhood|^route"), names(ProjectAdDetailList),value=TRUE)
ProjectAdDetailList<- ProjectAdDetailList[,!(names(ProjectAdDetailList) %in% cols)]

TestAdDetailList<-ProjectAdDetailList[with(ProjectAdDetailList, order(QueryTime,decreasing = T)),]
CoordinateCurrentTable<-TestAdDetailList %>% group_by(Latitude,Longitude) %>%summarise(total = n())%>%arrange(desc(total))

CoordinateCurrentTable<-merge(CoordinateCurrentTable,CoordinateTable[,!(colnames(CoordinateTable) %in% c("total"))],by=c("Latitude","Longitude"),all.x=TRUE)

CoordinateCurrentTable<-CoordinateCurrentTable[!is.na(CoordinateCurrentTable$Latitude) & !is.na(CoordinateCurrentTable$Longitude),]

b<-NULL
k<-0
TotalNum<-length(which(is.na(CoordinateCurrentTable$Zip),TRUE))
for(b in 1:length(CoordinateCurrentTable$Latitude)){
  Results<-NULL
  result<-NULL
  booroute<-TRUE
  boohousenum<-TRUE
  boodistrict<-TRUE
  booneighbourhood<-TRUE
  boozip<-TRUE
  ErrorTries<-0
  maxattempts=15

  if(is.na(CoordinateCurrentTable[b,'Zip'])){
    k<-k+1
    for(G in 1:maxattempts){
      cat(paste("\n",k,'\\' ,TotalNum," Attpt ",G,'|',CoordinateCurrentTable$Longitude[b]," ",CoordinateCurrentTable$Latitude[b],sep=""))
      result = tryCatch({
        revgeocode(c(as.numeric(CoordinateCurrentTable$Longitude[b]),as.numeric(CoordinateCurrentTable$Latitude[b])), output = "all",override_limit=TRUE,messaging = FALSE)
      }, warning = function(warning_message) {
        #print(warning_message)
      }, error = function(error_message) {
        #print(paste("error",error_message))
      } 
      )
      if (exists("result")&& !is.null(result) && length(result)>1 && !is.null(result$result) && result$status=="OK")
      {
        break}
      else{
        cat(" Result is no good, Boss")}
      Sys.sleep(2)
      if(G==maxattempts){
        stop("Attempts have exceeded the predefined limit.")
      }
    }
    if(!is.null(result)){
      CoordinateCurrentTable[b,'Status']<-result$status
      cat(paste(result$status,"\n"))
      if(result$status=="OK"){
        Results<-result$results
        saveme<-Results
        for(rez in 1:length(Results)){
          adcomp<-NULL
          adcomp<-Results[[rez]]$address_components
          for(adc in 1:length(adcomp)){
            first<-NULL
            second<-NULL
            third<-NULL
            first<-try(adcomp[[adc]]$types[[1]],silent=TRUE)
            second<-try(adcomp[[adc]]$types[[2]],silent=T)
            third<-try(adcomp[[adc]]$types[[3]],silent=T)
            if(length(adcomp[[adc]]$types)==1){
              if(booroute & adcomp[[adc]]$types=='route')
              {
                CoordinateCurrentTable[b,'route']<-adcomp[[adc]]$long_name
                booroute<-FALSE
              } else if( boozip & adcomp[[adc]]$types=='postal_code'){
                CoordinateCurrentTable[b,'Zip']<-adcomp[[adc]]$long_name
                boozip<-FALSE
              }
              else if(boohousenum & adcomp[[adc]]$types=='street_number'){
                CoordinateCurrentTable[b,'hazszam']<-adcomp[[adc]]$long_name
                boohousenum<-FALSE
              }
            }
            else if(boodistrict & second=="sublocality" & third=="sublocality_level_1"){
              CoordinateCurrentTable[b,'District']<-adcomp[[adc]]$long_name
              boodistrict<-FALSE}
            else if(booneighbourhood  & second=="political" & first=="neighborhood"){
              CoordinateCurrentTable[b,'neighborhood']<-adcomp[[adc]]$long_name
              booneighbourhood<-FALSE}
            if(!booroute& !boozip & !boohousenum& !boodistrict& !booneighbourhood)
            {break}
          }
          if(!booroute& !boozip & !boohousenum& !boodistrict& !booneighbourhood)
          {break}
        }
      }
    }else{
      saveRDS(CoordinateCurrentTable,file=paste(getwd(),"/BackUp/",CoordinateCurrentTable,"_",format(Sys.time(),"%Y%m%d_%H%M"),"_backup.Rda",sep=''))
      
      
      stop("Not enough observations")
    }
  }
}

print(length(which(is.na(CoordinateCurrentTable$Zip),TRUE)))
CoordinateCurrentTable<-CoordinateCurrentTable[!is.na(CoordinateCurrentTable$Zip),]
CoordinateCurrentTable<-CoordinateCurrentTable[with(CoordinateCurrentTable, order(Zip,decreasing = T)),]
CoordinateCurrentTable<-CoordinateCurrentTable[!duplicated(CoordinateCurrentTable[c("Longitude","Latitude")]),]

saveRDS(CoordinateCurrentTable,file=CoordinateTableFile)

ProjectAdDetailList<-merge(ProjectAdDetailList,CoordinateCurrentTable[,!(colnames(CoordinateCurrentTable)%in% c("total","Status"))],by=c("Latitude","Longitude"),all.x=TRUE)

#NAs are incomparable
ProjectAdDetailList[is.na(ProjectAdDetailList$District),'District']<-"Missing"


ProjectAdDetailList$District<-ProjectAdDetailList[ProjectAdDetailList$District=="District V.",'District']<-"V. kerület"
ProjectAdDetailList$District<-ProjectAdDetailList[ProjectAdDetailList$District=="District VI.",'District']<-"VI. kerület"
ProjectAdDetailList$District<-ProjectAdDetailList[ProjectAdDetailList$District=="District VII.",'District']<-"VII. kerület"
ProjectAdDetailList$District<-ProjectAdDetailList[ProjectAdDetailList$District=="District VIII.",'District']<-"VIII. kerület"
ProjectAdDetailList$District<-ProjectAdDetailList[ProjectAdDetailList$District=="District XIX",'District']<-"XIX. kerület"
ProjectAdDetailList$District<-ProjectAdDetailList[ProjectAdDetailList$District=="District XII.",'District']<-"XII. kerület"
ProjectAdDetailList$District<-ProjectAdDetailList[ProjectAdDetailList$District=="District XIII.",'District']<-"XIII. kerület"
ProjectAdDetailList$District<-ProjectAdDetailList[ProjectAdDetailList$District=="Missing",'District']<-NA
saveRDS(ProjectAdDetailList,file=ProjectAdDetailListFile)


#Ditching the large data file.
if(exists("ProjectAdList")){rm(ProjectAdList)}

# Producing the report ----------------------------------------------------
cat("Execution completed")