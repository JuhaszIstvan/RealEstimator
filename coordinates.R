library(dplyr)
library(ggmap)
library(stringi)
library(XML)
CoordinateTableFile<-"CoordinateTable.Rda"
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



TestAdDetailList<-ProjectAdDetailList


TestAdDetailList<-TestAdDetailList[with(TestAdDetailList, order(QueryTime,decreasing = T)),]

CoordinateCurrentTable<-TestAdDetailList %>% group_by(Latitude,Longitude) %>%summarise(total = n())%>%arrange(desc(total))

TestAdDetailList<-TestAdDetailList[with(TestAdDetailList, order(QueryTime,decreasing = T)),]
#TestAdDetailList<-TestAdDetailList[!duplicated(TestAdDetailList$SiteID),]
#TestAdDetailList<-TestAdDetailList[!duplicated(TestAdDetailList[c("MeretCleared","Description","Kerulet")]),]


CoordinateCurrentTable<-TestAdDetailList %>% group_by(Latitude,Longitude) %>%summarise(total = n())%>%arrange(desc(total))

CoordinateCurrentTable<-merge(CoordinateCurrentTable,CoordinateTable[,!(colnames(CoordinateTable) %in% c("total"))],by=c("Latitude","Longitude"),all.x=TRUE)
#CoordinateCurrentTable$Zip<-NA
#res <- mapply(FUN = function(lon, lat) {revgeocode(c(lon, lat), output = "all")},as.numeric(TestAdDetailList2$Longitude), as.numeric(TestAdDetailList2$Latitude))



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
    for(G in 1:maxattempts){
      cat(paste("\n",b,'\\' ,TotalNum," Attpt ",G,'|',CoordinateCurrentTable$Longitude[b]," ",CoordinateCurrentTable$Latitude[b],sep=""))
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
        cat("Result is no good, Boss")}
      Sys.sleep(2)
      if(G==maxattempts){
        stop("Attempts have exceeded the predefined limit.")
      }
    }
    #result<-revgeocode(c(as.numeric(CoordinateCurrentTable$Longitude[b]),as.numeric(CoordinateCurrentTable$Latitude[b])), output = "all",override_limit = T)
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
      #try(CoordinateCurrentTable[b,'Zip']<-Results[[1]]$address_components[[6]]$long_name,silent=TRUE)
      #CoordinateCurrentTable[b,'hazszam']<-Results[[1]]$address_components[[1]]$long_name
      #CoordinateCurrentTable[b,'route']<-Results[[1]]$address"_components[[2]]$long_name
      #CoordinateCurrentTable[b,'district']<-Results[[1]]$address_components[[3]]$long_name
      #CoordinateCurrentTable[b,'neighbourhood']<-Results[[2]]$address_components[[1]]$short_name
    }else{
      saveRDS(CoordinateCurrentTable,file=paste("backup_",CoordinateTableFile))
      stop("Not enough observations")
      }
  }
  }

print(length(which(is.na(CoordinateCurrentTable$Zip),TRUE)))
CoordinateCurrentTable<-CoordinateCurrentTable[!is.na(CoordinateCurrentTable$Zip),]
CoordinateCurrentTable<-CoordinateCurrentTable[with(CoordinateCurrentTable, order(Zip,decreasing = T)),]
CoordinateCurrentTable<-CoordinateCurrentTable[!duplicated(CoordinateCurrentTable[c("Longitude","Latitude")]),]

saveRDS(CoordinateCurrentTable,file=CoordinateTableFile)

CoordinateCurrentTable3<-CoordinateCurrentTable[,!(colnames(CoordinateCurrentTable)%in% c("total","Status"))]
ProjectAdDetailList2<-merge(ProjectAdDetailList,CoordinateCurrentTable3,by=c("Latitude","Longitude"),all.x=TRUE)


ProjectAdDetailList2[is.na(ProjectAdDetailList2$District),'District']<-"Missing"

ProjectAdDetailList2$District<-ProjectAdDetailList2[ProjectAdDetailList2$District=="District V.",'District']<-"V. kerület"
ProjectAdDetailList2$District<-ProjectAdDetailList2[ProjectAdDetailList2$District=="District VI.",'District']<-"VI. kerület"
ProjectAdDetailList2$District<-ProjectAdDetailList2[ProjectAdDetailList2$District=="District VII.",'District']<-"VII. kerület"
ProjectAdDetailList2$District<-ProjectAdDetailList2[ProjectAdDetailList2$District=="District VIII.",'District']<-"VIII. kerület"
ProjectAdDetailList2$District<-ProjectAdDetailList2[ProjectAdDetailList2$District=="District XIX",'District']<-"XIX. kerület"
ProjectAdDetailList2$District<-ProjectAdDetailList2[ProjectAdDetailList2$District=="District XII.",'District']<-"XII. kerület"
ProjectAdDetailList2$District<-ProjectAdDetailList2[ProjectAdDetailList2$District=="District XIII.",'District']<-"XIII. kerület"

ProjectAdDetailList2$District<-ProjectAdDetailList2[ProjectAdDetailList2$District=="Missing",'District']<-NA

saveRDS(ProjectAdDetailList2,file=ProjectAdDetailListFile)
write.csv(ProjectAdDetailList2,file="ads.csv",fileEncoding = "UTF-8")
