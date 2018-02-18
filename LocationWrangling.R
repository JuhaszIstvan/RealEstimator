library(dplyr)
library(ggmap)
library(stringi)
library(XML)
replaceMe<-function(df,from,to,level='LocStreet1')
{
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

#TestAdDetailList<-TestAdDetailList[with(TestAdDetailList, order("QueryTime"),decreasing=TRUE), ]
TestAdDetailList<-TestAdDetailList[!duplicated(TestAdDetailList$SiteID),]
TestAdDetailList<-TestAdDetailList[!duplicated(TestAdDetailList[c("MeretCleared","Description","Kerulet")]),]

library(ggmap)
library(mapproj)


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

#TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros","",TestAdDetailList$LocStreet1,ignore.case = TRUE))
#TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros","",TestAdDetailList$LocStreet2,ignore.case = TRUE))



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

#TestAdDetailList$LocStreet1<-trimws(gsub("[Belső-|Külső-|Középső-]{0,1}[Józsefváros|Terézváros|Ferencváros|Erzsébetváros]{0,1}[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Nagykörúton Belül[i]{0,1}","Nagykörúton Belüli Terület",TestAdDetailList$LocStreet1,ignore.case = TRUE))
#TestAdDetailList$LocStreet1<-trimws(gsub("[Belső-|Külső-|Középső-]{0,1}[Józsefváros|Terézváros|Ferencváros|Erzsébetváros]{0,1}[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Nagykörúton Kívül[i]{0,1}","Nagykörúton Kívüli Terület",TestAdDetailList$LocStreet1,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("(Erzsébetváros|Józsefváros|Terézváros)?(\\, )?Nagykörúton Belül(i)?( Terület)?"," Nagykörúton Belüli Terület",TestAdDetailList$LocStreet1,ignore.case = TRUE,perl=TRUE))
TestAdDetailList$LocStreet1<-trimws(gsub("(Erzsébetváros|Józsefváros|Terézváros)?(\\, )?Nagykörúton Kívül(i)?( Terület)?"," Nagykörúton Kívüli Terület",TestAdDetailList$LocStreet1,ignore.case = TRUE,perl=TRUE))

TestAdDetailList$LocStreet2<-trimws(gsub("(Erzsébetváros|Józsefváros|Terézváros)?(\\, )?Nagykörúton Belül(i)?( Terület)?"," Nagykörúton Belüli Terület",TestAdDetailList$LocStreet2,ignore.case = TRUE,perl=TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("(Erzsébetváros|Józsefváros|Terézváros)?(\\, )?Nagykörúton Kívül(i)?( Terület)?"," Nagykörúton Kívüli Terület",TestAdDetailList$LocStreet2,ignore.case = TRUE,perl=TRUE))



#TestAdDetailList$LocStreet2<-trimws(gsub("[Belső-|Külső-|Középső-]{0,1}[Józsefváros|Terézváros|Ferencváros|Erzsébetváros]{0,1}[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Nagykörúton Belül[i]{0,1}","Nagykörúton Belüli Terület",TestAdDetailList$LocStreet2,ignore.case = TRUE))
#TestAdDetailList$LocStreet2<-trimws(gsub("[Belső-|Külső-|Középső-]{0,1}[Józsefváros|Terézváros|Ferencváros|Erzsébetváros]{0,1}[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Nagykörúton Kívül[i]{0,1}","Nagykörúton Kívüli Terület",TestAdDetailList$LocStreet2,ignore.case = TRUE))


#TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros, Nagykörúton Belül","Józsefváros Nagykörúton Belül",TestAdDetailList$LocStreet1,ignore.case = TRUE))
#TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros, Nagykörúton Belül","Józsefváros Nagykörúton Belül",TestAdDetailList$LocStreet2,ignore.case = TRUE))

#TestAdDetailList$LocStreet1<-trimws(gsub("Józsefváros, Nagykörúton Kívül","Józsefváros Nagykörúton Kívül",TestAdDetailList$LocStreet1,ignore.case = TRUE))
#TestAdDetailList$LocStreet2<-trimws(gsub("Józsefváros, Nagykörúton Kívül","Józsefváros Nagykörúton Kívül",TestAdDetailList$LocStreet2,ignore.case = TRUE))


TestAdDetailList$LocStreet1<-trimws(gsub("Középső-Ferencváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Rehabilitációs Terület","Középső Ferencváros - Rehabilitációs Terület",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Középső-Ferencváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Rehabilitációs Terület","Középső Ferencváros - Rehabilitációs Terület",TestAdDetailList$LocStreet2,ignore.case = TRUE))

TestAdDetailList$LocStreet1<-trimws(gsub("Középső-Ferencváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Malmok","Középső Ferencváros - Malmok",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub("Középső-Ferencváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}Malmok","Középső Ferencváros - Malmok",TestAdDetailList$LocStreet2,ignore.case = TRUE))


#TestAdDetailList$LocStreet1<-trimws(gsub("Terézváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}","",TestAdDetailList$LocStreet1,ignore.case = TRUE))
#TestAdDetailList$LocStreet2<-trimws(gsub("Terézváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}","",TestAdDetailList$LocStreet2,ignore.case = TRUE))

#TestAdDetailList$LocStreet1<-trimws(gsub("Ferencváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}","",TestAdDetailList$LocStreet1,ignore.case = TRUE))
#TestAdDetailList$LocStreet2<-trimws(gsub("Ferencváros[,]{0,1}[ ]{0,1}[-]{0,1}[ ]{0,1}","",TestAdDetailList$LocStreet2,ignore.case = TRUE))





TestAdDetailList$LocStreet1<-trimws(gsub(" Negyed","negyed",TestAdDetailList$LocStreet1,ignore.case = TRUE))
TestAdDetailList$LocStreet2<-trimws(gsub(" Negyed","negyed",TestAdDetailList$LocStreet2,ignore.case = TRUE))



#TestAdDetailList$LocStreet2<-gsub(".[ ]{0,1}kerület[i]{0,1}","",TestAdDetailList$LocStreet2,ignore.case = TRUE)

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

LocationFreqs2<-LocationFreqs
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


LocationFreqs$drop<-ifelse(LocationFreqs$Total<7,TRUE,FALSE)


LocationFreqsDrop<-LocationFreqs[!LocationFreqs$drop,]



TestAdDetailList<-merge(TestAdDetailList,LocationFreqsDrop[,c('Location','Cat')],by.x="LocStreet1",by.y="Location",all.x=TRUE)
TestAdDetailList$Cat1<-TestAdDetailList$Cat
TestAdDetailList$Cat<-NULL

TestAdDetailList<-merge(TestAdDetailList,LocationFreqsDrop[,c('Location','Cat')],by.x="LocStreet2",by.y="Location",all.x=TRUE)
TestAdDetailList$Cat2<-TestAdDetailList$Cat
TestAdDetailList$Cat<-NULL
TestAdDetailList[is.na(TestAdDetailList$Cat1),'Cat1']<-0
TestAdDetailList[is.na(TestAdDetailList$Cat2),'Cat2']<-0

TestAdDetailList$Location1<-ifelse(TestAdDetailList$Cat1==1,TestAdDetailList$LocStreet1,ifelse(TestAdDetailList$Cat2==1,TestAdDetailList$LocStreet2,"NotSpecified"))
TestAdDetailList$Location2<-ifelse(TestAdDetailList$Cat1==2,TestAdDetailList$LocStreet1,ifelse(TestAdDetailList$Cat2==2,TestAdDetailList$LocStreet2,"NotSpecified"))

LocationTable<-TestAdDetailList[,c('SiteID','Location1','Location2')]

LocationTable[LocationTable$Location1=="NotSpecified",'Location1']<-NA
LocationTable[LocationTable$Location2=="NotSpecified",'Location2']<-NA


ProjectAdDetailList<-merge(ProjectAdDetailList,LocationTable,by.x="SiteID",by.y="SiteID",all.x=TRUE)

#ProjectAdDetailList[ProjectAdDetailList$Location1=="NotSpecified","Location1"]<-NA
#ProjectAdDetailList[ProjectAdDetailList$Location2=="NotSpecified","Location2"]<-NA


saveRDS(ProjectAdDetailList,file = ProjectAdDetailListFile)


#duh=list()
#for(i in 1:nrow(TestAdDetailList2)) {
  
#  TestAdDetailList2[i,'googleAPIURL']=paste("https://maps.googleapis.com/maps/api/geocode/xml?latlng=",TestAdDetailList2[i,'Latitude'],",",TestAdDetailList2[i,'Longitude'],sep="")
#print(TestAdDetailList2[i,'googleAPIURL'])
#duh[i]<-list(readLines(TestAdDetailList2[i,'googleAPIURL']))
#}


#https://maps.googleapis.com/maps/api/geocode/xml?latlng=40.88925,-73.89858

