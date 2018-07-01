library(shiny)
#library(dplyr)
library(rdrop2)
library(ggplot2)
library(shinydashboard)
#dropbox token
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

AnalysisAdDetailListFile<-"ShinyDetailTable.Rda"

if (!file.exists(AnalysisAdDetailListFile)) {
#  drop_download(AnalysisAdDetailListFile,overwrite = TRUE)
  }
#AnalysisAdDetailList<-readRDS(file=AnalysisAdDetailListFile, refhook =NULL)

AnalysisAdListFile<-"ShinyAdlistTable.Rda"
if (!file.exists(AnalysisAdListFile)) {
  drop_download(AnalysisAdListFile,overwrite = TRUE)
  }
AnalysisAdList<-readRDS(file=AnalysisAdListFile, refhook =NULL)


SessionTableFile<-"ShinySessionTable.Rda"
if (!file.exists(SessionTableFile)) {
  drop_download(SessionTableFile,overwrite = TRUE)}
AnalysisSessionTable<-readRDS(file=SessionTableFile, refhook =NULL)
AnalysisSessionTable$ResultNumber<-as.integer(as.character(AnalysisSessionTable$ResultNumber))
AnalysisSessionTable$ReceivedAds<-as.integer(as.character(AnalysisSessionTable$ReceivedAds))

#let's rebuild him

# Pushed back to Analysis.R   Sessions<-AnalysisSessionTable[AnalysisSessionTable$ProjectName=="JofogasLakasokBPFull" & AnalysisSessionTable$ResultNumber>0 & !is.na(AnalysisSessionTable$StartTime) & AnalysisSessionTable$Status=="Completed",c("SessionID","StartTime","ResultNumber","ReceivedAds","NewAdNumber","ClosedAdNumber")]
Sessions<-AnalysisSessionTable # added when the subsetting # Pushed back to Analysis.R 
Sessions$SessionID<-droplevels(Sessions$SessionID) # Probably unnecessary
ListofSnapshotIDs<-levels(Sessions$SessionID)
TitleList<-levels(AnalysisAdList$RC_Title)
ListofSnapshotIDs<-sort(ListofSnapshotIDs, decreasing = TRUE)
#Sessions<-Sessions[order(xtfrm(Sessions$StartTime),desc=T ),, drop = FALSE] # candidate for deletion
#Sessions<-Sessions[with(Sessions, order(xtfrm(Sessions$StartTime)),decreasing=TRUE),] # candidate for deletion

# Pushed back to Analysis.R  Sessions<-Sessions[order(Sessions$StartTime, decreasing = TRUE),] 

#Sessions$StartTime<-format(Sessions$StartTime,"%Y-%m-%d")

Keruletek<-levels(AnalysisAdList$Kerulet)

sqMinSize<-min(AnalysisAdList$MeretCleared)
sqMaxSize<-max(AnalysisAdList$MeretCleared)


sqMinSnapTime<-min(Sessions$StartTime, na.rm=TRUE)
sqMaxSnapTime<-max(Sessions$StartTime,na.rm=TRUE)


print(dim(AnalysisAdList))


ui<-dashboardPage(
  dashboardHeader(title = "RealEstimator dashboard - WIP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters - TrendLines", tabName = "Trends", icon = icon("th"),badgeLabel ="WIP",badgeColor = "yellow"),
      menuItem("Filters(2) - Snapshot", tabName = "Singlesnapshot", icon = icon("th")),
      menuItem("Data Quality", tabName = "TabTechnical", icon = icon("dashboard")),
      menuItem("About", tabName = "About", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "TabTechnical",
              fluidRow(
                box(width=12,
                  #  height=200,
              plotOutput("Snapshots"),
              solidHeader = TRUE,
              collapsible = TRUE,
              title = "Snapshots"
                )),
              fluidRow(
                box(width=12,
                  title="Successful Snapshots",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  status = "primary",
                  tableOutput("SessionTable"))
              )
      ),
      # Second tab content
      tabItem(tabName = "Singlesnapshot",
              fluidRow(
                box(width=3,
                    solidHeader = TRUE,
                    height = 100,
                    h2("Single snapshots")),
                box(width=9,
                    solidHeader = TRUE,
                    height = 100,
                selectInput("SnapshotImput", "Select a snapshot",choices = ListofSnapshotIDs,multiple = FALSE))
                ),
              fluidRow(
                box(width=5,
                  solidHeader = TRUE,
                  height = 150,
                  selectInput("KeruletInput", 
                              "Kerulet",choices = Keruletek,
                              multiple = TRUE,
                              selected=Keruletek
                              )
                  ),
                box(width=3,
                    solidHeader = TRUE,
                    height = 150
   #                 selectInput("TitleInput", "Kerulet",choices = TitleList,multiple = TRUE,selected="Title_Ownership")
                )
                ),
              fluidRow(
              box(title = "Price Histogram",
                  status = "primary",
                  width=6
 #                  plotOutput("PriceHistogram", height = 200)
 )
              ,
                box(title = "District volume",
                    status = "primary",
                    width=6
    #                plotOutput("KeruletDiagram", height = 200)
    )
              )
      ),
      tabItem(tabName = "Trends",title="The data is subsetted on site, please be patient while it loads",
              fluidRow(
                box(width=12,
                    title="Time Range",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    sliderInput("InputSnapTimeRange", label=NULL,
                                min = sqMinSnapTime, max = sqMaxSnapTime,
                                value = c(sqMinSnapTime, sqMaxSnapTime)
                    )
                    )
                ),
                fluidRow(
                  box(solidHeader = TRUE,
                      collapsible = TRUE,
                checkboxGroupInput("InputRangeTitleGroupCheck", label=NULL, choices = TitleList, selected = TitleList,
                                   inline = FALSE)
                  ),
                box(solidHeader = TRUE,
                    collapsible = TRUE,
                    checkboxGroupInput("InputRangeKeruletGroupCheck", label=NULL, choices = Keruletek, selected = Keruletek,
                                       inline = TRUE)
                )
                ),
              fluidRow(
                box(width=12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    sliderInput("PriceSlider", "Price Range",
                                min = 0, max = 1000,
                                value = c(0, 200), post=" million"
                    ),
                    sliderInput("floorInput", "Floor Size",
                                min = 0, max = 1000,
                                value = c(0, 1000), post=" m2"
                    )
                )
                ),
                fluidRow(
                box(
                  width=12,
                  title="Selection Information",
                  solidHeader=TRUE,
                  collapsible=TRUE,
                  infoBoxOutput("NumberofSelectedSnapshots"),
                  infoBoxOutput("NumberofSelectedAds"),
                  infoBoxOutput("NumberofFilteredAds")
                  )
                ),
                fluidRow(
                  box(
                    width=12,
                    title="Selection Top 5",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    tableOutput("FilteredTable")
                  )
                ),
              fluidRow(
                box(
                  width=12,
                  title="Chart goes here but it is Temporarily Disabled. ",
                  solidHeader=TRUE,
                  collapsible=TRUE
                  #plotOutput("PriceBoxPlot")
                )
              )
      ),
      tabItem(tabName = "About",
              fluidRow(
                box(width=12,
                    title="About",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    "This Shiny app was made by István Juhász",
                    br(),
                    "The purpose of this application to stand as an interactive dashboard for the data collected by the RealEstimator webcrawler.The source code of the crawler may be fetched from https://github.com/JuhaszIstvan/RealEstimator",
                    "Questions and inquiries shall be posted at istvan.juhasz at gmail.com"
                    )
              )

      )      
      
      
    )
  )
)

server <- function(input, output) {
#  filtered <-reactive({
#    FilteredSnapSet() %>%
#    filter(MeretCleared >= input$floorInput[1],
#           MeretCleared <= input$floorInput[2],
#           RC_Title == input$TitleInput,
#           Kerulet == input$KeruletInput
#    )
#  })
  
  FilteredSnapShots<-reactive({
    Sessions[Sessions$StartTime >= input$InputSnapTimeRange[1] & Sessions$StartTime<=input$InputSnapTimeRange[2],]
  })
  
  RangedDataSet<-reactive({
    # Pushed back to Analysis.R TempSet<-AnalysisAdList[AnalysisAdList$SessionID %in% FilteredSnapShots()$SessionID,c("SiteID","SessionID","QueryTime")]
    AnalysisAdList[AnalysisAdList$SessionID %in% FilteredSnapShots()$SessionID,] # Added after the above line was # Pushed back to Analysis.R
    # Pushed back to Analysis.R TempSet<-merge(x=TempSet,y=Sessions[,c("SessionID","StartTime")],by=c("SessionID"),all.x = TRUE,all.y=FALSE)
    # Pushed back to Analysis.R TempSet<-merge(x=TempSet,y=AnalysisAdDetailList,by=c("SiteID","QueryTime"),all.x = TRUE,all.y=FALSE)
    # Pushed back to Analysis.R TempSet$SnapshotDate<-as.Date(TempSet$StartTime)
    })
  
  FilteredRangedDataSet<-reactive({
    RangedDataSet()[RangedDataSet()$RC_Title %in% input$InputRangeTitleGroupCheck & RangedDataSet()$Kerulet %in% input$InputRangeKeruletGroupCheck & RangedDataSet()$MeretCleared >= input$floorInput[1] & RangedDataSet()$MeretCleared <= input$floorInput[2] & RangedDataSet()$RC_ArMil >= input$PriceSlider[1] & RangedDataSet()$RC_ArMil <= input$PriceSlider[2],]
  })
  
  
  output$NumberofSelectedSnapshots <- renderInfoBox({
    infoBox(
      "Snapshots", nrow(FilteredSnapShots()), icon = icon("list"),
      color = "purple"
    )})
  output$NumberofSelectedAds <- renderInfoBox({
    infoBox(
      "Ads", nrow(RangedDataSet()), icon = icon("list"),
      color = "orange"
    )})
  output$NumberofFilteredAds <- renderInfoBox({
    infoBox(
      "Filtered", nrow(FilteredRangedDataSet()), icon = icon("list"),
      color = "blue"
    )})
  

  #QueryEndTime<-reactive({
  #  AnalysisSessionTable[AnalysisSessionTable$SessionID==input$SnapshotImput,'QueryAdDetailEndTime']
  #  })
  #observe({ print(QueryEndTime()) })
  
#  FilteredSnapSet<-reactive({
  #AnalysisAdList[AnalysisAdList$SessionID==input$SnapshotImput,'SiteID']
 # FilteredAdList<-AnalysisAdList[AnalysisAdList$SessionID==input$SnapshotImput,'SiteID']
  #FilteredAdList
  #FilteredAdDetailList<-AnalysisAdDetailList[AnalysisAdDetailList$SiteID %in% FilteredAdList,]
  #FilteredAdDetailList$Deviance<-ifelse(QueryEndTime()-FilteredAdDetailList$QueryTime>0,QueryEndTime()-FilteredAdDetailList$QueryTime,9999999999999)
  #FilteredAdDetailList<-FilteredAdDetailList[with(FilteredAdDetailList, order(Deviance)),]
  #FilteredAdDetailList<-FilteredAdDetailList[!duplicated(FilteredAdDetailList$SiteID),]
  #AnalysisAdDetailList<-AnalysisAdDetailList[!duplicated(AnalysisAdDetailList[c("MeretCleared","RC_Description","Kerulet")]),]
  #FilteredAdDetailList
  #})
  #observe({ print(paste("filtereddataset",class(FilteredRangedDataSet()$QueryTime)))})
  TempSessions<-Sessions
  TempSessions$StartTime <- format(TempSessions$StartTime,'%Y-%m-%d')
  output$SessionTable <-renderTable({TempSessions})

#output$Snapshots<- renderPlot({
#  ggplot(data=Sessions,aes(x=StartTime))+
#    geom_line(aes(y=ResultNumber,group=1,colour="Ads reported"))+
#    geom_line(aes(y=ReceivedAds,group=2, colour="Ads collected"))+
#    scale_x_datetime(name = 'Snapshot Date')+
#    xlab("Snapshot date")
#})

#output$PriceBoxPlot <- renderPlot({
#  ggplot(data = FilteredRangedDataSet(), mapping = aes(x = SnapshotDate, y = RC_ArMil, group= SnapshotDate)) + 
#    scale_x_date(name = 'SnapshotDate') +
#    geom_boxplot()
#})


#output$PriceHistogram <- renderPlot({
#  ggplot(filtered(), aes(RC_ArMil)) +
#    geom_histogram()
#})

#output$KeruletDiagram <- renderPlot({
#  ggplot(data=filtered())+
#    geom_bar(mapping = aes(x = Kerulet), fill='orange')
#})

#output$FilteredTable <-renderTable({head(FilteredRangedDataSet())})

}
shinyApp(ui = ui, server = server)