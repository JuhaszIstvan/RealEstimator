library(shiny)
library(dplyr)
library(rdrop2)
library(ggplot2)
library(shinydashboard)
#dropbox token
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)


AnalysisAdDetailListFile<-"exportedversion.Rda"
if (!file.exists(AnalysisAdDetailListFile)) {
  drop_download(AnalysisAdDetailListFile,overwrite = TRUE)}

AnalysisAdDetailList<-readRDS(file=AnalysisAdDetailListFile, refhook =NULL)

AnalysisAdListFile<-"FullBudapestAdLists.Rda"
if (!file.exists(AnalysisAdListFile)) {
  drop_download(AnalysisAdListFile,overwrite = TRUE)}
AnalysisAdList<-readRDS(file=AnalysisAdListFile, refhook =NULL)


SessionTableFile<-"SessionTable.Rda"
if (!file.exists(SessionTableFile)) {
  drop_download(SessionTableFile,overwrite = TRUE)}
AnalysisSessionTable<-readRDS(file="SessionTable.Rda", refhook =NULL)
AnalysisSessionTable$ResultNumber<-as.numeric(as.character(AnalysisSessionTable$ResultNumber))
AnalysisSessionTable$ReceivedAds<-as.numeric(as.character(AnalysisSessionTable$ReceivedAds))

Sessions<-AnalysisSessionTable[AnalysisSessionTable$ProjectName=="JofogasLakasokBPFull" & AnalysisSessionTable$ResultNumber>0 & !is.na(AnalysisSessionTable$StartTime),c("SessionID","StartTime","ResultNumber","ReceivedAds","NewAdNumber","ClosedAdNumber")]
Sessions$SessionID<-droplevels(Sessions$SessionID)
ListofSnapshotIDs<-levels(Sessions$SessionID)
TitleList<-levels(AnalysisAdDetailList$RC_Title)
ListofSnapshotIDs<-sort(ListofSnapshotIDs, decreasing = TRUE)
#Sessions<-Sessions[order(xtfrm(Sessions$StartTime),desc=T ),, drop = FALSE]
#Sessions<-Sessions[with(Sessions, order(xtfrm(Sessions$StartTime)),decreasing=TRUE),]

Sessions<-Sessions[order(Sessions$StartTime, decreasing = TRUE),]


#Sessions$StartTime<-format(Sessions$StartTime,"%Y-%m-%d")
#Sessions <-reactive({
#  AnalysisSessionTable[AnalysisSessionTable$ProjectName=="JofogasLakasokBPFull",c("ResultNumber","StartTime","ReceivedAds","NewAdNumber","ClosedAdNumber")]
#})
print(dim(Sessions))

Keruletek<-levels(AnalysisAdDetailList$Kerulet)

sqMinSize<-min(AnalysisAdDetailList$MeretCleared)
sqMaxSize<-max(AnalysisAdDetailList$MeretCleared)


sqMinSnapTime<-min(Sessions$StartTime)
sqMaxSnapTime<-max(Sessions$StartTime)

  AnalysisAdDetailList$MeretCleared
print(dim(AnalysisAdDetailList))


ui<-dashboardPage(
  dashboardHeader(title = "RealEstimator dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters - Snapshot", tabName = "Singlesnapshot", icon = icon("th")),
      menuItem("Trends", tabName = "Trends", icon = icon("th"),badgeLabel ="inactive",badgeColor = "black"),
      menuItem("Technical", tabName = "TabTechnical", icon = icon("dashboard")),
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
                box(width=4,
                    solidHeader = TRUE,
                    height = 150,
                sliderInput("floorInput", "Floor Size",
                            min = sqMinSize, max = 1000,
                            value = c(sqMinSize, 1000), post=" m2"
                            )
                ),
                box(width=3,
                    solidHeader = TRUE,
                    height = 150,
                    selectInput("TitleInput", "Kerulet",choices = TitleList,multiple = TRUE,selected="Title_Ownership")
                )
                ),
              fluidRow(
              box(title = "Price Histogram",
                  status = "primary",
                  width=6,
                  plotOutput("PriceHistogram", height = 200))
              ,
                box(title = "District volume",
                    status = "primary",
                    width=6,
                    plotOutput("KeruletDiagram", height = 200))
              )
      ),
      tabItem(tabName = "Trends",
              fluidRow(
                box(width=12,
                    title="Trends",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    sliderInput("TimeRangeInput", "Time Range",
                                min = sqMinSnapTime, max = sqMaxSnapTime,
                                value = c(sqMinSnapTime, sqMaxSnapTime)
                    )
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
  filtered <-reactive({
    FilteredSnapSet() %>%
    filter(MeretCleared >= input$floorInput[1],
           MeretCleared <= input$floorInput[2],
           RC_Title == input$TitleInput,
           Kerulet == input$KeruletInput
    )
  })
  QueryEndTime<-reactive({
    AnalysisSessionTable[AnalysisSessionTable$SessionID==input$SnapshotImput,'QueryAdDetailEndTime']
    })
  observe({ print(QueryEndTime()) })
  
  FilteredSnapSet<-reactive({
  #AnalysisAdList
  #AnalysisAdList[AnalysisAdList$SessionID==input$SnapshotImput,'SiteID']
  FilteredAdList<-AnalysisAdList[AnalysisAdList$SessionID==input$SnapshotImput,'SiteID']
  #FilteredAdList
  FilteredAdDetailList<-AnalysisAdDetailList[AnalysisAdDetailList$SiteID %in% FilteredAdList,]
  FilteredAdDetailList$Deviance<-ifelse(QueryEndTime()-FilteredAdDetailList$QueryTime>0,QueryEndTime()-FilteredAdDetailList$QueryTime,9999999999999)
  FilteredAdDetailList<-FilteredAdDetailList[with(FilteredAdDetailList, order(Deviance)),]
  FilteredAdDetailList<-FilteredAdDetailList[!duplicated(FilteredAdDetailList$SiteID),]
  #AnalysisAdDetailList<-AnalysisAdDetailList[!duplicated(AnalysisAdDetailList[c("MeretCleared","RC_Description","Kerulet")]),]
  FilteredAdDetailList
  })
  observe({ print(paste("filtereddataset",dim(FilteredSnapSet())))})
  TempSessions<-Sessions
  TempSessions$StartTime <- format(TempSessions$StartTime,'%Y-%m-%d')
  output$SessionTable <-renderTable({TempSessions})

output$Snapshots<- renderPlot({
  ggplot(data=Sessions,aes(x=StartTime))+
    geom_line(aes(y=ResultNumber,group=1,colour="Ads reported"))+
    geom_line(aes(y=ReceivedAds,group=2, colour="Ads collected"))+
    scale_x_datetime(name = 'Snapshot Date')+
    xlab("Snapshot date")
})


output$PriceHistogram <- renderPlot({
  ggplot(filtered(), aes(RC_ArMil)) +
    geom_histogram()
})

output$KeruletDiagram <- renderPlot({
  ggplot(data=filtered())+
    geom_bar(mapping = aes(x = Kerulet), fill='orange')
})


#pCT<-ggplot(data = TrainingSet, mapping = aes(x = RC_Title, y = RC_ArMil)) + 
#  geom_boxplot() +
#  coord_cartesian(ylim = c(0, 100))



}
shinyApp(ui = ui, server = server)