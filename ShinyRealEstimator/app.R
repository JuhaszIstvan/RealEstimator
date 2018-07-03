library(shiny)
#library(dplyr)
library(rdrop2)
library(ggplot2)
library(shinydashboard)
#library(ggvis)
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
Sessions<-AnalysisSessionTable[!is.na(AnalysisSessionTable$SessionID),] # added when the subsetting # Pushed back to Analysis.R 
Sessions$SessionID<-droplevels(Sessions$SessionID) # Probably unnecessary
ListofSnapshotIDs<-levels(Sessions$SessionID)
TitleList<-levels(AnalysisAdList$RC_Title)
ConditionList<-levels(AnalysisAdList$RC_Condition)
ParkingList<-levels(AnalysisAdList$RC_PARKING)
MaterialList<-levels(AnalysisAdList$RC_Material)
HeatingList<-levels(AnalysisAdList$RC_Heating)
ListofSnapshotIDs<-sort(ListofSnapshotIDs, decreasing = TRUE)
#Sessions<-Sessions[order(xtfrm(Sessions$StartTime),desc=T ),, drop = FALSE] # candidate for deletion
#Sessions<-Sessions[with(Sessions, order(xtfrm(Sessions$StartTime)),decreasing=TRUE),] # candidate for deletion

# Pushed back to Analysis.R  Sessions<-Sessions[order(Sessions$StartTime, decreasing = TRUE),] 

#Sessions$StartTime<-format(Sessions$StartTime,"%Y-%m-%d")

Keruletek<-levels(AnalysisAdList$Kerulet)

sqMinSize<-min(AnalysisAdList$MeretCleared)
sqMaxSize<-max(AnalysisAdList$MeretCleared)


sqMinSnapTime<-min(Sessions$SnapshotDate, na.rm=TRUE)
sqMaxSnapTime<-max(Sessions$SnapshotDate,na.rm=TRUE)


print(dim(AnalysisAdList))


ui<-dashboardPage(
  dashboardHeader(title = "RealEstimator dashboard - WIP"),
  dashboardSidebar(
    width=350,
    sidebarMenu(
      menuItem("Filters - TrendLines", tabName = "Trends", icon = icon("th"),badgeLabel ="WIP",badgeColor = "yellow"),
      menuItem("Cross Section Charts", tabName = "Charts", icon = icon("th"),badgeLabel ="WIP",badgeColor = "orange"),
      menuItem("Data Quality", tabName = "TabTechnical", icon = icon("dashboard")),
      menuItem("About", tabName = "About", icon = icon("th"))
    ),
    fluidRow(
      box(width=12,
          background="black",
          solidHeader = TRUE,
          collapsible = TRUE,
          sliderInput("PriceSlider", "Price Range",
                      min = 0, max = 1000,
                      value = c(5, 200), post=" million"
          ),
          sliderInput("floorInput", "Floor Size",
                      min = 0, max = 1000,
                      value = c(20, 1000), post=" m2"
          )
      )
    )
    ,
    fluidRow(
    box(width=12,
        background="black",
        collapsible = TRUE,
    box(solidHeader = TRUE,
        #collapsible = TRUE,
        background="black",
        width=8,
        checkboxGroupInput("InputRangeTitleGroupCheck", label="Type of right", choices = TitleList, selected = TitleList,
                           inline = FALSE),
        checkboxGroupInput("ConditionGroupCheck", label="Condition", choices = ConditionList, selected = ConditionList,
                           inline = FALSE),
        checkboxGroupInput("HeatingGroupCheck", label="Heating Type", choices = HeatingList, selected = HeatingList,
                           inline = FALSE),
        checkboxGroupInput("ParkingGroupCheck", label="Parking", choices = ParkingList, selected = ParkingList,
                           inline = FALSE)
    ),
    box(solidHeader = TRUE,
        #collapsible = TRUE,
        background="black",
        width=4,
        checkboxGroupInput("KeruletGroupCheck", label="District", choices = Keruletek, selected = Keruletek,
                           inline = FALSE),
        checkboxGroupInput("MaterialGroupCheck", label="Building Material", choices = MaterialList, selected = MaterialList,
                           inline = FALSE)

    )
    )
  )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.info-box {min-height: 50px; min-width: 50px;} .info-box-icon {width: 45px; height: 45px; line-height: 45px;} .info-box-content {padding-right: 0px;padding-left: 0px;padding-top: 0px; padding-bottom: 0px;}'))),
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
      tabItem(tabName = "Charts",
        fluidRow(
        box(title = "Price Histogram",
            status = "primary",
            width=6,
              plotOutput("PriceHistogram", height = 200)
        )
        ,
          box(title = "District volume",
              status = "primary",
              width=6,
              plotOutput("KeruletDiagram", height = 200)
          )
        ),
        fluidRow(
        box(title = "Price-FloorSize",
            status = "primary",
            collapsible = TRUE,
            width=12,
            plotOutput("PriceFloorSize", height = 300)
        )
        ),
        fluidRow(
        box(title="Categorical Descriptiors",
            collapsed = TRUE,
          width=12,
            collapsible = TRUE,
      fluidRow(
        box(title = "Type of right",
          status = "primary",
          width=4,
          plotOutput("TitleBoxPlot", height = 200)
          ),
        box(title = "Condition",
            status = "primary",
            width=8,    
        plotOutput("ConditionBoxPlot", height = 200)
        )
      ),
      fluidRow(
        box(title = "Building Material",
            status = "primary",
            width=4,
            plotOutput("MaterialBoxPlot", height = 200)
        ),
        box(title = "Floor level",
            status = "primary",
            width=8,    
            plotOutput("EtageBoxPlot", height = 200)
        )
      ),
      fluidRow(
        box(title = "Parking Options",
            status = "primary",
            width=4,    
            plotOutput("ParkingBoxPlot", height = 200)
        ),
        box(title = "Type of Heating",
            status = "primary",
            width=8,    
            plotOutput("HeatingBoxPlot", height = 200)
        )
      )
      )
        )
    )
    ,
      tabItem(tabName = "Trends",title="The data is subsetted on site, please be patient while it loads",
              fluidRow(
                box(width=12,
                    title="Time Range",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    sliderInput("InputSnapTimeRange", label=NULL,
                                min = sqMinSnapTime, max = sqMaxSnapTime,
                                value = c(sqMaxSnapTime, sqMaxSnapTime)
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
                  title="Selection Price Graph",
                  solidHeader=TRUE,
                  collapsible=TRUE,
                  plotOutput("PriceBoxPlot")
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
                    "The purpose of this application to stand as an interactive dashboard for the data collected by the RealEstimator webcrawler.The source code of the crawler, the processer and Shiny app can be fetched from https://github.com/JuhaszIstvan/RealEstimator",br(),
                    "It is still very much work in progress, seeing how I am new to the technology, this must have come without much surpise, really. The ultimate goal is to add a the output of the machine learning exercise.",br(),
                    "Due to the exploratory nature of the this exercise,  items do tend to wander withing sections producing unfeasible configurations. Terribly sorry about that.",br(),
                    "Questions and comments could be posted at istvan.juhasz at gmail.com"
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
    Sessions[Sessions$SnapshotDate >= input$InputSnapTimeRange[1] & Sessions$SnapshotDate<=input$InputSnapTimeRange[2],,drop=F]
  })
  observe({ print(paste(input$InputSnapTimeRange[1],input$InputSnapTimeRange[2])) })
  
  
  RangedDataSet<-reactive({
    # Pushed back to Analysis.R TempSet<-AnalysisAdList[AnalysisAdList$SessionID %in% FilteredSnapShots()$SessionID,c("SiteID","SessionID","QueryTime")]
    AnalysisAdList[AnalysisAdList$SessionID %in% FilteredSnapShots()$SessionID,] # Added after the above line was # Pushed back to Analysis.R
    # Pushed back to Analysis.R TempSet<-merge(x=TempSet,y=Sessions[,c("SessionID","StartTime")],by=c("SessionID"),all.x = TRUE,all.y=FALSE)
    # Pushed back to Analysis.R TempSet<-merge(x=TempSet,y=AnalysisAdDetailList,by=c("SiteID","QueryTime"),all.x = TRUE,all.y=FALSE)
    # Pushed back to Analysis.R TempSet$SnapshotDate<-as.Date(TempSet$StartTime)
    })
  
  FilteredRangedDataSet<-reactive({
    RangedDataSet()[RangedDataSet()$RC_Title %in% input$InputRangeTitleGroupCheck & RangedDataSet()$Kerulet %in% input$KeruletGroupCheck & RangedDataSet()$MeretCleared >= input$floorInput[1] & RangedDataSet()$MeretCleared <= input$floorInput[2] & RangedDataSet()$RC_ArMil >= input$PriceSlider[1] & RangedDataSet()$RC_ArMil <= input$PriceSlider[2] & RangedDataSet()$RC_Condition %in% input$ConditionGroupCheck & RangedDataSet()$RC_Heating %in% input$HeatingGroupCheck & RangedDataSet()$RC_Material %in% input$MaterialGroupCheck & RangedDataSet()$RC_PARKING %in% input$ParkingGroupCheck,]
  })
  
  
  output$NumberofSelectedSnapshots <- renderInfoBox({
    infoBox(
      "Snapshots", nrow(FilteredSnapShots()), icon = icon("list"),
      color = "purple"
    )})
  output$NumberofSelectedAds <- renderInfoBox({
    infoBox(
      "Total Collected Ads", nrow(RangedDataSet()), icon = icon("list"),
      color = "orange"
    )})
  output$NumberofFilteredAds <- renderInfoBox({
    infoBox(
      "Filtered Ads", nrow(FilteredRangedDataSet()), icon = icon("list"),
      color = "blue"
    )})
  

  #QueryEndTime<-reactive({
  #  AnalysisSessionTable[AnalysisSessionTable$SessionID==input$SnapshotImput,'QueryAdDetailEndTime']
  #  })

  
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
  TempSessions$SnapshotDate <- format(TempSessions$SnapshotDate,'%Y-%m-%d')
  TempSessions<-TempSessions[,c("SessionID","SnapshotDate","ResultNumber","ReceivedAds","NewAdNumber","MissRatePercent","ClosedAdNumber")]
  output$SessionTable <-renderTable({TempSessions})

output$Snapshots<- renderPlot({
  ggplot(data=Sessions,aes(x=StartTime))+
    geom_line(aes(y=ResultNumber,group=1,colour="Ads reported"))+
    geom_line(aes(y=ReceivedAds,group=2, colour="Ads collected"))+
    scale_x_datetime(name = 'Snapshot Date')+
    xlab("Snapshot date")
})

output$PriceBoxPlot <- renderPlot({
  ggplot(data = FilteredRangedDataSet(), mapping = aes(x = SnapshotDate, y = RC_ArMil, group= SnapshotDate)) + 
    scale_x_date(name = 'SnapshotDate') +
    geom_boxplot()
})


output$TitleBoxPlot<- renderPlot({
ggplot(data = FilteredRangedDataSet(), mapping = aes(x = RC_Title, y = RC_ArMil)) +
  geom_boxplot() +
    coord_cartesian(ylim = c(0, 100))
})

# output$TitleBoxPlot<- renderPlot({
#   FilteredRangedDataSet() %>% ggvis(~RC_Title, ~RC_ArMil) %>% layer_boxplots()
# })




output$ParkingBoxPlot<- renderPlot({
  ggplot(data = FilteredRangedDataSet(), mapping = aes(x = RC_PARKING, y = RC_ArMil)) + 
    geom_boxplot() +
    coord_cartesian(ylim = c(0, 100))
})
output$HeatingBoxPlot<- renderPlot({
  ggplot(data = FilteredRangedDataSet(), mapping = aes(x = RC_Heating, y = RC_ArMil)) + 
    geom_boxplot() +
    coord_cartesian(ylim = c(0, 100))
})
output$MaterialBoxPlot<- renderPlot({
  ggplot(data = FilteredRangedDataSet(), mapping = aes(x = RC_Material, y = RC_ArMil)) + 
    geom_boxplot() +
    coord_cartesian(ylim = c(0, 100))
})

output$ConditionBoxPlot<- renderPlot({
  ggplot(data = FilteredRangedDataSet(), mapping = aes(x = RC_Condition, y = RC_ArMil)) + 
    geom_boxplot() +
    coord_cartesian(ylim = c(0, 100))
})


output$EtageBoxPlot <- renderPlot({
ggplot(data = FilteredRangedDataSet(), mapping = aes(x = RC_FLOOR, y = RC_ArMil)) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 100))
})



output$PriceFloorSize <- renderPlot({

  ggplot(data=FilteredRangedDataSet())+
    geom_point(mapping = aes(x=MeretCleared, y=RC_ArMil), position="jitter")+
    geom_smooth(mapping = aes(x = MeretCleared, y = RC_ArMil, group = Kerulet))
  })

output$PriceHistogram <- renderPlot({
  ggplot(FilteredRangedDataSet(), aes(RC_ArMil)) +
    geom_histogram()
})

output$KeruletDiagram <- renderPlot({
  ggplot(data=FilteredRangedDataSet())+
    geom_bar(mapping = aes(x = Kerulet), fill='orange')
})

output$FilteredTable <-renderTable({head(FilteredRangedDataSet()[, -which(names(FilteredRangedDataSet()) %in% c("SessionID", "StartTime","QueryTime","SnapshotDate"))])})

}
shinyApp(ui = ui, server = server)