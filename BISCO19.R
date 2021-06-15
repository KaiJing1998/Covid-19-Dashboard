library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(scales)
library(ggpubr)


#Connect to database
mydb <- dbConnect(MySQL(),
                  user = 'techvest_covidAdmin',
                  password = 'gRBOp;$[F[!g',
                  dbname = 'techvest_covid',
                  host = '143.198.217.144'
)

#Retrieve data from MYSQL
dbListTables(mydb)



dbListFields(mydb,'DAILYCASES')
rs01 = dbSendQuery(mydb, "SELECT `Daily New Cases` FROM DAILYCASES ORDER BY `Date` DESC LIMIT 1")
dailynewcases = fetch (rs01, n= -1)

dbListFields(mydb,'DAILYCASES')
rs02 = dbSendQuery(mydb, "SELECT `Daily Deaths` FROM DAILYCASES ORDER BY `Date` DESC LIMIT 1")
dailydeaths = fetch (rs02, n= -1)

dbListFields(mydb,'DAILYCASES')
rs03 = dbSendQuery(mydb, "SELECT `Daily Recovery` FROM DAILYCASES ORDER BY `Date` DESC LIMIT 1")
dailyrecovery = fetch (rs03, n= -1)

dbListFields(mydb,'DAILYCASES')
rs04 = dbSendQuery(mydb, "SELECT `Active Cases` FROM DAILYCASES ORDER BY `Date` DESC LIMIT 1")
activecases = fetch (rs04, n= -1)

dbListFields(mydb,'DAILYCASES')
rs = dbSendQuery(mydb, "SELECT * FROM DAILYCASES")
datatable = fetch (rs, n= -1)

dbListFields(mydb,'STATECASES')
rs1 = dbSendQuery(mydb, "SELECT * FROM STATECASES")
state_cases_table = fetch (rs1, n= -1)

dbListFields(mydb,'DAILYSTATECASES')
rs2 = dbSendQuery(mydb, "SELECT * FROM DAILYSTATECASES")
daily_state_cases_table = fetch (rs2, n= -1)

dbListFields(mydb,'SELANGOR_DISTRICT')
rs3 = dbSendQuery(mydb, "SELECT * FROM SELANGOR_DISTRICT ")
selangor_district = fetch (rs3, n= -1)

dbListFields(mydb,'SELANGOR')
rs4 = dbSendQuery(mydb, "SELECT * FROM SELANGOR")
selangor = fetch (rs4, n= -1)

dbListFields(mydb,'KUALALUMPURDISTRICT')
rs5 = dbSendQuery(mydb, "SELECT * FROM KUALALUMPURDISTRICT")
kualalumpur_district = fetch (rs5, n= -1)

dbListFields(mydb,'KUALALUMPUR')
rs6 = dbSendQuery(mydb, "SELECT * FROM KUALALUMPUR")
kualalumpur = fetch (rs6, n= -1)

dbListFields(mydb,'JOHOR_DISTRICT')
rs7 = dbSendQuery(mydb, "SELECT * FROM JOHOR_DISTRICT")
johor_district = fetch (rs7, n= -1)

dbListFields(mydb,'JOHOR')
rs8 = dbSendQuery(mydb, "SELECT * FROM JOHOR")
johor= fetch (rs8, n= -1)

dbListFields(mydb,'PERLIS_DISTRICT')
rs9 = dbSendQuery(mydb, "SELECT * FROM PERLIS_DISTRICT")
perlis_district = fetch (rs9, n= -1)

dbListFields(mydb,'PERLIS')
rs10 = dbSendQuery(mydb, "SELECT * FROM PERLIS")
perlis = fetch (rs10, n= -1)

dbListFields(mydb,'PPINANG_DISTRICT')
rs11 = dbSendQuery(mydb, "SELECT * FROM PPINANG_DISTRICT")
penang_district = fetch (rs11, n= -1)

dbListFields(mydb,'PPINANG')
rs12 = dbSendQuery(mydb, "SELECT * FROM PPINANG")
penang = fetch (rs12, n= -1)

dbListFields(mydb,'KEDAH_DISTRICT')
rs13 = dbSendQuery(mydb, "SELECT * FROM KEDAH_DISTRICT")
kedah_district = fetch (rs13, n= -1)

dbListFields(mydb,'KEDAH')
rs14 = dbSendQuery(mydb, "SELECT * FROM KEDAH")
kedah = fetch (rs14, n= -1)

dbListFields(mydb,'KELANTAN_DISTRICT')
rs15 = dbSendQuery(mydb, "SELECT * FROM KELANTAN_DISTRICT")
kelantan_district = fetch (rs15, n= -1)

dbListFields(mydb,'KELANTAN')
rs16 = dbSendQuery(mydb, "SELECT * FROM KELANTAN")
kelantan = fetch (rs16, n= -1)

dbListFields(mydb,'TERENGGANU_DISTRICT')
rs17 = dbSendQuery(mydb, "SELECT * FROM TERENGGANU_DISTRICT")
terengganu_district = fetch (rs17, n= -1)

dbListFields(mydb,'TERENGGANU')
rs18 = dbSendQuery(mydb, "SELECT * FROM TERENGGANU")
terengganu= fetch (rs18, n= -1)

dbListFields(mydb,'MELAKA_DISTRICT')
rs19 = dbSendQuery(mydb, "SELECT * FROM MELAKA_DISTRICT")
melaka_district = fetch (rs19, n= -1)

dbListFields(mydb,'MELAKA')
rs20 = dbSendQuery(mydb, "SELECT * FROM MELAKA")
melaka = fetch (rs20, n= -1)

dbListFields(mydb,'PAHANG_DISTRICT')
rs21 = dbSendQuery(mydb, "SELECT * FROM PAHANG_DISTRICT")
pahang_district = fetch (rs21, n= -1)

dbListFields(mydb,'PAHANG')
rs22 = dbSendQuery(mydb, "SELECT * FROM PAHANG")
pahang = fetch (rs22, n= -1)

dbListFields(mydb,'LABUAN_DISTRICT')
rs23 = dbSendQuery(mydb, "SELECT * FROM LABUAN_DISTRICT")
labuan_district = fetch (rs23, n= -1)

dbListFields(mydb,'LABUAN')
rs24 = dbSendQuery(mydb, "SELECT * FROM LABUAN")
labuan = fetch (rs24, n= -1)

dbListFields(mydb,'PUTRAJAYA_DISTRICT')
rs25 = dbSendQuery(mydb, "SELECT * FROM PUTRAJAYA_DISTRICT")
putrajaya_district = fetch (rs25, n= -1)

dbListFields(mydb,'PUTRAJAYA')
rs26 = dbSendQuery(mydb, "SELECT * FROM PUTRAJAYA")
putrajaya = fetch (rs26, n= -1)

dbListFields(mydb,'NSEMBILAN_DISTRICT')
rs27 = dbSendQuery(mydb, "SELECT * FROM NSEMBILAN_DISTRICT")
nsembilan_district = fetch (rs27, n= -1)

dbListFields(mydb,'NSEMBILAN')
rs28 = dbSendQuery(mydb, "SELECT * FROM NSEMBILAN")
nsembilan = fetch (rs28, n= -1)

dbListFields(mydb,'SABAH_DISTRICT')
rs29 = dbSendQuery(mydb, "SELECT * FROM SABAH_DISTRICT")
sabah_district = fetch (rs29, n= -1)

dbListFields(mydb,'SABAH')
rs30 = dbSendQuery(mydb, "SELECT * FROM SABAH")
sabah = fetch (rs30, n= -1)

dbListFields(mydb,'SARAWAK_DISTRICT')
rs31 = dbSendQuery(mydb, "SELECT * FROM SARAWAK_DISTRICT")
sarawak_district = fetch (rs31, n= -1)

dbListFields(mydb,'SARAWAK')
rs32 = dbSendQuery(mydb, "SELECT * FROM SARAWAK")
sarawak = fetch (rs32, n= -1)


# Avoid all scientific notation
options(scipen=999)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Covid-19 Malaysia Dashboard",
                  titleWidth = 300
  ),
  footer = dashboardFooter(
    left = "Copyright 2021",
    right = "Source: CPRC, KKM, Worldometers, covid19.place, WHO "
  ),
  
  dashboardSidebar(
    width = 300,
    
    sidebarMenu(
      img(src = "Capture.PNG", align = "center"),
      menuItem("Dashboard", tabName = "home", icon = icon("dashboard")),
      menuItem("State in Malaysia", tabName = "state", icon = icon("flag-checkered")),
      menuSubItem("Kuala Lumpur",tabName = "kualalumpur"),
      menuSubItem("Selangor",tabName = "selangor"),
      menuSubItem("Johor",tabName = "johor"),
      menuSubItem("Penang",tabName = "penang"),
      menuSubItem("Perlis",tabName = "perlis"),
      menuSubItem("Kedah",tabName = "kedah"),
      menuSubItem("Kelantan",tabName = "kelantan"),
      menuSubItem("Terengganu",tabName = "terengganu"),
      menuSubItem("Melaka",tabName = "melaka"),
      menuSubItem("Pahang",tabName = "pahang"),
      menuSubItem("Labuan",tabName = "labuan"),
      menuSubItem("Putrajaya",tabName = "putrajaya"),
      menuSubItem("Negeri Sembilan",tabName = "negerisembilan"),
      menuSubItem("Sabah",tabName = "sabah"),
      menuSubItem("Sarawak",tabName = "Sarawak"),
      menuItem("Symptoms of Covid-19", tabName = "symptoms", icon = icon("hand-holding-medical"))
    )
  ),
  dashboardBody(
   
    
    fluidRow(
      fluidPage(
      "updated",
      em("June 14, 2021"),
      br(),
      
      # A static value Box
      valueBox(dailynewcases,"New Cases", color = "red", width = 3, icon = icon("arrow-alt-circle-up")),
      valueBox(dailydeaths,"New Deaths", color = "olive",width = 3, icon = icon("arrow-alt-circle-up")),
      valueBox(dailyrecovery,"New Recovered", color = "light-blue",width = 3, icon = icon("arrow-alt-circle-up")),
      valueBox(activecases,"Active Cases", color = "navy", width = 3, icon = icon("arrow-alt-circle-up")),
      )
    ),
   
    
    tabItems(
      
      tabItem("home",
              tabsetPanel(
                tabPanel("Daily New Cases",plotlyOutput("lineChart2")),
                tabPanel("Active Cases",plotlyOutput("lineChart3")),
                tabPanel("Total Cases",plotlyOutput("lineChart")),
                type = "tab"
                
              ),
              br(),
              
              tabsetPanel(
                tabPanel("Daily Deaths",plotlyOutput("lineChart5")),
                tabPanel("Total Deaths",plotlyOutput("lineChart4")),
                type = "tab"
                
              ),
              
              br(),     
              
              fluidRow(
                tabItem("home",
                        box(title = "State Confirmed Cases",background = "black", solidHearder = TRUE,
                            collapsible = TRUE,
                            plotlyOutput("stateBarGraph"))),
                
                box(title = "Correlation Analysis between Daily Confirmed and Daily Deaths Cases in Malaysia",
                    background = "green", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotlyOutput("correlationGraph"))
                
                
               
              ),
              
              br(),
              
             
                tabItem("home",
                       plotlyOutput("linestate")),
             
      ),
      
      tabItem("state",
              fluidPage(
                h2("State in Malaysia"),
                "updated",
                em("June 14, 2021"),
                br(),
                dataTableOutput("state_cases")
              )
      ),
      
      tabItem("kualalumpur",
              fluidPage(
                h2("Kuala Lumpur"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("kualaLumpurDistrict"),
                br(),
                dataTableOutput("kualaLumpur")
              ),
      ),
      
      tabItem("selangor",
              fluidPage(
                h2("Selangor"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("selangorDistrict"),
                br(),
                dataTableOutput("selangor")
              ),
      ),
      
      tabItem("johor",
              fluidPage(
                h2("Johor"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("johorDistrict"),
                br(),
                dataTableOutput("johor")
              ),
      ),
      
      tabItem("perlis",
              fluidPage(
                h2("Perlis"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("perlisDistrict"),
                br(),
                dataTableOutput("perlis")
              ),
      ),
      
      tabItem("penang",
              fluidPage(
                h2("Penang"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("penangDistrict"),
                br(),
                dataTableOutput("penang")
              ),
      ),
      
      tabItem("kedah",
              fluidPage(
                h2("Kedah"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("kedahDistrict"),
                br(),
                dataTableOutput("kedah")
              ),
      ),
      
      tabItem("kelantan",
              fluidPage(
                h2("Kelantan"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("kelantanDistrict"),
                br(),
                dataTableOutput("kelantan")
              ),
      ),
      
      tabItem("terengganu",
              fluidPage(
                h2("Terengganu"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("terengganuDistrict"),
                br(),
                dataTableOutput("terengganu")
              ),
      ),
      
      tabItem("melaka",
              fluidPage(
                h2("Melaka"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("melakaDistrict"),
                br(),
                dataTableOutput("melaka")
              ),
      ),
      
      tabItem("pahang",
              fluidPage(
                h2("Pahang"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("pahangDistrict"),
                br(),
                dataTableOutput("pahang")
              ),
      ),
      
      tabItem("labuan",
              fluidPage(
                h2("Labuan"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("labuanDistrict"),
                br(),
                dataTableOutput("labuan")
              ),
      ),
      
      
      tabItem("putrajaya",
              fluidPage(
                h2("Putrajaya"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("putrajayaDistrict"),
                br(),
                dataTableOutput("putrajaya")
              ),
      ),
      
      tabItem("negerisembilan",
              fluidPage(
                h2("Negeri Sembilan"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("nsembilanDistrict"),
                br(),
                dataTableOutput("nsembilan")
              ),
      ),
      
      tabItem("sabah",
              fluidPage(
                h2("Sabah"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("sabahDistrict"),
                br(),
                dataTableOutput("sabah")
              ),
      ),
      
      tabItem("Sarawak",
              fluidPage(
                h2("Sarawak"),
                "updated",
                em("June 14, 2021"),
                br(),
                plotlyOutput("sarawakDistrict"),
                br(),
                dataTableOutput("sarawak")
              ),
      ),
      
      tabItem("symptoms",
              fluidPage(
                h2("Symptoms of Covid-19", align = "center"),
                h3("COVID-19 affects different people in different ways. Most infected people will develop mild to moderate illness and recover without hospitalization."),
                
                wellPanel(
                  fluidRow(
                    h3("Most common symptoms:"),
                    h4(" - Fever"),
                    h4("- Dry cough"),
                    h4("- Tiredness")
                  ),
                ),
                
                br(),
                
                wellPanel(
                  fluidRow(
                    h3("Less common symptoms:"),
                    h4(" - Aches and pains"),
                    h4(" - Sore throat"),
                    h4(" - Diarrhoea"),
                    h4(" - Conjunctivitis"),
                    h4(" - Headache"),
                    h4(" - Loss of taste or smell"),
                    h4(" - A rash on skin, or discolouration on fingers or toes")
                    
                  ),
                ),
                
                br(),
                
                wellPanel(
                  fluidRow(
                    h3("Serious symptoms:"),
                    h4(" - Difficulty breathing or shortness or breath"),
                    h4(" - Chest pain or pressure"),
                    h4(" - Loss of speech or movement")
                    
                  ),
                  
                ),
                
                br(),
                em("Seek immediate medical attention if you have serious symptoms. Always call before visiting your doctor or health facility. People with mild symptoms who are otherwise healthy should manage their symptoms at home. On average it takes 5 - 6 days from when someone is infected with the virus for symptoms to show, however it can take up to 14 days.")
                
              )
      )
      
      
      
    )
  )
  
)



#Where application logic lives
server <- function(input,output){
  
  # Total Cases 
  output$lineChart <- renderPlotly({
    datatable$Date <- as.Date(datatable$Date)
    
    p <- ggplot(data=datatable, aes(x= Date, y= `Total Cases`, group=1))+
      geom_line(color = 'lightyellow')+
      geom_point()+
      scale_x_date(labels = date_format("%m-%Y"))+
      ylab("Total Cases")+
      xlab("Date")
    
    
    p<- ggplotly(p)
    p
    
   
  })
  
  #Daily Cases
  output$lineChart2 <- renderPlotly({
    
    datatable$Date <- as.Date(datatable$Date)
    
    ggplot(data=datatable, aes(x= Date, y= `Daily New Cases`, group=1))+
      geom_line(stat = "identity") +
      geom_line()+
      geom_point(colour = 'red')+
      theme_bw()+
      scale_x_date(labels = date_format("%m-%Y"))+
      ylab("Daily New Cases")+
      xlab("Date")
    
    
  })
  
  #Active Cases
  output$lineChart3 <- renderPlotly({
    
    datatable$Date <- as.Date(datatable$Date)
    
    p2 <- ggplot(data=datatable, aes(x= Date, y= `Active Cases`, group=1))+
      geom_line(color = "salmon1")+
      geom_point(color="lightpink4")+
      scale_x_date(labels = date_format("%m-%Y"))+
      ylab("Active Cases")+
      xlab("Date")
    
    p2<- ggplotly(p2)
    p2
    
  })
  
  #Total Deaths
  output$lineChart4 <- renderPlotly({
    
    datatable$Date <- as.Date(datatable$Date)
    
    p3 <- ggplot(data=datatable, aes(x= Date, y= `Total Deaths`, group=1))+
      geom_line(linetype = "dashed")+
      geom_point()+
      scale_x_date(labels = date_format("%m-%Y"))+
      ylab("Total Deaths")+
      xlab("Date")
    
    p3<- ggplotly(p3)
    p3
    
  })
  
  #Daily Deaths
  output$lineChart5 <- renderPlotly({
    
    datatable$Date <- as.Date(datatable$Date)
    
    ggplot(data=datatable, aes(x= Date, y=`Daily Deaths`, group = 1))+
      geom_line()+
      geom_point(color = "blue")+
      theme_bw()+
      scale_x_date(labels = date_format("%m-%Y"))+
      xlab("State")+
      ylab("Total Confirmed Cases")
    
    
    
  })
  
  #State
  output$linestate <- renderPlotly({
    
    daily_state_cases_table$Date <- as.Date(daily_state_cases_table$Date)
    color_group <- c("red","blue","green","orange","sky blue","tomato","snow","tan","ivory","lightsalmon","orchid4","indianred3","mintcream","palevioletred","linen","rosybrown4")
    
    ggplot() + 
      geom_line(data = daily_state_cases_table,aes(x = Date, y= `Perlis`, group = 1), color = "red") + 
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Kedah`, group = 1), color="blue") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Pulau_Pinang`, group = 1), color="green") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Perak`, group = 1), color="orange") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Selangor`, group = 1), color="sky blue") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Negeri Sembilan`, group = 1), color="tomato") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Melaka`, group = 1), color="snow") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Johor`, group = 1), color="tan") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Pahang`, group = 1), color="ivory") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Terengganu`, group = 1), color="lightsalmon") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Kelantan`, group = 1), color="orchid4") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Sabah`, group = 1), color="indianred3") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `Sarawak`, group = 1), color="mintcream")+
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `WP_Kuala_Lumpur`, group = 1), color="palevioletred")+
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `WP_Putrajaya`, group = 1), color="linen") +
      geom_line(data = daily_state_cases_table,aes(x = Date,y = `WP_Labuan`, group = 1), color="rosybrown4") +
      geom_point()+
      ggtitle("Daily State Confirmation Cases") +
      theme_classic()+
      scale_x_date(labels = date_format("%m-%Y"))+
      xlab("Day")+
      ylab("State")+
      
      
      
      
      
      scale_colour_manual(values = color_group)+
      labs(colour = "State")
    
  })
  
  #State Bar Graph (Total Cases)
  output$stateBarGraph <- renderPlotly({
    stateBar <- ggplot(data = state_cases_table, aes(x= reorder(`State`, - `Confirmed`), y = `Confirmed`, fill = `State`)) +
      geom_bar(stat = "identity", color = "bisque3" )+
      theme()+
      xlab("State in Malaysia")+
      ylab("Total Confirmed Cases")
    
    stateBar
    stateBar + coord_flip() 
    
  })
  
  # correlation analysis
  output$correlationGraph <- renderPlotly({
 
   #ggplot(datatable, aes(x=`Daily New Cases`, y=`Daily Deaths`)) + 
   #     geom_point(color='#2980B9', size = 1) + 
    #   geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#2C3E50')
    
    ggscatter(data = datatable,x='Daily New Cases', y ='Daily Deaths', 
              color = "red",
              add = "reg.line",
              smooth = TRUE,
              conf.int = TRUE,
              cor.coef = TRUE,
              cor.method = "pearson",
              xlab = "Daily New Cases",
              ylab = "Daily Deaths",
              add.params = list(color = "blue",
                                fill = "lightgray")
    )

   
  })
  
  # State Cases in Daily 
  output$state_cases <- renderDataTable(
    state_cases_table
  )
  
  
  # Selangor
  output$selangorDistrict <- renderPlotly({
    selangorDist <- ggplot(data= selangor_district, aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "darkred")+
      theme(axis.text.x=element_text(angle=90))
    
    selangorDist
    
  })
  
  output$selangor <- renderDataTable(selangor)
  
  
  # Kuala Lumpur 
  output$kualaLumpurDistrict <- renderPlotly({
    klDist <- ggplot(data=kualalumpur_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "darksalmon")+
      theme(axis.text.x=element_text(angle=90))
    
    klDist
    
  })
  
  output$kualaLumpur <- renderDataTable(kualalumpur)
  
  #Johor
  
  output$johorDistrict <- renderPlotly({
    johorDist <- ggplot(data=johor_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "darkslategray4")+
      theme(axis.text.x=element_text(angle=90))
    
    johorDist
    
  })
  
  output$johor <- renderDataTable(johor)
  
  #Perlis
  
  output$perlisDistrict <- renderPlotly({
    perlisDist <- ggplot(data=perlis_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "deepskyblue")+
      theme(axis.text.x=element_text(angle=90))
    
    perlisDist
    
  })
  
  output$perlis <- renderDataTable(perlis)
  
  #Penang
  
  output$penangDistrict <- renderPlotly({
    penangDist <- ggplot(data=penang_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "cadetblue")+
      theme(axis.text.x=element_text(angle=90))
    
    penangDist
    
  })
  
  output$penang <- renderDataTable(penang)
  
  #Kedah
  
  output$kedahDistrict <- renderPlotly({
    kedahDist <- ggplot(data=kedah_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "antiquewhite3")+
      theme(axis.text.x=element_text(angle=90))
    
    kedahDist
    
  })
  
  output$kedah <- renderDataTable(kedah)
  
  #Kelantan
  
  output$kelantanDistrict <- renderPlotly({
    kelantanDist <- ggplot(data=kelantan_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "coral")+
      theme(axis.text.x=element_text(angle=90))
    
    kelantanDist
    
  })
  
  output$kelantan <- renderDataTable(kelantan)
  
  #Terengganu
  
  output$terengganuDistrict <- renderPlotly({
    terengganuDist <- ggplot(data=terengganu_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "azure3")+
      theme(axis.text.x=element_text(angle=90))
    
    terengganuDist
    
  })
  
  output$terengganu <- renderDataTable(terengganu)
  
  #Melaka
  
  output$melakaDistrict <- renderPlotly({
    melakaDist <- ggplot(data=melaka_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "darkseagreen2")+
      theme(axis.text.x=element_text(angle=90))
    
    melakaDist
    
  })
  
  output$melaka <- renderDataTable(melaka)
  
  #Pahang
  
  output$pahangDistrict <- renderPlotly({
    pahangDist <- ggplot(data=pahang_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "lightcoral")+
      theme(axis.text.x=element_text(angle=90))
      
    
    pahangDist
    
  })
  
  output$pahang <- renderDataTable(pahang)
  
  #Labuan
  
  output$labuanDistrict <- renderPlotly({
    labuanDist <- ggplot(data=labuan_district , aes(x= `District`, y = `14 Day Total`,fill = `District`))+
      geom_bar(stat ="identity", color = "lightcyan3")+
      theme(axis.text.x=element_text(angle=90))
      
    
    labuanDist
    
  })
  
  output$labuan <- renderDataTable(labuan)
  
  #Putrajaya
  
  output$putrajayaDistrict <- renderPlotly({
    putrajayaDist <- ggplot(data=putrajaya_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "mediumpurple4")+
      theme(axis.text.x=element_text(angle=90))
      
    
    putrajayaDist
    
  })
  
  output$putrajaya <- renderDataTable(putrajaya)
  
  #Negeri Sembilan
  
  output$nsembilanDistrict <- renderPlotly({
    nsembilanDist <- ggplot(data=nsembilan_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "lightslategrey")+
      theme(axis.text.x=element_text(angle=90))
      
    
    nsembilanDist
    
  })
  
  output$nsembilan <- renderDataTable(nsembilan)
  
  #Sabah
  
  output$sabahDistrict <- renderPlotly({
    sabahDist <- ggplot(data=sabah_district , aes(x= `District`, y = `14 Day Total`, fill = `District`))+
      geom_bar(stat ="identity", color = "navyblue")+
      theme(axis.text.x=element_text(angle=90))
     
    
    sabahDist
    
  })
  
  output$sabah <- renderDataTable(sabah)
  
  #Sarawak
  
  output$sarawakDistrict <- renderPlotly({
    sarawak_district$District <- sarawak_district$District
    sarawakDist <- ggplot(data=sarawak_district , aes(x= District, y = `14 Day Total`, fill = District))+
      geom_bar(stat = "identity")+
      theme(axis.text.x = element_text(angle=90))
    
    sarawakDist
    
  })
  
  output$sarawak <- renderDataTable(sarawak)
  
}

#Combine UI + server into a Shiny app and run it 
shinyApp(ui, server)