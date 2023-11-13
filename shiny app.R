library(shiny)
library(shinydashboard)
library(fontawesome)
library(plotly)
library(crayon)
library(readr)
library(DT)





ui <- shinyUI(
  dashboardPage( skin = 'green',
                 dashboardHeader(title = "Rwanda Labour Force Survey Annual Report 2022",
                                 titleWidth =600),
                 dashboardSidebar(
                   sidebarMenu( 
                     menuItem("Labour force overview", tabName = "maindash", icon = icon("dashboard")),
                     menuItem("Labor force distribution", icon = icon("person-digging"),
                              menuSubItem("Province",tabName = "province",icon = icon("compass"))
                     ))),
                 
                 dashboardBody(
                   tabItems(
                     tabItem(tabName = "maindash",
                             
                             fluidRow( h1("Population Overview"),
                                       infoBox("Total population",value = " 13,078,028 people",color = "orange",
                                               icon = icon("people-group"),fill = TRUE),
                                       infoBox("Working age population (16+)",subtitle = "60.8% of Total Population",value = " 7,963,586 people",
                                               color = 'blue',icon = icon("chart-simple") ,fill = TRUE),
                                       infoBox("DEPENDENCY RATIO",value = "Dependency ratio was estimated to be 64.2%",
                                               color = "yellow",icon = icon("sitemap"),fill = TRUE),
                             ),
                             
                             fluidRow( h3("Working age composition"),
                                       
                                       infoBox("Labor Force",subtitle = "56% of working population",value = "4,463,296 people",
                                               color = 'blue',icon = icon("users-viewfinder") ,fill = TRUE),
                                       infoBox("outside labor force",subtitle = "44% of working population",value = "3,500,290 people",
                                               color = "blue",icon = icon("person-circle-xmark"),fill = TRUE),
                                       infoBox("Subsistence food stuff production",subtitle = "37.5% of outside labor force people",value = "1,312,609 people",
                                               color = "blue",icon = icon("seedling"),fill = TRUE)
                             ),
                             fluidRow(
                               box(plotlyOutput("gen"),collapsible = TRUE, collapsed = FALSE,width = 5,
                                   status = "primary",solidHeader = TRUE, title = "Working age population"),
                               box(plotlyOutput("occupation"),collapsible = TRUE, collapsed = FALSE,width = 6,
                                   status = "primary",solidHeader = TRUE, title = "Work force occupation categories")
                             ),
                             
                             fluidRow(h3("Employment status"),
                                      infoBox("Unemployemnt rate",icon = icon("arrow-trend-down"),value = "The annual unemployment rate in year 2022 was 20.5%",
                                              subtitle = "916,944 persons were not employed but seeking and available to work for pay or profit.",color = "olive",fill = TRUE),
                             ),
                             fluidRow(box(plotlyOutput("unemployment"),title = "Unemployment by gender, age and location",
                                          solidHeader = TRUE,status = "primary",collapsible = TRUE,collapsed = FALSE),
                                      box(plotlyOutput("employ"), title = "Unemployment rate by educational attainment ",solidHeader = TRUE, 
                                          collapsible = TRUE, collapsed = FALSE,width = 6, status = "primary")
                                      
                             ),
                             fluidRow(h3("Branches of economic activities"),
                                      
                                      infoBox("Market oriented agriculture", value = "46.8% of employed population",icon = icon("tractor"),
                                              color = 'green',subtitle = "1,659,693 population",fill = TRUE),
                                      infoBox("Services",value = "35.9% were employed in service sector",
                                              color = 'navy',icon = icon("bell-concierge"),subtitle = "1,273,140 population" ,fill = TRUE),
                                      infoBox("Industry",value = "17.3% were located in service sector",
                                              color = 'purple',icon = icon("industry"),subtitle = "613,519 population" ,fill = TRUE)
                             ),
                             
                             
                             fluidRow(box(plotlyOutput("status"),collapsible = TRUE, collapsed = FALSE,width = 5,
                                          status = "primary",solidHeader = TRUE, title = "Sex distribution of employment status"),
                                      box(plotlyOutput("trend"),title = "6 Year labour force indicator's trend", solidHeader = TRUE,
                                          status = "primary", collapsible = TRUE,collapsed = FALSE, width = 5)
                                      
                             ),
                             
                             fluidRow(h3("Average monthly income of employed"),
                                      infoBox("Overall hours worked per week",value = "35.9 hours",subtitle = "Employed population on average they work 35.9 hours per week",
                                              icon = icon("clock"), color = 'navy',fill = TRUE),
                                      
                                      box(title = "Average income destribution/Education",solidHeader = TRUE,status = "primary",plotlyOutput("income"))
                                      
                             )),
                     
                     
                     
                     tabItem(tabName = "province",h2("Labour force employability"),
                             tabBox(id="t1", width =4,
                                    tabPanel(title = "Kigali City",plotlyOutput("kigali")),
                                    tabPanel(title = "Northern Province",plotlyOutput("north")),
                                    tabPanel(title = "Southern Province",plotlyOutput("south")),
                                    tabPanel(title = "Eastern Province", plotlyOutput("east")),
                                    tabPanel(title = "Western Province", plotlyOutput("west"))),
                             fluidRow(box(title = "population outside labor force",plotlyOutput("outside"),solidHeader = TRUE,
                                          status = "primary",width = 7)
                             )
                     )
                     
                   )
                 )
  ))

server <- shinyServer(function(input, output){
  
  # working age occupation
  output$occupation <- renderPlotly({
    occupation <- c("subsistance agriculture","market orientaded agri","non agriculture sector","others outside labour force")
    values <- c(1759278,1660185,1886167, 2657956)
    colorsss <- c("blue","purple","chocolate","maroon")
    plot_ly(x = occupation, y = values, color = occupation,colors = colorsss,type = "bar")%>%
      layout(title = "Work force occupation chat",margin = list(t = 80), xaxis = list(title= "categories"), yaxis = list(title = "population"),
             paper_bgcolor = "lightgrey",plot_bgcolor = "lightgrey")
  })
  
  #Distribution by province
  output$kigali <- renderPlotly({
    categories <- factor(c("Employed", "Unemployed","Total labor force"),levels = c("Unemployed", "Employed","Total labor force"))
    values <- c(647629,171084,818713)
    plot_ly(x = categories, y = values, type = "bar",color = categories,colors = c("purple","orange","navy"))%>%
      layout(title = "Kigali city",margin = list(t = 80), xaxis = list(title = "Categories"), yaxis =list(title = "population"),
             legend = list(orientation = "h", y = -0.2))
  })
  output$north <- renderPlotly({
    categories <- factor(c("Employed", "Unemployed","Total labor force"),levels = c("Unemployed", "Employed","Total labor force"))
    values <- c(599887, 154168,754055)
    plot_ly(x = categories, y = values,type = "bar",color = categories,colors = c("purple","orange","navy") )%>%
      layout(title = "Northern Province",margin = list(t = 80), xaxis = list(title = "Categories"), yaxis =list(title = "population"),
             legend = list(orientation = "h", y = -0.2))
    
  })
  output$south <- renderPlotly({
    categories <- factor(c("Employed", "Unemployed","Total labor force"),levels = c("Unemployed", "Employed","Total labor force"))
    values <- c(811479,222851,1034330)
    plot_ly(x = categories, y = values,type = "bar",color = categories,colors = c("blue","orange","navy"))%>%
      layout(title = "Southern Province",margin = list(t = 80), xaxis = list(title = "Categories"), yaxis =list(title = "population"),
             legend = list(orientation = "h", y = -0.2))
  })
  output$east <- renderPlotly({
    categories <- factor(c("Employed", "Unemployed","Total labor force"),levels = c("Unemployed", "Employed","Total labor force"))
    values <- c( 837313,197904,1035217)
    plot_ly(x = categories, y = values,type = "bar",color = categories,colors = c("blue","orange","navy"))%>%
      layout(title = "Eastern Province",margin = list(t = 80), xaxis = list(title = "Categories"), yaxis =list(title = "population"),
             legend = list(orientation = "h", y = -0.2))
  })
  output$west <- renderPlotly({
    categories <- factor(c("Employed", "Unemployed","Total labor force"),levels = c("Unemployed", "Employed","Total labor force"))
    values <- c( 650043,170937,820980)
    plot_ly(x = categories, y = values,type = "bar",color = categories,colors = c("green","orange","navy"))%>%
      layout(title = "Western Provinve",margin = list(t = 80), xaxis = list(title = "Categories"), yaxis =list(title = "population"),
             legend = list(orientation = "h", y = -0.2))
  })
  
  #Unemployment in different categories
  output$unemployment <- renderPlotly({
    category <- factor(c("Male", "Female", "Youth", "Adults","Urban", "Rural"), levels = c("Male", "Female", "Youth", "Adults","Urban", "Rural"))
    gender <- c( 17.9, 23.7)
    age <- c( 25.6, 17.1)
    location <- c(20.4, 20.6)
    plot_ly( x = category[1:2] , y = gender ,name = "Gender" ,type = "bar")%>%
      add_trace(x = category[3:4], y = age,name = "Age")%>%
      add_trace(x = category[5:6], y = location, name = "Location")%>%
      layout(title = "Unemployment Distribution",margin = list(t = 80),xaxis = list(title = "Categories"), yaxis = list(title = "Percentages"),
             plot_bgcolor = 'lightgrey',legend = list(orientation = "h", y = -0.2))
  })
  
  #outside labor force pop
  
  output$outside <- renderPlotly({
    category <- factor(c("Kigali city", "Northern province","southern province", "western province","Eastern provonce"),
                       levels =c("Kigali city", "Northern province","southern province", "western province","Eastern provonce"))
    pop <- c(398068, 586854, 956483, 697402, 861483)
    data <- data.frame(category,pop)
    plot_ly(x = category, y = pop, type = "bar",color = category, colors = c('yellow', 'deepskyblue', 'purple','gray', 'green'))%>%
      layout(title = "Outside labor force bar chat",margin = list(t = 80), xaxis = list(title = "province"),
             yaxis = list(title = "population"),plot_bgcolor = "lightgrey",paper_bgcolor = "lightgrey",legend = list(orientation = "h", y = -0.2))
  })
  
  # warking age pie
  output$gen <- renderPlotly({
    
    plot_ly( labels = c("male","Female"), values = c(2407448, 2055848), hole = 0.4,
             type = "pie")%>%
      layout(title = "16+ above by sex chat",margin = list(t = 80),plot_bgcolor = "lightgrey",paper_bgcolor = "lightgrey")
  })
  
  #Employment status
  output$status <- renderPlotly({
    categories <- factor(c("Employed", "Unemployed","Total labor force"),levels = c("Employed", "Unemployed","Total labor force"))
    Male <- c( 1977704, 429744)
    Female <- c( 1568648, 487200)
    
    colors <- c("green","deepskyblue",'navy')
    plot_ly(x = categories[1:2], y = Male,name = 'Male',marker = list(color = colors[1]), type = 'bar')%>%
      add_trace(y = Female, name = 'Female',marker = list(color = colors[2]) )%>%
      add_trace(x = categories[3], y = 4463296, name = "Total labor force" ,marker =list(color = colors[3]))%>%
      layout(title = "Employment status",margin = list(t = 80),xaxis = list(title = "Categories"), yaxis =list(title = "population"),
             paper_bgcolor = "lightgrey",plot_bgcolor = "lightgrey",legend = list(orientation = "h",y = -0.2))
    
  })
  
  #Employment by education attained
  
  output$employ <- renderPlotly({
    education <- factor(c("None","Primary","Lower Sec","Upper Sec","University","Completed TVET"), levels = c("None","Primary","Lower Sec","Upper Sec","University","Completed TVET"))
    rate <- c(18.4,19.7, 23.8, 32.4, 17.3,18.9)
    color_set <- c("orange","blue","navy","grey","green","yellow")
    plot_ly(x = education, y = rate,color = color_set, type = "bar")%>%
      layout(title = "Unemployment chat",margin = list(t = 80),xaxis = list(title = "Education Completed"),
             yaxis =list(title = "percentages"),plot_bgcolor = "lightgrey",showlegend = FALSE )
    
  })
  
  #6year labor force indication  
  output$trend <- renderPlotly({
    years <- c("2017","2018","2019","2020","2021","2022")
    percen <- c(17.3, 15.1, 15.2, 17.9, 21.1, 20.5)
    participation <- c(53.4,54.2,53.4,56.4,54,56)
    Emp_pop <- c(44.2, 46, 45.3, 46.3, 42.6, 44.5)
    plot_ly(x = years,y = percen,name = "Unemployment rate",type = "scatter",mode = "lines+markers")%>%
      add_trace(y = participation,name = "Labor force perticipation rate")%>%
      add_trace(y = Emp_pop, name = "Employment to population ratio")%>%
      layout(xaxis = list(title = "years"),yaxis =list(title = "percentages"),plot_bgcolor = "lightgrey",
             legend = list(orientation = "h", y = -0.2))
    
  })
  
  #Income distribution by education
  output$income <- renderPlotly({
    education <- factor(c("None","Primary","L Secondary","U Secondary","University"),levels = c("None","Primary","L Secondary","U Secondary","University"))
    cash <- c( 29090, 38470, 54152, 93175, 305585)
    colorss <- c('black',"purple",'chocolate','green','deepskyblue') 
    plot_ly(x = education, y = cash,color = education, colors = colorss,type = 'bar')%>%
      layout(xaxis = list(title = "Level of education"),yaxis =list(title = "amount in Rwf"),plot_bgcolor = "lightgrey")
  })
  
  
  
  output$bar <- renderPlot({
    pie(c("Employed" = 3546352 , "Unemployed" = 916944 , "Subsistence food staff producers" = 1310734 , "others" = 2235618),
        anti.angle=90, col = c("deepskyblue","yellow", "blue", "green"),
        main = "Working age population",border ="black")
  })
  
}) 

shinyApp(ui = ui, server = server)

