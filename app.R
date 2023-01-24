library(shiny)
library(dashboardthemes)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse) 
library(ggalt) 
library(countrycode) 
library(rworldmap) 
library(gridExtra) 
library(broom) 
library(readxl)
library(DT)
library(highcharter)
library(viridis)
library(plotly)
library(scales)
library(viridis)
library(googleVis)
library(maps)
library(ggthemes)
library(gganimate)
library(crosstalk)
library(sandwich)
library(stargazer)
library(Rcpp)
library(leaflet)
library(heatmaply)
library(ggcorrplot)

data <- read_csv("master.csv")

data <- data %>%
  select(-c('HDI for year','country-year')) %>%
  rename(gdp_for_year = 'gdp_for_year ($)', 
         gdp_per_capita = 'gdp_per_capita ($)',
         suicide_per_100k = 'suicides/100k pop') %>%
  as.data.frame()

data<- data %>%
  mutate(country = fct_recode(country, "The Bahamas" = "Bahamas"),
         country = fct_recode(country, "Cape Verde" = "Cabo Verde"),
         country = fct_recode(country, "South Korea" = "Republic of Korea"),
         country = fct_recode(country, "Russia" = "Russian Federation"),
         country = fct_recode(country, "Republic of Serbia" = "Serbia"),
         country = fct_recode(country, "United States of America" = "United States"))

# excluding countries with <= 3 years of data:
minimum_years <- data %>%
  group_by(country) %>%
  summarize(rows = n(), 
            years = rows / 12) %>%
  arrange(years)

data <- data %>%
  filter(!(country %in% head(minimum_years$country, 7)))

data$continent <- countrycode(sourcevar = data[, "country"],
                              origin = "country.name",
                              destination = "continent")

data <- data %>%
  select(continent, everything())


# Reclassify into North America and South America
south_america <- c('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Guyana', 'Paraguay', 'Suriname', 'Uruguay')
data$continent[data$country %in% south_america] <- 'South America'
data$continent[data$continent=='Americas'] <- 'North America'


data$age <- gsub(" years", "", data$age)
data$sex <- ifelse(data$sex == "male", "Male", "Female")
data_nominal <- c('country', 'sex', 'continent')
data[data_nominal] <- lapply(data[data_nominal], function(x){factor(x)})

data$age <- factor(data$age, 
                   ordered =TRUE, 
                   levels = c("5-14",
                              "15-24", 
                              "25-34", 
                              "35-54", 
                              "55-74", 
                              "75+"))


data %>% group_by(generation) %>% summarize(rows=n())
data$generation <- factor(data$generation, 
                          ordered = TRUE, 
                          levels = c("G.I. Generation", 
                                     "Silent",
                                     "Boomers", 
                                     "Generation X", 
                                     "Millenials", 
                                     "Generation Z"))

data <- as_tibble(data)

global_average <- (sum(as.numeric(data$suicides_no)) / sum(as.numeric(data$population)))*100000

overall_tibble <- data %>%
  select(year, suicides_no, population) %>%
  group_by(year) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 

avg_dt <- data %>%
  group_by(country)%>%
  summarise(avg_gdp = round(sum(`gdp_per_capita`)/length(`gdp_per_capita`)),
            avg_sui = sum(`suicide_per_100k`)/length(`suicide_per_100k`)
  )

# Create a tibble with suicide per capita by country 
country_tibble <- data %>%
  select(country, suicides_no, population) %>%
  group_by(country) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))

# Create tibble for our line plot.  
overall_tibble <- data %>%
  select(year, suicides_no, population) %>%
  group_by(year) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 

#----Color/Theme----
#Pick color for continent.
continent_color <- c('#058DC7', '#50B432', '#ED561B', '#FFF263', '#6AF9C4','#FF9655')

#Pick color for gender.
sex_color <- c("#F88B9D","#8ECEFD") # baby blue & pink

#Create a custom theme for the plots. 
custom_theme <- hc_theme(
  colors = c('#5CACEE', 'green', 'red'),
  chart = list(
    backgroundColor = '#FAFAFA', 
    plotBorderColor = "black"),
  xAxis = list(
    gridLineColor = "E5E5E5", 
    labels = list(style = list(color = "#333333")), 
    lineColor = "#E5E5E5", 
    minorGridLineColor = "#E5E5E5", 
    tickColor = "#E5E5E5", 
    title = list(style = list(color = "#333333"))), 
  yAxis = list(
    gridLineColor = "#E5E5E5", 
    labels = list(style = list(color = "#333333")), 
    lineColor = "#E5E5E5", 
    minorGridLineColor = "#E5E5E5", 
    tickColor = "#E5E5E5", 
    tickWidth = 1, 
    title = list(style = list(color = "#333333"))),   
  title = list(style = list(color = '#333333', fontFamily = "Lato")),
  subtitle = list(style = list(color = '#666666', fontFamily = "Lato")),
  legend = list(
    itemStyle = list(color = "#333333"), 
    itemHoverStyle = list(color = "#FFF"), 
    itemHiddenStyle = list(color = "#606063")), 
  credits = list(style = list(color = "#666")),
  itemHoverStyle = list(color = 'gray'))


#-----Create model------
#-suicide rate vs gdp
country_mean_gdp <- data %>%
  group_by(country, continent) %>%
  summarize(suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, 
            gdp_per_capita = mean(gdp_per_capita))

model1 <- lm(suicides_per_100k ~ gdp_per_capita, data = country_mean_gdp)

#-remove outliers
gdp_suicide_no_outliers <- model1 %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  filter(.cooksd < 4/nrow(.)) %>% # removes 5/93 countries
  inner_join(country_mean_gdp, by = c("suicides_per_100k", "gdp_per_capita")) %>%
  select(country, continent, gdp_per_capita, suicides_per_100k)

model2 <- lm(suicides_per_100k ~ gdp_per_capita, data = gdp_suicide_no_outliers)

#-suicide rate vs total population
country_total_population <- data %>%
  group_by(country, continent) %>%
  filter(year!=2016)%>% 
  summarize(suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, total_population= sum(as.numeric(population)))
country_total_population$suicides_per_100k <- round(country_total_population$suicides_per_100k, 2)

model3 <- lm(suicides_per_100k ~ total_population, data = country_total_population)

#-remove outliers
population_suicide_no_outliers <- model3 %>%
  augment() %>%
  filter(total_population<5000000000)%>% 
  select(total_population, suicides_per_100k )

model4 <- lm(suicides_per_100k  ~ total_population, data = population_suicide_no_outliers)

#-total suicide number vs total population
country_pop_num <- data %>%
  group_by(country, continent) %>%
  filter(year!=2016)%>% 
  summarize(total_suicides = (sum(as.numeric(suicides_no))), total_population= sum(as.numeric(population)))

model5 <- lm(total_suicides ~ total_population, data = country_pop_num)

#-remove outliers
population_suicide_outliers <- model5 %>%
  augment() %>%
  filter(total_population<5000000000)%>% 
  select(total_population, total_suicides )

model6 <- lm(total_suicides  ~ total_population, data = population_suicide_outliers)


###-------------------------------------------ui------------------------------------------------###
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = 'Global Suicide Analysis', titleWidth = 260),
                    dashboardSidebar(width = 230,
                                     sidebarMenu(
                                       menuItem('Home', tabName = 'home', icon = icon('house')),
                                       menuItem('Maps', tabName = 'maps', icon = icon('globe')),
                                       menuItem('Analysis', icon = icon("dashboard"),
                                                menuSubItem("Worldwide", icon = icon("fas fa-chart-line"),tabName = "worldwide"),
                                                menuSubItem("Continent", icon = icon("fas fa-chart-line"), tabName = "continent"),
                                                menuSubItem("Country", icon = icon("fas fa-chart-line"),tabName = "country"),
                                                menuSubItem("Linear Regression", icon = icon("fas fa-chart-line"), tabName = "liReg")),
                                       menuItem('Data Overview', tabName = 'data', icon = icon('database')), 
                                       menuItem('Info', tabName = 'info', icon = icon('circle-info'))
                                     )),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      tabItems(
                        tabItem(tabName = "home", 
                                textOutput("introduction_title"),
                                textOutput("introduction1"), 
                                HTML( paste('<br/>')),
                                textOutput("introduction2"), 
                                HTML( paste('<br/>')),
                                HTML('<center><img src="suicidepic.png"></center>'),
                                HTML( paste('<br/>')),
                                tags$head(tags$style("#introduction_title{ color:#0B0B45; font-size: 25px; text-align: center;}
                                        #introduction1{text-align: justify;font-size: 17px;}
                                        #introduction2{text-align: justify;font-size: 17px;}"))
                        ),
                        tabItem(tabName = "maps",
                                fluidRow(
                                  box(
                                    title = "WORLDWIDE OVERVIEW",
                                    width = 12,
                                    valueBoxOutput(outputId = "totalsuicide", width = 3),
                                    valueBoxOutput(outputId = "avgrate", width = 3),
                                    valueBoxOutput(outputId = "maxnum", width = 3),
                                    valueBoxOutput(outputId = "maxrate", width = 3)
                                  )),
                                fluidRow(
                                  column(width = 9, box(htmlOutput("map"), height = "auto", width = "auto")),
                                  column(width = 3,
                                         radioButtons("type", label = h4("Display map by: "),
                                                      choices = list("Suicides (/100k)" = "suicide_rate",
                                                                     "Suicides" = "suicides"),
                                                      selected = "suicide_rate"),
                                         checkboxGroupInput("checkGroup", label = h4("Filter by age group (years): "),
                                                            choices = list("5-14", "15-24",
                                                                           "25-34", "35-54",
                                                                           "55-74", "75+"),
                                                            selected = c("5-14", "15-24", "25-34",
                                                                         "35-54", "55-74","75+")),
                                         sliderInput("slider", label = h4("Year Range"), min = 1985, 
                                                     max = 2016, value = c(1985, 2016), sep = ""))
                                )
                        ),
                        tabItem(tabName = "worldwide",
                                fluidPage(
                                  tabBox(
                                    width = 12,
                                    tabPanel("Worldwide Trend",
                                             fluidPage(
                                               fluidRow(
                                                 sliderInput("yearslider", label = h4("Years of Interest"), min = 1985, 
                                                             max = 2016,
                                                             value = c(1985, 2016),
                                                             sep = '')),
                                               fluidRow(   
                                                 highchartOutput("worldwidetrend")
                                               ))
                                    ),
                                    tabPanel("Worldwide By Gender", 
                                             fluidRow(
                                               column(
                                                 width = 5,
                                                 highchartOutput("genderPiePlot")
                                               ),
                                               column(
                                                 width = 7,
                                                 highchartOutput("genderlineplot")
                                               )
                                             )
                                    ),
                                    tabPanel("Worldwide By Age", 
                                             fluidRow(
                                               column(
                                                 width = 5,
                                                 highchartOutput("agePiePlot")
                                               ),
                                               column(
                                                 width = 7,
                                                 plotlyOutput(outputId = "ageBoxPlot", width = 'auto', height='auto')
                                               )
                                             )
                                    ),
                                    
                                    tabPanel("Worldwide By Generation",
                                             fluidPage(
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   sliderInput("yearGen", label = h4("Years of Interest"), min = 1985, 
                                                               max = 2016,
                                                               value = c(1985, 2016),
                                                               sep = ''), 
                                                   HTML( paste('<br/>')),
                                                   h4("The Generations Defined:"),
                                                   p("❖ G.I. Generation: Born 1901 – 1926"),
                                                   p("❖ Silent: Born 1928-1945"), 
                                                   p("❖ Baby Boomers: Born 1946-1964"), 
                                                   p("❖ Generation X: Born 1965-1980"), 
                                                   p("❖ Millennials: Born 1981-1996"), 
                                                   p("❖ Generation Z: Born 1997-2012"),
                                                 ),
                                                 mainPanel(
                                                   highchartOutput("generationplot")
                                                 )
                                               )
                                             )
                                    )
                                  )
                                  
                                )
                        ), 
                        tabItem(tabName = "continent",
                                fluidPage(
                                  fluidRow(
                                    column(
                                      width = 6,
                                      highchartOutput("continentBarPlot")
                                    ),
                                    column(
                                      width = 6,
                                      highchartOutput("continentMapPlot")
                                    )
                                  ),
                                  p(" "),
                                  fluidRow(
                                    tabBox(
                                      width=12,
                                      tabPanel("By Continent and Age", highchartOutput("continentAndAgeBarPlot")),
                                      tabPanel("By Continent and Gender", highchartOutput("continentAndGenderBarPlot"))
                                    )
                                  )
                                )
                        ),
                        tabItem(tabName = "country",
                                fluidPage(
                                  tabBox(
                                    width=12,
                                    tabPanel("Overview", 
                                             fluidRow(
                                               column(
                                                 width = 6,
                                                 highchartOutput("countryBarPlot")
                                               ),
                                               column(
                                                 width = 6,
                                                 plotlyOutput(outputId = "countryGDPPlot")
                                               )
                                             ),
                                             fluidRow(
                                               column(
                                                 width = 6,
                                                 h3(" "),
                                                 highchartOutput("countryAndAgeBarPlot")
                                               ),
                                               column(
                                                 width = 6,
                                                 h3(" "),
                                                 highchartOutput("countryAndGenderBarPlot")
                                               )
                                             )
                                    ),
                                    
                                    tabPanel("Trend", fluidPage(
                                      fluidRow(
                                        column(4,                                         
                                               selectInput("var1",
                                                           "Select a country",
                                                           choices = c(unique(as.character(data$country))),
                                                           selected = 'Austria'
                                               )),
                                        column(4,
                                               sliderInput("var2",
                                                           "Year over time",
                                                           min = 1985,max = 2016,value = c(1985,2016), sep = '')
                                        )),
                                      fluidRow(
                                        column(6,
                                               highchartOutput("countrytrend")),
                                        column(6,
                                               highchartOutput("GDPyear"))
                                      ),
                                      p(" "),
                                      p(" "),
                                      fluidRow(
                                        column(12,
                                               p(" "),
                                               p(" "),
                                               textOutput("bar_title"),
                                               selectInput("oneyear", "Select One Specific Year", choices = as.integer(sort(data$year))),
                                               highchartOutput("ageSexPlot"),
                                               tags$head(tags$style("#bar_title{color: #0B0B45; font-size: 20px}"))
                                        )
                                      )
                                    )
                                    )
                                  )
                                )
                        ),
                        tabItem(tabName = "liReg",
                                fluidPage(
                                  fluidRow(
                                    column(
                                      width=8,
                                      plotOutput("lmGDP"),
                                      HTML( paste('<br/>'))
                                    ),
                                    column(
                                      width=4,
                                      h4("Slope:"), 
                                      textOutput("pred1slope"),
                                      h4("Intercept:"),
                                      textOutput("pred1intercept"),
                                      h4("P value of regression model:"),
                                      textOutput("pred1p"),
                                      HTML( paste('<br/>')),
                                      textOutput("note"),
                                      tags$head(tags$style("#pred1slope{font-size: 17px;} 
                                        #pred1intercept{font-size: 17px;}
                                        #note{font-size: 15px;}
                                        #pred1p{font-size: 17px;}"))
                                    )
                                  )
                                ),
                                fluidRow(
                                  HTML( paste('<br/>')),
                                  column(
                                    width=11,
                                    textOutput("sentence"),
                                    tags$head(tags$style("#sentence{text-align: justify;font-size: 17px;}")),
                                    HTML( paste('<br/>'))),
                                  column(
                                    width=6,
                                    plotOutput("lmPopTotal")
                                  ),
                                  column(
                                    width=6,
                                    plotOutput("lmPopRate")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width=8,
                                    HTML( paste('<br/>')),
                                    h4("   Correlation Heatmap"),
                                    plotOutput("corrplot"))
                                )
                        ),
                        tabItem(tabName = 'data',
                                fluidPage(
                                  fluidRow(
                                    column(4,
                                           selectInput('country',
                                                       'Country',
                                                       c('All',
                                                         unique(as.character(data$country)
                                                         )))),
                                    
                                    column(4,
                                           selectInput('year',
                                                       'Year',
                                                       c('All',sort(unique(as.character(data$year))
                                                       )))),
                                    
                                    column(4,
                                           selectInput('age',
                                                       'Age',
                                                       c('All', "5-14", "15-24", "25-34",
                                                         "35-54", "55-74","75+")
                                           ))
                                  ),
                                  fluidRow(
                                    DT::dataTableOutput('table')
                                  ),
                                  fluidRow(
                                    downloadButton("downloadData", "Download")
                                  )
                                )
                        ),
                        tabItem(tabName = "info", 
                                includeMarkdown("info.Rmd")
                        )
                      )
                    )  
)
###----------------------------------------Server------------------------------------------------###
server <- function(input, output){
  
  ##-------------------------Home-------------------------------
  output$introduction_title <- renderText({
    intro <- paste0("An Introduction to Our Findings, Our Purpose, and Our Goal")
  })
  output$introduction1 <- renderText({
    text <- paste("Welcome to the Global Suicide Analysis Project homepage. This project is dedicated to addressing the issue of suicide, which is a significant 
                  public health concern. Our purpose in creating this application is to provide a tool for understanding and analysing the suicide rates of different 
                  countries across the globe. Our application allows users to examine the trends in suicide rates over time. This feature enables users gain a better 
                  understanding of the trajectory of suicide rates and determine whether they are increasing or decreasing. Our application also allows users to examine
                  the patterns in suicide rates among different age groups. Users can utilise this tool to identify which age groups may be at higher risk of suicide so 
                  that efforts can be prioritised to support and protect these individuals.  
                  ")
  })
  
  output$introduction2 <- renderText({
    text <- paste("Additionally, our application presents data on the relationship between different variables, providing a glimpse into potential contributing factors to 
                  suicide rates. By examining these patterns and correlations, we hope to gain a deeper understanding of the factors that influence suicide rates and inform 
                  the development of effective prevention strategies. By presenting the data in an accessible and user-friendly way, we hope to raise awareness about this vital 
                  issue and encourage people to take action to prevent suicide. Our goal is to let related authorities gain some insights from the suicides analysis to develop 
                  strategies for reducing suicide rates and helping those who are at risk. We hope the information and resources provided on this site will provide assistance 
                  in reducing the suicide rate. We believe that with better understanding and knowledge, we can work towards a future where fewer lives lost to suicide.")
  })
  
  ##----------------------------Map-----------------------------------
  df1 <- reactive({
    if(is.null(input$checkGroup)){
      df1 = data %>% 
        filter(between(year, input$slider[1], input$slider[2])) %>%
        group_by(country) %>%
        summarise(suicide_rate = (suicide_per_100k = sum(suicides_no) / sum(population) * 100000),
                  suicides = sum(suicides_no))
    }
    else{
      df1 = data%>%
        filter(age %in% input$checkGroup) %>%
        filter(between(year, input$slider[1], input$slider[2])) %>% 
        group_by(country) %>%
        summarise(suicide_rate = (suicide_per_100k = sum(suicides_no) / sum(population) * 100000),
                  suicides = sum(suicides_no))
    }
  })
  
  output$map <-renderGvis({
    gvisGeoChart(data = df1(), locationvar = "country", colorvar = input$type,
                 options = list(region="world", displayMode="auto",
                                resolution="countries", width="100%", height="100%",
                                colorAxis="{colors:['#6f92e6', '#f9897e']}"))
    
  })
  
  output$totalsuicide <- renderValueBox({
    totalsuicide <-  data %>%
      summarise(total = sum(suicides_no)) %>% 
      pull(total)
    
    valueBox(
      value = comma(totalsuicide),
      subtitle = "Number of Suicides Recorded",
      color = "aqua",
      icon = icon("database"),
      width = 12
    )
  })
  
  output$avgrate <- renderValueBox({
    avgrate <- data %>%
      summarise(avg = round(global_average,2)) %>% 
      pull(avg)
    
    valueBox(
      value = avgrate,
      subtitle = "Average Global Suicide Rates",
      icon = icon("globe"),
      color = "aqua",
      width = 12
    )
  })
  
  output$maxnum <- renderValueBox({
    maxnum <- data %>% 
      group_by(country) %>% 
      summarise(freq=sum(suicides_no)) %>% 
      arrange(desc(freq)) %>% 
      head(1) %>% 
      pull(country)
    
    valueBox(
      value = maxnum,
      subtitle = "Country With Highest Number of Suicides",
      color = "teal",
      icon = icon("exclamation-triangle"),
      width = 12
    )
  })  
  
  output$maxrate <- renderValueBox({
    maxrate <- data %>% 
      group_by(country) %>% 
      summarise(freq=mean(suicide_per_100k)) %>% 
      arrange(desc(freq)) %>% 
      head(1) %>% 
      pull(country)
    
    valueBox(
      value = maxrate,
      subtitle = "Country With Highest Suicide Rate",
      icon = icon("chart-line"),
      color = "teal",
      width = 12
    )
  })
  
  ##-------------------------------Worldwide------------------------------------##
  
  ##-----------------------worldwide trend-----------------------------
  
  output$worldwidetrend <- renderHighchart({
    highchart() %>% 
      hc_add_series(overall_tibble, hcaes(x = year, y = suicide_capita, color = suicide_capita), type = "line") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br> Suicides: <b>{point.y}</b>")) %>%
      hc_title(text = "Worldwide Suicides By Year") %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_xAxis(title = list(text = "Year"), 
               min = input$yearslider[1], 
               max = input$yearslider[2]) %>%
      hc_yAxis(title = list(text = "Suicides per 100K population"),
               allowDecimals = FALSE,
               plotLines = list(list(
                 color = "black", width = 1, dashStyle = "Dash", 
                 value = mean(overall_tibble$suicide_capita),
                 label = list(text = "Mean = 13.12", 
                              style = list(color = "black"))))) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_add_theme(custom_theme)  
  })
  
  ##---------------------Worldwide by Gender-----------------------------
  
  ##Gender Pie Chart
  output$genderPiePlot <- renderHighchart({
    
    pie_sex <- data %>% 
      group_by(sex) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    highchart() %>% 
      hc_add_series(pie_sex, hcaes(x = sex, y = suicide_capita, color = c("#F88B9D", "#8ECEFD")), type = "pie") %>%
      hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("Gender: <b>{point.sex} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
      hc_title(text = "Suicides by Gender") %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_plotOptions(pie = list(dataLabels = list(distance = 5, 
                                                  style = list(fontSize = 10)), 
                                size = 250))  %>% 
      hc_add_theme(custom_theme)
  })
  
  ##Gender line chart
  output$genderlineplot <- renderHighchart({
    
    sex_line <- data %>% 
      group_by(year,sex) %>%
      summarize(suicide_per_100k = round(sum(suicides_no)/sum(population)*100000,2)) 
    
    highchart() %>%
      hc_add_series(sex_line, hcaes(x = year, y = suicide_per_100k, group = "sex"), type = "line") %>%
      hc_colors(c("#f8766d", "#00bfc4"))%>%  
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Gender: <b>{point.sex}</b> <br> Year: <b>{point.x}</b> <br> Suicides: <b>{point.y}</b>")) %>%
      hc_title(text = "Trends Over Time, by Gender") %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Suicides per 100K population"),
               allowDecimals = FALSE) %>%
      hc_legend(enabled = TRUE) %>% 
      hc_add_theme(custom_theme) 
  })
  
  
  ##--------------------Worldwide by Age----------------------------------- 
  
  ## Age Pie Chart
  output$agePiePlot <- renderHighchart({
    
    age_color <- rev(plasma(6))
    
    pie_age <- data %>%
      select(age, suicides_no, population) %>%
      group_by(age) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
      arrange(suicide_capita)
    
    highchart() %>% 
      hc_add_series(pie_age, hcaes(x = age, y = suicide_capita, 
                                   color = rev(plasma(6))), type = "pie") %>%
      hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("Age: <b>{point.age} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%  
      hc_title(text = "Worldwide Suicides by Age") %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_plotOptions(pie = list(dataLabels = list(distance = 5, 
                                                  style = list(fontSize = 10)), 
                                size = 250)) %>% 
      hc_add_theme(custom_theme)
  })
  
  ##Worldwide by age boxplot
  age_tibble <- data %>%
    select(year, age, suicides_no, population) %>%
    group_by(year, age) %>%
    summarise(suicide_per_100k = round((sum(suicides_no)/sum(population))*100000, 2))
  
  output$ageBoxPlot <- renderPlotly({
    
    plot_ly(age_tibble, y = ~suicide_per_100k, color= ~age, type = 'box', showlegend = TRUE) %>% layout(
      title = 'Age Distribution by Suicides', 
      xaxis = list(title = 'Age Range',autorange = "reversed"),
      yaxis = list(title = 'Suicide per 100K Population'))
  })
  
  ##Worldwide by generation
  output$generationplot <- renderHighchart({
    
    genline <- data %>% 
      group_by(year,generation) %>%
      summarize(suicide_per_100k = round(sum(suicides_no)/sum(population)*100000,2)) 
    
    gencolor <- rev(plasma(6))
    
    highchart() %>%
      hc_add_series(genline, hcaes(x = year, y = suicide_per_100k, group = "generation"), type = "line") %>%
      hc_colors(colors = gencolor)%>%  
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Generation: <b>{point.generation}</b> <br> Year: <b>{point.x}</b> <br> Suicides: <b>{point.y}</b>")) %>%
      hc_title(text = "Worldwide Suicides by Generation") %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_xAxis(title = list(text = "Year"),
               min = input$yearGen[1],
               max = input$yearGen[2]) %>%
      hc_yAxis(title = list(text = "Suicides per 100K population"),
               allowDecimals = FALSE) %>%
      hc_legend(enabled = TRUE) %>% 
      hc_add_theme(custom_theme)
  })
  
  ##--------------------------------- Continent --------------------------------------------##
  
  ##Continent Bar plot
  output$continentBarPlot <- renderHighchart({
    continent_tibble <- data %>%
      select(continent, sex, suicides_no, population) %>%
      group_by(continent) %>%
      summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    highchart() %>%
      hc_add_series(continent_tibble, hcaes(x = continent, y = suicide_capita, color = suicide_capita), type = "column")  %>% 
      hc_title(text = "Suicides by Continent", style = (list(fontSize = '14px'))) %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Suicides: <b>{point.y}</b>")) %>%
      hc_xAxis(categories = c("Africa", "Asia", "Europe", "North <br> America", "Oceania", "South <br> America"), labels = list(style = list(fontSize = 8))) %>%
      hc_yAxis(labels = list(style = list(fontSize = 10)),
               title = list(text = "Suicides per 100K population",
                            style = list(fontSize = 10)),
               plotLines = list(
                 list(color = "black", width = 1, dashStyle = "Dash", 
                      value = mean(overall_tibble$suicide_capita),
                      label = list(text = "Mean = 13.12", style = list(color = "black", fontSize = 6))))) %>%     
      hc_legend(verticalAlign = 'top', enabled = FALSE) %>% 
      hc_add_theme(custom_theme)
  })
  
  ##Continent map plot
  output$continentMapPlot <- renderHighchart({
    
    map_data <- download_map_data("custom/world-continents")
    
    continent_tibble <- data %>%
      select(continent, suicides_no, population) %>%
      group_by(continent) %>%
      summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
      arrange(suicide_capita)
    
    highchart() %>%
      hc_add_series_map(map_data, continent_tibble, value = "suicide_capita", joinBy = c('name','continent'), name = "Suicide rate")  %>% 
      hc_add_series(continent_tibble, hcaes(x = continent, y = suicide_capita, color = suicide_capita), type = "pie", name = 'Suicide rate')  %>% 
      hc_colorAxis(stops = color_stops()) %>% 
      hc_title(text = "Suicides by Continent", style = (list(fontSize = '14px'))) %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_tooltip(borderWidth = 1.5, valueSuffix = ' ') %>%
      hc_plotOptions(
        pie = list(center = c('10%', '80%'), size = 110, dataLabels = list(enabled = FALSE))) %>% 
      hc_add_theme(custom_theme)
    
  })
  
  ##Continent and sex
  output$continentAndGenderBarPlot <- renderHighchart({
    continent_sex_tibble <- data %>%
      select(continent, sex, suicides_no, population) %>%
      group_by(continent, sex) %>%
      summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    highchart() %>%
      hc_add_series(continent_sex_tibble, hcaes(x = continent, y = suicide_capita, group = sex), type = "column")  %>% 
      hc_colors(colors = sex_color) %>%
      hc_title(text = "Suicides by Continent and <b>Gender</b>", style = (list(fontSize = '14px'))) %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Gender: <b> {point.sex} </b> <br> Suicides: <b>{point.y}</b>")) %>%
      hc_xAxis(categories = c("Africa", "Asia", "Europe", "North <br> America", "Oceania", "South <br> America"), labels = list(style = list(fontSize = 8))) %>%
      hc_yAxis(labels = list(style = list(fontSize = 10)),
               title = list(text = "Suicides per 100K population",
                            style = list(fontSize = 10)),
               plotLines = list(
                 list(color = "black", width = 1, dashStyle = "Dash", 
                      value = mean(overall_tibble$suicide_capita),
                      label = list(text = "Mean = 13.12", style = list(color = "black", fontSize = 6))))) %>%     
      hc_legend(verticalAlign = 'top', enabled = TRUE)%>% 
      hc_add_theme(custom_theme) 
  })
  
  ##Create a tibble for continent and age.
  output$continentAndAgeBarPlot <- renderHighchart({
    
    age_color <- rev(plasma(6))
    
    continent_age_tibble <- data %>%
      select(continent, age, suicides_no, population) %>%
      group_by(continent, age) %>%
      summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    #Create histogram of suicides by continent and age.
    highchart() %>%
      hc_add_series(continent_age_tibble, hcaes(x = continent, y = suicide_capita, group = age), type = "column")  %>% 
      hc_colors(colors = age_color) %>%
      hc_title(text = "Suicides by Continent and <b>Age</b>", style = (list(fontSize = '14px'))) %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Age: <b> {point.age} </b> <br> Suicides: <b>{point.y}</b>")) %>%
      hc_xAxis(categories = c("Africa", "Asia", "Europe", "North <br> America", "Oceania", "South <br> America"), labels = list(style = list(fontSize = 8))) %>%
      hc_yAxis(labels = list(style = list(fontSize = 10)),
               title = list(text = "Suicides per 100K population",
                            style = list(fontSize = 10)),
               plotLines = list(
                 list(color = "black", width = 1, dashStyle = "Dash", 
                      value = mean(overall_tibble$suicide_capita),
                      label = list(text = "Mean = 13.12", style = list(color = "black", fontSize = 6))))) %>%    
      hc_legend(verticalAlign = 'top', enabled = TRUE) %>% 
      hc_add_theme(custom_theme)
  })
  
  ##---------------------------------------Country---------------------------------------##
  
  ##--------------------Overview-----------------------------------
  
  ##Country suicide rate
  output$countryBarPlot <- renderHighchart({
    country_bar <- data %>%
      select(country, suicides_no, population) %>%
      group_by(country) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
      arrange(desc(suicide_capita))
    
    ##Bar plot.
    highchart() %>%
      hc_add_series(country_bar, hcaes(x = country, y = suicide_capita, color = suicide_capita), type = "bar")  %>% 
      hc_tooltip(borderWidth = 1.5, 
                 pointFormat = paste("Suicides: <b>{point.y}</b>")) %>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = "Suicides by Country") %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_xAxis(categories = country_bar$country, 
               labels = list(step = 1),
               min = 0, max = 25,
               scrollbar = list(enabled = TRUE)) %>%
      hc_yAxis(title = list(text = "Suicides per 100K population")) %>%
      hc_plotOptions(bar = list(stacking = "normal", 
                                pointPadding = 0, groupPadding = 0, borderWidth = 0.5)) %>% 
      hc_add_theme(custom_theme) 
  })
  
  ##Country and gdp
  output$countryGDPPlot  <- renderPlotly({
    plot_ly(data = avg_dt, y = ~avg_sui, x= ~avg_gdp, color= ~country,size = ~avg_sui, type = 'scatter', mode = 'markers') %>%
      layout(title = "Suicides vs GDP Per Capita",
             xaxis = list(title = "GDP Per Capita"),
             yaxis = list(title = "Suicides per 100K population")
      )
  }) 
  
  ##Country and sex
  output$countryAndGenderBarPlot <- renderHighchart({
    
    country_bar_sex <- data %>%
      select(country, sex, suicides_no, population) %>%
      group_by(country, sex) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    country_tibble <- data %>%
      select(country, suicides_no, population) %>%
      group_by(country) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 
    
    ##bar plot
    highchart() %>%
      hc_add_series(country_bar_sex, hcaes(x = country, y = suicide_capita, group = sex), type = "bar", color = sex_color)  %>% 
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Gender: <b>{point.sex} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
      hc_legend(enabled = FALSE, colorByPoint = TRUE) %>%
      hc_title(text = "Suicides by Country and Gender") %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_xAxis(categories = country_tibble$country,
               labels = list(step = 1),
               min = 0, max = 25,
               scrollbar = list(enabled = TRUE)) %>%
      hc_yAxis(title = list(text = "Percentage of total suicides")) %>%
      hc_plotOptions(bar = list(stacking = "percent", 
                                pointPadding = 0, groupPadding = 0, borderWidth = 0.4)) %>% 
      hc_add_theme(custom_theme)
  })
  
  ##Country and Age.
  output$countryAndAgeBarPlot <- renderHighchart({
    
    age_color <- rev(plasma(6))
    
    country_bar_age <- data %>%
      select(country, age, suicides_no, population) %>%
      group_by(country, age) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    highchart() %>%
      hc_add_series(country_bar_age, hcaes(x = country, y = suicide_capita, group = age), type = "bar", color = age_color)  %>% 
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Age: <b>{point.age} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
      hc_title(text = "Suicides by Country and Age") %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_xAxis(categories = country_tibble$country,
               labels = list(step = 1),
               min = 0, max = 25,
               scrollbar = list(enabled = TRUE)) %>%
      hc_yAxis(title = list(text = "Percentage of total suicides")) %>%
      hc_plotOptions(bar = list(stacking = "percent", 
                                pointPadding = 0, groupPadding = 0, borderWidth = 0.5)) %>% 
      hc_legend(enabled = FALSE)%>% 
      hc_add_theme(custom_theme)
  })
  
  ##----------------------------Trend-----------------------------------
  
  ##By year
  country_year<-reactive({
    data %>% 
      filter(input$var1 == country) %>% 
      select(country,year,suicides_no,population,gdp_per_capita) %>%
      group_by(year) %>% 
      summarise(suicide_per_100k = round((sum(suicides_no)/sum(population))*100000, 2),gdp_capita = gdp_per_capita) %>% 
      filter( input$var2[1] <= year & input$var2[2] >= year )
  })
  
  output$countrytrend <- renderHighchart({
    highchart() %>%  
      hc_add_series(country_year(), hcaes(x = year, y = suicide_per_100k, color = suicide_per_100k), type = "line") %>%
      hc_colors(c("#95a2f0"))%>%  
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br> Suicides: <b>{point.y}</b>")) %>%
      hc_title(text = "Suicide Rate by Year") %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_xAxis(title = list(text = "Year"), 
               min = input$var2[1],
               max = input$var2[2]) %>%
      hc_yAxis(title = list(text = "Suicides per 100K population"),
               allowDecimals = FALSE) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_add_theme(custom_theme)
  })
  
  ##country gdp year
  output$GDPyear <- renderHighchart({
    highchart() %>%  
      hc_add_series(country_year(),hcaes(x = year, y = gdp_capita, color = gdp_capita), type = "line") %>%
      hc_colors(c("#83c99d"))%>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br> GDP per Capita: <b>{point.y}</b>")) %>%
      hc_title(text = "GDP per Capita by Year") %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_xAxis(title = list(text = "Year"), 
               min = input$var2[1],
               max = input$var2[2]) %>%
      hc_yAxis(title = list(text = "GDP per Capita"),
               allowDecimals = FALSE) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_add_theme(custom_theme)
  })
  
  output$bar_title <- renderText({
    explanation <- paste0("Which is the most vulnerable age group in ", input$oneyear, " in ", input$var1, "?")
  })
  
  output$ageSexPlot <- renderHighchart({
    ageSexTibble<- data %>% filter(input$oneyear == year, input$var1 == country)
    
    highchart() %>%
      hc_add_series(ageSexTibble, hcaes(x = age, y = suicides_no, group = sex), type = "column")  %>% 
      hc_colors(colors = sex_color) %>%
      hc_title(text = "Suicides Number by Each Age Groups", style = (list(fontSize = '14px'))) %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Gender: <b>{point.sex}</b> <br>Suicides: <b>{point.y}</b>")) %>%
      hc_xAxis(categories = c("75+", "55-74", "35-54", "25-34", "15-24", "5-14"), reversed = TRUE, labels = list(style = list(fontSize = 8)),
               title = list(text = "Age Groups",
                            style = list(fontSize = 10))) %>%
      hc_yAxis(labels = list(style = list(fontSize = 10)),
               title = list(text = "Suicides Number",
                            style = list(fontSize = 10)))%>%
      hc_legend(verticalAlign = 'top', enabled = TRUE) %>% 
      hc_add_theme(custom_theme)
    
  })
  
  ##----------------------------------Linear regression---------------------------------##
  output$lmGDP <- renderPlot({
    ggplot(gdp_suicide_no_outliers, aes(x = gdp_per_capita, y = suicides_per_100k, col = continent)) + 
      geom_point() + 
      geom_smooth(method = "lm", aes(group = 1),linewidth=1.25, alpha = 0.2) + 
      scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
      labs(title = "Relationship between GDP per Capita and Suicides per 100k", 
           
           x = "GDP (per capita)", 
           y = "Suicides_no per 100k", 
           col = "Continent") + 
      theme_light()})
  
  output$pred1p <- renderText(anova(model2)$'Pr(>F)'[1])
  
  output$pred1slope <- renderText(model2[[1]][2])
  
  output$pred1intercept <- renderText(model2[[1]][1])
  
  output$note <- renderText({
    text <- paste("Note that: If p value is < 0.05, we conclude that the relationship between the independent 
                  variable and suicides value is statistically significant.")
  })
  
  output$sentence <- renderText({
    text <- paste(" If we look at the relationship between total population and the total number of suicides,the regression line is upsloping, 
    indicating suicides number increases as population increases. However, if we look at the suicide rates, the regression line is flat which 
                  indicates increase in population does not have effect on the suicide rates.")
  })
  
  output$lmPopRate <- renderPlot({
    ggplot(population_suicide_no_outliers, aes(x = total_population, y = suicides_per_100k)) +
      geom_point() +
      geom_smooth(method = 'lm', aes(group = 1)) +
      labs(title = 'Suicides Rate vs Total Population', x = "total_population", 
           y = "suicides_per_100k") +
      theme_light()
  })
  
  output$lmPopTotal <- renderPlot({
    ggplot(population_suicide_outliers, aes(x = total_population, y = total_suicides)) +
      geom_point() +
      geom_smooth(method = 'lm', aes(group = 1)) +
      labs(title = 'Total Suicides Number vs Total Population', x = "total_population", 
           y = "total_suicides") +
      theme_light()
  })
  
  ##correlation
  c2 <- data  %>% select(-c('continent','country','gdp_for_year')) 
  cols = c("year","sex","age","generation")
  c2[, cols] <- c2 %>% select(all_of(cols)) %>% lapply(as.numeric)
  
  cor2 <- cor(c2)
  
  corr <- round(cor(c2, use="complete.obs"), 2)
  output$corrplot <- renderPlot({
    ggcorrplot(corr, lab = TRUE, colors = c("lightcoral", "white", "dodgerblue"), 
               show.legend = T, outline.color = "gray", type = "upper", hc.order = TRUE,
               tl.cex = 15, lab_size = 5, sig.level = .2) +labs(fill = "Correlation")
  })
  
  ##-----------------------------------Data---------------------------------------------##
  output$table <- renderDataTable(DT::datatable({
    data <- data
    if (input$country != "All") {
      data <- data[data$country == input$country,]
    }
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$age != "All") {
      data <- data[data$age == input$age,]
    }
    data
  }))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data <- data
      if (input$country != "All") {
        data <- data[data$country == input$country,]
      }
      if (input$year != "All") {
        data <- data[data$year == input$year,]
      }
      if (input$age != "All") {
        data <- data[data$age == input$age,]
      }
      data
      write.csv(data, file)
    }
  )
}

shinyApp(ui = ui, server = server)     
