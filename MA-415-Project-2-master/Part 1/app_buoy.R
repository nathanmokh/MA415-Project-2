library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)

##Data Load

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1987:2017)[-27]

urls <- str_c(url1, years, url2, sep = "")

filenames <- str_c("mr", years, sep = "")




N <- length(urls)

for (i in 1:N){
  suppressMessages(
    assign(filenames[i], read_table(urls[i], col_names = TRUE))
  )

  file <- get(filenames[i])

  colnames(file)[1] <-"YYYY"

  if (i < 19) {
    file <- file %>% mutate(mm = "00")
  }

  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)

  if (i >= 21) {
    file <- file[-1,]
  }

  # put '19' in front of 2 digit years
  if (i >= 27) {
    file[1] <- i + 1987
  }
  else{
    file[1] <- i + 1986
  }



  if(i == 1){
    MR <- file
  }

  else{
    MR <- rbind.data.frame(MR, file)
  }



}


##Data clean up

Data <- MR %>% filter((hh == "11" & mm == "50") | (hh == "12" & mm == "00")) %>%
  mutate(ATMP = as.numeric(ATMP), WTMP = as.numeric(WTMP), MM = as.integer(MM),
         DD = as.integer(DD)) %>% mutate(ATMP = ifelse(ATMP > 90, NA, ATMP)) %>%
  mutate(WTMP = ifelse(WTMP > 90, NA, WTMP)) %>% unite("Date", c("YYYY", "MM", "DD"), sep = "-") %>%
  mutate(Date = as.Date(Date))


##App

#Ui
ui <- dashboardPage(
  dashboardHeader(title = "Buoy 46035"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series", tabName = "time_series"),
      menuItem("Air Temperature by Year", tabName = "boxplot_air"),
      menuItem("Sea Temperature by Year", tabName = "boxplot_sea")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "time_series",
              fluidRow(
                box(selectInput("slot_mode",
                                "Mode:",
                                choices = list("Air Temperature", "Sea Temperature",
                                               "Air and Sea Temperatures")), 
                    plotOutput("plot1"), width = 12)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "boxplot_air",
              fluidRow(
                box(selectInput("slot_year_air",
                                "Year:",
                                choices = c(1987:2017)[-27]), 
                    plotOutput("plot2"), width = 12)
              )
      ),
      
      # Third tab content
      tabItem(tabName = "boxplot_sea",
              fluidRow(
                box(selectInput("slot_year_sea",
                                "Year:",
                                choices = c(1987:2017)[-27]), 
                    plotOutput("plot3"), width = 12)
              )
      )
    )
  )
)

#Server
server <- function(input, output) { 
  
  output$plot1 <- renderPlot({
    
    if (input$slot_mode == "Air Temperature") {
      
      ##TS Using tidyverse (Air)
      p <- Data %>% ggplot(aes(Date, ATMP)) +
        geom_line(na.rm = TRUE, col = "red") +
        labs(title = "Air Temperature at the Bering Sea",
             subtitle = "Data obtained from the National Data Buoy Center",
             y = "Temperature (Celcius)",
             x = "Year")
      print(p)
    }
    
    if (input$slot_mode == "Sea Temperature") {
      
      ##TS Using tidyverse (Sea)
      p <- Data %>% ggplot(aes(Date, WTMP)) +
        geom_line(na.rm = TRUE, col = "blue") +
        labs(title = "Sea Temperature at the Bering Sea",
             subtitle = "Data obtained from the National Data Buoy Center",
             y = "Temperature (Celcius)",
             x = "Year")
      print(p)
    }
    
    if (input$slot_mode == "Air and Sea Temperatures") {
      
      ##TS Using tidyverse (Both)
      df <- data.frame(Data$Date, Data$ATMP, Data$WTMP)
      p <- ggplot(df, aes(Data$Date, y = value, color = variable)) +
        geom_line(aes(y = Data$ATMP, col = "Air Temperature")) +
        geom_line(aes(y = Data$WTMP, col = "Sea Temperature")) +
        labs(title = "Temperature at the Bering Sea",
             subtitle = "Data obtained from the National Data Buoy Center",
             y = "Temperature (Celcius)",
             x = "Year")
      print(p)
    }

  })
  
  output$plot2 <- renderPlot({
    
    #Monthly ATMP boxplot

    by_year <- Data %>% separate(Date, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
      filter(YYYY == input$slot_year_air)
    
    p <- ggplot(data = by_year, mapping = aes(x = MM, y = ATMP)) + 
      geom_boxplot(na.rm = TRUE) +
      coord_flip() +
      labs(title = "Air Temperature by Month",
           subtitle = "Data obtained from the National Data Buoy Center",
           y = "Temperature (Celcius)",
           x = "Month")
    print(p)

  })
  
  output$plot3 <- renderPlot({
    
    #Monthly WTMP boxplot
    
    by_year <- Data %>% separate(Date, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
      filter(YYYY == input$slot_year_sea)
    
    p <- ggplot(data = by_year, mapping = aes(x = MM, y = WTMP)) + 
      geom_boxplot(na.rm = TRUE) +
      coord_flip() +
      labs(title = "Sea Temperature by Month",
           subtitle = "Data obtained from the National Data Buoy Center",
           y = "Temperature (Celcius)",
           x = "Month")
    print(p)
    
  })
  
}

shinyApp(ui, server)