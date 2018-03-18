library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(stringr)

## Load Data
options(warn=-1)
veg.1 <- read_xlsx("veg1.xlsx")

toxicity <- read_xlsx("toxicitiesAndChemicals.xlsx")

cnames.1 <- colnames(veg.1)

## try
n_distinct(veg.1[,1])

n_distinct(veg.1[,2])

unique(veg.1[,2])

## now get the count for each column

c <- apply(veg.1, 2, n_distinct)
c


c[c>1]


d <- names(c[c==1])
d

e <- names(c[c>1])
e


veg.2 <- select(veg.1, e)

cnames.2 <- colnames(veg.2)
cnames.2

apply(veg.2, 2, n_distinct)

veg.3 <- dplyr::rename(veg.2, 
                       Geo = `Geo Level`, 
                       State = `State ANSI`,
                       Data = `Data Item`,
                       Category = `Domain Category`)

cnames.3 <- colnames(veg.3)
cnames.3

veg.3

unique(veg.3[,"Commodity"])

unique(veg.3[,"Data"]) %>% print(n=60)

unique(veg.3[,"Domain"])

unique(veg.3[,"Category"])

unique(veg.3[,"Value"])

### Tidy data
Data <- veg.3 %>% separate(Domain, into = c("Chemical", "Type"), sep = ", ") %>% 
  separate(Category, into = c("Delete", "Category"), sep = ": ") %>% select(-Delete) %>% 
  mutate(Category = ifelse(is.na(Category), "(NOT SPECIFIED)", Category)) %>% 
  select(-c(State,Region)) %>% mutate(Geo = ifelse(Geo=="STATE", "STATE", "MULTI-STATE")) %>% 
  mutate(Category = stringr::str_replace_all(Category, "[()]", "")) %>% 
  mutate(Data = substr(Data, str_length(Commodity) + 3, str_length(Data))) %>% 
  mutate(Type = ifelse(Chemical == "FERTILIZER", Chemical, Type)) %>% 
  mutate(Chemical = ifelse(Chemical=="FERTILIZER", NA, Chemical)) %>% 
  left_join(toxicity, by = "Category") %>% separate(Category, into = c("Category", "Code"),
                                                    sep = "=") %>% 
  mutate(Code = as.numeric(Code)) %>% mutate(Category = str_trim(Category)) %>% 
  separate(Data, into = c("Data", "Measurement"), sep = ", MEASURED IN ") %>% 
  mutate(Measurement = ifelse(is.na(Measurement), "ACRES POLLINATED, PAID BASIS",
                              Measurement)) %>% 
  mutate(Data = str_replace(Data, " - ACRES POLLINATED, PAID BASIS", ""))

##App

#Ui
ui <- dashboardPage(
  dashboardHeader(title = "Vegetables"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Chemical Use Percentage", tabName = "pie_chart"),
      menuItem("Toxicity Levels", tabName = "bar_graph"),
      menuItem("Chemical Breakdown", tabName = "pie_chart2")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "pie_chart",
              fluidRow(
                box(radioButtons("slot_veg", label = "Vegetable:",
                                 choices = unique(Data$Commodity)), 
                    plotOutput("plot1"), width = 12)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "bar_graph",
              fluidRow(
                box(
                    plotOutput("plot2"), width = 12)
              )
              
      ),
      
      # Third tab content
      tabItem(tabName = "pie_chart2",
              fluidRow(
                box(radioButtons("slot_veg2", label = "Vegetable:",
                                 choices = unique(Data$Commodity)), 
                    plotOutput("plot3"), width = 12)
              )
      )
      
    )
  )
)


#Server
server <- function(input, output) { 
  
  output$plot1 <- renderPlot({
    
    ## Pie Chart
    broccoli.count <- filter(Data, Commodity == input$slot_veg)
    bcount <- nrow(broccoli.count)
    
    rsc.count <- filter(Data, Commodity == input$slot_veg, Chemical == "RESTRICTED USE CHEMICAL")
    rsc <- nrow(rsc.count)
    
    slices <- c(rsc, bcount - rsc) 
    lbls <- c("RESTRICTED USE CHEMICALS", "NONRESTRICTED USE CHEMICALS")
    pct <- round(slices/bcount*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(slices,labels = lbls, col=rainbow(length(lbls)),
        main=input$slot_veg)
    
  })
  
  output$plot2 <- renderPlot({
    
    #Bar Graph
    titles <- as.vector(toxicity[[1]])
    values <- as.vector(toxicity[[2]])
    op <- par(mar=c(11,4,4,2))
    barplot(values, main='Chemicals and their Toxicity Levels', names.arg = titles, las = 2, col = 'skyblue', ylab = 'Toxicity Level (mg/kg)')
    
  })
  
  output$plot3 <- renderPlot({
    
    ## Pie Charts 
    # Type of Chemicals
    broccoli.count <- filter(Data, Commodity == input$slot_veg2)
    bcount <- nrow(broccoli.count)
    
    rsc.count <- filter(Data, Commodity == input$slot_veg2, Type == "FUNGICIDE")
    rsc <- nrow(rsc.count)
    
    herbicide <- filter(Data, Commodity == input$slot_veg2, Type == "HERBICIDE")
    h <- nrow(herbicide)
    
    insecticide <- filter(Data, Commodity == input$slot_veg2, Type == "INSECTICIDE")
    i <- nrow(insecticide)
    
    other <- filter(Data, Commodity == input$slot_veg2, Type == "OTHER")
    o <- nrow(other)
    
    avoidance <- filter(Data, Commodity == input$slot_veg2, Type == "AVOIDANCE")
    a <- nrow(avoidance)
    
    monitoring <- filter(Data, Commodity == input$slot_veg2, Type == "MONITORING")
    m <- nrow(monitoring)
    
    prevention <- filter(Data, Commodity == input$slot_veg2, Type == "PREVENTION")
    p <- nrow(prevention)
    
    suppression <- filter(Data, Commodity == input$slot_veg2, Type == "SUPPRESSION")
    s <- nrow(suppression)
    
    fertilizer <- filter(Data, Commodity == input$slot_veg2, Type == "FERTILIZER")
    f <- nrow(fertilizer)
    
    slices <- c(rsc, h, i, o, a, m, p, s, f, bcount - (rsc + h + i + o + a + m + p + s + f)) 
    lbls <- c("FUNGICIDE", "HERBICIDE", "INSECTICIDE", "OTHER", "AVOIDANCE",
              "MONITORING", "PREVENTION", "SUPPRESSION", "FERTILIZER", "NA")
    pct <- round(slices/bcount*100)
    lbls <- paste(ifelse(pct>1, lbls, ""), ifelse(pct>1, pct, "")) # add percents to labels 
    lbls <- paste(lbls, ifelse(pct>1, "%", ""),sep="") # ad % to labels 
    pie(slices,labels = lbls, col=rainbow(length(lbls)),
        main=input$slot_veg2, radius=1, cex=1)
    
  })
  
}

shinyApp(ui, server)

