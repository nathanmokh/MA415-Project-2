library(tidyverse)
library(stringr)


# Read in data from website

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1987:2017)[-27] #2013 is not available

urls <- str_c(url1, years, url2, sep = "")

filenames <- str_c("mr", years, sep = "")


# separates file into each year

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


# Data clean up
# filter time to get records at noon
# mutate to change into numeric and interger class
# mutate to fill 999 values as NA
# unite and mutate to put date into one column

Data <- MR %>% filter((hh == "11" & mm == "50") | (hh == "12" & mm == "00")) %>% 
  mutate(ATMP = as.numeric(ATMP), WTMP = as.numeric(WTMP), MM = as.integer(MM), 
         DD = as.integer(DD)) %>% mutate(ATMP = ifelse(ATMP > 90, NA, ATMP)) %>% 
  mutate(WTMP = ifelse(WTMP > 90, NA, WTMP)) %>% unite("Date", c("YYYY", "MM", "DD"), sep = "-") %>% 
  mutate(Date = as.Date(Date))

# Monthly ATMP boxplot (example for 1988) for shiny
mymonths <- c("Jan","Feb","Mar", "Apr","May","Jun", "Jul","Aug","Sep", "Oct","Nov","Dec")

by_year <- Data %>% separate(Date, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
  filter(YYYY == 1988) 

ggplot(data = by_year, mapping = aes(x = MM, y = ATMP)) + 
  geom_boxplot(na.rm = TRUE) +
  coord_flip() +
  labs(title = "Air Temperature by Month",
       subtitle = "Data obtained from the National Data Buoy Center",
       y = "Temperature (Celcius)",
       x = "Month")

# Monthly WTMP boxplot (example for 1988) for shiny
mymonths <- c("Jan","Feb","Mar", "Apr","May","Jun", "Jul","Aug","Sep", "Oct","Nov","Dec")

by_year <- Data %>% separate(Date, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
  filter(YYYY == 1988) 

ggplot(data = by_year, mapping = aes(x = MM, y = WTMP)) + 
  geom_boxplot(na.rm = TRUE) +
  coord_flip() +
  labs(title = "Sea Temperature by Month",
       subtitle = "Data obtained from the National Data Buoy Center",
       y = "Temperature (Celcius)",
       x = "Month")



# Time Series Using tidyverse (Air)
Data %>% ggplot(aes(Date, ATMP)) +
  geom_line(na.rm = TRUE, col = "red") +
  labs(title = "Air Temperature at the Bering Sea",
       subtitle = "Data obtained from the National Data Buoy Center",
       y = "Temperature (Celcius)",
       x = "Year")

# Time Series Using tidyverse (Sea)
Data %>% ggplot(aes(Date, WTMP)) +
  geom_line(na.rm = TRUE, col = "blue") +
  labs(title = "Sea Temperature at the Bering Sea",
       subtitle = "Data obtained from the National Data Buoy Center",
       y = "Temperature (Celcius)",
       x = "Year")

# Time Series Using tidyverse (Both)

df <- data.frame(Data$Date, Data$ATMP, Data$WTMP)
ggplot(df, aes(Data$Date, y = value, color = variable)) + 
  geom_line(aes(y = Data$ATMP, col = "Air Temperature")) + 
  geom_line(aes(y = Data$WTMP, col = "Sea Temperature")) +
  labs(title = "Temperature at the Bering Sea",
       subtitle = "Data obtained from the National Data Buoy Center",
       y = "Temperature (Celcius)",
       x = "Year")

# Correlation of air and sea temperatures
ATPM <- Data$ATMP
WTMP <- Data$WTMP
cor.test(c(ATPM,NA),c(WTMP,NA),use = "complete.obs")

# T test of mean Air temperature from the past 30 years using noon observations
x <- Data %>% separate(Date, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
  filter(YYYY == 1987)
y <- Data %>% separate(Date, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
  filter(YYYY == 2017)

x <- x$ATMP
y <- y$ATMP

t.test(x, y, na.rm=TRUE)

# T test of mean Water temperature from the past 30 years using noon observations
x <- Data %>% separate(Date, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
  filter(YYYY == 1987)
y <- Data %>% separate(Date, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
  filter(YYYY == 2017)

x <- x$WTMP
y <- y$WTMP

t.test(x, y, na.rm=TRUE)

# T test of mean Air temperature using all observations (not just noon)
mr1987 <- mr1987 %>% mutate(ATMP = ifelse(ATMP > 90, NA, ATMP)) %>% 
  mutate(ATMP = as.numeric(ATMP))
mr2017 <- mr2017 %>% mutate(ATMP = ifelse(ATMP > 90, NA, ATMP)) %>% 
  mutate(ATMP = as.numeric(ATMP))

x <- mr1987$ATMP
y <- mr2017$ATMP

t.test(x, y, na.rm=TRUE)

# T test of mean Sea temperature using all observations (not just noon)
mr1987 <- mr1987 %>% mutate(WTMP = ifelse(WTMP > 90, NA, WTMP)) %>% 
  mutate(WTMP = as.numeric(WTMP))
mr2017 <- mr2017 %>% mutate(WTMP = ifelse(WTMP > 90, NA, WTMP)) %>% 
  mutate(WTMP = as.numeric(WTMP))

x <- mr1987$WTMP
y <- mr2017$WTMP

t.test(x, y, na.rm=TRUE)
