library(tidyverse)
library(readxl)
library(stringr)

# read in vegetable data
veg.1 <- read_xlsx("veg1.xlsx")

# created data table of restricted use chemicals (toxicity measurements)
# toxicity measurements taken from EPA, average toxicity given to rat
toxicity <- read_xlsx("toxicitiesAndChemicals.xlsx")

# get column names of vegetable data
cnames.1 <- colnames(veg.1)

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

# columns with more than one unique value 
veg.2 <- select(veg.1, e)

cnames.2 <- colnames(veg.2)
cnames.2

apply(veg.2, 2, n_distinct)

# rename veg.2 columns
veg.3 <- dplyr::rename(veg.2, 
                       Geo = `Geo Level`, 
                       State = `State ANSI`,
                       Data = `Data Item`,
                       Category = `Domain Category`)

cnames.3 <- colnames(veg.3)
cnames.3

veg.3

# look into what each column consists of
unique(veg.3[,"Commodity"])

unique(veg.3[,"Data"]) %>% print(n=60)

unique(veg.3[,"Domain"])

unique(veg.3[,"Category"])

unique(veg.3[,"Value"])

### Tidy data
# separate/mutate data so that each column has a different sector 

Data <- veg.3 %>% 
  separate(Domain, into = c("Chemical", "Type"), sep = ", ") %>% 
  separate(Category, into = c("Delete", "Category"), sep = ": ") %>% 
  select(-Delete) %>%
  
  # change "not specified" to NA 
  mutate(Category = ifelse(is.na(Category), "(NOT SPECIFIED)", Category)) %>% 
  
  # mutate to only have multi-state vs. state
  select(-c(State,Region)) %>% 
  mutate(Geo = ifelse(Geo=="STATE", "STATE", "MULTI-STATE")) %>% 
  
  # get rid of repeating commodity in category
  mutate(Category = stringr::str_replace_all(Category, "[()]", "")) %>% 
  mutate(Data = substr(Data, str_length(Commodity) + 3, str_length(Data))) %>% 

  mutate(Type = ifelse(Chemical == "FERTILIZER", Chemical, Type)) %>% 
  mutate(Chemical = ifelse(Chemical=="FERTILIZER", NA, Chemical)) %>% 
  
  # augment evaluation of chemical treatments (toxicity measurements) applied to vegetables
  left_join(toxicity, by = "Category") %>% 
  separate(Category, into = c("Category", "Code"), sep = "=") %>% 
  mutate(Code = as.numeric(Code)) %>% 
  mutate(Category = str_trim(Category)) %>% 
  
  separate(Data, into = c("Data", "Measurement"), sep = ", MEASURED IN ") %>% 
  mutate(Measurement = ifelse(is.na(Measurement), "ACRES POLLINATED, PAID BASIS", Measurement)) %>% 
  mutate(Data = str_replace(Data, " - ACRES POLLINATED, PAID BASIS", ""))



## Pie Charts 
#### Type of Chemicals (Broccoli example for shiny)

# get count of number of broccoli
broccoli.count <- filter(Data, Commodity == "BROCCOLI")
bcount <- nrow(broccoli.count)

# get counts of different types
rsc.count <- filter(Data, Commodity == "BROCCOLI", Type == "FUNGICIDE")
rsc <- nrow(rsc.count)

herbicide <- filter(Data, Commodity == "BROCCOLI", Type == "HERBICIDE")
h <- nrow(herbicide)

insecticide <- filter(Data, Commodity == "BROCCOLI", Type == "INSECTICIDE")
i <- nrow(insecticide)

other <- filter(Data, Commodity == "BROCCOLI", Type == "OTHER")
o <- nrow(other)

avoidance <- filter(Data, Commodity == "BROCCOLI", Type == "AVOIDANCE")
a <- nrow(avoidance)

monitoring <- filter(Data, Commodity == "BROCCOLI", Type == "MONITORING")
m <- nrow(monitoring)

prevention <- filter(Data, Commodity == "BROCCOLI", Type == "PREVENTION")
p <- nrow(prevention)

suppression <- filter(Data, Commodity == "BROCCOLI", Type == "SUPPRESSION")
s <- nrow(suppression)

fertilizer <- filter(Data, Commodity == "BROCCOLI", Type == "FERTILIZER")
f <- nrow(fertilizer)

# create slices for different types
slices <- c(rsc, h, i, o, a, m, p, s, f, bcount - (rsc + h + i + o + a + m + p + s + f)) 
lbls <- c("FUNGICIDE", "HERBICIDE", "INSECTICIDE", "OTHER", "AVOIDANCE",
          "MONITORING", "PREVENTION", "SUPPRESSION", "FERTILIZER", "NA")
pct <- round(slices/bcount*100)
lbls <- paste(ifelse(pct>1, lbls, ""), ifelse(pct>1, pct, "")) # add percents to labels 
lbls <- paste(lbls, ifelse(pct>1, "%", ""),sep="") # add % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Broccoli", radius=1, cex=0.7)


#### Chemicals Pie Chart for Broccoli (Restricted vs Non)
broccoli.count <- filter(Data, Commodity == "BROCCOLI")
bcount <- nrow(broccoli.count)

# get count for restricted chemical use
rsc.count <- filter(Data, Commodity == "BROCCOLI", Chemical == "RESTRICTED USE CHEMICAL")
rsc <- nrow(rsc.count)

slices <- c(rsc, bcount - rsc) 
lbls <- c("RESTRICTED USE CHEMICALS", "NONRESTRICTED USE CHEMICALS")
pct <- round(slices/bcount*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # add % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Broccoli")


# create barplot for restricted use chemicals vs toxicity measurement
titles <- as.vector(toxicity[[1]])
values <- as.vector(toxicity[[2]])
op <- par(mar=c(11,4,4,2))
barplot(values, main='Chemicals and their Toxicity Levels', names.arg = titles, las = 2, col = 'skyblue', ylab = 'Toxicity Level (mg/kg)', cex.names = 0.7)
  
