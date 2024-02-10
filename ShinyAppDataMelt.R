## Melt data for Shiny App Data Overview Tab
# End result: want a table with time (years) on the X and solutes/ discharge on the y axis
# colored by # of sites that have each solute/ discharge

# Load libraries needed for script
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, purrr, readxl, supportR, dplyr, tidyr, ggplot2)

# Clear environment
rm(list = ls())

## originally this was linked to the google drive but that's not working right now
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Read in dataset that has the stream names for the 201 sites
sites200 <-read.csv("AllSiClusterData.csv")
# check the length of this data set
length(unique(sites200$Site))

# Read in master chemistry dataset
chem_v1 <- read.csv("20240130_masterdata_chem.csv") %>%
  # Fix some column names
  dplyr::rename(Date = date) %>%
  #put date in "date" format
  dplyr::mutate(Date = as.Date(Date))

# Drop any rows where date or variable/value is not included
chem_v2 <- chem_v1 %>%
  dplyr::filter(!is.na(Date) & !is.na(variable) & !is.na(value))

# Filter to only the 201 sites included in cluster dataset
chemQ <- chem_v2 %>%
  dplyr::filter(Stream_Name %in% sites200$Site)

# check the length of this data set
length(unique(chemQ$Stream_Name)) # 194?! ABSOLUTE BAG OF PANTS

# remove variables not interested in ever including
chemQ <- dplyr::select(chemQ, -c("LTER","Dataset", "units", "value", "Raw_Filename"))

# rename columns
colnames(chemQ) <-c("Stream_Name","Date", "variable")

# now combine all N (NO3, NOx) and P (PO4, SRP) species, change name to DOC
# kinda dumb but I'm doing it this way

chemQ$std_variable <- chemQ$variable
chemQ <- chemQ %>%
  mutate(std_variable = case_when(
    str_detect(std_variable, "SRP") ~ "P",
    str_detect(std_variable, "PO4") ~ "P",
    str_detect(std_variable, "TP") ~ "P",
    str_detect(std_variable, "TN") ~ "N",
    str_detect(std_variable, "NO3") ~ "N",
    str_detect(std_variable, "NOx") ~ "N",
    str_detect(std_variable, "tot dissolved N") ~ "N",
    str_detect(std_variable, "NHX") ~ "N",
    str_detect(std_variable, "NH4") ~ "N",
    str_detect(std_variable, "dissolved inorg N") ~ "N",
    str_detect(std_variable, "dissolved org C") ~ "DOC",
    TRUE ~ std_variable))

# format date as date
chemQ$Date<-as.Date(chemQ$Date) 

# extract the year and convert to numeric format
chemQ$year <- as.numeric(format(chemQ$Date, "%Y"))

#create a column of "unique" values - stream name, type of sample, and year collected
chemQ$unique<-paste0(chemQ$Stream_Name, chemQ$year, chemQ$std_variable)

# remove all duplicated samples
chemQ_unique <-chemQ[!duplicated(chemQ$unique),] 

# there's one site for N that was collected in 1924 and its the only one, lets get that outta here
chemQ_unique <- chemQ_unique %>%
  filter(year > 1950)

# get freq by year and by variable
chemQ_final <- setDT(chemQ_unique)[, .(freq = .N) , by = c("year","std_variable")]

## Filter which variables we wanna use in our final plot
chemQ_final <- chemQ_final %>% filter(grepl("DSi|Ca|Mg|N|SO4|P|Na|DOC", std_variable))

chemQ_final <-setDF(chemQ_final)

ggplot(chemQ_final, aes(x=year, y=std_variable))+
  labs(x=NULL, y = "Variable")+
  geom_line(aes(col=freq), lwd=15)+
  scale_color_gradient(low = "lightblue", high="darkblue")+
  theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title = element_text(size=16, face="bold"))

# ## Export csv
write.csv(chemQ_final,file="shinyapp_201_chem_sites_by_year.csv")
