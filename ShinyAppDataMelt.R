## Melt data for Shiny App Data Overview Tab
# End result: want a table with time (years) on the X and solutes/ discharge on the y axis
# colored by # of sites that have each solute/ discharge
# Going to use Nick's workflow for merging C and Q data from the google drive, but skip the step
# where we delete non-matched CQ data

## -------------------------------------------------------- ##
#     Combining Concentration and Discharge for plotting
## -------------------------------------------------------- ##
# Script Authors: Nick J Lyon

# PURPOSE:
## Combine solute & discharge information

# Load libraries needed for script
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, purrr, readxl, supportR, dplyr, tidyr, ggplot2,scales)

# Clear environment
rm(list = ls())

## ----------------------------------- ##
# Read in Data ----
## ----------------------------------- ##

## originally this was linked to the google drive but that's not working right now
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Read in reference table
# ref_table <- readxl::read_excel(path = file.path("raw_data", "Site_Reference_Table.xlsx"))
ref_table <-read.csv("Site_Reference_Table.csv")

# Check the structure of that
dplyr::glimpse(ref_table)
length(unique(ref_table$Stream_Name))
length(unique(ref_table$Discharge_File_Name)) #Discharge_File_Name denotes where the discharge data is coming from

# Make a simpler version with just the various stream names
name_table <- ref_table %>%
  dplyr::select(LTER, Stream_Name, Discharge_File_Name) %>%
  # Drop non-unique rows
  dplyr::distinct() #this drops 3 non-unique sites

# Check strcture
dplyr::glimpse(name_table)

# Read in chemistry data
# chem_v1 <- read.csv(file.path("raw_data", "20240102_masterdata_chem.csv")) %>%
chem_v1 <- read.csv("20240130_masterdata_chem.csv") %>%
  # Fix some column names
  dplyr::rename(Date = date) %>%
  #put date in "date" format
  dplyr::mutate(Date = as.Date(Date))

# Check structure
dplyr::glimpse(chem_v1)
#how many sites?
length(unique(chem_v1$Stream_Name))

# Drop any rows where date or variable/value is not included
chem_v2 <- chem_v1 %>%
  dplyr::filter(!is.na(Date) & !is.na(variable) & !is.na(value))
#how many sites?
length(unique(chem_v2$Stream_Name))

# Read in discharge
# disc_v1 <- read.csv(file.path("raw_data", "Discharge_master_10232023.csv")) %>%
disc_v1 <- read.csv("20240201_masterdata_discharge.csv") %>%
  # Fix stream name column
  # dplyr::rename(Discharge_File_Name = DischargeFileName) %>%
  # change date to "date" format
  dplyr::mutate(Date = as.Date(Date))

#check
glimpse(disc_v1)
length(unique(disc_v1$Discharge_File_Name))
#511 unique sites

# Drop any rows where date or variable/value is not included
disc_v2 <- disc_v1 %>%
  dplyr::filter(!is.na(Date) & !is.na(Qcms))
#check number of sites
length(unique(disc_v2$Discharge_File_Name))
#494 unique sites

## ----------------------------------- ##
# Prep for Merge ----
## ----------------------------------- ##

# Remove rivers not in reference table
## Discharge
disc_v3 <- disc_v2 %>%
  dplyr::filter(Discharge_File_Name %in% name_table$Discharge_File_Name)
## Chemistry
chem_v3 <- chem_v2 %>%
  dplyr::filter(Stream_Name %in% name_table$Stream_Name)

# Lose any rows?
nrow(disc_v2) - nrow(disc_v3)
nrow(chem_v2) - nrow(chem_v3)

# Which rivers do we lose?
## Discharge
supportR::diff_check(old = unique(disc_v2$Discharge_File_Name), 
                     new = unique(disc_v3$Discharge_File_Name))
## Chemistry
supportR::diff_check(old = unique(chem_v2$Stream_Name), 
                     new = unique(chem_v3$Stream_Name))

#How many in new files?
length(unique(disc_v3$Discharge_File_Name)) #494
length(unique(chem_v3$Stream_Name)) #585

## ----------------------------------- ##
# Join Chem & Discharge ----
## ----------------------------------- ##

#check structure of disc_v2 and chem_v2
glimpse(disc_v3)
glimpse(chem_v3)

#create discharge and chem "master data" with similar columns for easy merging in future
disc_v4 <- disc_v3 %>%
  # Attach the name table
  dplyr::left_join(y = name_table, by = c("Discharge_File_Name"))
  # select(Qcms,Date, Discharge_File_Name, LTER,Stream_Name) #for data.frames with more columns, will need a check to make sure columns aren't dropped

#check this
glimpse(disc_v4)
length(unique(disc_v4$Stream_Name))

#check the many-to-many warnings
name_table %>% filter(Discharge_File_Name=="ARC_Imnavait_fill_Q")
disc_v2 %>% filter(Discharge_File_Name=="AAGEVEG_Q") %>% pull(Discharge_File_Name) %>% unique()
#pull() makes a column and returns it as a vector

#the number of rows should be the same, to make check discharge is added for multi-alias'd sites
disc_v2 %>% filter(Discharge_File_Name=="AAGEVEG_Q") %>% nrow()
disc_v3 %>% filter(Discharge_File_Name=="AAGEVEG_Q") %>% nrow()

# write.csv(disc_v4,file="discharge_with_stream_name.csv")

#add discharge file name to chem_v2
chem_v4 <- chem_v3 %>%
  select(-LTER) %>% #don't use LTER from chemistry file, will get version from name_table
  dplyr::left_join(y = name_table, by = c("Stream_Name"))

glimpse(chem_v4)
length(unique(chem_v4$Stream_Name))

# Create the first combination dataframe
glimpse(chem_v4)
glimpse(disc_v4)
disc_v4<-dplyr::select(disc_v4, -c("LTER.y","Stream_Name.y"))
glimpse(disc_v4)
colnames(disc_v4) <-c("Qcms","Date","Discharge_File_Name","LTER","Stream_Name")
glimpse(disc_v4)
disc_v4$variable <-"Q"
glimpse(disc_v4)

cq_v1 <- disc_v4 %>% 
  # Now we can attach chemistry, join only by Stream_Name and Date, not LTER
  dplyr::full_join(y = chem_v4 , by = c("Stream_Name", "Date"))
#there will be .x columns for LTER and Discharge_File_Name
glimpse(cq_v1)

chemQ <-cq_v1

#remove variables not interested in ever including
chemQ <- dplyr::select(chemQ, -c("LTER.x","Discharge_File_Name.x","Dataset","units", "value", "Qcms",
                                 "Dataset", "Raw_Filename", "units", "LTER.y", "Discharge_File_Name.y"))

colnames(chemQ) <-c("Date","Stream_Name","Discharge","Chemistry")

# now we want to melt the variable column so that discharge data and chemistry solutes are in one column
chemQ_melt <-melt(chemQ, id.vars = c("Date", "Stream_Name"))

# change column names
colnames(chemQ_melt) <-c("Date","Stream_Name","type","variable")

chemQ_melt <- chemQ_melt %>%
  dplyr::filter(!is.na(Date) & !is.na(variable))

chemQ_melt$Date<-as.Date(chemQ_melt$Date) #format date as date

# extract the year and convert to numeric format
chemQ_melt$year <- as.numeric(format(chemQ_melt$Date, "%Y"))

chemQ_melt$unique<-paste0(chemQ_melt$Stream_Name, chemQ_melt$year, chemQ_melt$variable) #create a column of "unique" values - type of sample and date collected
chemQ_unique <-chemQ_melt[!duplicated(chemQ_melt$unique),] #remove all duplicated samples

#remove variables not interested in ever including
chemQ_unique <- dplyr::select(chemQ_unique, -c("Date","unique"))

# get freq by year and by variable
chemQ_final <- setDT(chemQ_unique)[, .(freq = .N) , by = c("year","variable")]

## Filter which variables we wanna use
chemQ_final <- chemQ_final %>% filter(grepl("Q|DSi|Ca|Mg|NOx|SO4|PO4|TN|TP", variable))

## For plotting we need an actual datetime format, not just year as a number:
# chemQ_final$year <- as.Date(chemQ_final$year,
#                                    format = "%Y-%m-%d")

chemQ_final_one<-subset(chemQ_final, chemQ_final$variable=="PO4")

ggplot(chemQ_final, aes(x=year, y=variable))+
  geom_line(aes(col=freq), lwd=15)+
  

options(scipen = 999)
ggplot(chemQ_final, aes(x=year, y = variable, fill = freq))+
  labs(x="Year", y=NULL)+
  scale_fill_gradient(low = "lightblue", high = "darkblue")+
  geom_bar(width=0.9, stat = "identity")+
  #scale_x_datetime(labels = date_format("%Y"))+
  scale_x_continuous(trans = "reverse")+
  theme_bw()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16, face = "bold"))

# ## Export csv
# write.csv(chemQ_final,file="shinyapp_chemQ_overview.csv")
