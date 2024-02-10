## Melt data for Shiny App Data Overview Tab
# End result: want a table with time (years) on the X and solutes/ discharge on the y axis
# colored by # of sites that have each solute/ discharge

## originally this was linked to the google drive but that's not working right now
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Read in reference table
chemQ <-read.csv("Site_Reference_Table.csv")

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
