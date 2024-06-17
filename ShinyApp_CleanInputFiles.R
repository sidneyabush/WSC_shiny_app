##Shiny App formatting of files
#clean master_chem, master_q, and 
install.packages("fst")
require(fst)

#read in csv of list of sites that are published
published_data<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/PublishedSites.csv")

#read in master chemistry
master_chem<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/20240520_masterdata_chem.csv")

#keep only solutes of interest
master_chem<-subset(master_chem, master_chem$variable %in% c("SRP", "PO4", "DSi", "Ca", "K", "Na", "Mg", "NO3", "NOx"))

#convert date column to date and remove any rows missing dates
master_chem$date<-as.Date(master_chem$date)
master_chem<-master_chem[complete.cases(master_chem$date),]

#manually convert site names that changed between V1 and V2 of the master chemistry files
master_chem<-master_chem %>%
  mutate(Stream_Name = case_when(
    Stream_Name=="COMO"~"Como Creek",
    Stream_Name=="East Fork"~"east fork",
    Stream_Name=="West Fork"~"west fork",
    Stream_Name=="MG_WEIR"~"Marshall Gulch",
    Stream_Name=="OR_low"~"Oracle Ridge",
    .default = Stream_Name
  ))

#remove any sites from master chemistry file not included in the published data
master_chem<-master_chem[master_chem$Stream_Name %in% published_data$Stream_Name,]

#convert units from uM to mg/L
master_chem <- master_chem %>%
  mutate(value = case_when(
    variable=="NO3"~value*14.0067/1000,
    variable=="NOx"~value*14.0067/1000,
    variable=="PO4"~value*30.973762/1000,
    variable=="SRP"~value*30.973762/1000,
    variable=="Mg"~value*24.30500/1000,
    variable=="Ca"~value*40.0780/1000,
    variable=="K"~value*39.09830/1000,
    variable=="Na"~value*22.989769280/1000,
    variable=="DSi"~value*28.0855/1000,
    .default = value
  ))

#combine NOx/NO3 and SRP/PO4 columns into N and P, respectively
master_chem<-master_chem %>%
  mutate(variable = case_when(
    variable=="NO3"~"N",
    variable=="NOx"~"N",
    variable=="SRP"~"P",
    variable=="PO4"~"P",
    .default = variable
  ))

#rename some LTER names so that they are consistent between master chem and master Q
#note that NEON is converted to NWT since the site from NEON is Como Creek, which is part of Niwot Ridge LTER
master_chem<-master_chem %>%
  mutate(LTER = case_when(
    LTER=="UMR(Jankowski)"~"UMR",
    LTER=="LMP(Wymore)"~"LMP",
    LTER=="KRR(Julian)"~"KRR",
    LTER=="Sagehen(Sullivan)"~"Sagehen",
    LTER=="WalkerBranch"~"Walker Branch",
    LTER=="CZO-Catalina Jemez"~"Catalina Jemez",
    LTER=="NEON"~"NWT",
    .default = LTER
  ))

##these lines are for the solute availability plot

#create a new column called year 
master_chem$year<-lubridate::year(master_chem$date)

#get number of sites with data from each year for each solute
master_chem_summary<-master_chem %>%
  dplyr::group_by(year, variable) %>%
  dplyr::summarise(num_streams=n_distinct(Stream_Name))

#order the solutes by longest -> shortest record for solute availability plot
master_chem_summary$variable<-factor(master_chem_summary$variable, levels = c("Ca", "Mg", "K", "Na", "N", "DSi", "P"))


#read in master discharge and format it
master_q<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/20240201_masterdata_discharge.csv")

#convert date column to date and remove any rows missing dates
master_q$Date<-as.Date(master_q$Date)
master_q<-master_q[complete.cases(master_q$Date),]

#manually convert site names that changed between V1 and V2 of the master discharge files
master_q<-master_q %>%
  mutate(Stream_Name = case_when(
    Stream_Name=="COMO"~"Como Creek",
    Stream_Name=="East Fork"~"east fork",
    Stream_Name=="West Fork"~"west fork",
    Stream_Name=="MG_WEIR"~"Marshall Gulch",
    Stream_Name=="OR_low"~"Oracle Ridge",
    .default = Stream_Name
  ))

#remove any sites from master chemistry file not included in the published data
master_q<-master_q[master_q$Stream_Name %in% published_data$Stream_Name,]

####this subsets the master chem and q files to only included data where there is overlap for chem and q for each site

#find the minimum and maximum date associated with an observation of ANY SOLUTE for each site
chem_dates<-master_chem %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(min_date_chem=min(date), max_date_chem=max(date))

#find the minimum and maximum date associated with flow observation for each site
q_dates<-master_q %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(min_date_q=min(Date), max_date_q=max(Date))

#merge togetehr
dates_all<-merge(chem_dates, q_dates, by="Stream_Name")

#find overall minimum date between chem and q
dates_all$min<-as.Date(ifelse(dates_all$min_date_chem < dates_all$min_date_q, 
                              dates_all$min_date_q, dates_all$min_date_chem), origin = "1970-01-01")

#find overall maximum date between chem and q
dates_all$max<-as.Date(ifelse(dates_all$max_date_chem > dates_all$max_date_q, 
                              dates_all$max_date_q, dates_all$max_date_chem), origin = "1970-01-01")

#merge this dates_all file with the master chemistry file
master_chem<-merge(master_chem, dates_all[,c(1,6,7)], by="Stream_Name")

#crop data record of each site in the master chem file to only include data between the min and max date
master_chem<-master_chem %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::filter(date >= min & date <= max)

#merge this dates_all file with the master q file
master_q<-merge(master_q, dates_all[,c(1,6,7)], by="Stream_Name")

#crop data record of each site in the master q file to only include data between the min and max date
master_q<-master_q %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::filter(Date >= min & Date <= max)

setwd("/Users/keirajohnson/WSC_shiny_app")

write.fst(master_chem,"Master_Chem_ShinyAppCleanedFST.fst")

write.fst(master_q,"Master_Q_ShinyAppCleanedFST.fst")

#save as RDS files
save(master_chem, file="Master_Chem_ShinyAppCleaned.RData")
save(master_q, file="Master_Q_ShinyAppCleaned.RData")



#read in solute drivers file
load("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/CQ_file.RData")

cq_all_nodups$N<-rowMeans(cq_all_nodups[,c(8,9)], na.rm = T)

cq_all_nodups$P<-rowMeans(cq_all_nodups[,c(10,11)], na.rm = T)

cq_all_nodups<-cq_all_nodups[c(1:7,12:15)]

setwd("/Users/keirajohnson/WSC_shiny_app")

save(cq_all_nodups, file="CQAll_ShinyAppCleaned.RDS")

#read in drivers table
drivers<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/MapDriversTable2.csv")
drivers<-drivers[,-c(3,4)]
drivers_quant<-melt(drivers[c(1,6:15)], id.vars=c("Stream_Name"))
drivers_color<-melt(drivers[c(1:5)], id.vars = c("Stream_Name"))

solutes<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/Mean_Solute_Drivers.csv")
solutes<-solutes[,c(2,5,6)]
solutes<-subset(solutes, solutes$Solute %in% c("SRP", "PO4", "DSi", "Ca", "K", "Na", "Mg", "NO3", "NOx"))
colnames(solutes)<-c("Stream_Name", "variable", "value")
solutes<-solutes[!duplicated(solutes),]

drivers_quant<-bind_rows(drivers_quant, solutes)

drivers_quant<-drivers_quant %>%
  mutate(value = case_when(
    variable=="MedianQ" ~ log(value),
    variable=="drainSqKm" ~ log(value),
    .default = value
  ))

drivers_quant$value[is.infinite(drivers_quant$value)]<-NA

colnames(drivers_quant)<-c("Stream_Name", "Driver", "Mean_Value")

solute_drivers<-drivers_quant

#renmame driver variables to more intuituive names
#combine NOx/NO3 and SRP/PO4 to N and P, respectively
solute_drivers<-solute_drivers %>%
  mutate(Driver = case_when(
    Driver=="MedianQ" ~ "Log(Median Discharge (m3/s))",
    Driver=="npp" ~ "NPP (kgC/m2)",
    Driver=="cycle0" ~ "Green Up Day (day of year)",
    Driver=="prop_area" ~ "Maximum Snow Covered Area (proportion)",
    Driver=="precip" ~ "Precipitation (mm/year)",
    Driver=="drainSqKm" ~ "Log(Drainage Area (km2))",
    Driver=="evapotrans" ~ "Evapotranspiration (kg/m2)",
    Driver=="elevation_median_m" ~ "Median Elevation (m)",
    Driver=="Max_Daylength" ~ "Maximum Daylength (hours)",
    Driver=="temp" ~ "Temperature (deg C)",
    Driver=="NO3"~"N",
    Driver=="NOx"~"N",
    Driver=="PO4"~"P",
    Driver=="SRP"~"P",
    .default = Driver
  ))

#convert all solutes from uM to mg/L
solute_drivers <- solute_drivers %>%
  mutate(Mean_Value = case_when(
    Driver=="N"~Mean_Value*14.0067/1000,
    Driver=="P"~Mean_Value*30.973762/1000,
    Driver=="Mg"~Mean_Value*24.30500/1000,
    Driver=="Ca"~Mean_Value*40.0780/1000,
    Driver=="K"~Mean_Value*39.09830/1000,
    Driver=="Na"~Mean_Value*22.989769280/1000,
    Driver=="DSi"~Mean_Value*28.0855/1000,
    .default = Mean_Value
  ))

setwd("/Users/keirajohnson/WSC_shiny_app")

save(solute_drivers, file="SoluteDrivers_ShinyAppCleaned.RDS")

solute_drivers_all<-merge(drivers_quant, drivers_quant, by=c("Stream_Name"))
solute_drivers_all<-merge(solute_drivers_all, drivers_color, by=c("Stream_Name"))
colnames(solute_drivers_all)<-c("Stream_Name", "Driver", "Mean_Value", "Driver2", "Mean_Value2", "Driver3", "Mean_Value3")

#renmame driver variables to more intuituive names in Driver column 1
#combine NOx/NO3 and SRP/PO4 to N and P, respectively
solute_drivers_all<-solute_drivers_all %>%
  mutate(Driver = case_when(
    Driver=="MedianQ" ~ "Log(Median Discharge (m3/s))",
    Driver=="npp" ~ "NPP (kgC/m2)",
    Driver=="cycle0" ~ "Green Up Day (day of year)",
    Driver=="prop_area" ~ "Maximum Snow Covered Area (proportion)",
    Driver=="precip" ~ "Precipitation (mm/year)",
    Driver=="drainSqKm" ~ "Log(Drainage Area (km2))",
    Driver=="Max_Daylength" ~ "Maximum Daylength (hours)",
    Driver=="evapotrans" ~ "Evapotranspiration (kg/m2)",
    Driver=="elevation_median_m" ~ "Median Elevation (m)",
    Driver=="temp" ~ "Temperature (deg C)",
    Driver=="NO3"~"N",
    Driver=="NOx"~"N",
    Driver=="PO4"~"P",
    Driver=="SRP"~"P",
    .default = Driver
  ))

#convert all solutes from uM to mg/L in value column 1
solute_drivers_all <- solute_drivers_all %>%
  mutate(Mean_Value = case_when(
    Driver=="N"~Mean_Value*14.0067/1000,
    Driver=="P"~Mean_Value*30.973762/1000,
    Driver=="Mg"~Mean_Value*24.30500/1000,
    Driver=="Ca"~Mean_Value*40.0780/1000,
    Driver=="K"~Mean_Value*39.09830/1000,
    Driver=="Na"~Mean_Value*22.989769280/1000,
    Driver=="DSi"~Mean_Value*28.0855/1000,
    .default = Mean_Value
  ))

#renmame driver variables to more intuituive names in Driver column 2
#combine NOx/NO3 and SRP/PO4 to N and P, respectively
solute_drivers_all<-solute_drivers_all %>%
  mutate(Driver2 = case_when(
    Driver2=="MedianQ" ~ "Log(Median Discharge (m3/s))",
    Driver2=="npp" ~ "NPP (kgC/m2)",
    Driver2=="cycle0" ~ "Green Up Day (day of year)",
    Driver2=="prop_area" ~ "Maximum Snow Covered Area (proportion)",
    Driver2=="Max_Daylength" ~ "Maximum Daylength (hours)",
    Driver2=="precip" ~ "Precipitation (mm/year)",
    Driver2=="drainSqKm" ~ "Log(Drainage Area (km2))",
    Driver2=="evapotrans" ~ "Evapotranspiration (kg/m2)",
    Driver2=="elevation_median_m" ~ "Median Elevation (m)",
    Driver2=="temp" ~ "Temperature (deg C)",
    Driver2=="NO3"~"N",
    Driver2=="NOx"~"N",
    Driver2=="PO4"~"P",
    Driver2=="SRP"~"P",
    .default = Driver2
  ))

#convert all solutes from uM to mg/L in value column 2
solute_drivers_all <- solute_drivers_all %>%
  mutate(Mean_Value2 = case_when(
    Driver2=="N"~Mean_Value2*14.0067/1000,
    Driver2=="P"~Mean_Value2*30.973762/1000,
    Driver2=="Mg"~Mean_Value2*24.30500/1000,
    Driver2=="Ca"~Mean_Value2*40.0780/1000,
    Driver2=="K"~Mean_Value2*39.09830/1000,
    Driver2=="Na"~Mean_Value2*22.989769280/1000,
    Driver2=="DSi"~Mean_Value2*28.0855/1000,
    .default = Mean_Value2
  ))

#renmame driver variables to more intuituive names in Driver column 2
#combine NOx/NO3 and SRP/PO4 to N and P, respectively
solute_drivers_all<-solute_drivers_all %>%
  mutate(Driver3 = case_when(
    Driver3=="Observation.Network" ~ "Observation Network",
    Driver3=="KG.Climate.Zone" ~ "Climate Zone",
    Driver3=="major_rock" ~ "Dominant Lithology",
    Driver3=="major_land" ~ "Dominant Land Cover",
    .default = Driver3
  ))

solute_drivers_all$Mean_Value3<-ifelse(solute_drivers_all$Mean_Value3=="", NA, solute_drivers_all$Mean_Value3)

setwd("/Users/keirajohnson/WSC_shiny_app")

save(solute_drivers_all, file="SoluteDriversAll_ShinyAppCleaned.RDS")

