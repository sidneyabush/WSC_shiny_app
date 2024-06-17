#install packages
#setwd("/Users/keirajohnson/Desktop/ShinyAppExternalDeployment")
# 
#renv::init()
# 
#renv::restore()

require(shiny)
require(htmltools)
require(dplyr)
require(leaflet)
require(maps)
require(rcartocolor)
require(DT)
require(ggpubr)
require(shinythemes)
require(plotly)
require(ggpmisc)
require(fst)
require(lubridate)
require(ggplot2)

####read in data for tables, plotting, mapping####

#load in cleaned data files
master_chem<-read.fst("Master_Chem_ShinyAppCleanedFST.fst")

master_q<-read.fst("Master_Q_ShinyAppCleanedFST.fst")

#this one is called "cq_all_nodups"
load("CQAll_ShinyAppCleaned.RDS")

#this one is called "solute_drivers"
load("SoluteDrivers_ShinyAppCleaned.RDS")

#this one is called "solute_drivers_all"
load("SoluteDriversAll_ShinyAppCleaned.RDS")

#for data exploration panel
quant_drivers<-c("Log(Drainage Area (km2))","Evapotranspiration (kg/m2)", "Green Up Day (day of year)",
                 "Maximum Daylength (hours)", "Maximum Snow Covered Area (proportion)", "Log(Median Discharge (m3/s))", 
                 "Median Elevation (m)","NPP (kgC/m2)", "Precipitation (mm/year)", "Temperature (deg C)",
                 "Ca", "DSi", "K","Mg", "N", "Na", "P")

color_drivers<-c("Observation Network","Climate Zone","Dominant Lithology","Dominant Land Cover")

#create summary file for solute record plot
master_chem_summary<-master_chem %>%
  dplyr::group_by(year, variable) %>%
  dplyr::summarise(num_streams=n_distinct(Stream_Name))

master_chem_summary$variable<-factor(master_chem_summary$variable, levels = c("Ca", "Mg", "K", "Na", "N", "DSi", "P"))


#read in source data for table of driver information and rename columns
data_source_table<-read.csv("ShinyAppDataSourceTable.csv")
colnames(data_source_table)<-c("Variable", "Source", "Units", "Spatial Scale", "Temporal Scale", "Temporal Coverage", "Dataset Link")

#read in source data for table and map of site characteristic information
all_drivers_table<-read.csv("MapDriversTable2.csv", na.strings=c("","NA"))

#rename columns
colnames(all_drivers_table)<-c("Stream Name", "Observation Network", "Latitude", "Longitude", "Climate Zone",
                               "Dominant Lithology", "Dominant Land Cover", "Mean Elevation", "Drainage Area (km2)",
                               "Median Discharge (cms)", "Maximum Proportion Snow Covered Area (proportion)", "Precipitation (mm/year)",
                               "Evapotranspiration (kg/m2)","Mean Annual Temperature (deg C)", "NPP (kgC/m2)", "Green Up Day (day of year)",
                               "Maximum Daylength (hours)")

#convert character columns to factors so they can be filtered in the shiny data table
all_drivers_table$`Stream Name`<-as.factor(all_drivers_table$`Stream Name`)
all_drivers_table$`Observation Network`<-as.factor(all_drivers_table$`Observation Network`)
all_drivers_table$`Climate Zone`<-as.factor(all_drivers_table$`Climate Zone`)
all_drivers_table$`Dominant Lithology`<-as.factor(all_drivers_table$`Dominant Lithology`)
all_drivers_table$`Dominant Land Cover`<-as.factor(all_drivers_table$`Dominant Land Cover`)

all_drivers_map<-all_drivers_table

all_drivers_map$`Drainage Area (km2)`<-log(all_drivers_map$`Drainage Area (km2)`)
all_drivers_map$`Median Discharge (cms)`<-log(all_drivers_map$`Median Discharge (cms)`)

colnames(all_drivers_map)[c(9,10)]<-c("Log(Drainage Area (km2))", "Log(Median Discharge (cms))")

all_drivers_map$`Log(Median Discharge (cms))`<-ifelse(all_drivers_map$`Log(Median Discharge (cms))`=="-Inf", NA, all_drivers_map$`Log(Median Discharge (cms))`)

#read in data for table of information about each observation network
ON_table<-read.csv("ObservationNetworkTable.csv")

#read in data for solute record length table
#solute_record_length<-read.csv("/Users/keirajohnson/Desktop/ShinyAppFinalFolder/shinyapp_chemQ_overview.csv")

#### make base map and add options for coloration ####
world_countries<-map_data('world')

site_map<-leaflet(all_drivers_map) %>% addTiles() %>% 
  addCircleMarkers(lat = ~Latitude, lng= ~Longitude, color = "darkgrey",
                   popup = ~paste(`Stream Name`, `Observation Network`, sep = "<br>"), weight = 1, fillOpacity = 0.7)

map_color_options<-c("Observation Network", "Climate Zone", "Dominant Lithology", "Dominant Land Cover",
                     "Mean Elevation", "Log(Drainage Area (km2))", "Log(Median Discharge (cms))", 
                     "Maximum Proportion Snow Covered Area (proportion)", "Precipitation (mm/year)",
                     "Evapotranspiration (kg/m2)", "Mean Annual Temperature (deg C)", "NPP (kgC/m2)",
                     "Green Up Day (day of year)", "Maximum Daylength (hours)")

cat_color_options<-c("Observation Network", "Climate Zone", "Dominant Lithology", "Dominant Land Cover")

cont_color_options<-c("Mean Elevation", "Drainage Area (km2)", "Median Discharge (cms)", 
                      "Maximum Proportion Snow Covered Area (proportion)", "Precipitation (mm/year)",
                      "Evapotranspiration (kg/m2)", "Mean Annual Temperature (deg C)", "NPP (kgC/m2)",
                      "Green Up Day (day of year)", "Maximum Daylength (hours)")

#define color palette for map
c25 <- c(
  "dodgerblue2", "#E31A1C","green4","#6A3D9A","#FF7F00","gold1","skyblue2", "#FB9A99","palegreen2","#CAB2D6","#FDBF6F",
  "khaki2","maroon", "orchid1", "deeppink1", "blue1", "steelblue4","darkturquoise", "green1", "yellow3","darkorange4")

####User Interface####
overview_ui <- navbarPage(
  # Title of the app
  "Synthesize It!",
  
  ####About Panel####
  tabPanel("About",
           #this opens the page
           fluidPage(
             # Add a row to add things in
             fluidRow(
               #in the first column add an image
               #in the second column add some text 
               column(width=4, align="center", 
                      
                      tags$img(src="title_picture.jpeg", width="100%")
                      
               ),
               column(width=8,
                      
                      htmltools::h2("About The Workshop"),
                      
                      htmltools::p("Big data integration offers the potential to advance water science by promoting a more comprehensive understanding of watershed function. 
                           During this workshop, we will explore an extensive dataset including field observations and modeled results of multiple stream chemical 
                           and hydrometric parameters spanning over 200 rivers. This dataset also includes associated spatial data describing climate, land cover, lithology, 
                           and topography. We will explore questions and hypotheses about stream biogeochemical function using data visualization
                           tools to evaluate relationships between catchment characteristics and watershed processes."),
                      
                      htmltools::h2("About This Project"),
                      
                      htmltools::p("This workshop stems out of a NCEAS funded working group to better understand controls on concentrations, fluxes, seasonality, and trends in
                           riverine silicon (Si). For the past 5 years, we have worked to compile and harmonize this dataset to answer questions focused on better
                           understanding Si cycling globally.")
               )
             ) #close fluid row
           ) #close fluid page
  ), #close about panel
  
  #### Data Panel ####
  #this panel starts to incorporate output from the server (below) including a map and plots
  tabPanel("Data Availability",
           
           fluidPage(
             
             htmltools::h2("About The Dataset"),
               
               htmltools::p("The dataset in this workshop includes stream chemistry, discharge, climate, land use, lithology, and topography data 
                            from over 200 across the globe. Stream discharge and chemistry data was contributed from public and private monitoring networks, research groups,
                            and govenmental agencies. Climate, productivity, land use, lithology, and topography associated with each watershed was derived from globally available modeled
                            and remotely sensed data products."),
               
               htmltools::h2("Discharge and Solute Availability"),
               
               htmltools::p("This dataset includes daily stream discharge and discrete stream chemistry. Solutes include inorganic nitrogren (NO3/NOx), 
                            inorganic phosphorus (PO4/SRP), silicon (Si), calcium (Ca), magnesium (Mg), sodium (Na), and potassium (K)."),

               plotOutput('plot1'),
               
               htmltools::h2("About the Spatial Data Products"),
               
               htmltools::p("We acquired watershed characteristic (e.g., climate, land use) data from globally available spatial datasets listed below. 
                            Only globally available data layers were used in order to have consistent data sources across the extent of our dataset. 
                            Watershed boundaries were used to extract spatial data from gridded data sources."),
               
               #see table1 output in server below
               dataTableOutput('table1')
           )#close fluid page
  ), #close tab panel
  
  #### Site Locations####
  tabPanel("Site Locations",
           
           fluidPage(
             
             htmltools::h2("Site Locations"),
             
             sidebarPanel(
               
               #this is the observation network map
               dataTableOutput('table3')
               
             ),
             
             mainPanel(
               
               htmltools::h2("Site Map"),
               
               htmltools::p("The map below displays the stream locations included in this dataset. Select a variable from the dropdown list to color sites by watershed 
                         Observation Network or catchment characteristic. Click on an individual point to see associated the name and Observation Network. For more information
                         on each Observation Network, please visit the associated webpage linked in the table to the left."),
               
               column(width = 8, offset = 8,
                      #add option to color points by different things
                      varSelectInput(
                        inputId = "map_option",
                        label = "Color Sites By:",
                        data = all_drivers_map %>% select(all_of(map_color_options))
                      )
                      ),
               
                
               
               #see mymap output in server below
               leafletOutput("mymap"),
               
               htmltools::h2("Site Information"),
               
               htmltools::p("Watershed characteristic data for each site included in the dataset. Click on the column names to sort
                          the data table alphabetically, or filter sites using the boxes at the top of each column. You may apply filters across
                          multiple columns at once. All climate values are mean annual values between 2001 and 2019."),
               
               #this is the table of site characteristic information
               DT::DTOutput('table2')
             ), #close main panel
           )#close fluid page
  ), #close tab panel
  
  ####Data Exploration####
  tabPanel("Data Exploration",
           
           fluidPage(
             #first set of plots to look at solute time series
             titlePanel("Explore Distributions of Solutes and Catchment Characteristics"),
             htmltools::p("Please select a solute or catchment characteristic from the dropdown list below to explore distributions of 
                          the selected variable across the dataset. All solute concentrations are in mg/L."),
             #side bar layout allows you to add box on the side of the page, good for plotting
             sidebarLayout(
               sidebarPanel(
                 #select input creates a dropdown menu
                 #this lists all site characteristic options from solute drivers dataframe
                 selectInput(inputId = "driver_solute_dropdown",
                             label = "Variable",
                             choices = quant_drivers)
               ),
               #plot histogram of average site characteristics
               mainPanel(
                 plotOutput("driver_histogram")
               )
             ),#close sidebar layout
             #first set of plots to look at solute time series
             titlePanel("Explore Relationships between Solutes and Catchment Characteristics"),
             htmltools::p("Please select a solute or catchment characteristic from each of the dropdown lists below. All solute concentrations are in mg/L."),
             
             #side bar layout allows you to add box on the side of the page, good for plotting
             sidebarLayout(
               sidebarPanel(
                 
                 #select input creates a dropdown menu
                 #this lists all observation networks in the master chemistry data
                 selectInput(inputId = "x_axis_dropdown",
                             label = "X-Axis Variable",
                             choices = quant_drivers),
                 
                 selectInput(inputId = "y_axis_dropdown",
                             label = "Y-Axis Variable",
                             choices = quant_drivers),
                 
                 selectInput(inputId = "color_dropdown",
                             label = "Color Variable",
                             choices = color_drivers),
                 
                 actionButton("LR_button","Add linear regression")
               ),
               #plot x-y plot of drivers against one another, with option to color points by third driver
               mainPanel(
                 plotOutput("driver_solute_plot")
               ),
             ), #close sidebar layout
           ) #close fluid page
  ), #close tab panel
  
  #### Site Comparison Panel####
  tabPanel("Site Comparison",
           
           fluidPage(
             
             #add secondary title for another set of plots, this time with more dropdowns for sites
             titlePanel("Compare Solute Distributions Across Sites"),
             
             htmltools::p("Using the buttons below, select a solute to compare boxplot distributions of one solute across sites. Select sites using the Observation Network
                          and Site drop down menus. We suggest displaying no more than 10 sites at a time."),
             
             #sidebar layout to select inputs
             sidebarLayout(
               sidebarPanel(
                 
                 # Radio buttons for solutes
                 radioButtons(inputId = "button_solute3",
                              label = "Solute",
                              choices = c(unique(master_chem$variable))),
                 
                 # Dropdown for watershed, since users can select three different sites this is repeated three times
                 #first for observation network
                 selectInput(inputId = "dropdown_ON3",
                             label = "Observation Network",
                             choices = c(unique(master_chem$LTER)),
                             multiple = TRUE),
                 
                 #then for site
                 selectInput(inputId = "dropdown_site3", 
                             label = "Site", 
                             choices = NULL,
                             multiple = TRUE)
                 
               ),
               #plot solute timeseries
               mainPanel(
                 plotOutput("solute_boxplot")
               )
             ), #close sidebarLayout
             
             #add secondary title for another set of plots, this time with more dropdowns for sites
             titlePanel("Compare Solute Time Series Across Sites"),
             
             htmltools::p("Using the buttons below, select a solute to compare time series of one solute across sites. Select sites using the Observation Network
                          and Site drop down menus. Use the Toggle button to switch between instantanous and average monthly data. We suggest displaying no more than 3 sites at a time."),
             
             #sidebar layout to select inputs
             sidebarLayout(
               sidebarPanel(
                 
                 # Radio buttons for solutes
                 radioButtons(inputId = "button_solute2",
                              label = "Solute",
                              choices = c(unique(master_chem$variable))),
                 
                 # Dropdown for watershed, since users can select three different sites this is repeated three times
                 #first for observation network
                 selectInput(inputId = "dropdown_ON2",
                             label = "Observation Network",
                             choices = c(unique(master_chem$LTER)),
                             multiple = TRUE),
                 
                 #then for site
                 selectInput(inputId = "dropdown_site2", 
                             label = "Site", 
                             choices = NULL,
                             multiple = TRUE),
                 
                 #toggle between seasonal and all data
                 actionButton("seasonal_button2", "Toggle between instantanous and annual average plot")
                 
               ),
               #plot solute timeseries
               mainPanel(
                 sliderInput("xslider2", label = "Select Date Range of Interest", min = 0, max = 10, value = c(1,9)),
                 plotOutput("timeseries_out2")
               )
             ), #close sidebarLayout
             
             #first set of plots to look at solute time series
             titlePanel("Explore Concentration-Discharge Relationships Across Sites"),
             htmltools::p("Using the buttons below, select a solute to compare concentration-discharge (C-Q) relationships of one solute across sites. 
                          Select sites using the Observation Network and Site drop down menus. The equation and R2 in the upper left of the plot 
                          provide slope and fit information for each C-Q relationship. We suggest displaying no more than 3 sites at a time."),
             
             sidebarLayout(
               sidebarPanel(
                 
                 # Radio buttons for solutes
                 radioButtons(inputId = "button_solute4",
                              label = "Solute",
                              choices = c(unique(master_chem$variable))),
                 
                 # Dropdown for watershed, since users can select three different sites this is repeated three times
                 #first for observation network
                 selectInput(inputId = "dropdown_ON4",
                             label = "Observation Network",
                             choices = c(unique(master_chem$LTER)),
                             multiple = TRUE),
                 
                 #then for site
                 selectInput(inputId = "dropdown_site4", 
                             label = "Site", 
                             choices = NULL,
                             multiple = TRUE)
               ), #close sidebar panel
               #plot solute timeseries
               mainPanel(
                 plotOutput("cq_plot")
               )
             )#close sidebar layout
           )#close fluid page
  ),#close tab panel
  
  #### Time Series Panel ####
  #here we add interactive plotting functions
  #users select the input using the "selectInput" function, and that input feeds into input used in the server code below
  tabPanel("Site Specific Time Series",
           
           fluidPage(
             
             #first set of plots to look at solute time series
             titlePanel("Compare Multi-Solute Time Series at One Site"),
             htmltools::p("Using the buttons below, select multiple solutes to time series solute concentrations at one site. Select one site using the Observation Network
                          and Site drop down menus. Discharge is shown over the same period of record. Use the Toggle button to switch between 
                          instantanous and average monthly solute concentration data, and daily and average daily discharge data."),
             
             #side bar layout allows you to add box on the side of the page, good for plotting
             sidebarLayout(
               sidebarPanel(
                 #select input creates a dropdown menu
                 #this lists all observation networks in the master chemistry data
                 selectInput(inputId = "dropdown_ON",
                             label = "Observation Network",
                             choices = c(unique(master_chem$LTER))),
                 
                 #this takes the output from the observation network and lists all sites wihtin the observation network
                 #see input "dropdown_site" in server below
                 selectInput(inputId = "dropdown_site", 
                             label = "Site", 
                             choices = NULL),
                 
                 #radio buttons allow you to check multiple options (think select many)
                 #we are using these to list solutes
                 checkboxGroupInput(inputId = "button_spp",
                                    label = "Solute",
                                    choices = c(unique(master_chem$variable))),
                 
                 #action buttons allow you to toggle between things (e.g., all data vs annual data, log vs not log plot scales, etc)
                 actionButton("seasonal_button", "Toggle between instantanous and annual average plot")
                 
               ),
               
               #now we open a main panel, this will be to the right of the sidebarLayout panel
               mainPanel(
                 sliderInput("xslider", label = "Select Date Range of Interest", min = 0, max = 10, value = c(1,9)),
                 plotOutput("timeseries_out"), #plot solute timeseries data, see timeseries_out plot in server below
                 plotOutput("timeseries_out_Q") #plot discharge timeseries data, see timeseries_out_Q plot in server below
               )
             )
           ) #close fluid page
  ), #close panel
  
  ####Resources Panel####  
  tabPanel("Resources",
           
           fluidPage(
             
             fluidRow(
               
               column(width = 4,
                      
                      tags$img(src="Slide1.jpeg", width="100%"),
                      
                      tags$div("This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 
                          4.0 International License. To view a copy of this license, visit:",
                          tags$a(href="https://creativecommons.org/licenses/by-nc-sa/4.0/", "https://creativecommons.org/licenses/by-nc-sa/4.0/.")
                      )
               ),
               
               column(width = 8,
                      
                      htmltools::h2("Manuscripts"),
                      
                      uiOutput("long_term_MS"),
                      
                      uiOutput("regimes_MS"),
                      
                      htmltools::h2("Data Releases"),
                      
                      uiOutput("long_term_data"),
                      
                      uiOutput("regimes_data"),
                      
                      htmltools::h2("Interested in building your own ShinyApp?"),
                      
                      uiOutput("github_page"),
                      
                      uiOutput("shiny_page"),
                      
                      htmltools::h2("Contact Us!"),
                      
                      htmltools::h3("Workshop Conveners"),
                      
                      htmltools::p("Keira Johnson: johnkeir@oregonstate.edu"),
                      
                      htmltools::p("Sidney Bush: bushsi@oregonstate.edu"),
                      
                      htmltools::h3("Project PIs"),
                      
                      htmltools::p("Kathi Jo Jankowski: kjankowski@usgs.gov"),
                      
                      htmltools::p("Joanna Carey: jcarey@babson.edu"),
                      
                      htmltools::p("Pamela Sullivan: pamela.sullivan@oregonstate.edu"),
                      
               )
             ) #close fluid row
           ) #close fluid page
  ) # close tab panel
) #close UI

#### Server ####
#the server is where the all of the interactive selection from the user interface happens
#the code below is split into sections by panels denoted in the UI function above
overview_server <- function(input, output, session){
  
  ####About Panel Server####
  #nothing here, no server requirements in the about panel
  
  #### Data Availability Panel Server ####
  
  #plot of number of sites with data for each solute over time
  output$plot1<-renderPlot({
    
    ggplot(master_chem_summary, aes(x=year, y=variable, col=num_streams))+geom_line(lwd=15)+
      theme_classic()+labs(y="", x="Year", col="Number of Sites")+
      theme(text = element_text(size = 20, family = "Times"))+
      scale_color_gradient(low = "grey76", high = "deepskyblue3",limits=c(0,200))
    
  })
  
  #drivers table information
  output$table1<- renderDataTable({
    
    my_table <- data_source_table
    colnames(my_table)<-c("Variable", "Source", "Units", "Spatial Scale", "Temporal Scale", "Temporal Coverage", "Dataset Link")
    #this makes the links "clickable"
    my_table$`Dataset Link`<-paste0('<a href="', my_table$`Dataset Link`, '">',"Visit the Data Source Website!",'</a>')
    return(my_table)
    selection="multiple"
    options=list(stateSave = TRUE)
  }, 
  #edit how table appears
  options = list(searching = FALSE, paging = FALSE, autoWidth = TRUE, info=FALSE, autoWidth=TRUE),
  escape = FALSE)
  
  ####Site Location Panel Server####
  
  #table of information on each observation network
  output$table3<- renderDataTable({
    
    my_table3 <- ON_table
    colnames(my_table3)<-c("Abbreviation", "Observation Network", "Dataset Link")
    #this makes the links "clickable"
    my_table3$`Dataset Link`<-paste0('<a href="', my_table3$`Dataset Link`, '">',"Visit the Observation Network Website!",'</a>')
    return(my_table3)},
    #edit how table appears
    options = list(searching = FALSE, paging = FALSE, autoWidth = TRUE, info=FALSE, autoWidth=TRUE),
    escape = FALSE)
  
  #map output
  output$mymap<-renderLeaflet(site_map)
  
  map_color <- reactive({
    
    all_drivers_map %>%
      select(input$map_option) %>%
      unlist()
    
  })
  
  #create a palette, if the variable is a factor use "c25" pallette creaded above, otherwise use the RdYlGn pallette
  pal<-reactive({
    
    if(input$map_option == "Observation Network"){
      colorFactor(c25, domain = map_color())
    } else if(input$map_option == "Climate Zone"){
      colorFactor(c25, domain = map_color())
    } else if(input$map_option == "Dominant Lithology"){
      colorFactor(c25, domain = map_color())
    } else if(input$map_option == "Dominant Land Cover"){
      colorFactor(c25, domain = map_color())
    } else {
      colorNumeric("PuOr", domain = map_color())
    }
    
  })
  
  #color map by variable using palette defined above
  observe({
    
    leafletProxy("mymap", data = all_drivers_map) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(lat = ~Latitude, lng= ~Longitude,
                       popup = ~paste(`Stream Name`, `Observation Network`, sep = "<br>"), weight = 1, fillOpacity = 0.7,
                       color = ~pal()(map_color())) %>%
      addLegend(position = "topright", pal = pal(), values = map_color(), title = "Legend")
    
  })
  
  #drivers table information
  output$table2<- DT::renderDT({
    
    DT::datatable(
      all_drivers_table,
      #selection = "none",
      filter="top",
      #options = list(paging = TRUE, autoWidth = TRUE, info=FALSE, searching=FALSE),
      escape = FALSE)
    
  }
  )
  
  #### Data Exploration Panel Server ####
  #for histogram plot
  solute_drivers_v3<-reactive({
    
    solute_drivers %>%
      dplyr::filter(Driver==input$driver_solute_dropdown)
    
  })
  
  #render plot
  output$driver_histogram <- renderPlot({   
    
    ggplot(solute_drivers_v3(), aes(x=Mean_Value))+geom_histogram(bins = 50, fill="black")+theme_bw()+
      labs(x=input$driver_solute_dropdown, y="Count")+theme(text = element_text(size = 20))
    
  })
  
  #for x-y plot
  solute_drivers_v2<-reactive({
    
    solute_drivers_all %>%
      dplyr::filter(Driver==input$x_axis_dropdown & Driver2==input$y_axis_dropdown & Driver3==input$color_dropdown)
    
  })
  
  #whichplot2, to plot seasonal vs annual data
  whichplot3<-reactiveVal(TRUE)
  
  driver_solute_plot_noLR<-reactive(ggplot(solute_drivers_v2(), aes(Mean_Value, Mean_Value2, col=Mean_Value3))+geom_point()+
        labs(x=input$x_axis_dropdown, y=input$y_axis_dropdown, col=input$color_dropdown)+
        theme_bw()+theme(text = element_text(size = 20))+scale_color_manual(values = c25))
      

  
  driver_solute_plot_LR<-reactive(ggplot(solute_drivers_v2(), aes(Mean_Value, Mean_Value2, col=Mean_Value3))+geom_point()+
                                    labs(x=input$x_axis_dropdown, y=input$y_axis_dropdown, col=input$color_dropdown)+theme_bw()+theme(text = element_text(size = 20))+
                                    geom_smooth(method = "lm", se=F, col="firebrick4")+
                                    stat_poly_eq(use_label(c("eq", "R2")), size=7, col="firebrick4")+
                                    scale_color_manual(values = c25))
  
  observeEvent(input$LR_button, {
    whichplot3(!whichplot3())
  })
  
  which_graph3 <- reactive({
    if (whichplot3()) {
      driver_solute_plot_noLR()
    } else {
      driver_solute_plot_LR()
    }
  })
  
  #render plot
  output$driver_solute_plot <- renderPlot({   
    
    which_graph3()
    
  })
  
  #### Site Comparison Panel Server ####
  
  ##comparative boxplot of one solute across many sites##
  observe({
    
    sites_LTER3<-master_chem %>%
      filter(LTER %in% input$dropdown_ON3) %>%
      distinct(Stream_Name)
    
    updateSelectInput(session, "dropdown_site3", choices = sites_LTER3$Stream_Name)
    
  })
  
  
  #filter master chem by solute
  chem_v4 <- reactive({
    master_chem %>%
      dplyr::filter(variable %in% input$button_solute3)
  })
  
  #then by site selections
  chem_actual3<-reactive({
    
    chem_v4() %>%
      dplyr::filter(Stream_Name %in% c(input$dropdown_site3))
  })
  
  output$solute_boxplot<-renderPlot({
    
    ggplot(chem_actual3(), aes(x=Stream_Name, y=value))+geom_boxplot()+theme_bw()+
      labs(y=input$button_solute3, x="Site")+theme(text = element_text(size = 20), 
                                                   axis.text.x = element_text(angle = 45, hjust=1))
    
  })
  
  ##time series plot with slider bar across many sites##
  #here we have three sets of ON/site dropdown selection
  #return sites within given observation network (dropdown_site2)
  observe({
    
    sites_LTER2<-master_chem %>%
      filter(LTER %in% input$dropdown_ON2) %>%
      distinct(Stream_Name)
    
    updateSelectInput(session, "dropdown_site2", choices = sites_LTER2$Stream_Name)
    
  })
  
  #filter master chem by solute
  chem_v3 <- reactive({
    master_chem %>%
      dplyr::filter(variable %in% input$button_solute2)
  })
  
  #then by site selections
  chem_actual2<-reactive({
    
    chem_v3() %>%
      dplyr::filter(Stream_Name %in% c(input$dropdown_site2))
  })
  
  min_date2<-reactive({
    chem_actual2() %>%
      dplyr::summarise(min_overall=min(min)) %>%
      dplyr::slice_min(min_overall)
  })
  
  max_date2<-reactive({
    chem_actual2() %>%
      dplyr::summarise(max_overall=max(max)) %>%
      dplyr::slice_max(max_overall)
  })
  #   
  
  observe({
    
    updateSliderInput(session, "xslider2", min=as.Date(min_date2()$min_overall), 
                      max=as.Date(max_date2()$max_overall), value = c(as.Date(min_date2()$min_overall), 
                                                                      as.Date(max_date2()$max_overall)))
    
  })  
  
  #whichplot2, to plot seasonal vs annual data
  whichplot2<-reactiveVal(TRUE)
  
  #all time series data
  all_data2<-reactive(ggplot(chem_actual2(), aes(date, value, col=Stream_Name))+labs(x="Date", y=input$button_solute2, col="Site")+
                        geom_point()+theme_bw()+theme(text = element_text(size = 20))+xlim(input$xslider2))
  
  #seasonal time series data
  seasonal_data2<-reactive(ggplot(chem_actual2(), aes(factor(month(date)), value, fill=factor(Stream_Name)))+
                             labs(x="Month", y=input$button_solute2, fill="Site")+
                             geom_boxplot()+theme_bw()+theme(text = element_text(size = 20)))
  
  observeEvent(input$seasonal_button2, {
    whichplot2(!whichplot2())
  })
  
  which_graph2 <- reactive({
    if (whichplot2()) {
      all_data2()
    } else {
      seasonal_data2()
    }
  })
  
  #render plot
  output$timeseries_out2 <- renderPlot({   
    
    which_graph2()
    
  })
  
  ##CQ plot##
  observe({
    
    sites_LTER4<-master_chem %>%
      filter(LTER %in% input$dropdown_ON4) %>%
      distinct(Stream_Name)
    
    updateSelectInput(session, "dropdown_site4", choices = sites_LTER4$Stream_Name)
    
  })
  
  
  cq2<-reactive({
    cq_all_nodups %>%
      dplyr::select(Stream_Name, date, input$button_solute4, Qcms)
  })
  
  #then by site selections
  cq3<-reactive({
    
    cq2() %>%
      dplyr::filter(Stream_Name %in% c(input$dropdown_site4))
  })
  
  output$cq_plot<-renderPlot({
    
    ggplot(cq3(), aes(log(Qcms), log(get(input$button_solute4)), col=Stream_Name))+geom_point()+
      labs(x="Log(discharge (m3/s))", y="Log(concentration (mg/L))", col="Site")+
      geom_smooth(se=F, method = "lm")+
      stat_poly_eq(use_label(c("eq", "R2")), size=7) +
      theme_bw()+theme(text = element_text(size = 20))
    
  })
  
  #### Site Specific Time Series Panel Server ####
  
  #return sites within given observation network (dropdown_site)
  observe({
    
    sites_LTER<-master_chem %>%
      filter(LTER==input$dropdown_ON) %>%
      distinct(Stream_Name)
    
    updateSelectInput(session, "dropdown_site", choices = sites_LTER$Stream_Name)
    
  })
  
  #use the selection from dropdown_site to make plots
  #Subset dataframe to selected solute
  ## Must be done inside of "reactive" function
  chem_v2 <- reactive({
    master_chem %>%
      dplyr::filter(variable %in% input$button_spp)
  })
  
  #then filter solute information by selected site
  chem_actual<-reactive({
    
    chem_v2() %>%
      dplyr::filter(Stream_Name == input$dropdown_site)
  })
  
  min_date<-reactive({
    chem_actual() %>%
      select("min") %>%
      distinct()
  })
  
  max_date<-reactive({
    chem_actual() %>%
      select("max") %>%
      distinct()
  })
  #   
  
  observe({
    
    updateSliderInput(session, "xslider", min=as.Date(min_date()$min), 
                      max=as.Date(max_date()$max), value = c(as.Date(min_date()$min), as.Date(max_date()$max)))
    
  })  
  
  #reactiveVal allows you to toggle between two plots
  #here we will use it to display either the full time series OR seasonal data
  whichplot<-reactiveVal(TRUE)
  
  #define the two plots
  #first full time series
  all_data<-reactive(ggplot(chem_actual(), aes(date, value, col=variable))+labs(x="Date", y="Concentration (mg/L)", col="Solute")+
                       geom_point()+theme_bw()+theme(text = element_text(size = 20), legend.position = "bottom")+
                       xlim(input$xslider))
  
  #then seasonal data
  seasonal_data<-reactive(ggplot(chem_actual(), aes(factor(month(date)), value, fill=factor(variable)))+
                            labs(x="Month", y="Concentration (mg/L)", fill="Solute")+
                            geom_boxplot()+theme_bw()+theme(text = element_text(size = 20), legend.position = "bottom"))
  
  #this determines which plot to display based on "seasonal_button" which is defined in the UI above
  observeEvent(input$seasonal_button, {
    whichplot(!whichplot())
  })
  
  which_graph <- reactive({
    if (whichplot()) {
      all_data()
    } else {
      seasonal_data()
    }
  })
  
  #this renders the plot output based on the whichplot selection above (annual vs seasonal)
  output$timeseries_out <- renderPlot({   
    
    which_graph()
    
  })
  
  #do the same thing for dischagre plot
  #first filter
  q_v2 <- reactive({
    master_q %>%
      dplyr::filter(Stream_Name == input$dropdown_site)
  })
  
  #for seasonal Q plot
  q_v2_dailyavg<-reactive({
    q_v2() %>%
      dplyr::group_by(doy=yday(Date)) %>%
      dplyr::mutate(mean_daily=mean(Qcms)) %>%
      dplyr::mutate(doy_date=as.Date(paste("2020/", doy), "%Y/%j"))
  })
  
  #define whichplot function
  whichplot_Q<-reactiveVal(TRUE)
  
  #all Q plot
  all_data_Q<-reactive(ggplot(q_v2(), aes(Date, Qcms), col="deepskyblue3")+labs(x="Date", y="Discharge (m3/s)")+
                         geom_line()+theme_bw()+theme(text = element_text(size = 20))+xlim(input$xslider))
  #seasonal Q plot
  seasonal_data_Q<-reactive(ggplot(q_v2_dailyavg(), aes(doy_date, mean_daily))+
                              labs(x="Month", y="Discharge (m3/s)")+
                              geom_line()+theme_bw()+theme(text = element_text(size = 20))+
                              scale_x_date(date_breaks = "1 month", date_labels = "%b"))
  
  #select plot based on seasonal button
  observeEvent(input$seasonal_button, {
    whichplot_Q(!whichplot_Q())
  })
  
  which_graph_Q <- reactive({
    if (whichplot_Q()) {
      all_data_Q()
    } else {
      seasonal_data_Q()
    }
  })
  
  #render time series Q plot based on whichplot selection
  output$timeseries_out_Q <- renderPlot({   
    
    which_graph_Q()
    
  })
  
  #### Resources Panel Server ####
  url1 <- a("Long-Term Changes in Concentration and Yield of Riverine Dissolved Silicon From the Poles to the Tropics", href="https://doi.org/10.1029/2022GB007678")
  output$long_term_MS <- renderUI({
    tagList(url1)
  })
  
  url2 <- a("Establishing fluvial silicon regimes and their stability across the Northern Hemisphere", href="https://doi.org/10.1002/lol2.10372")
  output$regimes_MS <- renderUI({
    tagList(url2)
  })
  
  url3 <- a("Dissolved silicon concentration and yield estimates from streams and rivers in North America and Antarctica,1964-2021", href="https://www.sciencebase.gov/catalog/item/646610b3d34ec11ae4a76b25")
  output$long_term_data <- renderUI({
    tagList(url3)
  })
  
  url4 <- a("Monthly dissolved silicon concentrations from 198 rivers in the Northern Hemisphere", href="https://www.sciencebase.gov/catalog/item/6511aeabd34e823a0275dc3e")
  output$regimes_data <- renderUI({
    tagList(url4)
  })
  
  url5<-a("Shiny App GitHub Page", href="https://github.com/sidneyabush/WSC_shiny_app.git")
  output$github_page <- renderUI({
    tagList(url5)
  })
  
  url6<-a("Getting Started with Shiny", href="https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html")
  output$shiny_page <- renderUI({
    tagList(url6)
  })
  
} #close server function

#load shiny app!
shinyApp(ui=overview_ui, server = overview_server)

#runApp(ui=overview_ui, server = overview_server)