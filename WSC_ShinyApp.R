#install packages
librarian::shelf(shiny, htmltools, tidyverse, leaflet, maps, rcartocolor, DT, ggpubr)

####read in data for tables, plotting, mapping####

#get information for map of all sites
#read in world map and lat/long of each site
world_countries<-map_data('world')

published_data<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/PublishedSites.csv")

lat_long_sites<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/Sites_LatLongClimate.csv")
lat_long_sites$released<-ifelse(lat_long_sites$Stream_Name %in% published_data$Stream_Name, "yes", "no")

map_color_options<-c("released", "Climate_Zone_Name")

#read in source data for table of drivers
data_source_table<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/ShinyAppDataSourceTable.csv")
colnames(data_source_table)<-c("Variable", "Source", "Units", "Spatial Scale", "Temporal Scale", "Temporal Coverage", "Dataset Link")

#read in data for solute record length table
solute_record_length<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/shinyapp_chemQ_overview.csv")

#read in master chemistry and format it
master_chem<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/20240312_masterdata_chem.csv")

master_chem<-subset(master_chem, master_chem$variable %in% c("SRP", "PO4", "DSi", "Ca", "K", "Na", "DOC", "Mg", "NO3", "NOx"))
master_chem$date<-as.Date(master_chem$date)
master_chem<-master_chem[complete.cases(master_chem$date),]
master_chem<-master_chem[master_chem$Stream_Name %in% published_data$Stream_Name,]

#read in master discharge and format it
master_q<-read.csv("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/20240201_masterdata_discharge.csv")
master_q$Date<-as.Date(master_q$Date)
master_q<-master_q[complete.cases(master_q$Date),]
master_q<-master_q[master_q$Stream_Name %in% published_data$Stream_Name,]

chem_dates<-master_chem %>%
  group_by(Stream_Name) %>%
  summarise(min_date_chem=min(date), max_date_chem=max(date))

q_dates<-master_q %>%
  group_by(Stream_Name) %>%
  summarise(min_date_q=min(Date), max_date_q=max(Date))

dates_all<-merge(chem_dates, q_dates, by="Stream_Name")

dates_all$min<-as.Date(ifelse(dates_all$min_date_chem < dates_all$min_date_q, 
                              dates_all$min_date_q, dates_all$min_date_chem), origin = "1970-01-01")

dates_all$max<-as.Date(ifelse(dates_all$max_date_chem > dates_all$max_date_q, 
                              dates_all$max_date_q, dates_all$max_date_chem), origin = "1970-01-01")

master_chem<-merge(master_chem, dates_all[,c(1,6,7)], by="Stream_Name")

master_chem<-master_chem %>%
  group_by(Stream_Name) %>%
  filter(date >= min & date <= max)

master_q<-merge(master_q, dates_all[,c(1,6,7)], by="Stream_Name")

master_q<-master_q %>%
  group_by(Stream_Name) %>%
  filter(Date >= min & Date <= max)
  
#read in solute drivers file
load("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/Mean_Solute_Drivers.RData")

load("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/Solute_Drivers_OneCol.RData")

load("/Users/keirajohnson/Box Sync/Hydrology_Lab/Projects/WSC_ShinyApp/CQ_file.RData")

site_map<-leaflet(lat_long_sites) %>% addTiles() %>% addCircleMarkers(lat = ~Latitude, lng= ~Longitude)

####User Interface####
overview_ui <- navbarPage(
  # Title of the app
  "Sythesize It!",
  
  #Here is where you start adding pages
  #tab panel makes a new page, "About" is the name of the tab
  
  ####About Panel####
  tabPanel("About",
           #this opens the page
           fluidPage(
             # Add a row to add things in
             fluidRow(
               #in the first column add an image
               #in the second column add some text 
               column(width=4, align="center", 
                      
                      tags$img(src="title_picture.jpg", width="100%")
                    
                      ),
               column(width=8,
                      
                      htmltools::h2("About The Workshop"),
                      
                      htmltools::p("Big data integration offers the potential to advance water science by promoting a more comprehensive understanding of watershed function. 
                           During this workshop, we will explore an extensive dataset including field observations and modeled results of multiple stream chemical 
                           and hydrometric parameters spanning nearly 500 rivers. This dataset also includes associated spatial data describing climate, land cover, lithology, 
                           and topography associated with each watershed. We will explore questions and hypotheses about stream biogeochemical function using data visualization
                           tools to evaluate relationships between catchment characteristics and watershed processes."),
                      
                      htmltools::h2("About This Project"),
                      
                      htmltools::p("This workshop stems out of a NCEAS funded working group to better understand controls on concentrations, fluxes, seasonality, and trends in
                           riverine silicon (Si). For the past 5 years, we have worked to compile and harmonize this dataset to answer questions focused on better
                           understanding Si cycling globally."),
                      
                      htmltools::h3("Published Manuscripts"),
                      
                      uiOutput("long_term_MS"),
                      
                      uiOutput("regimes_MS"),
                      
                      htmltools::h3("Data Publications"),
                      
                      uiOutput("long_term_data"),
                      
                      uiOutput("regimes_data")
                      
                      )
             ) #close fluid row
           ) #close fluid page
  ), #close about panel
  
  #### Data Panel ####
  #this panel starts to incorporate output from the server (below) including a map and plots
  tabPanel("Data Availability",
           
           fluidPage(
             
             htmltools::h2("About The Dataset"),
             
             fluidRow(
               
               htmltools::p("The dataset included in this workshop includes stream chemistry, discharge, climate, land use, lithology, and topography data 
                            from over 500 rivers across all 7 continents. Stream discharge and chemistry data was contributed from public and private monitoring networks, research groups,
                            and govenmental agencies. Climate, productivity, land use, lithology, and topography associated with each watershed was derived from globally available modeled
                            and remotely sensed data products."),
               
               htmltools::h2("Site Locations"),
               
               htmltools::p("The map below shows sites where data has been published in blue. Sites shown in grey are places where we have data but it is not yet published."),
               
               #add option to color points by different things
               varSelectInput(
                 inputId = "map_option",
                 label = "Color Sites By:",
                 data = lat_long_sites %>% select(map_color_options)
               ),
               
               #see mymap output in server below
               leafletOutput("mymap"),
               
               htmltools::h2("Discharge and Solute Availability"),
               
               htmltools::p("This dataset includes daily stream discharge and discrete stream chemistry. Solutes include silicon (Si),
                            calcium (Ca), magnesium (Mg), sodium (Na), potassium (K), nitrogren (NO3/NOx), phosphorus (PO4/SRP), and
                            dissolved organic carbon (DOC)."),
                 
               #see plot1 output in server below
               plotOutput('plot1'),
               
               htmltools::h2("About the Spatial Data Products"),
               
               htmltools::p("We acquired watershed characteristic (e.g., climate, land use) data from globally available spatial datasets listed below. 
                            Only globally available data layers were used in order to have consistent data sources across the extent of our dataset. 
                            Watershed boundaries were used to extract spatial data from gridded data sources."),
               
               #see table1 output in server below
               dataTableOutput('table1')
              
             )#close fluid row
           )#close fluid page
  ), #close tab panel
  
  tabPanel("Data Exploration",
           
           fluidPage(
             
             #first set of plots to look at solute time series
             titlePanel("Explore Relationships between Catchment Characteristics and Solute Concentrations"),
             htmltools::p("Please select a climate variable and solute from the dropdown lists below."),
             
             #side bar layout allows you to add box on the side of the page, good for plotting
             sidebarLayout(
               sidebarPanel(
                 
                 #select input creates a dropdown menu
                 #this lists all observation networks in the master chemistry data
                 selectInput(inputId = "x_axis_dropdown",
                             label = "X-Axis Variable",
                             choices = c(unique(solute_drivers_all$Driver))),
                 
                 selectInput(inputId = "y_axis_dropdown",
                             label = "Y-Axis Variable",
                             choices = c(unique(solute_drivers_all$Driver2))),
                 
                 selectInput(inputId = "color_dropdown",
                             label = "Color Variable",
                             choices = c(unique(solute_drivers_all$Driver3))),
                 
                 actionButton("LR_button","Add linear regression")
                 
               ),
               
               #plot solute timeseries
               mainPanel(
                 plotOutput("driver_solute_plot")
               ),
               
             ), #close sidebar layout
            
           ) #close fluid page
           
  ), #close tab panel
  
  #### Time Series Panel ####
  #here we add interactive plotting functions
  #users select the input using the "selectInput" function, and that input feeds into input used in the server code below
  tabPanel("Time Series",
           
           fluidPage(
             
             #first set of plots to look at solute time series
             titlePanel("Explore Solute Time Series"),
             htmltools::p("Please select a site from the drop down menu below. You may display a maximum of three solutes."),
             
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
                 actionButton("seasonal_button", "Toggle Plot")
                 
               ),
               
               #now we open a main panel, this will be to the right of the sidebarLayout panel
               mainPanel(
                 sliderInput("xslider", label = "Select Date Range of Interest", min = 0, max = 10, value = c(1,9)),
                 plotOutput("timeseries_out"), #plot solute timeseries data, see timeseries_out plot in server below
                 plotOutput("timeseries_out_Q") #plot discharge timeseries data, see timeseries_out_Q plot in server below
                 )
             ),
             
             #add secondary title for another set of plots, this time with more dropdowns for sites
             titlePanel("Compare Solute Time Series Across Sites"),
             
             htmltools::p("Please select a solute from the drop down menu below. You may display a maximum of three sites."),
             
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
                 actionButton("seasonal_button2", "Toggle Plot")
              
               ),
               #plot solute timeseries
               mainPanel(
                 sliderInput("xslider2", label = "Select Date Range of Interest", min = 0, max = 10, value = c(1,9)),
                 plotOutput("timeseries_out2")
              )
             ) #close sidebarLayout
           ) #close fluid page
          ), #close panel
  
  #### Site Comparison Panel####
  tabPanel("Site Comparison",
           
           fluidPage(
             
             #first set of plots to look at solute time series
             titlePanel("Explore Distributions of Solutes and Drivers across the dataset"),
             htmltools::p("Please select a variable or solute from the dropdown list below."),
           
             #side bar layout allows you to add box on the side of the page, good for plotting
             sidebarLayout(
               sidebarPanel(
                 
                 #select input creates a dropdown menu
                 #this lists all observation networks in the master chemistry data
                 selectInput(inputId = "driver_solute_dropdown",
                             label = "Variable",
                             choices = c(unique(solute_drivers$Driver)))
                 
               ),
               
               #plot solute timeseries
               mainPanel(
                 plotOutput("driver_histogram")
               )
               
             ),#close sidebar layout
             
             #add secondary title for another set of plots, this time with more dropdowns for sites
             titlePanel("Compare Solute Distributions across sites"),
             
             htmltools::p("Please select a solute from the drop down menu below and a maximum of 10 sites from the site dropdown"),
             
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
             ) #close sidebarLayout
           )#close fluid page
  ),#close tab panel
  
  #### Data Comparison Panel####
  tabPanel("CQ",
           
           fluidPage(
             
           #first set of plots to look at solute time series
           titlePanel("Explore Concentration Discharge Relationships across sites"),
           htmltools::p("Please select a solute from the dropdown lists below."),
           
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
  )#close tab panel
)


#### Server ####
#the server is where the all of the interactive selection from the user interface happens
#the code below is split into sections by panels denoted in the UI function above
overview_server <- function(input, output, session){
  
  ####About Panel Server####
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
  
  #### Data Panel Server ####
  
  #map output
  output$mymap<-renderLeaflet(site_map)
  
  map_color <- reactive({
    
    lat_long_sites %>%
      select(input$map_option) %>%
      unlist()
    
  })
  
  pal <- reactive({colorFactor("RdYlBu", domain = map_color())(map_color())})
  
  pal2<-reactive({colorFactor("RdYlBu", domain = map_color())})
  
  legend_values <- reactive({lat_long_sites %>%
                              select(input$map_option)})
  
  observe({

      leafletProxy("mymap", data = lat_long_sites) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(lat = ~Latitude, lng= ~Longitude,
                       popup = ~as.character(Stream_Name), weight = 1, fillOpacity = 0.7,
                       color = ~pal())
      #addLegend(position = "topright", pal = pal2, values = legend_values(), title = "Legend")

  })
  
  #drivers table information
  output$table1<- renderDataTable({

    my_table <- data_source_table
    colnames(my_table)<-c("Variable", "Source", "Units", "Spatial Scale", "Temporal Scale", "Temporal Coverage", "Dataset Link")
    #this makes the links "clickable"
    my_table$`Dataset Link`<-paste0('<a href="', my_table$`Dataset Link`, '">',"Visit the Data Source Website!",'</a>')
    return(my_table)
    selection="none"
  }, 
  #edit how table appears
  options = list(searching = FALSE, paging = FALSE, autoWidth = TRUE, info=FALSE, autoWidth=TRUE),
  escape = FALSE)
  
  #plot 1 output
  output$plot1<-renderPlot({
    
    ggplot(solute_record_length, aes(year, variable, col=freq))+geom_line(lwd=15)+
    theme_classic()+labs(y="", x="Year", col="Number of Sites")+theme(text = element_text(size = 20, family = "Times"))
  
  })
  
  #### Time Series Panel Server ####
  #in the time series panel there is quite a bit of inputs that are dependent on previous selection
  #the observe function paired with tidyverse allows you to filter dropdown options based on user selection
  #e.g., give list of sites within a user-specified observation network
  
  #### For first half of time series panel ####
  
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
  all_data<-reactive(ggplot(chem_actual(), aes(date, value, col=variable))+labs(x="Date", y="Concentration (add units)", col="Solute")+
                       geom_point()+theme_bw()+theme(text = element_text(size = 20), legend.position = "bottom")+
                       xlim(input$xslider))
  
  #then seasonal data
  seasonal_data<-reactive(ggplot(chem_actual(), aes(factor(month(date)), value, fill=factor(variable)))+
                            labs(x="Month", y="Concentration (add units)", fill="Solute")+
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
  all_data_Q<-reactive(ggplot(q_v2(), aes(Date, Qcms), col="deepskyblue3")+labs(x="Date", y="Discharge (cms)")+
                         geom_line()+theme_bw()+theme(text = element_text(size = 20))+xlim(input$xslider))
  #seasonal Q plot
  seasonal_data_Q<-reactive(ggplot(q_v2_dailyavg(), aes(doy_date, mean_daily))+
                              labs(x="Month", y="Discharge (cms)")+
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
  
  #### For second half of time series panel ####
  
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
      summarise(min_overall=min(min)) %>%
      slice_min(min_overall)
  })
  
  max_date2<-reactive({
    chem_actual2() %>%
      summarise(max_overall=max(max)) %>%
      slice_max(max_overall)
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
  all_data2<-reactive(ggplot(chem_actual2(), aes(date, value, col=Stream_Name))+labs(x="Date", y="Concentration (add units)", col="Site")+
                        geom_point()+theme_bw()+theme(text = element_text(size = 20))+xlim(input$xslider2))
  
  #seasonal time series data
  seasonal_data2<-reactive(ggplot(chem_actual2(), aes(factor(month(date)), value, fill=factor(Stream_Name)))+
                             labs(x="Month", y="Concentration (add units)", fill="Site")+
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
  
  #### Site Comparison Panel Server ####
  solute_drivers_v3<-reactive({
    
    solute_drivers %>%
      dplyr::filter(Driver==input$driver_solute_dropdown)
    
  })
  
  #render plot
  output$driver_histogram <- renderPlot({   
    
    ggplot(solute_drivers_v3(), aes(x=Mean_Value))+geom_histogram(bins = 50, fill="black")+theme_bw()+
      labs(x=input$driver_solute_dropdown, y="Count")+theme(text = element_text(size = 20))
    
  })
  
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
      labs(y=input$button_solute3, x="Site")+theme(text = element_text(size = 20))
    
  })
  
  
  #### Data Comparison Panel Server####
  
  solute_drivers_v2<-reactive({
    
    solute_drivers_all %>%
      dplyr::filter(Driver==input$x_axis_dropdown & Driver2==input$y_axis_dropdown & Driver3==input$color_dropdown)
    
  })
  
  #whichplot2, to plot seasonal vs annual data
  whichplot3<-reactiveVal(TRUE)
  
  driver_solute_plot_noLR<-reactive(ggplot(solute_drivers_v2(), aes(Mean_Value, Mean_Value2, col=Mean_Value3))+geom_point()+
                                 labs(x=input$x_axis_dropdown, y=input$y_axis_dropdown, col=input$color_dropdown)+
                                theme_bw()+theme(text = element_text(size = 20)))
  
  driver_solute_plot_LR<-reactive(ggplot(solute_drivers_v2(), aes(Mean_Value, Mean_Value2, col=Mean_Value3))+geom_point()+
                                    labs(x=input$x_axis_dropdown, y=input$y_axis_dropdown, col=input$color_dropdown)+theme_bw()+theme(text = element_text(size = 20))+
                                    geom_smooth(method = "lm", se=F, col="firebrick4"))
  
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
    
    ggplot(cq3(), aes(log(get(input$button_solute4)), log(Qcms), col=Stream_Name))+geom_point()+
      labs(x="Log Concentration", y="Log Discharge", col="Site")+
      geom_smooth(se=F, method = "lm")+
      theme_bw()+theme(text = element_text(size = 20))
    
  })
  
  
} #close server function

#load shiny app!
shinyApp(ui=overview_ui, server = overview_server)

#runApp(ui=overview_ui, server = overview_server)

