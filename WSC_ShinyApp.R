#install packages
librarian::shelf(shiny, htmltools, tidyverse, leaflet, maps, rcartocolor, DT, ggpubr)

####read in data for tables, plotting, mapping####

#get information for map of all sites
#read in world map and lat/long of each site
world_countries<-map_data('world')

lat_long_sites<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/Sites_LatLongClimate.csv")

#read in source data for table of drivers
data_source_table<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/ShinyAppDataSourceTable.csv")
colnames(data_source_table)<-c("Variable", "Source", "Units", "Spatial Scale", "Temporal Scale", "Temporal Coverage", "Dataset Link")

#read in data for solute record length table
solute_record_length<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/shinyapp_chemQ_overview.csv")

#read in master chemistry and format it
master_chem<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/20240130_masterdata_chem.csv")
master_chem<-subset(master_chem, master_chem$variable %in% c("SRP", "DSi", "Ca", "K", "Na", "DOC", "Mg", "NO3"))
master_chem$date<-as.Date(master_chem$date)
master_chem<-master_chem[complete.cases(master_chem$date),]

#read in master discharge and format it
master_q<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/20240201_masterdata_discharge.csv")
master_q$Date<-as.Date(master_q$Date)
master_q<-master_q[complete.cases(master_q$Date),]

#create color pallette for map
pal <- colorFactor(
  palette = "Set3",
  domain=lat_long_sites$Name
)

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
             # Add title to the page
             titlePanel("About"),
             # Add a row to add things in
             fluidRow(
               # Then divide up the row into two columns, with width 4 and 8
               #in the first column add an image
               column(4,
                  img(src="https://lternet.edu/wp-content/uploads/2022/03/Lookout_Creek_in_the_HJ_Andrews_Experimental_Forest_Willamette_National_Forest_23908499686-1.jpg", width="100%")
               ),
               #in the second column add some text 
               #h2 makes a header, p adds a new paragraph
               column(8,
                      htmltools::h2("About The Workshop"),
                      
                      htmltools::p("Big data integration offers the potential to advance water science by promoting a more comprehensive understanding of watershed function. 
                           During this workshop, we will explore an extensive dataset including field observations and modeled results of multiple stream chemical 
                           and hydrometric parameters spanning nearly 500 rivers. This dataset also includes associated spatial data describing climate, land cover, lithology, 
                           and topography associated with each watershed. We will explore questions and hypotheses about stream biogeochemical function using data visualization
                           tools to evaluate relationships between catchment characteristics and watershed processes."),
                      
                      htmltools::h2("About This Project"),
                      
                      htmltools::p("This workshop stems out of a NCEAS funded working group to better understand controls on concentrations, fluxes, seasonality, and trends in
                           riverine silicon (Si). For the past 5 years, we have worked to compile and harmonize this dataset to answer questions focused on better
                           understanding Si cycling globally.")
                      
               ) #close column
             ) #close fluid row
           ) #close fluid page
  ), #close about panel
  
  #### Data Panel ####
  #this panel starts to incorporate output from the server (below) including a map and plots
  tabPanel("Data",
           
           fluidPage(
             
             titlePanel("About The Dataset"),
             
             fluidRow(
               
               htmltools::p("The dataset included in this workshop includes stream chemistry, discharge, climate, land use, lithology, and topography data 
                            from over 500 rivers across all 7 continents. Stream discharge and chemistry data was contributed from public and private monitoring networks, research groups,
                            and govenmental agencies. Climate, productivity, land use, lithology, and topography associated with each watershed was derived from globally available modeled
                            and remotely sensed data products."),
               
               htmltools::h2("Site Locations"),
               
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
                             choices = c(unique(master_chem$LTER))),
                 
                 #then for site
                 selectInput(inputId = "dropdown_site2", 
                             label = "Site", 
                             choices = NULL),
                 
                 #Second site selection
                 selectInput(inputId = "dropdown_ON3",
                             label = "Observation Network",
                             choices = c(unique(master_chem$LTER))),
                 
                 selectInput(inputId = "dropdown_site3", 
                             label = "Site", 
                             choices = NULL),
                 
                 #Third site selection
                 selectInput(inputId = "dropdown_ON4",
                             label = "Observation Network",
                             choices = c(unique(master_chem$LTER))),
                 
                 selectInput(inputId = "dropdown_site4", 
                             label = "Site", 
                             choices = NULL),
                 
                 #toggle between seasonal and all data
                 actionButton("seasonal_button2", "Toggle Plot")
              
               ),
               #plot solute timeseries
               mainPanel(
                 plotOutput("timeseries_out2")
              )
             ) #close sidebarLayout
           ) #close fluid page
          ), #close panel
  
  #### Site Comparison Panel####
  tabPanel("Site Comparison"),
  
  #### Data Comparison Panel####
  tabPanel("Data Comparison")
)


#### Server ####
#the server is where the all of the interactive selection from the user interface happens
#the code below is split into sections by panels denoted in the UI function above
overview_server <- function(input, output, session){
  
  #### Data Panel Server ####
  
  #map output
  output$mymap<-renderLeaflet(leaflet() %>% 
                                addTiles() %>%
                                addCircleMarkers(data=lat_long_sites, lat = ~Latitude, lng= ~Longitude, 
                                                popup = ~as.character(Stream_Name), weight = 1))
  
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
  
  #reactiveVal allows you to toggle between two plots
  #here we will use it to display either the full time series OR seasonal data
  whichplot<-reactiveVal(TRUE)
  
  #define the two plots
  #first full time series
  all_data<-reactive(ggplot(chem_actual(), aes(date, value, col=variable))+labs(x="Date", y="Concentration (add units)", col="Solute")+
                       geom_point()+theme_bw()+theme(text = element_text(size = 20)))
  
  #then seasonal data
  seasonal_data<-reactive(ggplot(chem_actual(), aes(factor(month(date)), value, fill=factor(variable)))+
                            labs(x="Month", y="Concentration (add units)", fill="Solute")+
                            geom_boxplot()+theme_bw()+theme(text = element_text(size = 20)))
  
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
                         geom_line()+theme_bw()+theme(text = element_text(size = 20)))
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
      filter(LTER==input$dropdown_ON2) %>%
      distinct(Stream_Name)
    
    updateSelectInput(session, "dropdown_site2", choices = sites_LTER2$Stream_Name)
    
  })
  
  #return sites within given observation network (dropdown_site3)
  observe({
    
    sites_LTER3<-master_chem %>%
      filter(LTER==input$dropdown_ON3) %>%
      distinct(Stream_Name)
    
    updateSelectInput(session, "dropdown_site3", choices = sites_LTER3$Stream_Name)
    
  })
  
  #return sites within given observation network (dropdown_site4)
  observe({
    
    sites_LTER4<-master_chem %>%
      filter(LTER==input$dropdown_ON4) %>%
      distinct(Stream_Name)
    
    updateSelectInput(session, "dropdown_site4", choices = sites_LTER4$Stream_Name)
    
  })
  
  #filter master chem by solute
  chem_v3 <- reactive({
    master_chem %>%
      dplyr::filter(variable %in% input$button_solute2)
  })
  
  #then by site selections
  chem_actual2<-reactive({
    
    chem_v3() %>%
      dplyr::filter(Stream_Name %in% c(input$dropdown_site2, input$dropdown_site3, input$dropdown_site4))
  })
  
  #whichplot2, to plot seasonal vs annual data
  whichplot2<-reactiveVal(TRUE)
  
  #all time series data
  all_data2<-reactive(ggplot(chem_actual2(), aes(date, value, col=Stream_Name))+labs(x="Date", y="Concentration (add units)", col="Site")+
                        geom_point()+theme_bw()+theme(text = element_text(size = 20)))
  
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
  
} #close server function

#load shiny app!
shinyApp(ui=overview_ui, server = overview_server)

