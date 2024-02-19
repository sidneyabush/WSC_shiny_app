#install.packages("librarian")
librarian::shelf(shiny, htmltools, tidyverse, leaflet, maps, rcartocolor, DT)

#get information for map of all sites
world_countries<-map_data('world')
#lat_long_sites<-read.csv("C:/Users/johnkeir/Box/Keira_Johnson/SiSyn/Sites_LatLongClimate.csv")

lat_long_sites<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/Sites_LatLongClimate.csv")

data_source_table<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/ShinyAppDataSourceTable.csv")
colnames(data_source_table)<-c("Variable", "Source", "Units", "Spatial Scale", "Temporal Scale", "Temporal Coverage", "Dataset Link")

solute_record_length<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/shinyapp_chemQ_overview.csv")

master_chem<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/20240130_masterdata_chem.csv")
master_chem<-subset(master_chem, master_chem$variable %in% c("SRP", "DSi", "Ca", "K", "Na", "DOC", "Mg", "NO3"))
master_chem$date<-as.Date(master_chem$date)
master_chem<-master_chem[complete.cases(master_chem$date),]

pal <- colorFactor(
  palette = "Set3",
  domain=lat_long_sites$Name
)


overview_ui <- navbarPage(
  # Title of the app
  "Sythesize It!",
  # Add the panels, with titles
  tabPanel("About",
           # Start scaffolding
           fluidPage(
             # Add title
             titlePanel("About"),
             # Add a row to add things in
             fluidRow(
               # Then divide up the row into two columns, with width 4 and 8
               column(4,
                  img(src="https://lternet.edu/wp-content/uploads/2022/03/Lookout_Creek_in_the_HJ_Andrews_Experimental_Forest_Willamette_National_Forest_23908499686-1.jpg", width="100%")
               ),
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
  tabPanel("Data",
           
           fluidPage(
             
             titlePanel("About The Dataset"),
             
             fluidPage(
               
               htmltools::p("The dataset included in this workshop includes stream chemistry, discharge, climate, land use, lithology, and topography data 
                            from over 500 rivers across all 7 continents. Stream discharge and chemistry data was contributed from public and private monitoring networks, research groups,
                            and govenmental agencies. Climate, productivity, land use, lithology, and topography associated with each watershed was derived from globally available modeled
                            and remotely sensed data products."),
               
               htmltools::h2("Site Locations"),
               
               leafletOutput("mymap"),
               
               htmltools::h2("Discharge and Solute Availability"),
               
               htmltools::p("This dataset includes daily stream discharge and discrete stream chemistry. Solutes include silicon (Si),
                            calcium (Ca), magnesium (Mg), sodium (Na), potassium (K), nitrogren (NO3/NOx), phosphorus (PO4/SRP), and
                            dissolved organic carbon (DOC)."),
                 
               plotOutput('plot1'),
               
               htmltools::h2("About the Spatial Data Products"),
               
               htmltools::p("We acquired watershed characteristic (e.g., climate, land use) data from globally available spatial datasets listed below. 
                            Only globally available data layers were used in order to have consistent data sources across the extent of our dataset. 
                            Watershed boundaries were used to extract spatial data from gridded data sources."),
               
               dataTableOutput('table1')
              
             )
             
             
           )#close fluid page
  ), #close map panel
  tabPanel("Time Series",
           
           fluidPage(
             
             titlePanel("Explore Solute Time Series"),
             
             htmltools::p("Please select a site from the drop down menu below. You may display a maximum of three solutes."),
             
             sidebarLayout(
               sidebarPanel(
                 
                 # Dropdown for watershed
                 selectInput(inputId = "dropdown_ON",
                             label = "Observation Network",
                             choices = c(unique(master_chem$LTER))),
                 
                 selectInput(inputId = "dropdown_site", 
                             label = "Site", 
                             choices = NULL),
                 
                 # Radio buttons for species
                 checkboxGroupInput(inputId = "button_spp",
                              label = "Solute",
                              choices = c(unique(master_chem$variable))),
                 
                 actionButton("seasonal_button", "Toggle Plot")
                 
               ),
               
               mainPanel(plotOutput("timeseries_out"))
             ),
             
             titlePanel("Compare Solute Time Series Across Sites"),
             
             htmltools::p("Please select a solute from the drop down menu below. You may display a maximum of three sites."),
             
             sidebarLayout(
               sidebarPanel(
                 
                 # Radio buttons for species
                 radioButtons(inputId = "button_solute2",
                                    label = "Solute",
                                    choices = c(unique(master_chem$variable))),
                 
                 # Dropdown for watershed
                 selectInput(inputId = "dropdown_ON2",
                             label = "Observation Network",
                             choices = c(unique(master_chem$LTER))),
                 
                 selectInput(inputId = "dropdown_site2", 
                             label = "Site", 
                             choices = NULL),
                 
                 # Dropdown for watershed
                 selectInput(inputId = "dropdown_ON3",
                             label = "Observation Network",
                             choices = c(unique(master_chem$LTER))),
                 
                 selectInput(inputId = "dropdown_site3", 
                             label = "Site", 
                             choices = NULL),
                 
                 # Dropdown for watershed
                 selectInput(inputId = "dropdown_ON4",
                             label = "Observation Network",
                             choices = c(unique(master_chem$LTER))),
                 
                 selectInput(inputId = "dropdown_site4", 
                             label = "Site", 
                             choices = NULL),
                 
                 actionButton("seasonal_button2", "Toggle Plot")
              
               ),
               
               mainPanel(plotOutput("timeseries_out2"))
             ),
             
           )
          ),
  tabPanel("Site Comparison"),
  tabPanel("Data Comparison")
)

overview_server <- function(input, output, session){
  
  output$mymap<-renderLeaflet(leaflet() %>% 
                                addTiles(world_countries) %>%
                                addCircleMarkers(data=lat_long_sites, lat = ~Latitude, lng= ~Longitude, 
                                                 popup = ~as.character(Stream_Name), weight = 1))
  
  output$table1<- renderDataTable({

    my_table <- data_source_table
    colnames(my_table)<-c("Variable", "Source", "Units", "Spatial Scale", "Temporal Scale", "Temporal Coverage", "Dataset Link")
    my_table$`Dataset Link`<-paste0('<a href="', my_table$`Dataset Link`, '">',"Visit the Data Source Website!",'</a>')

    return(my_table)
    selection="none"

  }, 
  
  options = list(searching = FALSE, paging = FALSE, autoWidth = TRUE, info=FALSE, autoWidth=TRUE),
  escape = FALSE)
  
  output$plot1<-renderPlot({
    
    ggplot(solute_record_length, aes(year, variable, col=freq))+geom_line(lwd=15)+
    theme_classic()+labs(y="", x="Year", col="Number of Sites")+theme(text = element_text(size = 20, family = "Times"))
  
  })
  
  
  observe({
    
    sites_LTER<-master_chem %>%
      filter(LTER==input$dropdown_ON) %>%
      distinct(Stream_Name)
      
    updateSelectInput(session, "dropdown_site", choices = sites_LTER$Stream_Name)
    
  })
  
  observe({
    
    sites_LTER2<-master_chem %>%
      filter(LTER==input$dropdown_ON2) %>%
      distinct(Stream_Name)
    
    updateSelectInput(session, "dropdown_site2", choices = sites_LTER2$Stream_Name)
    
  })
  
  observe({
    
    sites_LTER3<-master_chem %>%
      filter(LTER==input$dropdown_ON3) %>%
      distinct(Stream_Name)
    
    updateSelectInput(session, "dropdown_site3", choices = sites_LTER3$Stream_Name)
    
  })
  
  observe({
    
    sites_LTER4<-master_chem %>%
      filter(LTER==input$dropdown_ON4) %>%
      distinct(Stream_Name)
    
    updateSelectInput(session, "dropdown_site4", choices = sites_LTER4$Stream_Name)
    
  })
  
  # observe({
  #   if(length(input$button_spp) > 3){
  #     updateCheckboxGroupInput(session, "button_spp", selected= tail(input$button_spp,3))
  #   }
  # })
  
  #Subset dataframe to selected species
  ## Must be done inside of "reactive consumer"
  chem_v2 <- reactive({
    master_chem %>%
      dplyr::filter(variable %in% input$button_spp)
  })
  
  chem_actual<-reactive({
    
    chem_v2() %>%
      dplyr::filter(Stream_Name == input$dropdown_site)
  })
  
  whichplot<-reactiveVal(TRUE)
  
  all_data<-reactive(ggplot(chem_actual(), aes(date, value, col=variable))+labs(x="Date", y="Concentration (add units)", col="Solute")+
    geom_point()+theme_bw()+theme(text = element_text(size = 20)))
  
  seasonal_data<-reactive(ggplot(chem_actual(), aes(factor(month(date)), value, fill=factor(variable)))+
                            labs(x="Month", y="Concentration (add units)", fill="Solute")+
    geom_boxplot()+theme_bw()+theme(text = element_text(size = 20)))
  
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
  
  output$timeseries_out <- renderPlot({   
   
     which_graph()
    
  })
  
  # # Create a histogram of this content
  # output$timeseries_out <- renderPlot({
  #   ## Uses the reactively-created weight
  #   #plot(x=chem_actual()$date, y=chem_actual()$value, xlab="Date", ylab=paste(input$button_spp, "Concentration"))
  #   
  #   ggplot(chem_actual(), aes(date, value, col=variable))+labs(x="Date", y="Concentration (add units)", col="Solute")+
  #     geom_point()+theme_bw()+theme(text = element_text(size = 20))
  #   
  
  chem_v3 <- reactive({
    master_chem %>%
      dplyr::filter(variable %in% input$button_solute2)
  })
  
  chem_actual2<-reactive({
    
    chem_v3() %>%
      dplyr::filter(Stream_Name %in% c(input$dropdown_site2, input$dropdown_site3, input$dropdown_site4))
  })
  
  whichplot2<-reactiveVal(TRUE)
  
  all_data2<-reactive(ggplot(chem_actual2(), aes(date, value, col=Stream_Name))+labs(x="Date", y="Concentration (add units)", col="Site")+
                        geom_point()+theme_bw()+theme(text = element_text(size = 20)))
  
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
  
  output$timeseries_out2 <- renderPlot({   
    
    which_graph2()
    
  })
  
}

shinyApp(ui=overview_ui, server = overview_server)

