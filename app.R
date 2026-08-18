# app.R

# INIT -----------------

## LIBRARIES -----------
# Load required libraries
library(plyr)
library(brapi)
library(tidyverse)
library(shiny)
library(bs4Dash)
library(DT)
library(rjson)
library(reshape2)
library(AGHmatrix)
library(heatmaply)
library(shinyWidgets)
library(data.table)
library(writexl)
library(tis)
library(fresh)
library(networkD3)
library(visNetwork)
library(bslib)
library(SimpleMating)
library(sortable)
library(shinyjs)
library(plotly)
library(openxlsx)  # For Excel file handling
library(writexl)   # For Excel file writing



# Include necessary JavaScript libraries

tags$head(
  tags$script(src = "https://d3js.org/d3.v5.min.js"),
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/networkD3/0.4.1/networkD3.min.js")
)


## CUSTOM DATA and FUNCTION LOAD -----------
# Source custom functions and configurations from separate files
source("app_functions.R")
source("app_configs.R")
source("modules/flowering.R")
source("modules/pedigree.R")
source("modules/performance.R")
source("modules/crosses.R")
source("modules/download_page.R")
source("modules/optimization.R")
source("modules/cubicle.R")
#source("modules/cloneproperties.R")


## THEME
#TBA

## CHECK if true connection
brapi::ba_check(brap) # should be true, for debugging

brapi::ba_login(brap) #important if database requires login for brapi queries
brapi::ba_login(brap2) 
# USER INTERFACE  -------------------------------------------------------------

ui <- dashboardPage(
  

  title = "STC",
  dark = NULL,

  ## CONTROLBAR ----
  controlbar = dashboardControlbar(
    collapsed = TRUE,
    div(class = "p-3", skinSelector()),
    pinned = FALSE
  ),

  ## HEADER --------
  header = dashboardHeader(
    title = "SCT",
    rightUi = tagList(
      dropdownMenu(
        type = "notifications",
        badgeStatus = NULL,
        icon = icon("sun"),
        switchInput(
          inputId = "dark_mode",
          label = "Dark Mode",
          onStatus = "success",
          offStatus = "danger"
        )
      )
    )
  ),

  ## SIDEBAR ------
  sidebar = dashboardSidebar(
    selectInput("location", "Step 1: Select Location", choices = location_iid_map2),
    
    #this is kind of confusing. The idea is that multiple breeders might be working at same location (Florida) and they should be able to track crosses independently, even though cane lines are combined
    #so crossesid refers to crosses a specific breeder is making
    selectInput("crossesid", "Step 2: Select Breeder", choices=crosses_iid_map), 
    
    dateInput(
      "date",
      "Step 3: Choose A Date"
    ),
    #actionButton("brapipull", "Get Flower Inventory Data"),
    textOutput("dateWarning"), 
    

    
    actionButton(
      "brapipull",
     strong("Step 4. Get Flower Inventory")
    ),
    p("Don't forget to push 'Get Flower Inventory", strong("each"), "each time you choose a new date"),
    sidebarMenu(
      menuItem("Home",
               tabName = "home",
               icon = icon("home")
      ),
      menuItem("Inventory/Sorting",
               tabName = "flowering",
               icon = icon("seedling")
      ),
      # menuItem("Clone Properties",
      #          tabName = "properties",
      #          icon = icon("info")
      # ),
      menuItem("Pedigree/Properties",
               tabName = "kinship",
               icon = icon("people-group")
      ),
      menuItem("Clone Performance",
               tabName = "performance",
               icon = icon("star")
      ),
      menuItem("Previous Crosses",
               tabName = "crosses",
               icon = icon("xmark")
      ),
      menuItem("(BETA) Cross Optimization",
               tabName = "optimization",
               icon = icon("dna")
      ),
      menuItem("Download Data",
               tabName = "download",
               icon = icon("download")
      ),
      menuItem("Cubicle Manager",
               tabName = "cubicle",
               icon = icon("th"))
      )
    ),

  ## BODY -----

  body = dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .rank-list-container .rank-list-item {
          color: #333;
          background-color: #f8f9fa;
        }
        .dark-mode .rank-list-container .rank-list-item {
          color: #f8f9fa;
          background-color: #343a40;
        }
        /* Updated styles for sidebar elements */
        .dark-mode .main-sidebar {
          background-color: #343a40 !important;
        }
        .dark-mode .main-sidebar .nav-sidebar .nav-item .nav-link,
        .dark-mode .main-sidebar .nav-sidebar .nav-item .nav-link p,
        .dark-mode .main-sidebar .brand-text,
        .dark-mode .main-sidebar .user-panel .info,
        .dark-mode .sidebar .form-group label,
        .dark-mode .sidebar p,
        .dark-mode .sidebar .btn-default,
        .dark-mode .sidebar .form-control,
        .dark-mode .sidebar .input-group-text,
        .dark-mode .sidebar .selectize-input,
        .dark-mode .sidebar .selectize-dropdown {
          color: #f8f9fa !important;
        }
        .dark-mode .sidebar .form-control,
        .dark-mode .sidebar .input-group-text,
        .dark-mode .sidebar .selectize-input,
        .dark-mode .sidebar .selectize-dropdown {
          background-color: #454d55 !important;
          border-color: #6c757d !important;
        }
        .dark-mode .sidebar .btn-default {
          background-color: #454d55 !important;
          border-color: #6c757d !important;
        }
        .dark-mode .sidebar .btn-default:hover {
          background-color: #5a6268 !important;
        }
        /* Styles for optimized crossing plan */
        .dark-mode .box-body {
          color: #f8f9fa !important;
        }
        .dark-mode .dataTables_wrapper {
          color: #f8f9fa !important;
        }
        .dark-mode .dataTables_wrapper .dataTables_length,
        .dark-mode .dataTables_wrapper .dataTables_filter,
        .dark-mode .dataTables_wrapper .dataTables_info,
        .dark-mode .dataTables_wrapper .dataTables_processing,
        .dark-mode .dataTables_wrapper .dataTables_paginate {
          color: #f8f9fa !important;
        }
        .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #f8f9fa !important;
        }
        .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current,
        .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
          color: #333 !important;
        }
        /* Consistent tab styling */
        .nav-tabs .nav-link.active {
          background-color: #007bff !important;
          color: #ffffff !important;
          border-color: #007bff !important;
        }

        .nav-tabs .nav-link {
          color: #007bff !important;
        }

        .nav-tabs .nav-link:hover:not(.active) {
          border-color: #e9ecef #e9ecef #dee2e6;
          color: #0056b3 !important;
        }

        /* Ensure sidebar consistency */
        .nav-sidebar .nav-item .nav-link.active {
          background-color: #007bff !important;
          color: #ffffff !important;
        }

        .nav-sidebar .nav-item .nav-link {
          color: #333333 !important;  /* Changed to black */
        }

        .nav-sidebar .nav-item .nav-link:hover:not(.active) {
          color: #007bff !important;
        }

        /* Dark mode compatibility */
        .dark-mode .nav-tabs .nav-link.active {
          background-color: #375a7f !important;
          color: #ffffff !important;
          border-color: #375a7f !important;
        }

        .dark-mode .nav-tabs .nav-link {
          color: #375a7f !important;
        }

        .dark-mode .nav-sidebar .nav-item .nav-link.active {
          background-color: #375a7f !important;
          color: #ffffff !important;
        }

        .dark-mode .nav-sidebar .nav-item .nav-link {
          color: #f8f9fa !important;  /* Keep light color for dark mode */
        }
      "))
    ),
    tabItems(

      ### Home content ----
      tabItem(
        tabName = "home",
        
        h1("Sugarcane Integrated Breeding System (SIBS) Sugarcane Crossing Tool (SCT)"),
        p("Welcome to SCT! To use this app, follow the instructions below:"),
        p("* Start by loggin in: from the sidebar on the left, (1) select a location and (2) breeder name."),
        p("* Then, chose an (3) inventory date."),
        p("* Next, (4) Click on the 'Get Flower Inventory' button"),
        p("* After that, move to the Flowering Inventory tab to resort your inventory data"),
        p("* You can then click on other tabs to explore related breeding data"),
        p("Follow ", a(href="https://github.com/USDA-ARS-GBRU/SugarcaneCrossingTool", "this link"), " to the github repo for detailed instructions."),
       
        card(
          
        card_header("Login Information"),
        
        
        p("You've logged in to view inventory for this location: "),
        
        span(textOutput("inventoryPointer"), style="color:blue"),
        
        br(),
        
        p("You've logged in as User:"),
        
        span(textOutput("crossPointer"), style="color:blue")),
        
        card(
          card_header("Inventory Information"),
          
          p("You're viewing inventory for this day:"),
          span(textOutput("datePointer"), style="color:blue"))),

      ### Inventory content -----
      tabItem(
        tabName = "flowering",

          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Interactive Sorting Table",
        #this commented out code is only useful for troubleshooting the sorting lists
        # fluidRow(
        #   column(
        #     width = 12,
        #     tags$b("Result"),
        #     column(
        #       width = 12,
        # 
        #       tags$p("input$female_list"),
        #       verbatimTextOutput("results_1"),
        # 
        #       tags$p("input$male_list"),
        #       verbatimTextOutput("results_2"),
        # 
        #     )
        #   )
        # )
        # ,
        #   
        fluidRow(
          box(
            title = "Step 5: Sorting",
            p("Drag and drop the available flowering clones into their appropriate category.", strong("Only"), "sorted clones will be displayed in subsequent tabs and/or used in cross prediction so this step must be done first."),
            width = 12,
            fluidRow(
              # column(
              #   width = 4,
              #   h4("Available Clones"),
              #   uiOutput("available_clones")
              # ),
              column(
                width = 4,
                h4("Female Parents"),
                uiOutput("female_parents")
              ),
              column(
                width = 4,
                h4("Male Parents"),
                uiOutput("male_parents")
              )
            )
          )
        )),

        tabPanel(
          "Raw data",
        fluidRow(
        box(title="Raw Data",
        p("These tables shows you the", strong("raw data"), "for all parents flowering today, sorted into male and female columns based on techician's inventory"),
             textOutput("dataSourceText"),
            width=12,
            fluidRow(
              column(
                width=4,
                h4("Female Parents"),
                DTOutput("inventoryTableFemale")
              ),
            column(
                width=4,
                h4("Male Parents"),
                DTOutput("inventoryTableMale")
              )
            )))




        
         ))),
      
      
      # #### properties tab content ----
      # tabItem(
      #   tabName = "properties",
      #   fluidRow(
      #     box(
      #       title="Basic Passport/Accession Property Information",
      #       width=12,
      #       actionButton(
      #         inputId = "makeproperties",
      #         label = "Get Clone Passport Data"
      #       ),
      #       p("This table shows you selected passport data for flowering clones")
      #     ), 
      #     DTOutput("propertiesTable"),
      #   )
      # ),

      ### Pedigree tab content ----

      tabItem(
        tabName = "kinship",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Pedigree Table",
            fluidRow(
              box(
                title="Basic Pedigree Information",
                width=12,
                actionButton(
                  inputId = "makepedigree",
                  label = "Get Pedigree Data"
                ),
                p("This table shows you the pedigree of each", strong("sorted"), "and flowering clone and the number of progeny it produced")
              ),
              DTOutput("pedigreeTable"),
            )
          ),
          tabPanel(
            "Relationship Matrix",
            box(
              title="Relationship Heatmap",
              width=12,
              p("This is a relationship matrix of the flowering clones. Values closer to one indicate high relatedness. You can zoom in to particular regions of the matrix.")),
            plotlyOutput("pedigreeMatrix")
          ),
          tabPanel(
            "Visualize Pedigrees",
            fluidRow(
              box(
                title="Pedigree Trees",
                p("You can select clones from the drop-down menu to see their pedigree. Note- future work will allow you to select how many generations you want to see"),
                width=12,
                uiOutput("cloneDropdown"),
                visNetworkOutput("pedigreeGraph")
              )
            )
          )
        )
      ),

      ### Performance tab content ----
      tabItem(
        tabName = "performance",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Performance Table",
            fluidRow(
              box(
                width=12,
                actionButton(
                  inputId = "makeperformance",
                  label = "Get Performance Data"
                ),
                p("This table shows the mean and sd of the performance for each clone 
                  that is flowering on the date you selected. Once data has been pulled from the database, you will be able to select traits to view using the drop-down menu. This step may take several minutes, please be patient.", 
                  strong("Note, this data is very roughly averaged, please interpret with caution.")),
                
                
                # stuff for what phenotype to select
                uiOutput(outputId = "colSelect"), # render html list output
                actionButton("selectCol", "View Selected Data")
              )
            ),
            DTOutput("performanceTable")
          ),
          tabPanel(
            "Trait Scatter Plot",
            fluidRow(
              box(
                width=12,
                uiOutput("scatterPlotDropdown_x"),
                uiOutput("scatterPlotDropdown_y"),
                plotlyOutput("traitScatterPlot")
              )
            )
          )
        )
      ),
      
      ### Crosses tab content ----
      tabItem(
        tabName = "crosses",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Previous Crosses",
        fluidRow(
          box(
            title="Previous crosses made with selected clones",
            width=12,
            actionButton(
              inputId = "makecrosses",
              label = "Get Data on Previous Crosses and Seedlots"
            ),
            p("This table shows a count of", strong("previous"), "crosses made with your selected clones. It also shows the total number of progeny and availablity of seedlots for these crosses. This table is filtered based on categorizatons made in step 5.
              If the cross was made earlier this year, the 'Progeny.Per.Cross' column will read 'None yet, new cross this year'.")
          )
        ),
        DTOutput("crossesTable")
      ),
      tabPanel(
        "Previous Reciprocal Crosses",
        fluidRow(
          box(
            title="Previous RECIPROCAL crosses made with selected clones",
            width=12,
            actionButton(
              inputId = "makerecipcrosses",
              label = "Get Data on Previous RECIPROCAL Crosses and Seedlots"
            ),
            p("This table shows a count of", strong("previous RECIPROCAL"), "crosses made with your selected clones. It also shows the total number of progeny and availablity of seedlots for these crosses. This table is filtered based on categorizatons made in step 5.
              If the cross was made earlier this year, the 'Progeny.Per.Cross' column will read 'None yet, new cross this year'.")
          )
        ), 
        DTOutput("recipCrossesTable")
      )
      )
      )
      ,

  

      
      #### Download tab content ----
      tabItem(
  tabName = "download",
  fluidRow(
    box(
      title = "Download Full Data Report",
      p("This button will allow you to download a full data report as an excel file.
        A partial download will fail, so make sure you've pulled all the inventory, pedigree, performance and cross data.
        A successful download will have a date in the file name."),
      downloadButton("downloaddata", "Download Full Data Report")
    ),
    box(
      title = "Download Optimized Crossing Plan",
      p("Click the button below to download the optimized crossing plan as an Excel file."),
      downloadButton("download_optimized_plan", "Download Optimized Crossing Plan")
    ),
    box(
      title = "Download Cubicle Management Data",
      p("Click the button below to download the current cubicle assignments and notes as an Excel file."),
      downloadButton("download_cubicle_data", "Download Cubicle Data")
    )
  )
),

      ### Cross Optimization tab content ----
      tabItem(
        tabName = "optimization",
        p("BETA implementation of", a(href="https://github.com/Resende-Lab/SimpleMating", "SimpleMating R package"), "This optimizes the midparent value of potential cross combinations based on a weighted selection index of parental BVs. Breeding values were predicted from S4 trial data and a pedigree relationship matrix. Potential crosses are culled based on pairwise-K value (where value of K is proportional to relatedness."),
        fluidRow(
          box(
            title = "Optimization Parameters",
            numericInput("n_crosses", "Number of Crosses to Select:", 10, min = 1, max = 100),
            numericInput("max_crosses_per_parent", "Max Crosses per Parent:", 3, min = 1, max = 10),

            
            sliderInput("culling_k", "Culling Pairwise K:", 
                       min = 0, max = 1, value = 0.5, step = 0.05),
            
            p(strong("Trait Weights (must sum to 1):")),
            sliderInput("brix", "Average Brix:", 
                       min = 0, max = 1, value = 0.33, step = 0.01),
            sliderInput("biomass", "Total Biomass:", 
                       min = 0, max = 1, value = 0.33, step = 0.01),
            sliderInput("ratoon", "Ratooning Ability:", 
                       min = 0, max = 1, value = 0.34, step = 0.01),
            
            textOutput("weight_sum_warning"),
            
            actionButton("run_optimization", "Run Optimization")
          ),
          box(
            title = "Optimized Crossing Plan",
            DTOutput("optimized_crosses_table")
          )
        ),
        fluidRow(
          box(
            title = "Optimization Visualization",
            plotlyOutput("optimization_plot")
          )
        )
      ),
      tabItem(
        tabName = "cubicle",
        fluidRow(
          box(
            title = "Cubicle Management",
            width = 12,
            p("Organize your optimized crosses into breeding cubicles. Each cubicle can contain up to 3 crosses with the same male parent."),
            actionButton("create_cubicle", "Create Cubicle from Selected Crosses", 
                        class = "btn btn-primary"),
            actionButton("print_layout", "Print Layout", 
                        class = "btn btn-info"),
            downloadButton("save_data", "Save Layout"),
            fileInput("load_data", "Load Layout"),
            hr(),
            dateInput("pollination_date", "Pollination Date:", value = Sys.Date()),
            dateInput("processing_date", "Processing Date:", value = Sys.Date() + 60),
            hr(),
            helpText("1. Select crosses from the table below"),
            helpText("2. Click 'Create Cubicle' to group them"),
            helpText("3. All crosses in a cubicle must share the same male parent"),
            DTOutput("crossing_table")
          )
        ),
        fluidRow(
          column(
            width = 8,
            box(
              title = "Current Cubicles",
              width = NULL,
              DTOutput("cubicle_table")
            )
          ),
          column(
            width = 4,
            box(
              title = "Statistics",
              width = NULL,
              verbatimTextOutput("statistics")
            ),
            box(
              title = "Female to Male Ratios",
              width = NULL,
              uiOutput("cubicle_ratios")
            )
          )
        )
      )
    )
  )
)

# inventory_init <<- eventReactive(input$brapipull, withProgress(message = "Pulling Inventory Data", {
#   tryCatch({
#     
#     inven <- data.frame(brapi::ba_studies_table(con = brap, studyDbId = reactive_iid(), rclass="data.frame")) %>%
#       filter(observationLevel == "plot") %>% # select just plant rows
#       set_names(~(.)%>% str_replace_all("SUGARCANE.*","") %>% str_replace_all("\\.","")) %>% # take CO term out of colnames
#       mutate_at('blockNumber', as.factor)
#     
#     inven$blockNumber<-revalue(inven$blockNumber, c("1"="West", "2"="East", "3"="Railcarts", "4"="Back"))
#     
#     inven_male<-filter(inven, grepl(reactive_date(),TasselCountMale)) %>% 
#       select(germplasmName, blockNumber, notes, TasselCountMale) %>% 
#       separate(TasselCountMale, into=c("Count",NA), sep=",") %>%
#       group_by(germplasmName)
#     
#     male<-merge(aggregate(as.numeric(Count)~germplasmName,inven_male, sum ),
#                 aggregate(blockNumber~germplasmName,inven_male, function(x) paste(unique(x), collapse=":")))
#     
#     inven_female<-filter(inven, grepl(reactive_date(),TasselCountFemale)) %>% 
#       select(germplasmName, blockNumber, notes, TasselCountFemale) %>% 
#       separate(TasselCountFemale, into=c("Count",NA), sep=",") %>%
#       group_by(germplasmName)
#     
#     female<-merge(aggregate(as.numeric(Count)~germplasmName,inven_female, sum ),
#                   aggregate(blockNumber~germplasmName,inven_female, function(x) paste0(unique(x), collapse=":")))
#     
#     colnames(male)<-colnames(female)<-c("Clone", "FlowerCount", "Location")
#     
#     inven2<-list(male, female)
#     names(inven2)<-c("male", "female")
#     
#     dataSource("Data pulled from BrAPI")
#     inven2
#   }, error = function(e) {
#     dataSource("Saved data is being rendered")
#     data.frame(Clone = character(), FloweringCount = numeric(), Location = character()) # Return an empty data frame with the expected columns
#   })
# }))



# SERVER ---------------------------------------

server <- function(input, output, session) {
  library(networkD3)
  
  # Initialize reactive values
  rv <- reactiveValues(
    optimization_result = NULL,
    previous_crosses = NULL,
    temp_selected_crosses = NULL  # For storing temporarily selected crosses
  )
  
  # Initialize optimized_crosses in the global scope
  optimized_crosses <- reactiveVal(list(crosses = data.frame(), plot = NULL))
  
  # Add these reactive values for cubicle management
  crossing_plan <- reactiveVal(NULL)
  cubicles <- reactiveVal(list())
  
  # Reactive value for selected date
  reactive_date <- reactive({
    input$date
  })

  # Reactive value for data source
  dataSource <- reactiveVal()
  
  # Reactive values for selected columns in performance tab
  rv_trait_scatter <- reactiveValues(selectedColumns = NULL)

  # Reactive value for selected clone
  selectedClone <- reactiveVal()

  selected_clone <- reactive({
    req(input$selectedClone)
    input$selectedClone
  })

  # Update selectedColumns when the user selects new columns in the Performance tab
  observeEvent(input$selectCol, {
    rv$selectedColumns <- input$phenoPick
    rv_trait_scatter$selectedColumns <- input$phenoPick
  })


  # Update selected clone when user selects a clone in pedigree tab
  observeEvent(input$selectedClone, {
    selectedClone <- input$selectedClone
    updateSelectInput(session, "pedigreeGraphUI")
  })

  # Update X-axis and Y-axis dropdowns in performance scatter plot based on selected phenotypes
  observe({
    phenotypes <- input$phenoPick

    # Update X-axis dropdown
    updateSelectInput(session, "xAxis_scatter", choices = phenotypes, selected = phenotypes[1])

    # Update Y-axis dropdown
    updateSelectInput(session, "yAxis_scatter", choices = phenotypes, selected = phenotypes[2])
  })

  # Update selected columns for scatter plot when user selects new columns
  observeEvent(input$selectCol_scatter, {
    rv_trait_scatter$xAxis <- input$xAxis_scatter
    rv_trait_scatter$yAxis <- input$yAxis_scatter
  })

  observeEvent(input$selectCol_scatter, {
    rv_trait_scatter$selectedColumns <- input$phenoPick_scatter
  })

  # Reactive value for selected location
  reactive_iid <- reactive({
    as.character(input$location)
  })
  # Reactive value for selected cross ID
  reactive_cid <- reactive({as.character(input$crossesid)})
  
  # Call the server functions from separate files
  inventory_init <- flowering_server(input, output, session, reactive_date, reactive_iid, dataSource )
  #properties_server(input, output, session, reactive_iid, inventory_init, clone_assignments)
  pedigree_server(input, output, session, reactive_iid, selectedClone, inventory_init, clone_assignments)
  performance_server(input, output, session, reactive_iid, rv, rv_trait_scatter, inventory_init, clone_assignments)
  crosses_server(input, output, session, reactive_cid, inventory_init, clone_assignments, rv)
  download_page_server(input, output, session, reactive_date)
  
  ###this has a bug 
  
  # Output for inventory pointer
  # output$inventoryPointer <- renderText({
  #   #location <- names(location_iid_map2)[location_iid_map2 == input$location]
  #   #paste("Location:", location, "-", unique(brapi::ba_studies_table(con = brap, studyDbId = input$location)$studyName))
  # })
  # Output for cross pointer
  
  output$crossPointer <- renderText({
    validate(
      need(input$crossesid != "", "Please chose a breeder login:")
    )
    crosses <- names(crosses_iid_map)[crosses_iid_map == input$crossesid]
    paste("Breeder:", crosses, "-", unique(ba_crosses_study(con = brap2, crossingProjectDbId = input$crossesid, rclass = "data.frame")$data.crossingProjectName[[1]])) #crossing project name has a breedbase bug- should return text, not number
    
    
  })

  #output for inventory date pointer 
  output$datePointer <- renderText({
    # validate(
    #   need(input$date != "", "Please chose a date")
    # )
    
    paste(as.character(input$date))
    
  })
    

#### Sorting section for inventory


output$male_parents <- renderUI({

    rank_list(
      text = "Drag male parents here",
      labels = inventory_init()$male$Clone,
      input_id = "male_list",
      options=sortable_options(group="clone_buckets"),
      orientation = "vertical",
    )
})

output$female_parents <- renderUI({
    rank_list(
      text = "Drag female parents here",
      labels = inventory_init()$female$Clone,
      input_id = "female_list",
      options=sortable_options(group="clone_buckets"),
      orientation = "vertical",
      

  )
})

#this code is useful for troubleshooting and prints to app

# output$results_1 <-
#   renderPrint(
#     unique(input$female_list) # This matches the input_id of the first rank list
#   )
# output$results_2 <-
#   renderPrint(
#     unique(input$male_list) # This matches the input_id of the second rank list
#   )





  # Cross Optimization
  observeEvent(input$run_optimization, {
    # Get current inventory data
    inventory_data <- as.data.frame(rbind(inventory_init()$male, inventory_init()$female))
    
    # Get selected parents
    male_parent_list <- c(unique(input$male_list))
    female_parent_list <- c(unique(input$female_list))
    
    ck <- input$culling_k
    
    # Check if parents are selected
    if (length(male_parent_list) == 0 || length(female_parent_list) == 0) {
      showNotification("Please select both male and female parents before running optimization.", type = "error")
      return()
    }
    
    # Run optimization
    result <- tryCatch({
      optimize_crosses(inventory_data, 
                       male_parents = male_parent_list,
                       female_parents = female_parent_list,
                       n_crosses = input$n_crosses,
                       max_crosses_per_parent = input$max_crosses_per_parent,
                       culling_k = input$culling_k,
                       blup = blup_data[,1:4],
                       amat = as.matrix(parent_amat),
                       weights = c(input$brix, input$biomass, input$ratoon))
    }, error = function(e) {
      showNotification(paste("Error in optimization:", e$message), type = "error")
      return(list(crosses = data.frame(), plot = NULL))
    })
    
    # Store the original optimization result
    rv$optimization_result <- result
    
    # Create a copy for cubicle management with additional columns
    if (!is.null(result$crosses) && nrow(result$crosses) > 0) {
      cubicle_crosses <- result$crosses
      cubicle_crosses$status <- "Unassigned"  # Add status column
      cubicle_crosses$cubicle_id <- NA        # Add cubicle_id column
      crossing_plan(cubicle_crosses)          # Initialize crossing plan with modified crosses
      
      # Show success notification
      showNotification("Optimization complete. You can now assign crosses to cubicles.", 
                      type = "message")
    }
    
    # Display optimization results table
    output$optimized_crosses_table <- renderDT({
      crosses <- result$crosses
      if (!is.null(crosses) && nrow(crosses) > 0) {
        # Join with previous crosses if available
        if (!is.null(rv$previous_crosses)) {
          crosses <- crosses %>%
            left_join(rv$previous_crosses, 
                      by = c("Female.Parent", "Male.Parent"))
        }
        
        # Add rank column
        crosses <- crosses %>%
          mutate(Rank = 1:input$n_crosses)
        
        datatable(crosses, 
                  options = list(
                    scrollX = TRUE,
                    fixedColumns = list(leftColumns = 3),
                    pageLength = 10
                  ))
      } else {
        datatable(data.frame(Message = "No crosses found or error occurred"), 
                  options = list(pageLength = 10))
      }
    })
  })
  
  # Update the optimization plot to use the original result
  output$optimization_plot <- renderPlotly({
    req(rv$optimization_result)
    plot <- rv$optimization_result$plot
    
    if (!is.null(plot)) {
      # Extract plot data and ensure it has all required columns
      plot_data <- plot$data
      
      # Add Selected column if it doesn't exist
      if (!"Selected" %in% names(plot_data)) {
        # Determine which points are selected based on the actual crosses
        selected_crosses <- rv$optimization_result$crosses
        plot_data$Selected <- FALSE
        
        # Mark points as selected if they match the optimized crosses
        for (i in 1:nrow(selected_crosses)) {
          plot_data$Selected <- plot_data$Selected | 
            (plot_data$Parent1 == selected_crosses$Female.Parent[i] & 
             plot_data$Parent2 == selected_crosses$Male.Parent[i])
        }
      }
      
      # Add hover text
      hover_text <- paste(
        "\nFemale.Parent:", plot_data$Parent1,
        "\nMale.Parent:", plot_data$Parent2,
        "\nSelection Index:", round(plot_data$Y, 3),
        "\nKinship:", round(plot_data$K, 3)
      )
      
      # Create new ggplot with hover text and vertical line
      p <- ggplot(plot_data, aes(x = K, y = Y)) +
        geom_point(aes(color = Selected), size = 3, alpha = 0.7) +
        scale_color_manual(values = c("FALSE" = "gray70", "TRUE" = "#1f77b4")) +
        geom_vline(xintercept = input$culling_k, linetype = "dashed") +
        theme_minimal() +
        labs(
          x = "Kinship Coefficient",
          y = "Selection Index",
          title = paste("Cross Optimization Plot (Selected Crosses Highlighted)"),
          color = "Selected Crosses"
        ) +
        aes(text = hover_text)
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          hoverlabel = list(bgcolor = "white"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No plot available",
          x = 0.5,
          y = 0.5,
          showarrow = FALSE
        )
    }
  })
  
  # Add weight sum warning
  output$weight_sum_warning <- renderText({
    total_weight <- input$brix + input$biomass + input$ratoon
    if (abs(total_weight - 1) > 0.01) {
      return(paste("Warning: Weights sum to", round(total_weight, 2), "- should equal 1"))
    } else {
      return(paste("Weights sum to", round(total_weight, 2)))
    }
  })
  
  output$download_optimized_plan <- downloadHandler(
    filename = function() {
      paste("optimized_crossing_plan_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Check if optimized crosses exist
      crosses <- optimized_crosses()$crosses
      if (!is.null(crosses) && nrow(crosses) > 0) {
        writexl::write_xlsx(crosses, path = file)
      } else {
        # If no optimized crosses, create a dummy dataframe with a message
        dummy_data <- data.frame(Message = "No optimized crosses available. Please run the optimization first.")
        writexl::write_xlsx(dummy_data, path = file)
      }
    }
  )
  
  # Add global error handler
  options(shiny.error = function() {
    # Log the error
    cat(file=stderr(), "Error occurred:\n")
    print(geterrmessage())
    
    # Show error to user
    showNotification(
      "An error occurred. The app will attempt to continue running.",
      type = "error",
      duration = NULL
    )
    
    # Return empty/default values instead of crashing
    return(NULL)
  })

  # Add session ended handler
  session$onSessionEnded(function() {
    cat("Session ended\n")
    # Cleanup code here if needed
  })

  # Update the crossing table render to show optimization results
  output$crossing_table <- renderDT({
    req(crossing_plan())
    crosses_data <- crossing_plan()
    
    if (is.null(crosses_data) || nrow(crosses_data) == 0) {
      return(datatable(
        data.frame(Message = "No crosses available. Please run optimization first."),
        options = list(pageLength = 10)
      ))
    }
    
    datatable(
      crosses_data,
      selection = 'multiple',
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE
      )
    )
  })

  # Create new cubicle
  observeEvent(input$create_cubicle, {
    req(crossing_plan())
    selected_rows <- input$crossing_table_rows_selected
    
    if (length(selected_rows) == 0) {
      showNotification(
        "Please select crosses first",
        type = "warning",  
        duration = 5
      )
      return()
    }
    
    selected_crosses <- crossing_plan()[selected_rows, ]
    
    # Check if all selected crosses have the same male parent
    if (length(unique(selected_crosses$Male.Parent)) > 1) {
      showNotification(
        "All selected crosses must have the same male parent",
        type = "warning",  # Changed from "error" to "warning"
        duration = 5
      )
      return()
    }
    
    # Show modal for custom cubicle ID
    showModal(modalDialog(
      title = "Create New Cubicle",
      textInput("custom_cubicle_id", "Enter Cubicle ID (optional)", 
                value = paste0("C", length(cubicles()) + 1)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_cubicle", "Create")
      )
    ))
    
    # Store selected crosses temporarily
    rv$temp_selected_crosses <- selected_crosses
  })

  # Handle cubicle creation confirmation
  observeEvent(input$confirm_cubicle, {
    req(rv$temp_selected_crosses)
    req(input$pollination_date)
    req(input$processing_date)
    req(input$custom_cubicle_id)
    
    # Create new cubicle with custom or default ID
    new_cubicle <- list(
      id = input$custom_cubicle_id,
      male = rv$temp_selected_crosses$Male.Parent[1],
      crosses = rv$temp_selected_crosses,
      pollination_date = input$pollination_date,
      processing_date = input$processing_date,
      notes = ""
    )
    
    # Update cubicles
    current_cubicles <- cubicles()
    current_cubicles[[length(current_cubicles) + 1]] <- new_cubicle
    cubicles(current_cubicles)
    
    # Update crossing plan status
    plan_data <- crossing_plan()
    selected_rows <- which(plan_data$Female.Parent %in% rv$temp_selected_crosses$Female.Parent &
                          plan_data$Male.Parent == rv$temp_selected_crosses$Male.Parent[1])
    plan_data$status[selected_rows] <- "Assigned"
    plan_data$cubicle_id[selected_rows] <- new_cubicle$id
    crossing_plan(plan_data)
    
    # Clear temporary storage
    rv$temp_selected_crosses <- NULL
    
    # Remove the modal
    removeModal()
    
    # Show success notification
    showNotification(
      "Cubicle created successfully!",
      type = "message",  # Changed from "success" to "message"
      duration = 5
    )
  })

  # Display cubicle table
  output$cubicle_table <- renderDT({
    current_cubicles <- cubicles()
    
    if (length(current_cubicles) == 0) {
      return(NULL)
    }
    
    cubicle_df <- do.call(rbind, lapply(current_cubicles, function(cubicle) {
      data.frame(
        Cubicle_ID = cubicle$id,
        Male = cubicle$male,
        Females = paste(cubicle$crosses$Female.Parent, collapse = ", "),
        Pollination_Date = format(as.Date(cubicle$pollination_date), "%Y-%m-%d"),
        Processing_Date = format(as.Date(cubicle$processing_date), "%Y-%m-%d"),
        Notes = cubicle$notes,
        stringsAsFactors = FALSE
      )
    }))
    
    datatable(
      cubicle_df,
      editable = list(target = "cell", disable = list(columns = c(1:5))),  # Only allow editing Notes column
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      )
    )
  })

  # Handle ratio displays
  output$cubicle_ratios <- renderUI({
    current_cubicles <- cubicles()
    if (length(current_cubicles) == 0) return(NULL)
    
    lapply(current_cubicles, function(cubicle) {
      female_count <- length(cubicle$crosses$Female.Parent)
      ratio <- female_count / 1  # 1 male
      
      ratio_color <- if (ratio > 3) "red" else "black"
      
      div(
        style = "margin-bottom: 10px;",
        p(
          strong("Cubicle ", cubicle$id, ": "),
          span(
            style = paste0("color: ", ratio_color, ";"),
            sprintf("Female to Male Ratio: %.1f:1", ratio)
          )
        )
      )
    })
  })

  # Enhanced statistics output
  output$statistics <- renderText({
    plan_data <- crossing_plan()
    if (is.null(plan_data)) return("No crossing plan loaded. Please run optimization first.")
    
    total_crosses <- nrow(plan_data)
    assigned_crosses <- sum(plan_data$status == "Assigned", na.rm = TRUE)
    remaining_crosses <- total_crosses - assigned_crosses
    
    current_cubicles <- cubicles()
    
    # Check if there are any cubicles before calculating counts
    if (length(current_cubicles) == 0) {
      return(paste0(
        "Total Crosses in Plan: ", total_crosses, "\n",
        "No cubicles created yet."
      ))
    }
    
    # Safely calculate counts
    male_counts <- tryCatch({
      table(sapply(current_cubicles, function(x) x$male))
    }, error = function(e) NULL)
    
    female_counts <- tryCatch({
      table(unlist(sapply(current_cubicles, function(x) x$crosses$Female.Parent)))
    }, error = function(e) NULL)
    
    # Format the output safely
    male_text <- if (!is.null(male_counts)) paste(names(male_counts), "-", male_counts, collapse = "\n") else "None"
    female_text <- if (!is.null(female_counts)) paste(names(female_counts), "-", female_counts, collapse = "\n") else "None"
    
    paste0(
      "Total Crosses in Plan: ", total_crosses, "\n",
      "Assigned to Cubicles: ", assigned_crosses, "\n",
      "Remaining to Assign: ", remaining_crosses, "\n",
      "Number of Cubicles: ", length(cubicles()), "\n",
      "Progress: ", round(assigned_crosses/total_crosses * 100, 1), "%\n\n",
      "Male Usage:\n", male_text, "\n\n",
      "Female Usage:\n", female_text
    )
  })

  # Add this observer after the cubicle_table output
  observeEvent(input$cubicle_table_cell_edit, {
    info <- input$cubicle_table_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    
    # Get current cubicles
    current_cubicles <- cubicles()
    
    # Update the notes field for the specific cubicle
    if (j == 6) {  # 6 is the Notes column
      current_cubicles[[i]]$notes <- v
      cubicles(current_cubicles)
    }
  })

  # Add this handler:
  output$download_cubicle_data <- downloadHandler(
    filename = function() {
      paste("cubicle_management_", format(Sys.Date(), "%Y%m%d"), ".xlsx", sep = "")
    },
    content = function(file) {
      current_cubicles <- cubicles()
      
      if (length(current_cubicles) == 0) {
        # Create empty dataframe with column headers if no cubicles exist
        cubicle_df <- data.frame(
          Cubicle_ID = character(),
          Male = character(),
          Females = character(),
          Pollination_Date = character(),
          Processing_Date = character(),
          Notes = character(),
          stringsAsFactors = FALSE
        )
      } else {
        # Convert cubicles data to dataframe
        cubicle_df <- do.call(rbind, lapply(current_cubicles, function(cubicle) {
          data.frame(
            Cubicle_ID = cubicle$id,
            Male = cubicle$male,
            Females = paste(cubicle$crosses$Female.Parent, collapse = ", "),
            Pollination_Date = format(as.Date(cubicle$pollination_date), "%Y-%m-%d"),
            Processing_Date = format(as.Date(cubicle$processing_date), "%Y-%m-%d"),
            Notes = cubicle$notes,
            stringsAsFactors = FALSE
          )
        }))
      }
      
      # Write to Excel file
      writexl::write_xlsx(cubicle_df, path = file)
    }
  )

  # Call optimization server
  optimization_server(input, output, session, crossing_plan, inventory_init)
  
  # Call cubicle server
  cubicle_server(input, output, session, crossing_plan)
}

# Run the Shiny app
shinyApp(ui, server) 


  