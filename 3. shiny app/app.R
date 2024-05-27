library(shiny)               # main shiny app
library(DT)                  # for the data table 
library(readr)               # reading data 
library(dplyr)               # data manipulation
library(tidyr)               # data organisation
library(ggplot2)             # plotting
library(sf)                  # for the world map plot
library(ggVennDiagram)       # for the venn diagram plot 
library(visNetwork)          # for the interactive network
library(treemap)             # for the treemap
library(d3treeR)             # to make the treemap interactive
library(shinythemes)         # getting a theme for the UI 
library(bslib)               # also for themes (bootswatch)


# themes from https://rstudio.github.io/shinythemes/  
# I like "yeti" - Daniel had "lux" 
# I'll store the name here at the top 
my_theme <- "yeti"

# Load and organise the CSV data outside the server function (for user interface)
experiments <- read_csv("data/freshwater_multistressor_experiments.csv")
experiments$System <- tolower(experiments$System)
experiments$Country <- tolower(experiments$Country)
experiments <- mutate(experiments, TaxonomicGroup = ifelse(is.na(TaxonomicGroup), "community", TaxonomicGroup))

# get the list of classes and identities from the taxonomy (for the UI)
identity_data <- experiments %>%
  select(contains("Identity")) %>%
  gather(value = "Identity")
class_data <- experiments %>%
  select(contains("Class")) %>%
  gather(value = "Class")
taxonomy <- data.frame(Class = class_data$Class, 
                       Identity = identity_data$Identity)
taxonomy <- taxonomy %>%
  filter(!is.na(Class))
classes <- sort(unique(taxonomy$Class))
identities <- sort(unique(taxonomy$Identity))



################################################################################
################################## UI## ########################################
################################################################################

# Define UI using navbarPage function - each tab is in a tabPanel function
ui <- navbarPage(
  title = tags$span("Dataset of Freshwater Multiple-Stressor Experiments", 
                    style = "font-size: 16px; font-style: italic;"),
  #position = "fixed-top",
  theme = bs_theme(bootswatch = my_theme),
  
  # prevent navigation bar from collapsing (going verticle)
  tags$style(HTML("
    /* Custom CSS to prevent navbar from collapsing */
    @media (max-width: 1200px) {
      .navbar-nav {
        flex-direction: row !important;
      }
      .navbar-collapse {
        display: flex !important;
        flex-basis: auto !important;
      }
    }
  ")),
  
  # if you click on the title you go to the about page 
  tags$head(
    tags$script(HTML('
    $(document).on("shiny:connected", function() {
      // Add click event listener to the navbar title
      $(".navbar-brand").on("click", function() {
        // Switch to the "About" tab
        $(".nav-link[data-value=About]").click();
      });
    });
  '))
  ),
  
  
  ############### landing page ################
  tabPanel("About",
    tags$div(class = "landing-wrapper",
             # child: images
             tags$div(class = "landing-block background-content",
                      tags$img(src = "james-orr-glendalough.jpg", 
                               alt = "glendalough lake, ireland", 
                               style = "opacity: 0.9; width: 100%; min-width:1200px; 
                               height: auto;"),
                      
                      tags$div(
                        style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); 
                        background-color: rgba(250, 250, 250, 0.6); padding: 10px; font-size: 13px; 
                        color: black; text-align: justify; min-width: 400px",
                        
                        p("Welcome to this interactive dataset of multiple-stressor experiments from freshwater ecosystems. 
                        The dataset contains 2,396 experiments that tested the individual and 
                        combined effects of multiple anthropogenic stressors on freshwater biodiversity."),
                        
                        p(
                          HTML("Use the <b><i>Dataset</i></b> tab to visualize, filter, and download subsets of the dataset.
           The <b><i>Taxonomy</i></b> tab contains a treemap that can be used to explore the diversity of investigated stressors of freshwater ecosystems.
           The <b><i>Co-occurrence</i></b> tab contains an interactive network visualizing the co-occurrence of investigated stressors.
           Finally, in the <b><i>Contribute</i></b> tab, you will find guidelines and instructions outlining how to add additional knowledge to the dataset.")
                        ),
                        
                        p("If you have any questions about the dataset or about reuse of the data, please contact Dr. James Orr (jaorr@tcd.ie)")),
             )
    ),
    
    tags$div(style = "font-size: 11px; color: grey",
             br(),
             p("This interactive web application was created by Dr. James Orr and Daniel Leroux Padilla with 
               support from the British Ecological Society. The dataset is the proudct of a systematic review
               conducted by James A. Orr, Samuel J. Macaulay, Adriana Mordente, Benjamin Burgess, 
               Dania Albini, Julia G. Hunn, Katherin Restrepo-Sulez, Ramesh Wilson, 
               Anne Schechner, Aoife M. Robertson, Bethany Lee, Blake Stuparyk, 
               Delezia Singh, Isobel O’Loughlin, Jeremy J. Piggott, Jiangqiu Zhu, 
               Khuong V. Dinh, Louise C. Archer, Marcin Penk, Minh Thi Thuy Vu, 
               Noël P.D. Juvigny-Kkenafou, Peiyu Zhang, Philip Sanders, Ralf B. Schäfer, 
               Rolf Vinebrooke, Sabine Hilt, Thomas Reed, and Michelle C. Jackson."),
             p("If you have questions about the interactive web application or about 
               reuse of the data please contact Dr. James Orr (jaorr@tcd.ie)")),
    
    tags$p("Photograph of Glendalough Lake, Ireland, by James Orr", 
           style = "font-size: 9px; color: grey")
    
  ),
  
  ##### underneath figure have the background be gray and include my email and "this was created by" + link to paper/github
  
  ############### Explore - filters, datatable, plots ################
  tabPanel("Dataset",
           fluidPage(tags$style(type='text/css', "
           .selectize-input { font-size: 13px; height = 13px} 
           .selectize-dropdown { font-size: 13px;}
           .selectize-control.single .selectize-input {font-size: 13px}
           .form-group, .selectize-control {margin-bottom:5px;max-height: 100px !important;}
           .box-body {padding-bottom: 0px;}"),
                     
                     fluidRow(column(12, 
                                     h5("Explore the dataset"),
                                     tags$p("In this tab you can visualize, filter, and download subsets of the dataset.
                                     The different selection boxes allow you to select features of the experiments,
                                     features of the responses measured, or features of the stressors.
                                     The data table, which shows all 2,396 experiments by default, will be updated based on your selection. 
                                     The series of plots underneath the table are also updated based on your selection.", 
                                     style = "font-size: 13px;"),
                                     
                                     h6("General guidance"),
                                     
                                     tags$ul(
                                       tags$li("If you click inside the selection boxes, then hit backspace, you can type and search for your choice rather than scrolling through the dropdown (this is particularly useful for the country and stressor identity boxes).", style = "font-size: 13px;"),
                                       tags$li("If a selection box is set to All, that filter isn't being applied.", style = "font-size: 13px;"),
                                       tags$li("When you make multiple selections, experiments will be returned that meet all conditions (i.e., first selection AND second selection, rather than first selection OR second selection).", style = "font-size: 13px;"),
                                       tags$li("Many combinations of selections will return very few or no experiments, so some of the summary figures may become difficult to interpret.", style = "font-size: 13px;")
                                     ))),
                     
                     tags$div(style = "height: 20px;"),

                     fluidRow(
                       column(4,
                              h5("Experiment Features"),
                              p("You can select experiments from specific countries or experiments that were performed in particular systems or habitats.
                              You can also select experiments that quantified recovery following the removal of stressors, or experiments that manipulated a certain number of stressors, 
                              or experiments that were fully factorial in their design. or that manipulated.",
                                style = "font-size: 11px; color: grey; text-align: justify"),
                              selectInput("country", "Country", choices = c("All", sort(unique(experiments$Country)))),
                              selectInput("system", "System", choices = c("All", sort(unique(experiments$System)))),
                              selectInput("habitat", "Habitat", choices = c("All", c("lentic", "lotic", "other", "wetland"))),
                              selectInput("recovery", "Recovery Measured", choices = c("All", c("yes", "no"))),
                              selectInput("numberOfStressors", "Number of Stressors", choices = c("All", unique(experiments$NumberOfStressors))),
                              selectInput("fullyfactorial", "Fully Factorial", choices = c("All", c("yes", "no")))),
                       
                       column(4,
                              h5("Response Features"),
                              p("You can select experiments that focused on specific taxa (or on communities of species). 
                              You can also select experiments that measured responses at a certain level of the biological hierarchy 
                              from Physiological (e.g., tissue health) to Individual (e.g., behaviour) to Population (e.g., abundance)
                              to Community (e.g., diversity) to Ecosystem (e.g., productivity).",
                                style = "font-size: 11px; color: grey; text-align: justify"),
                              selectInput("taxonomicGroup", "Taxonomic Group", choices = c("All", sort(unique(experiments$TaxonomicGroup)))),
                              selectInput("physiological", "Physiological", choices = c("All", c("yes", "no")),
                                          tags$head(tags$style(HTML(".selectize-input {height: 50px;}")))),
                              selectInput("individual", "Individual", choices = c("All", c("yes", "no"))),
                              selectInput("population", "Population", choices = c("All", c("yes", "no"))),
                              selectInput("community", "Community", choices = c("All", c("yes", "no"))),
                              selectInput("ecosystem", "Ecosystem", choices = c("All", c("yes", "no")))),


                       column(4,
                              h5("Stressor Features"),
                              p("You can select up to three stressor classes and/or three stressor identities. Explore the different identites and classes in the Taxonomy tab.
                                If you only select one stressor, all experiments that manipulated that stressor will be returned. If you make multiple selections here, 
                                experiments that tested interactions between the stressors that you selected will be returned.",
                                style = "font-size: 11px; color: grey; text-align: justify"),
                              selectInput("stressorAclass", "Stressor Class A", choices = c("All", classes)),
                              selectInput("stressorBclass", "Stressor Class B", choices = c("All", classes)),
                              selectInput("stressorCclass", "Stressor Class C", choices = c("All", classes)),
                              selectInput("stressorAidentity", "Stressor Identity A", choices = c("All", identities)),
                              selectInput("stressorBidentity", "Stressor Identity B", choices = c("All", identities)),
                              selectInput("stressorCidentity", "Stressor Identity C", choices = c("All", identities)))),
                     
                    fluidRow(column(12, actionButton("resetFiltersBtn", "Reset Filters"))),
                     
                    tags$div(style = "height: 50px;"),
                     
                    fluidRow(
                      column(12, h4("Filtered Dataset")),
                      column(12, DTOutput("table"),
                             downloadButton("downloadFilteredTable", "Download"))),
                    
                    tags$div(style = "height: 50px;"),
                    
                    # plots 
                    fluidRow(column(12, h4("Summary Figures")),
                             column(6, plotOutput("time_plot", width = "100%", height = "300px")),
                             column(6, plotOutput("stressorclasses", width = "100%", height = "300px")),
                             column(4, plotOutput("world_plot", width = "100%", height = "300px")),
                             column(4, plotOutput("venn_plot", width = "100%", height = "300px")),
                             column(4, plotOutput("taxa", width = "100%", height = "300px")),
                             column(4, plotOutput("habitat_system_plot", width = "100%", height = "300px")),
                             column(4, plotOutput("factorial_plot", width = "100%", height = "300px")),
                             column(4, plotOutput("level_plot", width = "100%", height = "300px"))),
                    )
           ),
  
  ############### Stressor Taxonomy ################
  tabPanel("Taxonomy",
    fluidPage(
             
             fluidRow(column(12, 
                             h5("Stressor Taxonomy"),
                             tags$p("Each stressor treatment in the 
                               experiments in our dataset was assigned to a stressor identity. 
                               Each stressor identity belongs to a class of stressors. 
                               These stressor classes are physical, chemical or biological in nature 
                               (or a mixture of different natures). Microplastics and nanoparticles are considered 
                               a mixture of physical and chemical stressors while cyanotoxins are considered a 
                               mixture of chemical and biological stressors. Stressors that are a combination of all 
                               three natures are classified as “composite” stressors. Here we are only showing stressor 
                                    identities that appear at least twice in the dataset.",
                                    style = "font-size: 13px;"),
                             h6("Instructions"),
                             tags$ul(
                               tags$li("Click on the treemap to take a look each category in our stressor taxonomy.", style = "font-size: 13px;"),
                               tags$li("If you hover your mouse over a cell, the outlines of the underlying sub-categories and the number of times that that stressor category appears in the dataset will be shown.", style = "font-size: 13px;"),
                               tags$li("Click on the header (Taxonomy of Stressors.) to move backwards in the treemap.", style = "font-size: 13px;")
                             ))
                      ),

             
             fluidRow(
               column(12,
                      d3tree2Output("treemap_plot", width = "100%", height=600)))
           )
  ),
             

  ############### co-occurrenc netorks ################
  tabPanel("Co-occurrence",
    sidebarLayout(
             sidebarPanel(width = 3,
               fluidRow(
                 column(12,
                        h5("Co-occurrence network"),
                        p("In this co-occurrence network each is a stressor identity and if two 
                          nodes are connected it means that there is an experiment in our dataset
                          that quantified the interaction between those two stressor identities.
                          The algorithm used to draw the network tries to place stressors that have a 
                          strong co-occurrence near each other. 
                          Colours of nodes represente their stressor nature (see Taxonomy).",
                          style = "font-size: 13px; text-align: justify"),
                        
                        p("The network can take a little bit of time to settle down. 
                          Then you can zoom in or out and pan around to explore the network.
                          If you click on a node, it's links will be highlighted in yellow.
                          If you hover over a node, only the connected nodes will remain in colour.",
                          style = "font-size: 13px; text-align: justify"),
                        
                        p("The default network (small) contains stressor identities with at least
                        15 occurrences in the dataset and should load quite quickly. You can also select 
                        a large network that contains stressor identities with at least 5 occurrences in 
                        the dataset, but this network will take longer to form and will take up more computing power.",
                          style = "font-size: 13px; text-align: justify"),
                        
                        selectInput("datasetSelector", "Select Network",
                                    choices = c("small", "large"),
                                    selected = "small")
                 )
               )
             ),
             mainPanel(width = 9,
               fluidRow(
                 column(12,
                        visNetworkOutput("force")
                 )
               )
             )
           )
  ),
  
  
  ############### Contribute ################
  tabPanel("Contribute",
    fluidPage(
             
             h5("Contribute to the dataset"),
             
             p("If you would like to report an error in the dataset or if you
               would like to contribute additional knowledge to the dataset
               you can use the following google forms:",
               style = "font-size: 13px; text-align: justify"),
             
             HTML('<ul style="list-style-type: disc; padding-left: 20px;">
               <li style="font-size: 13px;"> 
                 <a href="https://forms.gle/tLTin1m49yndQRzo6" style="text-align: justify;">Contribute to the dataset</a>
               </li>
               <li style="font-size: 13px;">
                 <a href="https://forms.gle/Tr1tcWuvHfdYNPLu8" style="text-align: justify;">Report an error</a>
               </li>
           </ul>'),
             
             p("Use the decision tree below to determine if an experiment should be included in the dataset. 
               We are particularly interested in recieving submissions of additional 
               relevant experiments that were conducted before 2022 as we are interested in 
               seeing how many experiments were missed by our approach of combining a
               very broad initial search with machine learning aided abstract screening. 
               Experiments from 2022 onwawrds would never have made it into this dataset given when
               the original literature search was performed.
               ",
               style = "font-size: 13px; text-align: justify"),
             
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 HTML('<img src="decision tree.png" 
                      alt="decision tree - Orr et al" 
                      style="width: 75%; min-width: 600px; height: auto;">')))
             )
  )
)

################################################################################
################################ SERVER ########################################
################################################################################

# Define server logic
server <- function(input, output, session) {
  

  ######################### Data Table and Plots #############################
  
  # Read experiments data and do some organising 
  experiments <- read_csv("data/freshwater_multistressor_experiments.csv")
  experiments$System <- tolower(experiments$System)
  experiments$Country <- tolower(experiments$Country)
  experiments <- mutate(experiments, TaxonomicGroup = ifelse(is.na(TaxonomicGroup), "community", TaxonomicGroup))
  

  # Co-occurrence network data
  nodes <- read_csv("nodes.csv")
  edges <- read_csv("edges.csv")
  nodes15 <- read_csv("nodes15.csv")
  edges15 <- read_csv("edges15.csv")
  
  taxa_class_file <- read_csv("taxa_class_file.csv")
  
  
  # Store the initial state of filters when the app starts
  initial_filters <- reactiveValues(
    country = "All",
    system = "All",
    habitat = "All",
    recovery = "All",
    taxonomicGroup = "All",
    physiological = "All",
    individual = "All",
    population = "All",
    community = "All",
    ecosystem = "All",
    stressorAclass = "All",
    stressorBclass = "All",
    stressorCclass = "All",
    stressorAidentity = "All",
    stressorBidentity = "All",
    stressorCidentity = "All",
    numberOfStressors = "All",
    fullyfactorial = "All",
  )
  
  # Reactive element for handling filter reset
  observeEvent(input$resetFiltersBtn, {
    # Reset each filter to its initial value
    lapply(names(initial_filters), function(filter) {
      updateSelectInput(session, filter, selected = initial_filters[[filter]])
    })
  })
  
  # Render Data Table Code
  output$table <- renderDT({
    
    # default is "experiments" 
    if (input$country == "All" && 
        input$habitat == "All" && 
        input$system == "All" && 
        input$recovery == "All" && 
        input$taxonomicGroup == "All" &&
        input$physiological == "All" &&
        input$individual == "All" &&
        input$population == "All" &&
        input$community == "All" &&
        input$ecosystem == "All" &&
        input$stressorAclass == "All" && 
        input$stressorBclass == "All" && 
        input$stressorCclass == "All" && 
        input$stressorAidentity == "All" && 
        input$stressorBidentity == "All" &&     
        input$stressorCidentity == "All" &&         
        input$numberOfStressors == "All" &&
        input$fullyfactorial == "All") {
      filtered_data <- experiments} 
    
    else {
      filtered_data <- experiments # initiate 
      
      if (input$country != "All") {
        filtered_data <- filtered_data %>% filter(Country %in% input$country)
      }
      
      if (input$habitat != "All") {
        filtered_data <- filtered_data %>% filter(Habitat %in% input$habitat)
      }
      
      if (input$system != "All") {
        filtered_data <- filtered_data %>% filter(System %in% input$system)
      }
      
      if (input$recovery != "All") {
        filtered_data <- filtered_data %>% filter(Recovery %in% input$recovery)
      }
      
      if (input$taxonomicGroup != "All") {
        filtered_data <- filtered_data %>% filter(TaxonomicGroup %in% input$taxonomicGroup)
      }
      
      if (input$physiological != "All") {
        filtered_data <- filtered_data %>% filter(Physiological %in% input$physiological)
      }
      
      if (input$individual != "All") {
        filtered_data <- filtered_data %>% filter(Individual %in% input$individual)
      }
      
      if (input$population != "All") {
        filtered_data <- filtered_data %>% filter(Population %in% input$population)
      }
      
      if (input$community != "All") {
        filtered_data <- filtered_data %>% filter(Community %in% input$community)
      }
      
      if (input$ecosystem != "All") {
        filtered_data <- filtered_data %>% filter(Ecosystem %in% input$ecosystem)
      }
      if (input$stressorAclass != "All") {
        filtered_data <- filtered_data %>%
          rowwise() %>%
          filter(any(c_across(contains("Class")) == input$stressorAclass))
      }
      if (input$stressorBclass != "All") {
        filtered_data <- filtered_data %>%
          rowwise() %>%
          filter(any(c_across(contains("Class")) == input$stressorBclass))
      }     
      if (input$stressorCclass != "All") {
        filtered_data <- filtered_data %>%
          rowwise() %>%
          filter(any(c_across(contains("Class")) == input$stressorCclass))
        
      }          
      if (input$stressorAidentity != "All") {
        filtered_data <- filtered_data %>%
          rowwise() %>%
          filter(any(c_across(contains("Identity")) == input$stressorAidentity))
      }
      if (input$stressorBidentity != "All") {
        filtered_data <- filtered_data %>%
          rowwise() %>%
          filter(any(c_across(contains("Identity")) == input$stressorBidentity))
      }  
      
      if (input$stressorCidentity != "All") {
        filtered_data <- filtered_data %>%
          rowwise() %>%
          filter(any(c_across(contains("Identity")) == input$stressorCidentity))
      }   
      
      if (input$numberOfStressors != "All") {
        filtered_data <- filtered_data %>% filter(NumberOfStressors %in% input$numberOfStressors)
      }
      if (input$fullyfactorial != "All") {
        filtered_data <- filtered_data %>% filter(FullyFactorial %in% input$fullyfactorial)
      }
    }
    
    # removing columns for the table to visualize, but not the downloaded table!
    table_data <- filtered_data %>% 
      select(-c(WOS_ID)) %>%
      select(-contains("Note"))
    
    table_data <- as.data.frame(table_data)
    
    datatable(
      table_data,
      options = list(
        pageLength = 10,
        scrollY = "300px",
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = 1, width = "150px"),   # Adjust the width of the first column
          list(targets = 2, width = "400px"),  # Adjust the width of the second column
          list(targets = 3, width = "25px"),   # Adjust the width of the third column
          list(targets = 5, width = "180px")    # Adjust the width of the fourth column
        )
      )
    )
    
  })
  
  output$downloadFilteredTable <- downloadHandler(
    filename = function() {
      paste("filtered_freshwater_multiple_stressor_experiments_orr.et.all_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data2(), file, row.names = FALSE, fileEncoding = "UTF-8")
    },
    contentType = "text/csv"
  )
  
  
  # LOGIC FOR DOWNLOADING FILTERED DATA (need to go through it all again
  filtered_data2 <- reactiveVal(experiments)
  
  observe({
    filtered_data_result <- experiments
    if (input$country != "All") {
      filtered_data_result <- filtered_data_result %>% filter(Country %in% input$country)
    }
    if (input$habitat != "All") {
      filtered_data_result <- filtered_data_result %>% filter(Habitat %in% input$habitat)
    }
    if (input$system != "All") {
      filtered_data_result <- filtered_data_result %>% filter(System %in% input$system)
    }
    if (input$recovery != "All") {
      filtered_data_result <- filtered_data_result %>% filter(Recovery %in% input$recovery)
    }    
    if (input$taxonomicGroup != "All") {
      filtered_data_result <- filtered_data_result %>% filter(TaxonomicGroup %in% input$taxonomicGroup)
    }
    if (input$physiological != "All") {
      filtered_data_result <- filtered_data_result %>% filter(Physiological %in% input$physiological)
    }
    if (input$individual != "All") {
      filtered_data_result <- filtered_data_result %>% filter(Individual %in% input$individual)
    }
    if (input$population != "All") {
      filtered_data_result <- filtered_data_result %>% filter(Population %in% input$population)
    }
    if (input$community != "All") {
      filtered_data_result <- filtered_data_result %>% filter(Community %in% input$community)
    }
    if (input$ecosystem != "All") {
      filtered_data_result <- filtered_data_result %>% filter(Ecosystem %in% input$ecosystem)
    }
    if (input$numberOfStressors != "All") {
      filtered_data_result <- filtered_data_result %>% filter(NumberOfStressors %in% input$numberOfStressors)
    }
    if (input$stressorAclass != "All") {
      filtered_data_result <- filtered_data_result %>%
        rowwise() %>%
        filter(any(c_across(contains("Class")) == input$stressorAclass))
    }
    if (input$stressorBclass != "All") {
      filtered_data_result <- filtered_data_result %>%
        rowwise() %>%
        filter(any(c_across(contains("Class")) == input$stressorBclass))
    }   
    if (input$stressorCclass != "All") {
      filtered_data_result <- filtered_data_result %>%
        rowwise() %>%
        filter(any(c_across(contains("Class")) == input$stressorCclass))
    }     
    if (input$stressorAidentity != "All") {
      filtered_data_result <- filtered_data_result %>%
        rowwise() %>%
        filter(any(c_across(contains("Identity")) == input$stressorAidentity))
    }
    if (input$stressorBidentity != "All") {
      filtered_data_result <- filtered_data_result %>%
        rowwise() %>%
        filter(any(c_across(contains("Identity")) == input$stressorBidentity))
    }    
    if (input$stressorCidentity != "All") {
      filtered_data_result <- filtered_data_result %>%
        rowwise() %>%
        filter(any(c_across(contains("Identity")) == input$stressorCidentity))
    }        
    if (input$fullyfactorial != "All") {
      filtered_data_result <- filtered_data_result %>% filter(FullyFactorial %in% input$fullyfactorial)
    }
    
    # Add more filtering logic as filters needed
    
    filtered_data2(filtered_data_result)
  })
  
  ##### we can also use the filtered_data2() reactive object for plots ######
  
  
  ################################ Venn Diagram ###############################
  
  output$venn_plot <- renderPlot({
    x <- list(
      Physiological = which(filtered_data2()$Physiological == "yes"),
      Individual = which(filtered_data2()$Individual == "yes"), 
      Population = which(filtered_data2()$Population == "yes"),
      Community = which(filtered_data2()$Community == "yes"),
      Ecosystem = which(filtered_data2()$Ecosystem == "yes")
    )
    
    ggVennDiagram(x, label = "count", label_alpha = 0, edge_size = 0,
                  label_size = 3, set_size = 3,
                  category.names = c("Physiological", "Individual", 
                                     "Population", "Community", "Ecosystem")) +
      labs(fill = "experiments", title = "Biological Responses") +
      scale_fill_gradient(low = rgb(0.8, 0.8, 0.8), 
                          high = rgb(0.4, 0.4, 0.4), trans = 'sqrt') +
      
      scale_color_manual(values = c("grey30", "grey30", 
                                    "grey30", "grey30", 
                                    "grey30"))
  })
  
  
  
  ############################# Habitat / System ###############################
  output$habitat_system_plot <- renderPlot({
    
    data <- filtered_data2()
    data$System <- tolower(data$System)
    data$Habitat <- tolower(data$Habitat)
    data$Habitat[is.na(data$Habitat)] <- "NA"
    
    data$Habitat <- factor(data$Habitat,
                                  levels = c("NA", "lentic", "lotic",
                                             "wetland", "other"))
    
    ggplot(data = data, aes(x = reorder(System, -table(System)[System]), fill = Habitat)) +
      geom_bar() +
      xlab("System") +
      ylab("Experiments") +
      labs(fill = "Habitat", title = "Type of Experiment") +
      scale_fill_manual(values = c("NA" = rgb(0.9, 0.9, 0.9), 
                                   "lentic" = rgb(0.7, 0.7, 0.7), 
                                   "lotic" = rgb(0.5, 0.5, 0.5),
                                   "wetland" = rgb(0.3, 0.3, 0.3),
                                   "other" = rgb(0.1, 0.1, 0.1))) +
      theme_minimal() +
      theme(
        legend.position = c(0.85, 0.70),  # Adjust position as needed
        legend.background = element_rect(fill = "white", color = "gray"),
        legend.box.margin = margin(6, 6, 6, 6),
        axis.text.x = element_text(size = 10),  # Adjust x-axis text size
        panel.grid.major.x = element_line(color = "gray", size = 0.2),  # Customize major grid lines
        panel.grid.minor.x = element_blank()  # Remove minor grid lines
      )
  })
  
  ############################# Taxa ###############################
  output$taxa <- renderPlot({
    
    experiments <- filtered_data2()
      
    experiments$TaxonomicGroup <- tolower(experiments$TaxonomicGroup)
    experiments$TaxonomicGroup <- sub("terrestrial plants", "other plants", experiments$TaxonomicGroup)
    
    taxa <- experiments
    
    taxa$TaxonomicGroup[is.na(taxa$TaxonomicGroup)] <- "communities"
    
    # remove groups from the plot that occur less than 5 times
    taxa <- taxa %>%
      mutate(single = ifelse(TaxonomicGroup == "communities", "no", "yes")) %>%
      group_by(TaxonomicGroup) %>%
      filter(n() >= 5) %>%
      ungroup()
    
    ggplot(data = taxa,
           aes(x = reorder(TaxonomicGroup, -table(TaxonomicGroup)[TaxonomicGroup]))) +
      geom_bar(fill = rgb(0.4, 0.4, 0.4)) +
      
      xlab("") +
      ylab("Experiments") +
      labs(fill = "Single Species", title = "Taxonomic group") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Adjust x-axis text size
        panel.grid.major.x = element_line(color = "gray", size = 0.2),  # Customize major grid lines
        panel.grid.minor.x = element_blank()  # Remove minor grid lines
      )
    
    
  })
    
  
  ############################# Timeseries ###############################
  output$time_plot <- renderPlot({
    palette <- colorRampPalette(c(rgb(0.7, 0.7, 0.7), rgb(0.4, 0.4, 0.4)))(3)
    data <- filtered_data2()
    data$System <- factor(data$System,
                                 levels = c("lab", "mesocosm", "field"))
    
    ggplot(data = data, aes(x = Year, fill = System)) +
      geom_bar() +
      scale_fill_manual(values = c("lab" = palette[1], 
                                   "mesocosm" = palette[2],
                                   "field" = palette[3])) +
      xlab("Year") +
      ylab("Experiments") +
      labs(title = "Experiments per year") +
      scale_x_continuous(breaks = seq(min(data$Year), 
                                      max(data$Year), by = 5)) +  
      theme_minimal() +
      theme(
        legend.position = c(0.7, 0.80),  # Adjust position as needed
        legend.background = element_rect(fill = "white", color = "gray"),
        legend.box.margin = margin(6, 6, 6, 6),
        axis.text.x = element_text(size = 10),  # Adjust x-axis text size
        panel.grid.major.x = element_line(color = "gray", size = 0.2),  # Customize major grid lines
        panel.grid.minor.x = element_blank()  # Remove minor grid lines
      )
  })
  
  ############################# World Map ###############################
  output$world_plot <- renderPlot({
    countries <- filtered_data2() %>%
      mutate(counts = 1) %>%
      group_by(Country) %>%
      summarise(experiments = sum(counts)) %>%
      rename(name_long = Country)
    # load country shapes
    world <- st_transform(spData::world, st_crs('ESRI:54019')) 
    # cleaning to make the dataframes consistent
    world$name_long <- tolower(world$name_long)
    countries$name_long <- tolower(countries$name_long)
    countries$name_long <-replace(countries$name_long,
                                  countries$name_long=="russia", "russian federation")
    countries$name_long <-replace(countries$name_long,
                                  countries$name_long=="the netherlands", "netherlands")
    countries$name_long <-replace(countries$name_long,
                                  countries$name_long=="uk", "united kingdom")
    countries$name_long <-replace(countries$name_long,
                                  countries$name_long=="usa", "united states")
    countries$name_long <-replace(countries$name_long,
                                  countries$name_long=="slovak republic", "slovakia")
    # join experiment data to countries 
    world <- world %>%
      left_join(countries, by = "name_long") %>%
      select(name_long, experiments)
    
    ggplot(data = world) +
      geom_sf(aes(fill = experiments), color = NA) +
      geom_sf(data = subset(world, is.na(experiments)), 
      fill = rgb(1, 1, 1), 
      color = rgb(0.8, 0.8, 0.8)) +

      scale_fill_gradient(trans = "sqrt", 
                          low = rgb(0.8, 0.8, 0.8),
                          high = rgb(0.2, 0.2, 0.2)) +
      xlab("") + 
      ylab("") +
      labs(fill = "experiments", title = "Distribution of experiments") +
      theme_minimal() +
      theme(
        legend.position = c(0.15, 0.4),  # Adjust position inside the globe
        legend.background = element_rect(fill = "white", color = "gray"),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.title = element_text()
      )
  })
  
  ######################## Factorial/Number of Stressors #######################
  output$factorial_plot <- renderPlot({
    # Filtered experiments based on user input
    filtered_data <- filtered_data2()
    
    # Create the StressorCategory variable within the reactive context
    filtered_data$StressorCategory <- ifelse(filtered_data$NumberOfStressors > 10, ">10", as.character(filtered_data$NumberOfStressors))
    filtered_data$StressorCategory <- factor(filtered_data$StressorCategory,
                                             levels = c("2", "3", "4", "5", "6", "7", 
                                                        "8", "9", "10", ">10"))
    
    ggplot(data = filtered_data, aes(x = StressorCategory, fill = FullyFactorial)) +
      geom_bar(position = "stack") +
      scale_fill_manual(breaks = c("yes", "no"),
                        values = c("yes" = rgb(0.4, 0.4, 0.4),
                                   "no" = rgb(0.8, 0.8, 0.8))) +
      xlab("Number of Stressors") +
      ylab("Experiments") +
      labs(fill = "Fully factorial", title = "Stressors per experiment") +
      theme_minimal() +
      theme(
        legend.position = c(0.75, 0.75),
        legend.background = element_rect(fill = "white", color = "gray"),
        legend.box.margin = margin(6, 6, 6, 6),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x = element_line(color = "gray", size = 0.2),
        panel.grid.minor.x = element_blank()
      )
  })
  
  ######################## Stressor Levels Plot #######################
  
  output$level_plot <- renderPlot({
    # Find variables containing the word "Levels"
    levels_columns <- grep("Levels", names(filtered_data2()), value = TRUE)
    # Extract values and combine into a single vector
    combined_levels <- unlist(filtered_data2()[levels_columns])
    # Create a dataframe for the bar plot
    data_for_bar_plot <- data.frame(Levels = combined_levels)
    # Remove NAs and turn 1s to 2s
    data_for_bar_plot <- na.omit(data_for_bar_plot)
    data_for_bar_plot$Levels <- ifelse(data_for_bar_plot$Levels == 1, 2, data_for_bar_plot$Levels)
    # turn to character and add in >10
    data_for_bar_plot$Levels <- ifelse(data_for_bar_plot$Levels > 10, ">10", as.character(data_for_bar_plot$Levels))
    data_for_bar_plot$Levels <- factor(data_for_bar_plot$Levels,
                                       levels = c("2", "3", "4", "5", "6", "7", 
                                                  "8", "9", "10", ">10"))
    # Create the bar plot
    ggplot(data = data_for_bar_plot, aes(x = Levels, )) +
      geom_bar(fill = rgb(0.4, 0.4, 0.4)) +
      xlab("Levels") +
      ylab("Stressor Treatments") +
      labs(title = "Levels per stressor treatment") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 10),  # Adjust x-axis text size
        panel.grid.major.x = element_line(color = "gray", size = 0.2),  # Customize major grid lines
        panel.grid.minor.x = element_blank()  # Remove minor grid lines
      ) 
    
  })
  
  
  ######################### Stressor Classes   ############################# 
  
  output$stressorclasses <- renderPlot({
    
    class_data <- filtered_data2() %>%
      select(contains("Class")) %>%
      gather(value = "Class") %>%
      filter(!is.na(Class))
    class_data <- class_data %>%
      group_by(Class) %>%
      summarize(Count = n())
    taxa_class_file <- read_csv("taxa_class_file.csv")
    taxa_class_file$Nature <- tolower(taxa_class_file$Nature)
    class_data <- left_join(class_data, taxa_class_file, by = "Class")
    
    class_data <- class_data[order(-class_data$Count), ]
    
    ggplot(class_data, aes(x = reorder(Class, -Count), y = Count, fill = Nature)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("physical" = "#FCE699", 
                                   "biological" = "#B4C7E7", 
                                   "chemical" = "#C5E0B4", 
                                   "physical-chemical" = "#E1E3A7",
                                   "biological-chemical" = "#BDD3CF", 
                                   "composite" = "#E4E4E4")) +
      labs(title = "Stressor classes",
           x = "",
           y = "Stressor Treatments") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  })
  
  
  ########################### Taxonomy ###############################

  # organise the data 
  experiments2 <- read_csv("data/freshwater_multistressor_experiments.csv")
  identity_data <- experiments2 %>%
    select(contains("Identity")) %>%
    gather(value = "Identity")
  class_data <- experiments2 %>%
    select(contains("Class")) %>%
    gather(value = "Class")
  taxonomy <- data.frame(Class = class_data$Class, 
                         Identity = identity_data$Identity)
  rm(class_data, identity_data)
  taxonomy <- taxonomy %>%
    filter(!is.na(Class))
  taxa_class_file <- read_csv("taxa_class_file.csv")
  taxa_class_file$Nature <- tolower(taxa_class_file$Nature)
  taxonomy <- taxonomy %>%
    group_by(Class, Identity) %>%
    summarize(Count = n()) %>%
    filter(Count >= 2)
  taxa_treemap_data <- left_join(taxonomy, taxa_class_file, by = "Class") %>%
    filter(!is.na(Identity))
  
  # plot the treempa 
  output$treemap_plot <- renderD3tree2({
    inter <- d3tree2(treemap(taxa_treemap_data,
                             index=c("Nature","Class","Identity"),
                             vSize="Count",
                             type="index",
                             palette = c("#B4C7E7", "#BDD3CF", "#C5E0B4", "#E4E4E4", "#FCE699", "#E1E3A7"),
                             drop.unused.levels=TRUE,
                             overlap.labels = 0,
                             fontsize.labels = 13),
                     rootname = "Taxonomy of Stressors")
  })
  
  
  
  
  
  ########################### Network ###############################
  
  output$force <- renderVisNetwork({
    # Check the selected dataset
    if (input$datasetSelector == "large") {
      
      # Dataset 1
      nodes2 <- nodes
      edges2 <- edges
      # Select columns and rename them
      nodes2 <- nodes2 %>%
        select(id = NumericID, label = Node, group = Nature, value = TotalWeight)
      edges3 <- edges2 %>%
        select(from = source, to = target)
      # Convert 'id' column in nodes2 to integer
      nodes2$id <- as.integer(nodes2$id)
      
      # Convert 'from' and 'to' columns in edges3 to integer
      edges3$from <- as.integer(edges3$from)
      edges3$to <- as.integer(edges3$to)
      
      edges3$from <- edges3$from + 1
      edges3$to <- edges3$to + 1
      # Dataset 1
      visNetwork(nodes2, edges3) %>% 
        visPhysics(
          solver = "barnesHut",
          maxVelocity = 5,
          minVelocity = 1,
          timestep = 0.5,
          stabilization = list(enabled = TRUE, iterations = 20),
          adaptiveTimestep = TRUE
        ) %>%
        visNodes(
          size = nodes2$value)%>%
        visGroups(groupname = "Physical", color = "#E3CF8AFF") %>%
        visGroups(groupname = "Chemical", color = "#B1CAA2FF") %>%
        visGroups(groupname = "Biological", color = "#A2B3D0FF") %>% 
        visGroups(groupname = "Phys/Chem", color = "#E1E3A7") %>% 
        visGroups(groupname = "BioChem", color = "#BDD3CF") %>%
        visGroups(groupname = "Mixed", color = "#E4E4E4") %>% 
        visEdges(color = list(color = "#EBEBEBFF", highlight = "yellow")) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                   height = "900px")
      
    } else if (input$datasetSelector == "small") {
      # Dataset 2 (customize this block for your second dataset)
      nodes2 <- nodes15
      edges2 <- edges15
      # Select columns and rename them
      nodes2 <- nodes2 %>%
        select(id = NumericID, label = Node, group = Nature, value = Size)
      edges3 <- edges2 %>%
        select(from = Source, to = Target)
      # Convert 'id' column in nodes2 to integer
      nodes2$id <- as.integer(nodes2$id)
      
      # Convert 'from' and 'to' columns in edges3 to integer
      edges3$from <- as.integer(edges3$from)
      edges3$to <- as.integer(edges3$to)
      
      edges3$from <- edges3$from + 1
      edges3$to <- edges3$to + 1
      
      # Dataset 1
      visNetwork(nodes2, edges3) %>% 
        visPhysics(
          solver = "barnesHut",
          maxVelocity = 2,
          minVelocity = 1,
          timestep = 0.5,
          stabilization = list(enabled = TRUE, iterations = 20),
          adaptiveTimestep = TRUE
        ) %>%
        visNodes(
          size = nodes2$value)%>%
        visGroups(groupname = "Physical", color = "#E3CF8AFF") %>%
        visGroups(groupname = "Chemical", color = "#B1CAA2FF") %>%
        visGroups(groupname = "Biological", color = "#A2B3D0FF") %>% 
        visGroups(groupname = "Phys/Chem", color = "#E1E3A7") %>% 
        visGroups(groupname = "BioChem", color = "#BDD3CF") %>%
        visEdges(color = list(color = "#EBEBEBFF", highlight = "yellow")) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                   height = "900px")      
      
    } 
  })
  
  
}

# Run the Shiny app
shinyApp(ui, server)

