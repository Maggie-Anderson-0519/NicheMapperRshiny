# Filename: TNC_RShiny_GitHub_current
# Author: Maggie Anderson
# Last edited: 04/06/2023
# Version: pushed to GitHub

# Capabilities: app includes 
    # project description
    # radio buttons for selecting species by ecoregion (simplified)
    # ecoregions simplified to dry/mesic/wet prairie and savanna
    # bin-style drag and drop for adding/subtracting more species 
    # selection for latin vs. common names
    # drag and drop for adding dominant/invasive species 
    # interactive plotly niche graphic
    # drop-down for selecting species and visualizing pairwise distances
    # highlighted bucket for analysis species
    # change competitiveness to only consider closest 5-ish species
    # add loading circle/progress bar

# Helpful for constructing shiny apps
    # widgets gallery: shinyWidgets::shinyWidgetsGallery()
    # remotes::install_github("coolbutuseless/ggpattern", force = TRUE)

# Load libraries
library(shiny) # For shiny setup
library(shinydashboard)
library(shinyWidgets) # radioGroupButtons
library(plotly) # plotly interactive plots
library(dplyr) # mutate_if() and data wrangling
library(sortable) # bucket_list
library(stringr) # str_detect and metaMDS
library(vegan) # diversity indices)
library(data.table) # For data.table code in processing
library(betapart) # For beta.pair.abund
library(adespatial) # For beta.div.comp
library(tibble)  # columns to rownames
library(ggplot2)  # plotting
library(ggthemes) #ggthemes, # nice-looking plots
library(ggnewscale) # multiple color palettes
library(factoextra) # dendrogram
library(shinycssloaders) # loading bar
library(labdsv) # pairwise calculations (bray-curtis) 


# Load in data
m.sps.means <- read.csv("m.sps.means2.csv") # dataset of all possible species + MEAN trait information

# Load M and quick bug fix
m <- read.csv("m_1219.csv") # dataset of all possible species, without trait averages
m$MgmtTarget <- ifelse(m$MgmtTarget == 1, "concern","no concern") # alter 
m$MgmtTarget <- as.factor(m$MgmtTarget)

d <- read.csv("d.csv") # dissimilarity matrix for all species
param <- d[1:length(unique(d$X))]

# Set up user interface
################################################################################
###################################### UI ######################################
################################################################################
Header <- dashboardHeader(title = "TNC NicheMapper", titleWidth = "200") # add header 

Sidebar <- dashboardSidebar( # sidebar, appears on the left of the dashboard
  width = 200,
  sidebarMenu(
    menuItem("How to use this app", tabName = "info", icon = icon("info")), 
    menuItem("Setup", icon = icon("table"), tabName = "setup"), 
    menuItem("Output", icon = icon("signal"), tabName = "output",
             badgeLabel = "plots here", badgeColor = "green") 
  )
  
)

Body <- dashboardBody(
  fluidPage(
    tabItems(
      
      # TAB 1: Info (How to Use This App) Tab ----------------------------------
      tabItem(tabName = "info",
              
              # Changes color of the text "NiceMapper" 
              tags$style(HTML(".help-block {color: black;}")),
              
              fluidRow( # "about this app" text
                column(7,
                       h3("About NicheMapper"),
                       helpText(strong("NicheMapper"),
                                "allows you to visualize how North American grassland plant
                                species may interact based on their",em("niches"), "or the unique roles that 
                                each plant plays in each ecosystem based on their individual traits."),
                       br(),
                       h4(strong(em("Why do niches matter?"))),
                       helpText("Plants differ in how they photosynthesize, use nitrogen, and many other such traits.
                                These traits all come together to describe a species' unique niche in the environment."),
                       helpText("In this case, similar/overlapping niches indicate that plants are more similar in terms of their traits",em("functionally redundant,"),
                                "while larger/smaller niches indicate that a plant
                                may be more/less adaptable to its environment.")
                ),
                
                # This section adds an image from the images folder titled www
                column(5,
                       tags$img(src = "niches_demo.png", width = 400, height = 350) 
                ),
              ),
              fluidRow( # instructions
                column(7,
                       h3("How to use this app"),
                       h4(strong("1. Build a species list")),
                       helpText("Start by modifying existing species lists by ",
                                em("ecoregion")," or building your own list from
                                scratch by selecting species based on their",em("traits"))
                )
              ),
              fluidRow(
                column(5,
                       br(), # image of demo button
                       tags$img(src = "button_demo1.png", width = 350, height = 35) 
                )
              ),
              fluidRow( # help text
                column(4,
                       helpText(em("Note that you may have to then select the ecoregion in which you are interested")),
                       helpText("Then use the drag & drop feature to move species to and from the",
                                strong("SPECIES FOR ANALYSIS"),
                                "column. This list will be analyzed in Step 2")
                ),
                column(8,
                       tags$img(src = "dragdrop_demo1.png", width = 550, height = 125) 
                )
              ),
              fluidRow(
                column(12,
                       h4(strong("2. Select 'GO'...and maybe go get some coffee  ",
                                 icon("mug-hot", style = "color: darkgrey"))),
                       h2(" ",icon("clock", style = "color: black")),
                       helpText("The analysis is calculating",
                                em("exactly how similar every single species is to all other species"),
                                "and usually takes anywhere from ",
                                strong("30 seconds "), "(5-15 species) to ", strong("3 minutes" ),
                                "(50+ species)")
                )
              )
      ),
      
      # TAB 2: Setup Tab -------------------------------------------------------
      tabItem(tabName = "setup",
              fluid = TRUE,
              
              # Top Row with Instructions and Buttons 
              fluidRow(
                column(5,
                       h3("STEP 1: Create your community of species to analyze"),
                       h5("Seed mixes are often assembled based on prairie ecoregion (i.e., wet, mesic, or dry prairie),
                           so you may opt to select", strong("choose species by ecoregion")," and start by selecting a subset
                           of species that normally grow together. You may also opt to", strong("choose species by traits"),
                          "which allows you to buid your list by selecting species based on their characteristics (i.e., grass
                           vs. forb)"),
                       
                       # These radio buttons allow you to either select species by ecoregion or build your own list
                       radioGroupButtons(
                         inputId = "tabChoice",
                         label = "Choose species",
                         choices = c("Build your own list","By Ecoregion"), 
                         justified = TRUE,
                         checkIcon = list(yes = icon("ok", lib = "glyphicon")) 
                       )
                ),
                column(1), # break 
                column(6,
                       h3("STEP 2: When finished selecting species (below) click 'GO'"),
                       h2("  "),
                       actionButton(inputId = 'GoButton', # Go button initiates analyses in the server
                                    'GO',
                                    style="color: #fff;
                                        background-color: #337ab7;
                                        border-color: #2e6da4; #227b4b
                                        padding:20px;
                                        font-size:150%"),
                       br(), # line break
                       h5("This analysis may take a few minutes. Results will appear in the ",
                          strong("Output")," tab (left sidebar) when finished"
                       )
                  ) 
              ),
              
              # Second Row with Ecoregion and species buttons
              fluidRow(
                tags$style(
                  HTML( # this is just some .html code for aesthetics
                    "{.checkbox, .radio {
                      position: relative;
                      display: block;
                      margin-top: 5px;
                      margin-bottom: 5px
                            px
                            ;
                            }")
                ),
                
                # Ecoregion Conditional Panel based on species input selection 
                conditionalPanel(
                  condition = "input.tabChoice == 'By Ecoregion'",
                  # Row for Input Selections 
                  fluidRow(
                    
                    # Ecoregion Inputs 
                    column(6,
                           radioGroupButtons(
                             inputId = "radio",
                             label = h5(strong("Select by ecoregion"), em("(list updates below)")),
                             
                             choices = c("Wet Prairie"   = "Wet.Prairie", 
                                         "Dry Prairie"   = "Dry.Prairie",
                                         "Mesic Prairie" = "Mesic.Prairie",
                                         "Savanna"       = "Savanna"),
                             
                             # If selected show green check, if not show empty box 
                             checkIcon = list(
                               yes = tags$i(class = "fa fa-check-square",
                                            style = "color: green"),
                               no = tags$i(class = "fa fa-square-o",
                                           style = "color: green"))
                           )
                    ),
                    
                    # Species Name Inputs
                    column(6,
                           radioGroupButtons(
                             inputId = "radio_sps",
                             label = h5(strong("Display species by")),
                             
                             choices = c("Latin name"  = "Taxon",
                                         "Common name" = "species"),
                             selected = "species",
                             
                             # If selected show green check, if not show empty box 
                             checkIcon = list(
                               yes = tags$i(class = "fa fa-check-square",
                                            style = "color: darkgrey"),
                               no = tags$i(class = "fa fa-square-o",
                                           style = "color: darkgrey"))
                           )
                    ) 
                  ),
                  
                  # Row for drag and drop buckets 
                  fluidRow(
                    column(12,
                           tags$style(
                             HTML("#green {
                             background-color: #53C1BE;
                             }
                             .default-sortable .rank-list-container .rank-list-item {
                             padding: 0;
                             }
                             .rank-list-item > div {
                             line-height:42px;
                             }
                             
                             .rank-list-container.default-sortable.column_2 {
                             background-color: darkgrey
                             }
                             
                             .rank-list-container.default-sortable.column_4 {
                             background-color: lightgrey
                             }
                             ")
                           ),
                           
                           uiOutput("bucketListUI"))) # render reactive output variable
                ), # End of Ecoregion Conditional Panel 
                
                # Traits Conditional Panel
                conditionalPanel(
                  condition = "input.tabChoice == 'Build your own list'",
                  
                  # Row for Input Selections: taxonomic vs. latin name
                  fluidRow(
                    column(6,
                           radioGroupButtons(
                             inputId = "radio_sps2",
                             label = h5(strong("Display species by")),
                             
                             choices = c("Latin name"  = "Taxon",
                                         "Common name" = "species"),
                             selected = "species",
                             
                             checkIcon = list(
                               yes = tags$i(class = "fa fa-check-square",
                                            style = "color: darkgrey"),
                               no = tags$i(class = "fa fa-square-o",
                                           style = "color: darkgrey")
                             )
                           )
                    )
                  ),
                  
                  # Row for Drag and Drop 
                  fluidRow(
                    column(12,
                           tags$style(
                             HTML("#green {
                                  background-color: #53C1BE;
                                  }
                                  .default-sortable .rank-list-container .rank-list-item {
                                  padding: 1px;
                                  font-size: 10px;
                                  }
                                  .rank-list-item > div {
                                  line-height:42px;
                                  }
                                  
                                  .rank-list-container.default-sortable.column_2 {
                                  background-color: darkgrey
                                  }
                                 
                                  .rank-list-container.default-sortable.column_4 {
                                  background-color: lightgrey
                                  }
                                 
                                  .default-sortable.rank-list-container {
                                  flex: 1 0 100px;
                                  background-color: transparent;
                                  border: 1px solid black;
                                  padding: 5px;
                                  margin: 5px;
                                  display: flex;
                                  flex-flow: column nowrap;
                                  }
                                  ")
                           ),
                           uiOutput("bucketListTraitUI")
                    )
                  )
                ) # End of Traits Conditional Panel
              ) # End of second Setup Row 
      ), # End of setup tab 
      
      # Output tab -------------------------------------------------------------
      tabItem(tabName = "output", 
              fluid = TRUE,
              fluidRow(
                column(12,
                       h6(em("Note that graphs could take 1-2 minutes to appear")),
                       h2(em("How similar are the species in my selected mix?")),
                       h4("Below is a", strong(" Niche map")," of all your chosen species. 
                          By plotting species' niches in trait-space, its true that",strong(em(" closer/overlapping species are more similar to each other")),
                          " whereas far-away/isolated species have more unique traits"),
                       br(),
                       plotlyOutput("nmdsPlot", width = "1000px", height = "600px") %>% withSpinner(color="green"),
                       h6(em("Grey plot labels indicate which traits are ",em("pulling "), "a species in a given direction. For example, plants clustered by the `C4` label are more likely to be C4 grasses")),
                       h6(em("Note that the graph's axes are meaningless, since each species is plotted in terms of multiple traits at once")),
                       br(),
                       br(),
                       br(),
                       br(),
                       br()
                ),
                column(3,
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       radioButtons(inputId = "radioTraits", # change graph by traits
                                    h4("Distinguish species by their traits"),
                                    choices = c("Photosynthesis (C3 vs. C4)"                 = "Photosynthetic.pathway",
                                                "Life form (forbs, grasses & shrubs)"        = "Life.form",
                                                "N-fixation ability (legume vs. non-legume)" = "Nitrogen.fixation.capacity",
                                                "Early blooming"                             = "blooming.period..early",
                                                "Early/mid-season blooming"                  = "blooming.period..early.mid",
                                                "Mid-season blooming"                        = "blooming.period...mid",
                                                "Mid & late-season blooming"                 = "blooming.period..mid.late",
                                                "Late blooming"                              = "blooming.period..late",
                                                "Dominant or invasive species"               = "MgmtTarget"), #"None" = NULL
                                    selected = "Life.form"), 
                       br(),
                       br()
                ),
                column(9,
                       br(),
                       br(),
                       h2(em("Let's break down the graph above by species' characteristics")),
                       h5(em("Black and grey groups reflect the different values of selected categories (left). Note that red groups do not yet have complete trait information.")),
                       plotlyOutput("nmdsPlot2"),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br()
                ),
              ),
              fluidRow(
                column(12, 
                       h2(em("Species most likely to be competitive in selected mix")),
                       h5(em("Species with higher scores are more likely to survive")),
                       dataTableOutput('table'),
                       br(),
                       h2(em("Species grouped by similarity")),
                       h4(em("You can adjust the number of clusters to visualize which species are most related to each other functionally"))
                ),
                column(3,
                       sliderInput("sliderK", label = h3("Adjust number of groups"), min = 2, 
                                   max = 10, value = 6)
                ),
                column(9,
                       plotOutput("dendPlot", width = "1000px", height = "650px")),
                column(12,
                       #height = 10,
                       h2(em("How similar are species to each other?")),
                       h4("Visualize how similar/dissimilar selected species are from the rest of the community"),
                       h4("Your selected species is at the", strong( "bottom-left"), ". The species to its right is most similar while the species in the ", strong( "top-right"), " is least similar.")
                ),
                column( 
                  tags$style(
                    HTML(".sidebar {
                         height: 90vh; overflow-y: auto;
                         }"
                    )
                  ),
                  tags$b(" "),
                  width = 12,
                  uiOutput("dropdownUI") # render reactive output variable
                )
              ),
              
              fluidRow(
                column(12,
                       uiOutput("parameterUI"),
                       plotlyOutput("newPlot"),
                       h4("Use your curser to pan and zoom in on the graph above"),
                       br(),
                       br()
                ) 
              ) 
            ) 
          ) 
        ) 
      ) 

ui <- dashboardPage(Header,
                    skin = "green",
                    Sidebar,
                    Body)

################################################################################
#################################### Server ####################################
################################################################################
server <- function(input, output) {
  
  
  # reactive values for ecoregion-based species selection
  rv <- reactiveValues(subm.list = NULL, # create empty object to house reactive values?
                       antim.list = NULL,
                       mgmtm.list = NULL) # can store lots of things in this
  
  # reactive values for trait-based selection
  rv_t <- reactiveValues(forb_fern.list = NULL,
                         grass_sedge.list = NULL,
                         mgmtm_trait.list = NULL)
  
  # Populate the bucket-lists with default values 
  # Observe event is for all radio buttons and buckets in the "setup" tab, meaning that any time one of these is changed, it erases any changes in buckets
  observe({
    # reactive values for ecoregion-based list
    print('')
    print('START')
    print(input$radio)
    print(input$radio_sps)
    
    # A BUNCH of data wrangling is below
    # Subset df by ecoregion (input$radio) and either species or taxon (input$radio_sps)
    subm <- m %>%
      filter(get(input$radio) == 1) %>% # subset based on radio button input for ecoregion?
      distinct(sort(get(input$radio_sps))) %>%
      mutate_if(is.factor, as.character) %>%
      dplyr::rename(new_name = 1)
    
    # Create list of species for analysis 
    rv$subm.list <- reactiveVal(as.list(subm$`new_name`)) # convert to list (reactive value), reference in rv
    
    # find all taxa that are not in the subsetted dataframe via antijoin
    antim <- m %>%
      filter(get(input$radio) == 0) %>% # could also change to 1 and use antijoin below
      distinct(sort(get(input$radio_sps))) %>%
      mutate_if(is.factor, as.character) %>%
      dplyr::rename(new_name = 1)
    
    rv$antim.list <- reactiveVal(as.list(antim$new_name)) # reference
    
    mgmtm <- m %>%
      filter(MgmtTarget == "concern") %>%
      distinct(sort(get(input$radio_sps))) %>% 
      dplyr::rename(new_name = 1)
    
    rv$mgmtm.list <- reactiveVal(as.list(mgmtm$new_name)) # reference
    
    # reactive values list for trait-based selection 
    forb_fern <- m %>%
      filter(Forbs..Ferns.and.Fern.Allies == 1) %>% 
      distinct(sort(get(input$radio_sps2))) %>%
      mutate_if(is.factor, as.character) %>%
      rename(new_name = 1)
    
    rv_t$forb_fern.list <- reactiveVal(as.list(forb_fern$`new_name`))
    
    grass_sedge <- m %>%
      filter(Grasses.and.sedges == 1) %>% 
      distinct(sort(get(input$radio_sps2))) %>% 
      mutate_if(is.factor, as.character) %>%
      dplyr::rename(new_name = 1)
    
    rv_t$grass_sedge.list <- reactiveVal(as.list(grass_sedge$`new_name`))
    
    mgmtm_trait <- m %>%
      filter(MgmtTarget == "concern") %>% 
      distinct(sort(get(input$radio_sps2))) %>% 
      mutate_if(is.factor, as.character) %>%
      dplyr::rename(new_name = 1)
    
    rv_t$mgmtm_trait.list <- reactiveVal(as.list(mgmtm_trait$`new_name`))
    
  }) # end data wrangling for now
  
  # Species list for ecoregion-based selection
  output$bucketListUI <- renderUI({ # UI with options that change based on other RVs/inputs
    
    bucket_list(
      header = "Drag species to and from the SPECIES FOR ANALYSIS column (which will be analyzed)", #Select species for analysis (middle) using the drag-and-drop feature
      group_name = "bucket_list_group",
      orientation = "horizontal",
      add_rank_list(
        text = "Drag from here to ADD species",
        labels = rv$antim.list(), # referencing list
        input_id = "rank_list_1"
      ),
      add_rank_list(
        text = strong("SPECIES FOR ANALYSIS"),
        labels = rv$subm.list(),
        input_id = "rank_list_2"
      ),
      add_rank_list(
        text = "Drag to here to REMOVE species",
        labels = NULL,
        input_id = "rank_list_3"
      ),
      add_rank_list(
        text = "Species of managment concern (optional)",
        labels = rv$mgmtm.list(),
        input_id = "rank_list_4"
      )
    )
  })
  
  # Species list for trait-based selection
  output$bucketListTraitUI <- renderUI({ # UI with options that change based on other RVs/inputs
    
    bucket_list(
      header = "Drag species from trait groups to SPECIES FOR ANALYSIS box", #Select species for analysis (middle) using the drag-and-drop feature
      group_name = "bucket_list_group",
      orientation = "horizontal",
      add_rank_list(
        text = "Forbs and ferns",
        labels = rv_t$forb_fern.list(), # referencing list. 
        input_id = "rank_list_1_t"
      ),
      add_rank_list(
        text = strong("SPECIES FOR ANALYSIS"),
        labels = NULL,
        input_id = "rank_list_2_t"
      ),
      add_rank_list(
        text = "Grasses and sedges",
        labels = rv_t$grass_sedge.list(),
        input_id = "rank_list_3_t" 
      ),
      # add shrubs!
      add_rank_list(
        text = "Species of managment concern",
        labels = rv_t$mgmtm_trait.list(),
        input_id = "rank_list_4_t"
      )
    )
  })
  
  # Calculating Please Wait message when button pressed
  # Helpful hint: Check out "shinybusy::show_modal_spinner" for spinners that block the whole screen and prevent other app edits until the process is complete
  observeEvent(input$GoButton, {
    showModal(modalDialog(
      title = "Analyzing your species --please wait",
      "Analyses will take", strong("1 - 2 minutes"), "depending on the number of species added. Refer to the 'Output' tab for results.",
      easyClose = TRUE
    ))
  })
  
  # Observe Event is only triggered/executed when the event occurs
  observeEvent(input$GoButton,{

    print('Starting GO')
    
    if (input$tabChoice == "By Ecoregion") {
      actualRankList2 <- input$rank_list_2 
      inputLatinCommon <- input$radio_sps
    } else if (input$tabChoice == "Build your own list") {
      actualRankList2 <- input$rank_list_2_t
      inputLatinCommon <- input$radio_sps2
    } else {
      actualRankList2 <- NULL
    }
    
    # get species from m present in rank_list_2
    subm <- m %>%
      dplyr::filter(str_detect(get(inputLatinCommon), paste(actualRankList2, collapse="|"))) %>%
      dplyr::mutate_if(is.integer, as.numeric) #%>% rename(Taxon = 2) # renames whatever the chosen sps category was to "Taxon" (for consistency downstream)
    
    print("subm")
    print(head(subm))
    
    # TEST 1 
    x <- NULL
    makeReactiveBinding("x")
    
    ################################################################################   
    ############################### NMDS calculation ###############################
    print("NMDS calc. start")
    nmds_results <- metaMDS(comm = subm[,c(3:11,17:21,32:37)],   # Define the community data 
                            distance = "bray",       # Specify a bray-curtis distance
                            try = 100,
                            k=8)               # Number of iterations 
    # data wrangling 2
    # groups
    treat <- subm %>%
      dplyr::select(paste(inputLatinCommon)) # prev. input$radio_sps
    
    treat.df <- data.frame(treat) # convert to dataframe
    
    treat.df <- treat.df %>% 
      dplyr::group_by(paste(inputLatinCommon)) %>% # prev. input$radio_sps
      dplyr::mutate(Color = randomcoloR::randomColor()) %>%
      dplyr::ungroup()
    
    # First create a data frame of the scores from the individual sites.
    # This data frame will contain x and y values for where sites are located
    data_scores <- as.data.frame(scores(nmds_results)$sites)
    
    # Now add the extra species column
    groups <- treat 
    groups <- as.factor(groups[,c(1)])
    data_scores <- cbind(data_scores, groups) # use treatment column
    
    # Next, we can add the scores for species data
    species_scores <- as.data.frame(scores(nmds_results, "species"))
    
    # Add a column equivalent to the row name to create species labels
    species_scores$species <- rownames(species_scores)
    
    # You have to set the seed before you generate random data, not after
    set.seed(1) 
    # Here is where the magic happens:
    data_scores <- data.table(data_scores) # from data.table package
    
    # bug fix?
    m.sps.means <- m.sps.means %>%
      as.data.frame() 
    
    subm <- as.data.frame(subm)
    
    # Now add the extra Treatment column
    data_scores <- data_scores %>% 
      as.data.frame
    # debug?
    m.sps.means <- m.sps.means %>% 
      mutate_if(is.character, as.factor)
    
    join_var <- input$radio_sps2
    
    hulls <- left_join(data_scores, m.sps.means[,-c(9:12)], by = c("groups" = join_var)) 
    hulls <- distinct(hulls) # NEW
    print(head(hulls))
    
    hulls <- hulls %>% 
      mutate_if(is.character, as.factor)
    
    # end data wrangling 2
    ##### end NMDS calc. #####
    
    ##### start LCBD/pairwise dissimilarity calcs #####
    print("start LCBD/pairwise dissimilarity")
    
    treat <- treat %>% 
      mutate_if(is.character, as.factor) # debug
    g <- treat[,c(1)] # grouping variable
    
    dist<-beta.pair.abund(subm[,c(3:11,17:21,32:37)],index.family="bray")
    bd <- betadisper(dist[[3]],g) # ordination plot object with coords
    
    # beta diversity
    out.comp = beta.div.comp(subm[,c(3:11,17:21,32:37)], coef="S", quant=TRUE) # LCBD calcs: type out.sp.XXX$beta for values
    out.sp.D = LCBD.comp(out.comp$D, sqrt.D=TRUE) 
    
    centroid <- bd$centroids[,c(1:2)] # may need to be flipped using t() if there are too little/many groups
    centroid <- as.data.frame(centroid)
    
    dist.centroid <- dist(centroid, diag=T, upper=T)
    
    # BT Combined to one line
    d <- as.data.frame(as.matrix(dist.centroid)) %>% mutate(X = rownames(.)) # dissimilarity matrix for all centroids
    # distance matrix = d
    
    # reactive drop-down thingy
    print('Parameter UI')
    
    output$parameterUI <- renderUI({
      param <- unique(d$X) 
      param <- param[1:length(unique(d$X))]
      print("print param object")
      print(param)
      
      selectInput(inputId = "parameter",
                  label = "Select species:",
                  param)
    })

    
    
    dis <- dsvdis(subm[,c(3:11,17:21,27:32)],'bray/curtis') # creates a Bray/Curtis dissimilarity matrix
    
    # # this is a distance object that you could do any number of things with
    pco <- pco(dis,2) # produces a two-dimensional Principal Coordinates Ordination object
    distmat <- as.matrix(orddist(pco,dim=2)) # dissimilarity matrix for all species locations (not centroids)
    
    
    ##### start calculating species-specific dissimilarity #####
    
    hulls.centroids <- hulls %>%
      dplyr::group_by(groups, MgmtTarget) %>% # Taxon, species,
      summarize(NMDS1.centroid = mean(NMDS1),
                NMDS2.centroid = mean(NMDS2)) %>%
      remove_rownames %>% 
      column_to_rownames(var="groups") # or "Taxon"
    
    # find centroid for ALL points
    center <- hulls %>%
      summarise(NMDS1.center=mean(NMDS1),
                NMDS2.center=mean(NMDS2))
    
    # add distance of hull centroid to center of ordination space to dataframe
    hulls.centroids <- hulls.centroids %>%
      mutate(dist.to.center = sqrt(((abs(NMDS1.centroid)-abs(center$NMDS1.center))^2)+((abs(NMDS2.centroid)-abs(center$NMDS2.center))^2)))
    
    # convert to distance object 
    dist.hulls.centroids <- dist(hulls.centroids, diag=T, upper=T)
    
    d.nmds <- as.data.frame(as.matrix(dist.hulls.centroids)) # dissimilarity matrix for all
    
    # make new dataframe with summed distances to all other centroids
    d.nmds.sums <- d.nmds %>%
      mutate(groups = rownames(.)) %>% # Taxon = rownames(.)
      group_by(groups) %>% #Taxon
      summarise(dist.sums = rowSums(across(where(is.numeric)))) %>%
      as.data.frame() %>%
      dplyr::rename(Competitiveness = dist.sums) %>% 
      dplyr::rename(Species = groups) %>%
      dplyr::arrange(-Competitiveness)
    
    output$table <- renderDataTable(d.nmds.sums,
                                    options = list(
                                      pageLength = 8
                                    )
    )
    
    ##### end species-specific dissimilarity #####
    
    
    ##### Plot code #####
    
    p <- ggplot() +
      geom_polygon(data = hulls, alpha = 0.25, aes(x = NMDS1, y = NMDS2, fill = groups)) +
      ##geom_polygon(data = subset(hulls, MgmtTarget == "concern"),alpha = 0.5, fill='black',aes(x=NMDS1,y=NMDS2)) +
      geom_point(data = data_scores, aes(x = NMDS1, y = NMDS2, color = groups), size = 0.75, alpha = 0.5) + 
      ##geom_polygon(data = subset(hulls, MgmtTarget == "concern"), aes(x=NMDS1,y=NMDS2, shape = species),size=0.5,alpha=0.5, color='black', fill='black')+
      geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5, size = 2)+
      theme_few() +
      xlab("Trait space 1") +
      ylab("Trait space 2") +
      ggtitle("Mouse over the graph to see species' names") +
      theme(
        plot.title = element_text(colour = "grey40"),
        axis.text.x=element_text(colour="grey40"),
        axis.text.y=element_text(colour="grey40"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
    
    
    output$nmdsPlot <- renderPlotly({
      p_plotly <- ggplotly(p, height = 800, width = 1300)
      
      # plotly aesthetics for this graph
      for (i in 1:length(p_plotly$x$data)){
        if (!is.null(p_plotly$x$data[[i]]$name)){
          p_plotly$x$data[[i]]$name =  gsub("\\(","",str_split(p_plotly$x$data[[i]]$name,",")[[1]][1])
        }
      }
      
      p_plotly
    })
    
    # Helpful hint: allow plot to render even when not on tab
    #https://shiny.rstudio.com/reference/shiny/0.11/outputoptions
    outputOptions(output, "nmdsPlot", suspendWhenHidden = FALSE)
    
    print('Rendered First Plotly')
    
    observeEvent(input$radioTraits, {
      print('Radio Traits inside GO')
      
      # add labels
      hulls <- hulls %>%
        mutate_if(is.character, as.factor) %>%  # added
        mutate(blooming.period..early = ifelse(blooming.period..early == "1", "early-summer blooming","not early blooming"),
               blooming.period..early.mid = ifelse(blooming.period..early == "1", "early-mid blooming","not early-mid blooming"),
               blooming.period...mid = ifelse(blooming.period...mid == "1", "mid-summer blooming","not mid-blooming"),
               blooming.period..mid.late = ifelse(blooming.period..mid.late == "1", "mid-late blooming","not mid-late blooming"),
               blooming.period..late = ifelse(blooming.period..late == "1", "late-summer blooming","not late-blooming"),
               MgmtTarget = ifelse(MgmtTarget == "1", "Management concern","not concerning")) %>%
        as.data.frame()
      ###
      
      
      # -------- data wrangling 3 -------- #
      facet_var <- as.character(noquote(input$radioTraits))
      print("class facet_var")
      print(class(facet_var))
      hulls.p1 <- hulls %>%
        dplyr::select(facet_var, groups) %>% # add cat() or noquote() ?
        dplyr::rename(X = facet_var) %>%
      distinct()
      
      hulls.p2 <- hulls %>%
        dplyr::select(NMDS1,NMDS2,groups) %>% # removed "species,Taxon"
        distinct() # added
      
      hulls.p <- left_join(hulls.p1,hulls.p2, by = "groups") # replaced with "character()"
      hulls.p <- hulls.p %>%
        distinct() # debug?

      hulls.p <- hulls.p %>%
        mutate(X = as.factor(X)) 
      
      print("str(hulls.p)")
      print(str(hulls.p))
      
      data_scores <- data_scores %>%
        left_join(., hulls.p[,c(1:2)], by = "groups") %>% # eliminated Taxa and species from join b/c we don't technically need them
        distinct()
      
      # end data wrangling 3
      
      
      # more plots!
      
      p2<-ggplot() +
        geom_polygon(data = hulls.p, aes(x=NMDS1,y=NMDS2, col=X, fill = groups),alpha=0.5) +
        scale_color_grey(start = 0.25, end = 0.75) +
        geom_point(data = data_scores, aes(x=NMDS1,y=NMDS2,col = X, fill = groups),pch=21,size=0.75,alpha=0.5) +
        geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5, size=2)+
        theme_few() +
        xlab("Trait space 1") +
        ylab("Trait space 2") +
        #facet_grid(~facet_var) +
        theme(legend.position = "bottom",
              axis.text.x=element_text(colour="grey40"),
              axis.text.y=element_text(colour="grey40"),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank())
      
      output$nmdsPlot2 <- renderPlotly({
        ggplotly(p2, height = 600, width = 1000) # double this!
      }) # BT Updated spacing
      
    })
    
    observeEvent(input$sliderK, {
      print('Radio K inside GO')

      res.hc <- hclust(dist(d.nmds))
      # Don't color labels, add rectangles
      dend <- fviz_dend(res.hc, cex = 1, k = noquote(input$sliderK), # make k reactive?
                        color_labels_by_k = FALSE, rect = TRUE, 
                        rect_fill = FALSE,rect_lty = 2,
                        rect_border = "grey40",
                        labels_track_height = 2.5,
                        ylab = "Niche similarity",
                        main=" ") + #Clusters of similar species
        theme(axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 15))
      output$dendPlot <- renderPlot({
        dend
      })
      
    })
    
    observeEvent(input$parameter, {
      
      d <- as.data.frame(as.matrix(dist.centroid)) # dissimilarity matrix for all centroids
      d <- d %>% mutate(X = rownames(.))
      
      dat <- d
      
      dat <- dat %>%
        as.data.frame() %>%
        filter(X == input$parameter)  # input$parameter     #subset(., X == paste(input$parameter))
      
      dat <- dat %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column() #%>%
      
      dat <- dat %>%
        dplyr::rename(value=2, X = 1)
      
      dat$value <- as.numeric(dat$value)

      
      j <- ggplot(dat, aes(x = reorder(X, value), y=value,fill=X)) +
        stat_summary(fun=mean,geom="point", size=4, position = position_dodge(width = 0.5), colour="black") +
        ylab("Similarity to selected species (smaller = more similar)")+
        xlab("Species")+
        theme_few() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1));ggplotly(j)
      
      output$newPlot <- renderPlotly({
        ggplotly(j, height = 600, width = 1300) #, height = 600, width = 800
      })
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
