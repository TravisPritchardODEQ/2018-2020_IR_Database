
library(shiny)
library(tidyverse)
library(shinybusy)
library(knitr)
library(kableExtra)
library(shinythemes)
library(shinyWidgets)

# Needed data files
# assessment_display.Rdata = joined_BU_summary = ALL BASINS_categories.xlsx joined with station information from Assessment_summary.R
# reduced_size_2018AUs.Rdata reduced sized AUs shp file
# 


# Load in data files ------------------------------------------------------



# Load assessment result data
load("data/assessment_display.Rdata")


# # load ALL BASINS_Impaired_1orMoreUses.xlsx that was saved as Impairment_list_import
# load("data/Impairment_list_import")
# 
# 
# 
# # Add status and reduce columns to what we need to display in map popups
# Impairment_list <- Impairment_list_import %>%
#   mutate(status = case_when(Impairment_cause == "." & Impaired_Uses == "-" & attaining_uses == "-" ~ "Insufficient Data",
#                             Impairment_cause != "." ~ "Impaired",
#                             TRUE ~ "Attaining")) %>%
#   select(AU_ID, Impaired_Uses,Impairment_cause, 
#          attaining_uses, insufficient_data_uses, 
#          unassessed_uses, parameter_group_assessed,status )
# 
# 


# populate values in selectize boxes
#get a vector of unique AUs
AU_s = unique(joined_BU_summary$AU_ID)
AU_Names <- sort(unique(joined_BU_summary$AU_Name[joined_BU_summary$AU_Name != "" & joined_BU_summary$AU_Name != "<Null>" ]), na.last = TRUE)

#Get a vector of pollutants
pollutants <- sort(unique(joined_BU_summary$Char_Name))

admin_basins <- sort(unique(joined_BU_summary$OWRD_Basin))



# shiny ui section -------------------------------------------------------


#Create the page
ui <- fluidPage(
  theme = shinytheme("yeti"),
  # Application title
  titlePanel(
    fluidRow(
      column(6, img(src = "logo.png")), 
      column(6,  "2018/2020 Integrated Report",style = "font-family: 'Arial'; font-si16pt; vertical-align: 'bottom'")),
    windowTitle = "2018/2020 Integrated Report"
  ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      actionButton("go", "Filter",  icon("filter")),
      downloadButton('downloadData', label = "Download Assessment Results"),
      selectizeInput("AUs",
                     "Select Assessment Unit",
                     choices = AU_s,
                     multiple = TRUE,
                     options = list(maxOptions = 3000)),
      selectizeInput("Select_AUName",
                     "Select AU Name",
                     choices = sort(AU_Names),
                     multiple = TRUE,
                     options = list(maxOptions = 3000)),
      selectizeInput("admin_basin_selector",
                     "Select Admin Basin",
                     choices = admin_basins,
                     multiple = TRUE),
      selectizeInput("category_selector",
                     "Select IR category",
                     choices = unique(sort(joined_BU_summary$IR_category)),
                     multiple = TRUE),
      selectizeInput("pollutant_selector",
                     "Select Pollutant",
                     choices = pollutants,
                     multiple = TRUE)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  id = "Tabset",
                  tabPanel("Instructions",
                           value = "InstructionTab",
                           h2(strong(" Instructions to Review the 2018/2020 Integrated Report Database"), style = "font-family: 'Arial'"),
                           p("The 2018/2020 Integrated Report Assessment Database contains new assessment information and updates to assessments from 1998, 2002, 2004, 2010, and 2012. (See",
                             a("2012 Integrated Report Database", href="https://www.deq.state.or.us/wq/assessment/rpt2012/search.asp", target="_blank"),"). The current assessment categorizations  
                             are described in the “IR_category” report field. The “Assessed_in_2018” report field indicates if new data evaluations or assessments were done in 2018, othewise
                             the status assigned in previous assessments was carried forward. Assessment categorized as Category 4 or category 5 (includuing all subcategories) are considered impaired.", style = "font-family: 'times'"),
                           p( 
                             a("The 2018/2020 Assessment Methodology can be found here.", href="https://www.oregon.gov/deq/FilterDocs/ir2018assessMethod.pdf", target="_blank"), style = "font-family: 'times'"),
                           p("A more complete mapping and dataset, including water quality standards information can be found on the ", 
                             a("DEQ WQ Standards & Assessment tool.", href="https://hdcgcx2.deq.state.or.us/HVR291/?viewer=wqsa#", target="_blank"), style = "font-family: 'times'"),
                           p("The 2018/2020 page can be found at", a("link.", href="https://www.google.com", target="_blank"), style = "font-family: 'times'"),
                           p("Raw data used in assessments can be downloaded from ", a("AWQMS.", href="https://www.oregon.gov/deq/wq/Pages/WQdata.aspx", target="_blank"),  style = "font-family: 'times'"),
                           p(strong("Use seacrh criteria on left to filter results."), style = "font-family: 'times'"),
                           p(strong("Information for each record in the assessment database includes:"), style = "font-family: 'times'"),
                           tags$ul(
                             tags$li(strong("AU_ID "), " - Assessment Unit ID", style = "font-family: 'times'"), 
                             tags$li(strong("AU_Name "), " - Assessment Unit Name", style = "font-family: 'times'"), 
                             tags$li(strong("OWRD_Basin "), " - Oregon Water Resources Department Administrative Basin", style = "font-family: 'times'"), 
                             tags$li(strong("Assessment "), " - Parameter being assessed. Includes specifc standard, if applicable", style = "font-family: 'times'"), 
                             tags$li(strong("IR_category "), " - Current Integrated Report category for that specific assessment", style = "font-family: 'times'"), 
                             tags$ul(
                               tags$li(strong("Category 2"), " - Available data and information indicate that some designated uses are supported and the water quality standard is attained", style = "font-family: 'times'"), 
                               tags$li(strong("Category 3"), " - Insufficient dta to determine whether a designated use is supported", style = "font-family: 'times'"), 
                               tags$ul(
                                 tags$li(strong("Category 3B"), " - This category is used when there is insufficient data to determine use support, but some data indicate  possible impairment", style = "font-family: 'times'"), 
                                 tags$li(strong("Category 3C"), " - This category is used to identify waters whose biocriteria scores differ from reference condition, but are not classified as impaired", style = "font-family: 'times'"), 
                                 tags$li(strong("Category 3D"), " - This category is used when all the available data has criteria values below the test method’s quantification limits", style = "font-family: 'times'")
                                 
                                 
                               ),
                               tags$li(strong("Category 4"), " - Data indicate that at least one designated use is not supported, but a TMDL is not needed to address the pollutant", style = "font-family: 'times'"),
                               tags$ul(
                                 tags$li(strong("Category 4A"), " - Clean-up plans (also called TMDLs) that will result in the waterbody meeting water quality standards and supporting its beneficial uses have been approved", style = "font-family: 'times'"), 
                                 tags$li(strong("Category 4B"), " - Other pollution control requirements are expected to address pollutant of concern and will result in attainment of water quality standards", style = "font-family: 'times'"), 
                                 tags$li(strong("Category 4C"), " - The impairment is caused by pollution, not a pollutant. For example, flow, or lack of flow, are not considered pollutants, but may be affecting the waterbody’s beneficial uses", style = "font-family: 'times'")
                                 
                                 
                               ),
                               tags$li(strong("Category 3B"), " - Data indicate a designated use is not supported or a water quality standard is not attained and a TMDL is needed. this category constitites the Section 303(d) list that EPA will approve or disapprove under the Clean Water Act.", style = "font-family: 'times'")
                               
                             ),
                             tags$li(strong("Monitoring Locations "), " - Monitoring stations used in 2018/2020 assessment. Data from these monitoring locations can be downloaded from AWQMS, providing the raw data used in assessment.", style = "font-family: 'times'"), 
                             tags$li(strong("Year_listed "), " - If Assessment Unit is identified as impaired (Category 4 or 5), year it first appeared on the 303(d) List", style = "font-family: 'times'"), 
                             tags$li(strong("Assessed_in_2018 "), " - Identifies if assessment was conducted in 2018", style = "font-family: 'times'")
                           )
                           
                           
                  ),
                  tabPanel("Data",
                           value = "Datatab",
                           dataTableOutput('table')
                  )
      )
    )
    
    
    
    
  ),
  add_busy_spinner(spin = "fading-circle")
  
)



# shiny server section ----------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
 
  
 
  
  # Table_data --------------------------------------------------------------
  
  
  # Reactive table data
  # This is where all the filtering is going to happen.
  # Modify to include any filtering needed for webdiplay
  
  table_Data <- eventReactive(input$go,{
    
    t <- joined_BU_summary
    
    if (!is.null(input$AUs)){
      t <- t %>%
        filter(AU_ID %in% input$AUs)
    }
    
    if (!is.null(input$Select_AUName)){
      t <- t %>%
        filter(AU_Name %in% input$Select_AUName)
    }
    
    
    if(!is.null(input$category_selector)){
      
      t <- t %>%
        filter(IR_category %in% input$category_selector)
    }
    
    if(!is.null(input$pollutant_selector)){
      
      t <- t %>%
        filter(Char_Name %in% input$pollutant_selector)
    }
    
    if(!is.null(input$admin_basin_selector)){
      t <- t %>%
        filter(OWRD_Basin %in% input$admin_basin_selector)
      
    }
    t <- t %>%
      select(-Char_Name)
    
    t
    
  })
  
  
  
  # I have no idea what this does
  AU_Names_reactive <- eventReactive(input$AUs,{
    
    BU_filtered <- filter(joined_BU_summary, AU_ID %in% input$AUs )
    
    unique(BU_filtered$AU_Name)
    
    
    
  }, ignoreNULL = TRUE)
  
  
  
  # Render the table --------------------------------------------------------
  
  
  #render the table from the reactive table_Data 
  # Reactive function
  output$table <- renderDataTable(
    
    table_Data(),
    options = list(paging = FALSE)
    
    
  )
  
  
  # Download button actions -------------------------------------------------
  
  
  #Handle the download output.
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("Oregon 2020 Integrated Report Filtered Download", ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_Data(), file, row.names = FALSE,  na = "")
    })
  
  
  # Upate AU_Name selectize input -------------------------------------------
  
  #Update AU_Name choices based on selects AUS
  observe({
    BU_filtered_names <- filter(joined_BU_summary, AU_ID %in% input$AUs )
    filtered_Names <- BU_filtered_names$AU_Name
    
    if (is.null(input$AUs)){
      filtered_Names <- joined_BU_summary$AU_Name
    }
    
    
    if(is.null(input$Select_AUName)){
      
      updateSelectizeInput(session,
                           inputId = 'Select_AUName', 
                           choices = filtered_Names)
    }
    
  })
  
  # Upate AU selectize input -------------------------------------------
  
  #Update AU choices based on selected AU_Name
  
  observeEvent(input$Select_AUName, {
    BU_filtered_AUIDs <- filter(joined_BU_summary, AU_Name %in% input$Select_AUName)
    filtered_AUs <- unique(BU_filtered_AUIDs$AU_ID)
    
    if (is.null(input$Select_AUName)){
      filtered_AUs <- AU_s
    }
    
    
    if (!is.null(input$AUs)){
      updateSelectizeInput(session,
                           inputId = 'AUs', 
                           choices = filtered_AUs,
                           selected = input$AUs)
      
      
      
      
    } 
    
    
    if(is.null(input$Select_AUName)){
      updateSelectizeInput(session,
                           inputId = 'AUs', 
                           choices = filtered_AUs)
      
    }  
    
  })
  
  
  # When filter button is hit. move focus to data tab -----------------------
  
  observeEvent(input$go, {
    updateTabsetPanel(session, "Tabset",
                      selected = 'Datatab'
    )
  })
  
  
  
}



# Run the application  ----------------------------------------------------


shinyApp(ui = ui, server = server)
