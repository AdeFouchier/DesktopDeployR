# ==============================================================================
# PROJECT: Stimuli Panel Randomizer ####
# DESCRIPTION: A Shiny application to generate randomized stimulation lists ####
#              for olfactory electrophysiology. ####
#
# LOGIC: ####
#   1. Controls: Presented first in the order they are listed in the UI. ####
#   2. Tests: stimuluss are randomized as groups. If an stimulus has multiple ####
#      doses, they are presented from lowest to highest before moving to ####
#      the next random stimulus group. ####
#   3. Terminals: Presented last in the order they are listed in the UI. ####
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1: LIBRARY IMPORTS ####
# ------------------------------------------------------------------------------
if (!require("shiny")) install.packages("shiny") #
if (!require("dplyr")) install.packages("dplyr") #
if (!require("tidyr")) install.packages("tidyr") #
if (!require("DT")) install.packages("DT") #
if (!require("shinythemes")) install.packages("shinythemes") #

library(shiny) #
library(dplyr) #
library(tidyr) #
library(DT) #
library(shinythemes) #

# ------------------------------------------------------------------------------
# SECTION 2: USER INTERFACE (UI) ####
# ------------------------------------------------------------------------------
fluidPage(
  theme = shinytheme("flatly"), #
  titlePanel("Stimuli Panel Randomizer"), #
  
  sidebarLayout(
    # --- Sidebar: Input Controls (Now 1/2 Width) --- ####
    sidebarPanel(
      width = 6,
      
      actionButton("generate", "Generate Randomized List", 
                   icon = icon("play"), 
                   class = "btn-success btn-lg",
                   style = "width: 100%; margin-bottom: 20px;"), #
      
      # Updated Header and added Export Button #
      wellPanel(
        h4("Bulk Import / Export"), #
        p(tags$small("Upload a TSV with: 'stimulus_type', 'stimulus', 'quantity_ug'")), #
        fileInput("file_input", "Choose TSV File", accept = c(".tsv", ".txt")), #
        downloadButton("download_panel", "Export Current Panel (TSV)", class = "btn-info btn-sm", style = "width: 100%;")
      ),
      
      hr(),
      
      h3("1. Control Stimuli"), #
      uiOutput("control_inputs"), #
      actionButton("add_control", "Add Control", icon = icon("plus"), class = "btn-sm"), #
      
      hr(),
      
      h3("2. Test Panel Stimuli"), #
      uiOutput("test_inputs"), #
      actionButton("add_test", "Add Test Dose", icon = icon("plus"), class = "btn-sm"), #
      
      hr(),
      
      h3("3. Terminal Stimuli"), #
      uiOutput("terminal_inputs"), #
      actionButton("add_terminal", "Add Terminal", icon = icon("plus"), class = "btn-sm") #
    ),
    
    # --- Main Panel: Display Results (Now 1/2 Width) --- ####
    mainPanel(
      width = 6,
      tabsetPanel(
        tabPanel("Results", 
                 h3("Generated Sequence"), #
                 DTOutput("preview_table"), #
                 br(), #
                 wellPanel(
                   h4("Sequence Summary"), #
                   verbatimTextOutput("summary_stats") #
                 )
        ),
        tabPanel("Instructions",
                 h4("User Manual"), #
                 tags$ul(
                   tags$li("The app generates a sequence based on three categories."), #
                   tags$li("Each row in the sidebar represents a single stimulation (one stimulus at one dose)."), #
                   tags$li("Importing a TSV will automatically create a row for every entry found in the file."), #
                   tags$li("The 'Export Current Panel' button saves your current UI configuration for future use."), #
                   tags$li("Test stimuli are grouped by name and randomized. Within a group, doses are sorted low to high."), #
                   tags$li("Use the export buttons above the table to save your final sequence.") #
                 )
        )
      )
    )
  )
)

