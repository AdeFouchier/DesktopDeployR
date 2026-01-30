# ==============================================================================
# PROJECT: Stimuli Panel Randomizer ####
# SECTION: USER INTERFACE (UI)
# ==============================================================================

# Note: Library imports are handled in global.R

fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Stimuli Panel Randomizer"),
  
  sidebarLayout(
    # --- Sidebar: Configuration Inputs (1/2 Width) --- ####
    sidebarPanel(
      width = 6,
      actionButton("generate", "Generate Randomized List", 
                   icon = icon("play"), 
                   class = "btn-success btn-lg",
                   style = "width: 100%; margin-bottom: 20px;"),
      
      wellPanel(
        h4("Bulk Import / Export"),
        p(tags$small("Upload an Excel or TSV file with: 'stimulus_type', 'stimulus', 'quantity_ug'")), 
        fileInput("file_input", "Choose File", accept = c(".tsv", ".txt", ".xlsx", ".xls")),
        
        # side-by-side export buttons
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("download_panel", "Export TSV", 
                         class = "btn-info btn-sm", style = "width: 95%;"),
          downloadButton("download_panel_excel", "Export Excel", 
                         class = "btn-primary btn-sm", style = "width: 95%;")
        )
      ),
      
      tabPanel("Input Stimuli",
               br(),
               wellPanel(
                 h4("1. Control Stimuli"),
                 uiOutput("control_inputs"),
                 actionButton("add_control", "Add Control", icon = icon("plus"), class = "btn-info btn-xs")
               ),
               wellPanel(
                 h4("2. Test Stimuli"),
                 uiOutput("test_inputs"),
                 actionButton("add_test", "Add Test", icon = icon("plus"), class = "btn-info btn-xs")
               ),
               wellPanel(
                 h4("3. Terminal Stimuli"),
                 uiOutput("terminal_inputs"),
                 actionButton("add_terminal", "Add Terminal", icon = icon("plus"), class = "btn-info btn-xs")
               )
      )
    ),
    
    # --- Main Panel: Display Results (1/2 Width) --- ####
    mainPanel(
      width = 6,
      tabsetPanel(
        tabPanel("Results", 
                 h3("Generated Sequence"),
                 DTOutput("preview_table"),
                 br(),
                 wellPanel(
                   h4("Sequence Summary"),
                   verbatimTextOutput("summary_stats")
                 )
        ),
        tabPanel("Instructions",
                 h4("User Manual"),
                 tags$ul(
                   tags$li("Test stimuli are grouped and randomized; doses within a group are sorted low-to-high."),
                   tags$li("Use the red 'X' buttons to remove specific lines."),
                   tags$li("Importing an Excel or TSV file will automatically populate the sidebar."),
                   tags$li("Use the export buttons in the sidebar to save your current panel configuration.")
                 )
        )
      )
    )
  )
)