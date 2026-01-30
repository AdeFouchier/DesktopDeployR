# ==============================================================================
# PROJECT: Stimuli Panel Randomizer ####
# SECTION: SERVER LOGIC
# ==============================================================================

function(input, output, session) {
  
  # --- 3.1: Reactive State Containers --- ####
  # Now including a 'uuid' column to uniquely identify rows regardless of position
  controls <- reactiveVal(data.frame(uuid = character(0), stimulus = character(0), qty_ug = character(0), stringsAsFactors = FALSE))
  test_stimuli <- reactiveVal(data.frame(uuid = character(0), stimulus = character(0), qty_ug = character(0), stringsAsFactors = FALSE))
  terminal_stimuli <- reactiveVal(data.frame(uuid = character(0), stimulus = character(0), qty_ug = character(0), stringsAsFactors = FALSE))
  
  # --- 3.2: File Import Functionality --- ####
  observeEvent(input$file_input, {
    req(input$file_input)
    file_ext <- tools::file_ext(input$file_input$name)
    
    tryCatch({
      if (file_ext %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(input$file_input$datapath)
      } else {
        df <- read.table(input$file_input$datapath, sep = "\t", header = TRUE, 
                         stringsAsFactors = FALSE, check.names = FALSE)
      }
      
      df <- as.data.frame(df)
      colnames(df) <- tolower(gsub(" ", "_", colnames(df)))
      req("stimulus" %in% colnames(df))
      
      df <- df %>%
        mutate(across(everything(), as.character)) %>%
        mutate(across(everything(), ~tidyr::replace_na(.x, ""))) %>%
        filter(stimulus != "")
      
      # Helper to add UUIDs on import
      add_uuids <- function(d) {
        if(nrow(d) == 0) return(data.frame(uuid=character(0), stimulus=character(0), qty_ug=character(0)))
        d$uuid <- replicate(nrow(d), paste0("id_", as.integer(runif(1, 1e5, 1e9))))
        d
      }
      
      ctrl_df <- df %>% filter(tolower(stimulus_type) == "control") %>% select(stimulus, qty_ug = quantity_ug) %>% add_uuids()
      test_df <- df %>% filter(tolower(stimulus_type) == "test") %>% select(stimulus, qty_ug = quantity_ug) %>% add_uuids()
      term_df <- df %>% filter(tolower(stimulus_type) == "terminal") %>% select(stimulus, qty_ug = quantity_ug) %>% add_uuids()
      
      controls(ctrl_df)
      test_stimuli(test_df)
      terminal_stimuli(term_df)
      
      showNotification("Import successful!", type = "message")
    }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
  })
  
  # --- 3.3: Data Harvesting Function --- ####
  # Instead of an observer that constantly rewrites the state (causing re-renders),
  # we harvest the current input values only when needed (Generate or Export).
  harvest_data <- function(reactive_df) {
    df <- reactive_df()
    if (nrow(df) == 0) return(df)
    
    for (i in 1:nrow(df)) {
      id <- df$uuid[i]
      val_n <- input[[paste0("n_", id)]]
      val_q <- input[[paste0("q_", id)]]
      if (!is.null(val_n)) df$stimulus[i] <- val_n
      if (!is.null(val_q)) df$qty_ug[i] <- val_q
    }
    return(df)
  }
  
  # --- 3.4: UUID-Based Deletion Logic --- ####
  # When we delete, we harvest the latest data first so we don't lose typed text in other rows
  observeEvent(input$last_btn_clicked_ctrl, {
    req(input$last_btn_clicked_ctrl)
    curr <- harvest_data(controls)
    controls(curr[curr$uuid != input$last_btn_clicked_ctrl, , drop = FALSE])
  })
  
  observeEvent(input$last_btn_clicked_tst, {
    req(input$last_btn_clicked_tst)
    curr <- harvest_data(test_stimuli)
    test_stimuli(curr[curr$uuid != input$last_btn_clicked_tst, , drop = FALSE])
  })
  
  observeEvent(input$last_btn_clicked_term, {
    req(input$last_btn_clicked_term)
    curr <- harvest_data(terminal_stimuli)
    terminal_stimuli(curr[curr$uuid != input$last_btn_clicked_term, , drop = FALSE])
  })
  
  # --- 3.5: Dynamic Row UI Generator --- ####
  create_row <- function(uuid, n_val, q_val, type_label) {
    onclick_js <- sprintf("Shiny.setInputValue('last_btn_clicked_%s', '%s', {priority: 'event'})", type_label, uuid)
    
    fluidRow(
      column(8, textInput(paste0("n_", uuid), NULL, value = n_val, placeholder = "Stimulus")),
      column(3, textInput(paste0("q_", uuid), NULL, value = q_val, placeholder = "Qty")),
      column(1, actionButton(paste0("btn_", uuid), "X", 
                             class = "btn-danger btn-xs", 
                             style="margin-top: 5px; padding: 1px 5px;",
                             onclick = onclick_js))
    )
  }
  
  output$control_inputs <- renderUI({
    d <- controls() # This only triggers when rows are added/removed or imported
    if (nrow(d) == 0) return(NULL)
    lapply(1:nrow(d), function(i) create_row(d$uuid[i], d$stimulus[i], d$qty_ug[i], "ctrl"))
  })
  
  output$test_inputs <- renderUI({
    d <- test_stimuli()
    if (nrow(d) == 0) return(NULL)
    lapply(1:nrow(d), function(i) create_row(d$uuid[i], d$stimulus[i], d$qty_ug[i], "tst"))
  })
  
  output$terminal_inputs <- renderUI({
    d <- terminal_stimuli()
    if (nrow(d) == 0) return(NULL)
    lapply(1:nrow(d), function(i) create_row(d$uuid[i], d$stimulus[i], d$qty_ug[i], "term"))
  })
  
  # --- 3.6: Add Row Buttons --- ####
  new_uuid <- function() paste0("id_", as.integer(runif(1, 1e5, 1e9)))
  
  observeEvent(input$add_control, { 
    curr <- harvest_data(controls)
    controls(rbind(curr, data.frame(uuid=new_uuid(), stimulus="", qty_ug="", stringsAsFactors=FALSE)))
  })
  observeEvent(input$add_test, { 
    curr <- harvest_data(test_stimuli)
    test_stimuli(rbind(curr, data.frame(uuid=new_uuid(), stimulus="", qty_ug="", stringsAsFactors=FALSE)))
  })
  observeEvent(input$add_terminal, { 
    curr <- harvest_data(terminal_stimuli)
    terminal_stimuli(rbind(curr, data.frame(uuid=new_uuid(), stimulus="", qty_ug="", stringsAsFactors=FALSE)))
  })
  
  # --- 3.7: Export Logic --- ####
  export_data <- reactive({
    bind_rows(
      harvest_data(controls) %>% filter(stimulus != "") %>% mutate(stimulus_type = "Control"),
      harvest_data(test_stimuli) %>% filter(stimulus != "") %>% mutate(stimulus_type = "Test"),
      harvest_data(terminal_stimuli) %>% filter(stimulus != "") %>% mutate(stimulus_type = "Terminal")
    ) %>% select(stimulus_type, stimulus, quantity_ug = qty_ug)
  })
  
  output$download_panel <- downloadHandler(
    filename = function() { paste0("panel_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv") },
    content = function(file) { write.table(export_data(), file, sep = "\t", row.names = FALSE, quote = FALSE) }
  )
  
  output$download_panel_excel <- downloadHandler(
    filename = function() { paste0("panel_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx") },
    content = function(file) { writexl::write_xlsx(export_data(), path = file) }
  )
  
  # --- 3.8: Core Randomization Logic --- ####
  final_data <- eventReactive(input$generate, {
    c_list <- harvest_data(controls) %>% filter(stimulus != "") %>% mutate(Category = "Control")
    t_raw <- harvest_data(test_stimuli) %>% filter(stimulus != "")
    
    if(nrow(t_raw) > 0) {
      u_names <- sample(unique(t_raw$stimulus))
      t_final <- t_raw %>% 
        mutate(qty_num = as.numeric(qty_ug)) %>%
        mutate(stimulus = factor(stimulus, levels = u_names)) %>% 
        arrange(stimulus, qty_num) %>% 
        mutate(stimulus = as.character(stimulus), Category = "Test") %>%
        select(-qty_num)
    } else { t_final <- data.frame(stimulus=character(0), qty_ug=character(0), Category=character(0)) }
    
    tm_list <- harvest_data(terminal_stimuli) %>% filter(stimulus != "") %>% mutate(Category = "Terminal")
    
    bind_rows(c_list, t_final, tm_list) %>% 
      mutate(Order = row_number()) %>% 
      select(Order, Category, stimulus, qty_ug)
  })
  
  # --- 3.9: Output Rendering --- ####
  output$preview_table <- renderDT({
    req(final_data())
    ts <- format(Sys.time(), "%Y.%m.%d_%H.%M")
    fname <- paste0("randomized_stimuli_panel_", ts)
    
    datatable(final_data(), 
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                pageLength = -1,
                buttons = list(
                  list(extend = 'copy', title = NULL, header = FALSE,
                       exportOptions = list(rownames = FALSE, columns = c(2, 3))),
                  list(extend = 'csv', text = 'TSV', fieldSeparator = '\t', 
                       extension = '.tsv', filename = fname, title = NULL, 
                       exportOptions = list(rownames = FALSE)),
                  list(extend = 'excel', filename = fname, title = NULL, 
                       exportOptions = list(rownames = FALSE))
                )
              ),
              rownames = FALSE)
  })
  
  output$summary_stats <- renderText({
    d <- tryCatch(final_data(), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) return("Click 'Generate' to see the summary.")
    paste0("Total: ", nrow(d), "\nUnique: ", length(unique(d$stimulus)), "\nCtrl: ", sum(d$Category == "Control"), 
           "\nTest: ", sum(d$Category == "Test"), "\nTerm: ", sum(d$Category == "Terminal"))
  })
}