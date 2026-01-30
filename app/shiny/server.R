# ==============================================================================
# PROJECT: Stimuli Panel Randomizer ####
# SECTION: SERVER LOGIC
# ==============================================================================

function(input, output, session) {
  
  # --- 3.1: Reactive State Containers --- ####
  controls <- reactiveVal(data.frame(stimulus = character(0), qty_ug = character(0), stringsAsFactors = FALSE))
  test_stimuli <- reactiveVal(data.frame(stimulus = character(0), qty_ug = character(0), stringsAsFactors = FALSE))
  terminal_stimuli <- reactiveVal(data.frame(stimulus = character(0), qty_ug = character(0), stringsAsFactors = FALSE))
  
  # Tracks current number of rows for input binding
  counters <- reactiveValues(control = 0, test = 0, terminal = 0)
  
  # --- 3.2: File Import Functionality (Supports TSV & Excel) --- ####
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
      
      # Clean and normalize strings for textInputs
      df <- df %>%
        mutate(across(everything(), as.character)) %>%
        mutate(across(everything(), ~tidyr::replace_na(.x, ""))) %>%
        filter(stimulus != "")
      
      # Populate States
      ctrl_df <- df %>% filter(tolower(stimulus_type) == "control") %>% select(stimulus, qty_ug = quantity_ug)
      test_df <- df %>% filter(tolower(stimulus_type) == "test") %>% select(stimulus, qty_ug = quantity_ug)
      term_df <- df %>% filter(tolower(stimulus_type) == "terminal") %>% select(stimulus, qty_ug = quantity_ug)
      
      controls(ctrl_df)
      test_stimuli(test_df)
      terminal_stimuli(term_df)
      
      counters$control <- nrow(ctrl_df)
      counters$test <- nrow(test_df)
      counters$terminal <- nrow(term_df)
      
      showNotification("Import successful!", type = "message")
    }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
  })
  
  # --- 3.3: Row Deletion Logic --- ####
  observe({
    lapply(seq_len(counters$control), function(i) {
      observeEvent(input[[paste0("rem_ctrl_", i)]], {
        curr <- controls()
        if(nrow(curr) >= i) { controls(curr[-i, , drop = FALSE]); counters$control <- nrow(controls()) }
      }, ignoreInit = TRUE, once = TRUE)
    })
    lapply(seq_len(counters$test), function(i) {
      observeEvent(input[[paste0("rem_tst_", i)]], {
        curr <- test_stimuli()
        if(nrow(curr) >= i) { test_stimuli(curr[-i, , drop = FALSE]); counters$test <- nrow(test_stimuli()) }
      }, ignoreInit = TRUE, once = TRUE)
    })
    lapply(seq_len(counters$terminal), function(i) {
      observeEvent(input[[paste0("rem_term_", i)]], {
        curr <- terminal_stimuli()
        if(nrow(curr) >= i) { terminal_stimuli(curr[-i, , drop = FALSE]); counters$terminal <- nrow(terminal_stimuli()) }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
  # --- 3.4: Dynamic Row UI Generator --- ####
  create_row <- function(id, type, n_val, q_val) {
    fluidRow(
      column(8, textInput(paste0(type, "_n_", id), NULL, value = n_val, placeholder = "Stimulus")),
      column(3, textInput(paste0(type, "_q_", id), NULL, value = q_val, placeholder = "Qty")),
      column(1, actionButton(paste0("rem_", type, "_", id), "X", class = "btn-danger btn-xs", style="margin-top: 5px; padding: 1px 5px;"))
    )
  }
  
  output$control_inputs <- renderUI({
    d <- controls(); lapply(seq_len(nrow(d)), function(i) create_row(i, "ctrl", d$stimulus[i], d$qty_ug[i]))
  })
  output$test_inputs <- renderUI({
    d <- test_stimuli(); lapply(seq_len(nrow(d)), function(i) create_row(i, "tst", d$stimulus[i], d$qty_ug[i]))
  })
  output$terminal_inputs <- renderUI({
    d <- terminal_stimuli(); lapply(seq_len(nrow(d)), function(i) create_row(i, "term", d$stimulus[i], d$qty_ug[i]))
  })
  
  # --- 3.5: Synchronize UI Inputs to Reactive State (DEBOUNCED) --- ####
  # Debouncing prevents the UI from re-rendering on every keystroke, 
  # which avoids focus loss while typing.
  
  input_data_raw <- reactive({
    list(
      ctrl = lapply(seq_len(counters$control), function(i) list(n = input[[paste0("ctrl_n_", i)]], q = input[[paste0("ctrl_q_", i)]])),
      tst  = lapply(seq_len(counters$test), function(i) list(n = input[[paste0("tst_n_", i)]], q = input[[paste0("tst_q_", i)]])),
      term = lapply(seq_len(counters$terminal), function(i) list(n = input[[paste0("term_n_", i)]], q = input[[paste0("term_q_", i)]]))
    )
  })
  
  # Apply a 500ms delay before syncing to reactive values
  input_data_debounced <- debounce(input_data_raw, 500)
  
  observe({
    data <- input_data_debounced()
    
    if (counters$control > 0) {
      names <- sapply(data$ctrl, `[[`, "n")
      qtys  <- sapply(data$ctrl, `[[`, "q")
      if (!any(sapply(names, is.null))) {
        controls(data.frame(stimulus = as.character(unlist(names)), qty_ug = as.character(unlist(qtys)), stringsAsFactors = FALSE))
      }
    }
    if (counters$test > 0) {
      names <- sapply(data$tst, `[[`, "n")
      qtys  <- sapply(data$tst, `[[`, "q")
      if (!any(sapply(names, is.null))) {
        test_stimuli(data.frame(stimulus = as.character(unlist(names)), qty_ug = as.character(unlist(qtys)), stringsAsFactors = FALSE))
      }
    }
    if (counters$terminal > 0) {
      names <- sapply(data$term, `[[`, "n")
      qtys  <- sapply(data$term, `[[`, "q")
      if (!any(sapply(names, is.null))) {
        terminal_stimuli(data.frame(stimulus = as.character(unlist(names)), qty_ug = as.character(unlist(qtys)), stringsAsFactors = FALSE))
      }
    }
  })
  
  # --- 3.6: Add Row Buttons --- ####
  observeEvent(input$add_control, { controls(rbind(controls(), data.frame(stimulus="", qty_ug="", stringsAsFactors=F))); counters$control <- nrow(controls()) })
  observeEvent(input$add_test, { test_stimuli(rbind(test_stimuli(), data.frame(stimulus="", qty_ug="", stringsAsFactors=F))); counters$test <- nrow(test_stimuli()) })
  observeEvent(input$add_terminal, { terminal_stimuli(rbind(terminal_stimuli(), data.frame(stimulus="", qty_ug="", stringsAsFactors=F))); counters$terminal <- nrow(terminal_stimuli()) })
  
  # --- 3.7: Export Logic --- ####
  export_data <- reactive({
    bind_rows(
      controls() %>% filter(stimulus != "") %>% mutate(stimulus_type = "Control"),
      test_stimuli() %>% filter(stimulus != "") %>% mutate(stimulus_type = "Test"),
      terminal_stimuli() %>% filter(stimulus != "") %>% mutate(stimulus_type = "Terminal")
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
    c_list <- controls() %>% filter(stimulus != "") %>% mutate(qty_ug = as.numeric(qty_ug), Category = "Control")
    t_raw <- test_stimuli() %>% filter(stimulus != "") %>% mutate(qty_ug = as.numeric(qty_ug))
    if(nrow(t_raw) > 0) {
      u_names <- sample(unique(t_raw$stimulus))
      t_final <- t_raw %>% mutate(stimulus = factor(stimulus, levels = u_names)) %>% 
        arrange(stimulus, qty_ug) %>% mutate(stimulus = as.character(stimulus), Category = "Test")
    } else { t_final <- data.frame(stimulus=character(0), qty_ug=numeric(0), Category=character(0)) }
    tm_list <- terminal_stimuli() %>% filter(stimulus != "") %>% mutate(qty_ug = as.numeric(qty_ug), Category = "Terminal")
    
    bind_rows(c_list, t_final, tm_list) %>% mutate(Order = row_number()) %>% select(Order, Category, stimulus, qty_ug)
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