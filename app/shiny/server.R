# ------------------------------------------------------------------------------
# SECTION 3: SERVER LOGIC ####
# ------------------------------------------------------------------------------
function(input, output, session) {
  
  # --- 3.1: Reactive State Containers --- ####
  controls <- reactiveVal(data.frame(stimulus = character(0), qty_ug = character(0), stringsAsFactors = FALSE)) #
  test_stimuli <- reactiveVal(data.frame(stimulus = character(0), qty_ug = character(0), stringsAsFactors = FALSE)) #
  terminal_stimuli <- reactiveVal(data.frame(stimulus = character(0), qty_ug = character(0), stringsAsFactors = FALSE)) #
  
  # Tracks current number of rows for input binding #
  counters <- reactiveValues(control = 0, test = 0, terminal = 0) #
  
  # --- 3.2: File Import Functionality --- ####
  observeEvent(input$file_input, {
    req(input$file_input) #
    tryCatch({
      df <- read.table(input$file_input$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) #
      
      required <- c("stimulus_type", "stimulus", "quantity_ug") #
      if (!all(required %in% colnames(df))) {
        showNotification("Error: TSV missing required columns.", type = "error") #
        return()
      }
      
      df <- df %>% filter(!is.na(stimulus) & stimulus != "") #
      
      # Populate Controls #
      ctrls <- df %>% filter(tolower(stimulus_type) == "control") %>%
        select(stimulus = stimulus, qty_ug = quantity_ug) #
      controls(ctrls) #
      counters$control <- nrow(ctrls) #
      
      # Populate Tests #
      tsts <- df %>% filter(tolower(stimulus_type) == "test") %>%
        select(stimulus = stimulus, qty_ug = quantity_ug) #
      test_stimuli(tsts) #
      counters$test <- nrow(tsts) #
      
      # Populate Terminals #
      terms <- df %>% filter(tolower(stimulus_type) == "terminal") %>%
        select(stimulus = stimulus, qty_ug = quantity_ug) #
      terminal_stimuli(terms) #
      counters$terminal <- nrow(terms) #
      
      showNotification("Data loaded: created one row per dose.", type = "message") #
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error") #
    })
  })
  
  # --- 3.3: UI Row Generation Helpers --- ####
  create_row <- function(id, type, n_val, q_val) {
    fluidRow(
      column(8, textInput(paste0(type, "_n_", id), NULL, value = n_val, placeholder = "Stimulus")), #
      column(3, textInput(paste0(type, "_q_", id), NULL, value = q_val, placeholder = "Qty")), #
      column(1, actionButton(paste0("rem_", type, "_", id), "X", class = "btn-danger btn-xs", style="margin-top: 5px;")) #
    )
  }
  
  output$control_inputs <- renderUI({
    d <- controls(); if(nrow(d) == 0) return(NULL) #
    lapply(1:nrow(d), function(i) {
      # Add individual observers for removal buttons if they don't exist
      observeEvent(input[[paste0("rem_ctrl_", i)]], {
        current <- controls()
        if(nrow(current) >= i) {
          controls(current[-i, , drop = FALSE])
          counters$control <- nrow(controls())
        }
      }, ignoreInit = TRUE, once = TRUE)
      create_row(i, "ctrl", d$stimulus[i], d$qty_ug[i])
    })
  })
  
  output$test_inputs <- renderUI({
    d <- test_stimuli(); if(nrow(d) == 0) return(NULL) #
    lapply(1:nrow(d), function(i) {
      observeEvent(input[[paste0("rem_tst_", i)]], {
        current <- test_stimuli()
        if(nrow(current) >= i) {
          test_stimuli(current[-i, , drop = FALSE])
          counters$test <- nrow(test_stimuli())
        }
      }, ignoreInit = TRUE, once = TRUE)
      create_row(i, "tst", d$stimulus[i], d$qty_ug[i])
    })
  })
  
  output$terminal_inputs <- renderUI({
    d <- terminal_stimuli(); if(nrow(d) == 0) return(NULL) #
    lapply(1:nrow(d), function(i) {
      observeEvent(input[[paste0("rem_term_", i)]], {
        current <- terminal_stimuli()
        if(nrow(current) >= i) {
          terminal_stimuli(current[-i, , drop = FALSE])
          counters$terminal <- nrow(terminal_stimuli())
        }
      }, ignoreInit = TRUE, once = TRUE)
      create_row(i, "term", d$stimulus[i], d$qty_ug[i])
    })
  })
  
  # --- 3.4: Manual Row Addition Logic --- ####
  observeEvent(input$add_control, {
    controls(rbind(controls(), data.frame(stimulus="", qty_ug="", stringsAsFactors=F))) #
    counters$control <- nrow(controls()) #
  })
  
  observeEvent(input$add_test, {
    test_stimuli(rbind(test_stimuli(), data.frame(stimulus="", qty_ug="", stringsAsFactors=F))) #
    counters$test <- nrow(test_stimuli()) #
  })
  
  observeEvent(input$add_terminal, {
    terminal_stimuli(rbind(terminal_stimuli(), data.frame(stimulus="", qty_ug="", stringsAsFactors=F))) #
    counters$terminal <- nrow(terminal_stimuli()) #
  })
  
  # --- 3.5: Input Synchronization --- ####
  observe({
    if (counters$control > 0) {
      names <- sapply(1:counters$control, function(i) input[[paste0("ctrl_n_", i)]]) #
      qtys  <- sapply(1:counters$control, function(i) input[[paste0("ctrl_q_", i)]]) #
      if (!any(sapply(names, is.null))) {
        controls(data.frame(stimulus = as.character(names), qty_ug = as.character(qtys), stringsAsFactors = FALSE)) #
      }
    }
    if (counters$test > 0) {
      names <- sapply(1:counters$test, function(i) input[[paste0("tst_n_", i)]]) #
      qtys  <- sapply(1:counters$test, function(i) input[[paste0("tst_q_", i)]]) #
      if (!any(sapply(names, is.null))) {
        test_stimuli(data.frame(stimulus = as.character(names), qty_ug = as.character(qtys), stringsAsFactors = FALSE)) #
      }
    }
    if (counters$terminal > 0) {
      names <- sapply(1:counters$terminal, function(i) input[[paste0("term_n_", i)]]) #
      qtys  <- sapply(1:counters$terminal, function(i) input[[paste0("term_q_", i)]]) #
      if (!any(sapply(names, is.null))) {
        terminal_stimuli(data.frame(stimulus = as.character(names), qty_ug = as.character(qtys), stringsAsFactors = FALSE)) #
      }
    }
  })
  
  # --- 3.6: Panel Export Handler --- ####
  output$download_panel <- downloadHandler(
    filename = function() { paste0("stimuli_panel_", format(Sys.time(), "%Y.%m.%d_%H.%M"), ".tsv") },
    content = function(file) {
      c_df <- controls() %>% filter(stimulus != "") %>% mutate(stimulus_type = "control")
      t_df <- test_stimuli() %>% filter(stimulus != "") %>% mutate(stimulus_type = "test")
      tm_df <- terminal_stimuli() %>% filter(stimulus != "") %>% mutate(stimulus_type = "terminal")
      
      export_df <- bind_rows(c_df, t_df, tm_df) %>%
        select(stimulus_type, stimulus = stimulus, quantity_ug = qty_ug)
      
      write.table(export_df, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
  
  # --- 3.7: Randomization Engine --- ####
  final_data <- eventReactive(input$generate, {
    c_list <- controls() %>% 
      filter(stimulus != "") %>%
      mutate(qty_ug = as.numeric(qty_ug), Category = "Control") #
    
    t_raw <- test_stimuli() %>% 
      filter(stimulus != "") %>%
      mutate(qty_ug = as.numeric(qty_ug)) #
    
    if(nrow(t_raw) > 0) {
      u_names <- sample(unique(t_raw$stimulus)) #
      t_final <- t_raw %>% 
        mutate(stimulus = factor(stimulus, levels = u_names)) %>%
        arrange(stimulus, qty_ug) %>%
        mutate(stimulus = as.character(stimulus), Category = "Test") #
    } else {
      t_final <- data.frame(stimulus=character(0), qty_ug=numeric(0), Category=character(0)) #
    }
    
    tm_list <- terminal_stimuli() %>% 
      filter(stimulus != "") %>%
      mutate(qty_ug = as.numeric(qty_ug), Category = "Terminal") #
    
    bind_rows(c_list, t_final, tm_list) #
  })
  
  # --- 3.8: Output Rendering --- ####
  output$preview_table <- renderDT({
    req(final_data()) #
    # Dynamic timestamp for exports
    ts <- format(Sys.time(), "%Y.%m.%d_%H.%M")
    fname <- paste0("randomized_stimuli_panel_", ts)
    
    datatable(final_data(), 
              extensions = 'Buttons', #
              options = list(
                dom = 'Bfrtip', #
                pageLength = -1, #
                buttons = list(
                  list(extend = 'copy', title = NULL, exportOptions = list(rownames = FALSE)), #
                  list(extend = 'csv', text = 'TSV', fieldSeparator = '\t', 
                       extension = '.tsv', filename = fname, 
                       title = NULL, exportOptions = list(rownames = FALSE)), #
                  list(extend = 'excel', filename = fname,
                       title = NULL, exportOptions = list(rownames = FALSE)) #
                )
              ),
              rownames = FALSE) #
  })
  
  output$summary_stats <- renderText({
    d <- tryCatch(final_data(), error = function(e) NULL) #
    if (is.null(d)) return("Click 'Generate' to see the summary.") #
    paste0(
      "Total stimulations: ", nrow(d), "\n",
      "Unique stimuluss:    ", length(unique(d$stimulus)), "\n",
      "Controls:           ", sum(d$Category == "Control"), "\n",
      "Tests:              ", sum(d$Category == "Test"), "\n",
      "Terminals:          ", sum(d$Category == "Terminal") #
    )
  })
}