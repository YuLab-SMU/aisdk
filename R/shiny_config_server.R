#' @title API Configuration Server
#' @description
#' Server logic for the API Configuration UI.
#'
#' @param id The namespace ID for the module.
#' @return NULL
#' @export
apiConfigServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reactive State ---
    
    # Current selected provider
    current_provider <- shiny::reactiveVal("openai")
    
    # Current configuration values (loaded from env on start)
    config_values <- shiny::reactiveValues(
      openai_api_key = Sys.getenv("OPENAI_API_KEY"),
      openai_model = get_openai_model(),
      openai_base_url = Sys.getenv("OPENAI_BASE_URL"),
      
      anthropic_api_key = Sys.getenv("ANTHROPIC_API_KEY"),
      anthropic_model = get_anthropic_model(),
      anthropic_base_url = Sys.getenv("ANTHROPIC_BASE_URL"),
      
      nvidia_api_key = Sys.getenv("NVIDIA_API_KEY"),
      nvidia_model = Sys.getenv("NVIDIA_MODEL") %||% "z-ai/glm4.7",
      nvidia_base_url = Sys.getenv("NVIDIA_BASE_URL"),
      
      deepseek_api_key = Sys.getenv("DEEPSEEK_API_KEY"),
      deepseek_model = Sys.getenv("DEEPSEEK_MODEL") %||% "deepseek-chat",
      deepseek_base_url = Sys.getenv("DEEPSEEK_BASE_URL") %||% "https://api.deepseek.com",
      
      groq_api_key = Sys.getenv("GROQ_API_KEY"),
      groq_model = Sys.getenv("GROQ_MODEL") %||% "llama3-8b-8192",
      groq_base_url = Sys.getenv("GROQ_BASE_URL") %||% "https://api.groq.com/openai/v1"
    )
    
    # --- Navigation Handling ---
    
    shiny::observeEvent(input$btn_openai, {
      current_provider("openai")
      shinyjs::runjs(sprintf("$('.list-group-item').removeClass('active'); $('#%s').addClass('active');", ns("btn_openai")))
    })
    
    shiny::observeEvent(input$btn_anthropic, {
      current_provider("anthropic")
      shinyjs::runjs(sprintf("$('.list-group-item').removeClass('active'); $('#%s').addClass('active');", ns("btn_anthropic")))
    })
    
    shiny::observeEvent(input$btn_nvidia, {
      current_provider("nvidia")
      shinyjs::runjs(sprintf("$('.list-group-item').removeClass('active'); $('#%s').addClass('active');", ns("btn_nvidia")))
    })
    
    shiny::observeEvent(input$btn_deepseek, {
      current_provider("deepseek")
      shinyjs::runjs(sprintf("$('.list-group-item').removeClass('active'); $('#%s').addClass('active');", ns("btn_deepseek")))
    })
    
    shiny::observeEvent(input$btn_groq, {
      current_provider("groq")
      shinyjs::runjs(sprintf("$('.list-group-item').removeClass('active'); $('#%s').addClass('active');", ns("btn_groq")))
    })
    
    shiny::observeEvent(input$btn_custom, {
      current_provider("custom")
      shinyjs::runjs(sprintf("$('.list-group-item').removeClass('active'); $('#%s').addClass('active');", ns("btn_custom")))
    })
    
    # --- UI Rendering ---
    
    output$current_provider_title <- shiny::renderUI({
      provider <- current_provider()
      if (provider == "openai") {
        shiny::tagList(shiny::icon("robot"), " OpenAI Configuration")
      } else if (provider == "anthropic") {
        shiny::tagList(shiny::icon("brain"), " Anthropic Configuration")
      } else if (provider == "nvidia") {
        shiny::tagList(shiny::icon("bolt"), " NVIDIA Configuration")
      } else if (provider == "deepseek") {
        shiny::tagList(shiny::icon("search"), " DeepSeek Configuration")
      } else if (provider == "groq") {
        shiny::tagList(shiny::icon("microchip"), " Groq Configuration")
      } else {
        shiny::tagList(shiny::icon("cogs"), " Custom Provider")
      }
    })
    
    output$provider_settings_ui <- shiny::renderUI({
      render_provider_settings_ui(id, current_provider(), config_values)
    })
    
    # --- Model Fetching ---
    
    # --- Model Selection Logic ---
    
    # Reactive value to store the currently processing provider context
    active_selection_ctx <- shiny::reactiveValues(prefix = NULL, data = NULL)
    
    # Generic Modal Show
    observe_select_click <- function(provider, prefix) {
      shiny::observeEvent(input[[paste0(prefix, "_select")]], {
        # Check if DT is available, otherwise fall back
        dt_output <- if (requireNamespace("DT", quietly = TRUE)) DT::DTOutput else shiny::dataTableOutput
        # Get current values
        key <- input[[paste0(prefix, "_api_key")]]
        base <- input[[paste0(prefix, "_base_url")]]
        
        # Helper to fetch and store data
        fetch_data <- function() {
           if (is.null(key) || key == "") return(data.frame(id = "Please enter API Key first"))
           df <- fetch_api_models(provider, key, base)
           if (nrow(df) == 0) return(data.frame(id = "No models found"))
           # Store for retrieval on selection
           active_selection_ctx$data <- df
           active_selection_ctx$prefix <- prefix
           df
        }

        # Show modal
        shiny::showModal(shiny::modalDialog(
          title = paste("Select Model -", toupper(provider)),
          size = "l",
          shiny::div(
            class = "mb-3",
            shiny::p("Select a model from the list below:"),
            dt_output(ns("model_selection_table"))
          ),
          footer = shiny::modalButton("Cancel")
        ))
        
        # Render table
        output$model_selection_table <- DT::renderDT({
          df <- fetch_data()
          DT::datatable(df, selection = "single", options = list(pageLength = 15, scrollX = TRUE))
        })
      })
    }
    
    # Register observers
    observe_select_click("openai", "openai")
    observe_select_click("anthropic", "anthropic")
    observe_select_click("nvidia", "nvidia")
    observe_select_click("deepseek", "deepseek")
    observe_select_click("groq", "groq")
    
    # Handle selection
    shiny::observeEvent(input$model_selection_table_rows_selected, {
      shiny::req(active_selection_ctx$prefix)
      idx <- input$model_selection_table_rows_selected
      
      if (!is.null(idx) && length(idx) > 0) {
        # Retrieve data
        df <- active_selection_ctx$data
        if (!is.null(df) && nrow(df) >= idx) {
           selected_model <- df$id[idx]
           # Update the input
           shiny::updateSelectizeInput(session, paste0(active_selection_ctx$prefix, "_model"), selected = selected_model)
           shiny::removeModal()
           shiny::showNotification(paste("Selected model:", selected_model))
        }
      }
    })
    
    # --- Save Handling ---
    
    shiny::observeEvent(input$save_all, {
      shiny::req(current_provider())
      
      updates <- list()
      msg <- ""
      
      if (current_provider() == "openai") {
        updates[["OPENAI_API_KEY"]] <- input$openai_api_key
        updates[["OPENAI_MODEL"]] <- input$openai_model
        updates[["OPENAI_BASE_URL"]] <- input$openai_base_url
        msg <- "OpenAI configuration saved."
      } else if (current_provider() == "anthropic") {
        updates[["ANTHROPIC_API_KEY"]] <- input$anthropic_api_key
        updates[["ANTHROPIC_MODEL"]] <- input$anthropic_model
        updates[["ANTHROPIC_BASE_URL"]] <- input$anthropic_base_url
        msg <- "Anthropic configuration saved."
      } else if (current_provider() == "nvidia") {
        updates[["NVIDIA_API_KEY"]] <- input$nvidia_api_key
        updates[["NVIDIA_MODEL"]] <- input$nvidia_model
        updates[["NVIDIA_BASE_URL"]] <- input$nvidia_base_url
        msg <- "NVIDIA configuration saved."
      } else if (current_provider() == "deepseek") {
        updates[["DEEPSEEK_API_KEY"]] <- input$deepseek_api_key
        updates[["DEEPSEEK_MODEL"]] <- input$deepseek_model
        updates[["DEEPSEEK_BASE_URL"]] <- input$deepseek_base_url
        msg <- "DeepSeek configuration saved."
      } else if (current_provider() == "groq") {
        updates[["GROQ_API_KEY"]] <- input$groq_api_key
        updates[["GROQ_MODEL"]] <- input$groq_model
        updates[["GROQ_BASE_URL"]] <- input$groq_base_url
        msg <- "Groq configuration saved."
      }
      
      # Filter out NULLs
      updates <- updates[!vapply(updates, is.null, logical(1))]
      
      if (length(updates) > 0) {
        # Update .Renviron
        tryCatch({
          update_renviron(updates)
          
          # Update internal reactive values to match inputs
          for (key in names(updates)) {
             # Map env var back to config_values names
             if (key == "OPENAI_API_KEY") config_values$openai_api_key <- updates[[key]]
             if (key == "OPENAI_MODEL") config_values$openai_model <- updates[[key]]
             if (key == "OPENAI_BASE_URL") config_values$openai_base_url <- updates[[key]]
             if (key == "ANTHROPIC_API_KEY") config_values$anthropic_api_key <- updates[[key]]
             if (key == "ANTHROPIC_MODEL") config_values$anthropic_model <- updates[[key]]
             if (key == "ANTHROPIC_BASE_URL") config_values$anthropic_base_url <- updates[[key]]
             if (key == "NVIDIA_API_KEY") config_values$nvidia_api_key <- updates[[key]]
             if (key == "NVIDIA_MODEL") config_values$nvidia_model <- updates[[key]]
             if (key == "NVIDIA_BASE_URL") config_values$nvidia_base_url <- updates[[key]]
             if (key == "DEEPSEEK_API_KEY") config_values$deepseek_api_key <- updates[[key]]
             if (key == "DEEPSEEK_MODEL") config_values$deepseek_model <- updates[[key]]
             if (key == "DEEPSEEK_BASE_URL") config_values$deepseek_base_url <- updates[[key]]
             if (key == "GROQ_API_KEY") config_values$groq_api_key <- updates[[key]]
             if (key == "GROQ_MODEL") config_values$groq_model <- updates[[key]]
             if (key == "GROQ_BASE_URL") config_values$groq_base_url <- updates[[key]]
          }
          
          shiny::showNotification(msg, type = "message")
        }, error = function(e) {
          shiny::showNotification(paste("Error saving:", e$message), type = "error")
        })
      } else {
        shiny::showNotification("No changes detected or supported for this provider.", type = "warning")
      }
    })
    
    # Refresh
    shiny::observeEvent(input$refresh, {
      reload_env()
      # Reload values
      config_values$openai_api_key <- Sys.getenv("OPENAI_API_KEY")
      config_values$openai_model <- get_openai_model()
      config_values$anthropic_api_key <- Sys.getenv("ANTHROPIC_API_KEY")
      config_values$anthropic_model <- get_anthropic_model()
      config_values$nvidia_api_key <- Sys.getenv("NVIDIA_API_KEY")
      config_values$nvidia_model <- Sys.getenv("NVIDIA_MODEL") %||% "z-ai/glm4.7"
      config_values$deepseek_api_key <- Sys.getenv("DEEPSEEK_API_KEY")
      config_values$groq_api_key <- Sys.getenv("GROQ_API_KEY")
      
      shiny::showNotification("Configuration reloaded from disk.", type = "message")
    })
  })
}
