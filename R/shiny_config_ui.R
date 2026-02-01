#' @title API Configuration UI
#' @description
#' Creates a Shiny UI for configuring API providers.
#'
#' @param id The namespace ID for the module.
#' @return A Shiny UI definition.
#' @export
apiConfigUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # Use bslib for modern theming
    bslib::page_fluid(
      theme = bslib::bs_theme(version = 5, bootswatch = "darkly"),
      shinyjs::useShinyjs(),
      
      shiny::div(
        class = "container-fluid p-4",
        style = "max-width: 1200px;",
        
        # Header
        shiny::div(
          class = "d-flex justify-content-between align-items-center mb-4",
          shiny::h2("API Configuration", class = "mb-0"),
          shiny::div(
            shiny::actionButton(ns("refresh"), "Refresh", icon = shiny::icon("sync"), class = "btn-secondary me-2"),
            shiny::actionButton(ns("save_all"), "Save Changes", icon = shiny::icon("save"), class = "btn-primary")
          )
        ),
        
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Providers",
            width = 300,
            shiny::div(
              class = "list-group",
              shiny::actionButton(ns("btn_openai"), "OpenAI", class = "list-group-item list-group-item-action active"),
              shiny::actionButton(ns("btn_anthropic"), "Anthropic", class = "list-group-item list-group-item-action"),
              shiny::actionButton(ns("btn_nvidia"), "NVIDIA", class = "list-group-item list-group-item-action"),
              shiny::actionButton(ns("btn_deepseek"), "DeepSeek", class = "list-group-item list-group-item-action"),
              shiny::actionButton(ns("btn_groq"), "Groq", class = "list-group-item list-group-item-action"),
              shiny::actionButton(ns("btn_gemini"), "Google Gemini", class = "list-group-item list-group-item-action disabled"),
              shiny::actionButton(ns("btn_custom"), "Add Custom Provider", class = "list-group-item list-group-item-action mt-3 border-top")
            )
          ),
          
          # Main Content
          bslib::card(
            height = "100%",
            min_height = "600px",
            bslib::card_header(
              shiny::uiOutput(ns("current_provider_title"))
            ),
            bslib::card_body(
              shiny::uiOutput(ns("provider_settings_ui"))
            )
          )
        )
      )
    )
  )
}

#' @keywords internal
render_provider_settings_ui <- function(id, provider_name, current_config) {
  ns <- shiny::NS(id)
  
  # Helper to render model input with Select button
  model_input_ui <- function(prefix, current_val, base_url_val = NULL) {
    # Check if DT is available, otherwise fall back (though we added it to Suggests)
    dt_output <- if (requireNamespace("DT", quietly = TRUE)) DT::DTOutput else shiny::dataTableOutput
    
    shiny::div(
      class = "mb-3",
      shiny::tags$label(paste("Default Model"), `for` = ns(paste0(prefix, "_model"))),
      shiny::div(
        class = "d-flex gap-2",
        shiny::div(
          style = "flex-grow: 1;",
          shiny::selectizeInput(
            ns(paste0(prefix, "_model")),
            label = NULL,
            choices = current_val,
            selected = current_val,
            multiple = FALSE,
            options = list(create = TRUE, placeholder = "Select or type model name")
          )
        ),
        shiny::actionButton(
          ns(paste0(prefix, "_select")),
          "Select",
          icon = shiny::icon("list"),
          class = "btn-outline-primary",
          style = "height: 38px;"
        )
      )
    )
  }
  
  if (provider_name == "openai") {
    shiny::tagList(
      shiny::div(
        class = "row g-3",
        shiny::div(
          class = "col-12",
          bslib::card(
            class = "mb-3",
            bslib::card_header("Authentication"),
            bslib::card_body(
              shiny::passwordInput(
                ns("openai_api_key"),
                "API Key",
                value = current_config$openai_api_key,
                placeholder = "sk-..."
              ),
              shiny::helpText("Get your API key from platform.openai.com")
            )
          )
        ),
        shiny::div(
          class = "col-12",
          bslib::card(
            class = "mb-3",
            bslib::card_header("Model Settings"),
            bslib::card_body(
              model_input_ui("openai", current_config$openai_model),
              shiny::textInput(
                ns("openai_base_url"),
                "Base URL (Optional)",
                value = current_config$openai_base_url,
                placeholder = "https://api.openai.com/v1"
              )
            )
          )
        )
      )
    )
  } else if (provider_name == "anthropic") {
    shiny::tagList(
      shiny::div(
        class = "row g-3",
        shiny::div(
          class = "col-12",
          bslib::card(
            class = "mb-3",
            bslib::card_header("Authentication"),
            bslib::card_body(
              shiny::passwordInput(
                ns("anthropic_api_key"),
                "API Key",
                value = current_config$anthropic_api_key,
                placeholder = "sk-ant-..."
              ),
              shiny::helpText("Get your API key from console.anthropic.com")
            )
          )
        ),
        shiny::div(
          class = "col-12",
          bslib::card(
            class = "mb-3",
            bslib::card_header("Model Settings"),
            bslib::card_body(
              model_input_ui("anthropic", current_config$anthropic_model),
              shiny::textInput(
                ns("anthropic_base_url"),
                "Base URL (Optional)",
                value = current_config$anthropic_base_url,
                placeholder = "https://api.anthropic.com"
              )
            )
          )
        )
      )
    )
  } else if (provider_name == "nvidia") {
     shiny::tagList(
      shiny::div(
        class = "row g-3",
        shiny::div(
          class = "col-12",
          bslib::card(
            class = "mb-3",
            bslib::card_header("Authentication"),
            bslib::card_body(
              shiny::passwordInput(
                ns("nvidia_api_key"),
                "API Key",
                value = current_config$nvidia_api_key,
                placeholder = "nvapi-..."
              ),
              shiny::helpText("Get your API key from build.nvidia.com")
            )
          )
        ),
        shiny::div(
          class = "col-12",
          bslib::card(
            class = "mb-3",
            bslib::card_header("Model Settings"),
            bslib::card_body(
              model_input_ui("nvidia", current_config$nvidia_model),
              shiny::textInput(
                ns("nvidia_base_url"),
                "Base URL (Optional)",
                value = current_config$nvidia_base_url,
                placeholder = "https://integrate.api.nvidia.com/v1"
              )
            )
          )
        )
      )
    )
  } else if (provider_name == "deepseek") {
     shiny::tagList(
      shiny::div(
        class = "row g-3",
        shiny::div(
          class = "col-12",
          bslib::card(
            class = "mb-3",
            bslib::card_header("Authentication"),
            bslib::card_body(
              shiny::passwordInput(
                ns("deepseek_api_key"),
                "API Key",
                value = current_config$deepseek_api_key,
                placeholder = "sk-..."
              ),
              shiny::helpText("Get your API key from platform.deepseek.com")
            )
          )
        ),
        shiny::div(
          class = "col-12",
          bslib::card(
            class = "mb-3",
            bslib::card_header("Model Settings"),
            bslib::card_body(
              model_input_ui("deepseek", current_config$deepseek_model),
              shiny::textInput(
                ns("deepseek_base_url"),
                "Base URL (Optional)",
                value = current_config$deepseek_base_url,
                placeholder = "https://api.deepseek.com"
              )
            )
          )
        )
      )
    )
  } else if (provider_name == "groq") {
     shiny::tagList(
      shiny::div(
        class = "row g-3",
        shiny::div(
          class = "col-12",
          bslib::card(
            class = "mb-3",
            bslib::card_header("Authentication"),
            bslib::card_body(
              shiny::passwordInput(
                ns("groq_api_key"),
                "API Key",
                value = current_config$groq_api_key,
                placeholder = "gsk_..."
              ),
              shiny::helpText("Get your API key from console.groq.com")
            )
          )
        ),
        shiny::div(
          class = "col-12",
          bslib::card(
            class = "mb-3",
            bslib::card_header("Model Settings"),
            bslib::card_body(
              model_input_ui("groq", current_config$groq_model),
              shiny::textInput(
                ns("groq_base_url"),
                "Base URL (Optional)",
                value = current_config$groq_base_url,
                placeholder = "https://api.groq.com/openai/v1"
              )
            )
          )
        )
      )
    )
  } else if (provider_name == "custom") {
    shiny::tagList(
      shiny::div(
        class = "alert alert-info",
        "Custom provider configuration is coming soon."
      )
    )
  } else {
    shiny::div("Select a provider to configure.")
  }
}
