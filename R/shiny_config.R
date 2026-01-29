#' @title Launch API Configuration App
#' @description
#' Launches a Shiny application to configure API providers and environment variables.
#'
#' @return A Shiny app object
#' @export
configure_api <- function() {
  rlang::check_installed(c("shiny", "bslib", "shinyjs"))
  
  ui <- apiConfigUI("config_module")
  
  server <- function(input, output, session) {
    apiConfigServer("config_module")
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
