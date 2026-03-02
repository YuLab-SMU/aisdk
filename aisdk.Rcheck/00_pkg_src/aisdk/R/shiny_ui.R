#' @title AI Chat UI
#' @description
#' Creates a modern, streaming-ready chat interface for Shiny applications.
#'
#' @param id The namespace ID for the module.
#' @param height Height of the chat window (e.g. "400px").
#' @return A Shiny UI definition.
#' @export
aiChatUI <- function(id, height = "500px") {
  # Check for suggested packages
  rlang::check_installed(c("shiny", "bslib", "commonmark"))
  
  ns <- shiny::NS(id)
  
  # Note: This UI requires Bootstrap 5 (provided by bslib::bs_theme(version = 5))
  # Standard Shiny uses Bootstrap 3 by default which may break the layout.
  
  # CSS for the chat interface
  # We embed this directly for now, later could move to inst/www
  chat_css <- "
    .chat-container {
      display: flex;
      flex-direction: column;
      height: 100%;
      border: 1px solid var(--bs-border-color, #e5e7eb);
      border-radius: 0.75rem;
      background-color: var(--bs-body-bg, #ffffff);
      box-shadow: 0 10px 30px rgba(15, 23, 42, 0.08);
      overflow: hidden;
    }
    .chat-messages {
      flex-grow: 1;
      overflow-y: auto;
      padding: 1.25rem;
      display: flex;
      flex-direction: column;
      gap: 0.85rem;
      background: var(--bs-body-bg, #ffffff);
    }
    .chat-input-area {
      padding: 0.85rem 1rem 1rem;
      border-top: 1px solid var(--bs-border-color, #e5e7eb);
      background-color: var(--bs-tertiary-bg, #f8fafc);
      border-bottom-left-radius: 0.75rem;
      border-bottom-right-radius: 0.75rem;
    }
    .message {
      max-width: 78%;
      padding: 0.7rem 0.95rem;
      border-radius: 1rem;
      position: relative;
      font-size: 1.05rem;
      line-height: 1.45;
      word-wrap: break-word;
      box-shadow: 0 2px 10px rgba(15, 23, 42, 0.08);
    }
    .message.user {
      align-self: flex-end;
      background: var(--bs-primary, #2563eb);
      color: var(--bs-white, #ffffff);
      border-bottom-right-radius: 0.4rem;
    }
    .message.assistant {
      align-self: flex-start;
      background-color: var(--bs-tertiary-bg, #f8fafc);
      color: var(--bs-body-color, #0f172a);
      border: 1px solid var(--bs-border-color, #e5e7eb);
      border-bottom-left-radius: 0.4rem;
    }
    .message p:last-child {
      margin-bottom: 0;
    }
    .message pre {
      background-color: var(--bs-dark, #0f172a);
      color: #e2e8f0;
      padding: 0.6rem 0.75rem;
      border-radius: 0.5rem;
      overflow-x: auto;
    }
    .message-content {
      white-space: pre-wrap;
    }
    .message-thinking {
      white-space: pre-wrap;
      color: var(--bs-secondary-color, #64748b);
      font-size: 0.95rem;
      padding: 0.4rem 0.6rem;
      border-radius: 0.6rem;
      background: rgba(148, 163, 184, 0.12);
      margin-bottom: 0.5rem;
    }
    .message-thinking:empty {
      display: none;
    }
    .message-thinking-block {
      margin-top: 0.6rem;
    }
    .message-thinking-block > summary {
      cursor: pointer;
      color: var(--bs-secondary-color, #64748b);
      font-size: 0.95rem;
    }
    .message.user pre {
      background-color: rgba(255, 255, 255, 0.18);
      color: #ffffff;
    }
    .chat-input-area textarea {
      border: 1px solid var(--bs-border-color, #e5e7eb);
      border-radius: 0.75rem;
      padding: 0.6rem 0.8rem;
      background: var(--bs-body-bg, #ffffff);
      font-size: 1rem;
    }
    .chat-input-area textarea:focus {
      border-color: var(--bs-primary, #2563eb);
      box-shadow: 0 0 0 0.2rem rgba(37, 99, 235, 0.2);
    }
      border-radius: 0.75rem;
      padding: 0.5rem 0.85rem;
    }
    .tool-execution {
      background-color: var(--bs-body-bg, #ffffff);
      border: 1px solid var(--bs-border-color, #e5e7eb);
      border-radius: 0.6rem;
      margin-top: 0.8rem;
      margin-bottom: 0.8rem;
      overflow: hidden;
      font-size: 0.9rem;
      box-shadow: 0 1px 3px rgba(0,0,0,0.05);
    }
    .tool-execution summary {
      padding: 0.6rem 0.8rem;
      cursor: pointer;
      background-color: var(--bs-secondary-bg, #f8fafc);
      color: var(--bs-secondary-color, #64748b);
      font-weight: 500;
      user-select: none;
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }
    .tool-execution summary:hover {
      background-color: var(--bs-tertiary-bg, #f1f5f9);
    }
    .tool-execution-body {
      padding: 0.8rem;
      border-top: 1px solid var(--bs-border-color, #e5e7eb);
      background-color: var(--bs-body-bg, #ffffff);
    }
    .tool-execution pre {
      margin: 0;
      background-color: var(--bs-dark, #0f172a);
      color: #e2e8f0;
      border-radius: 0.4rem;
      padding: 0.6rem;
    }
  "
  
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(chat_css)),
      # Prevent favicon 404
      shiny::tags$link(rel = "icon", href = "data:,") 
    ),
    
    bslib::card(
      height = height,
      full_screen = TRUE,
      class = "p-0", # Remove padding from card body to let chat fill it
      
      shiny::div(
        class = "chat-container",
        
        # Messages Area
        shiny::div(
          id = ns("chat_messages"),
          class = "chat-messages",
          # Initial welcome message could go here
          shiny::div(
            class = "message assistant",
            shiny::HTML("Hello! How can I help you today?")
          )
        ),
        
        # Input Area
        shiny::div(
          class = "chat-input-area",
          shiny::div(
            class = "d-flex gap-2",
            shiny::div(
              style = "flex-grow: 1;",
              shiny::textAreaInput(
                ns("user_input"),
                label = NULL,
                placeholder = "Type your message...",
                rows = 1,
                resize = "none" # We might want auto-resize JS later
              )
            ),
            shiny::actionButton(
              ns("send"),
              label = shiny::icon("paper-plane"),
              class = "btn-primary",
              style = "height: fit-content;"
            )
          ),
          shiny::helpText("Enter to send (Shift+Enter for new line)", class = "mb-0 mt-1 small")
        )
      )
    ),
    
    # JavaScript for scrolling and Enter key
    shiny::tags$script(shiny::HTML(sprintf("
      $('#%s').on('keydown', function(e) {
        if (e.keyCode === 13 && !e.shiftKey) {
          e.preventDefault();
          $('#%s').click();
        }
      });
      
      Shiny.addCustomMessageHandler('scroll_chat_%s', function(message) {
        var el = document.getElementById('%s');
        if (el) {
          el.scrollTop = el.scrollHeight;
        }
      });

      Shiny.addCustomMessageHandler('update_element', function(message) {
        var el = document.getElementById(message.id);
        if (el) {
          el.innerHTML = message.html;
        }
      });
      
      Shiny.addCustomMessageHandler('append_text', function(message) {
        var el = document.getElementById(message.id);
        if (!el) return;
        if (message.clear) {
          el.innerHTML = '';
        }
        el.appendChild(document.createTextNode(message.text));
      });
      
      Shiny.addCustomMessageHandler('append_html', function(message) {
        var el = document.getElementById(message.id);
        if (!el) return;
        // Append HTML to the end of the container (usually chat_messages)
        // If content_id is provided, append inside that
        if (message.content_id) {
           el = document.getElementById(message.content_id);
           if (!el) return;
        }
        
        var template = document.createElement('div');
        template.innerHTML = message.html;
        
        // If we simply append, it works
        while (template.firstChild) {
          el.appendChild(template.firstChild);
        }
      });
    ", ns("user_input"), ns("send"), id, ns("chat_messages"))))
  )
}
