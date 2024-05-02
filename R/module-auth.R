
#' Authentication module
#'
#' @param id Module's id.
#' @param status Bootstrap status to use for the panel and the button.
#'  Valid status are: \code{"default"}, \code{"primary"}, \code{"success"},
#'  \code{"warning"}, \code{"danger"}.
#' @param tags_top A \code{tags (div, img, ...)} to be displayed on top of the authentication module.
#' @param tags_bottom A \code{tags (div, img, ...)} to be displayed on bottom of the authentication module.
#' @param background A optionnal \code{css} for authentication background. See example.
#' @param choose_language \code{logical/character}. Add language selection on top ? TRUE for all supported languages
#' or a vector of possibilities like \code{c("en", "fr", "pt-BR", "es", "de", "pl", "ja", "el", "id", "zh-CN")}. If enabled, \code{input$shinymanager_language} is created
#' @param ... : Used for old version compatibility.
#'
#'
#' @export
#'
#' @name module-authentication
#'
#' @importFrom htmltools tagList tags singleton
#' @importFrom shiny NS fluidRow column textInput passwordInput actionButton uiOutput
#'
#' @example examples/module-auth.R
auth_ui <- function(id, status = "primary", tags_top = NULL,
                    tags_bottom = NULL, background = NULL,
                    choose_language = NULL, lan = NULL, ...) {
  
  ns <- NS(id)
  
  if(is.null(lan)){
    lan <- use_language()
  }
  
  # patch / message changing tag_img & tag_div
  deprecated <- list(...)
  if("tag_img" %in% names(deprecated)){
    tags_top <- deprecated$tag_img
    warning("'tag_img' (auth_ui, secure_app) is now deprecated. Please use 'tags_top'", call. = FALSE)
  }
  if("tag_div" %in% names(deprecated)){
    tags_bottom <- deprecated$tag_div
    warning("'tag_div' (auth_ui, secure_app) is now deprecated. Please use 'tags_bottom'", call. = FALSE)
  }
  tagList(
    singleton(tags$head(
      tags$link(href="shinymanager/styles-auth.css", rel="stylesheet"),
      tags$script(src = "shinymanager/bindEnter.js"),
      if(!is.null(background)){
        background <- gsub(";$", "", background)
        tags$style(HTML(paste0(".panel-auth {background:", background, ",#FFF;}")))
      }
    )),
    tags$div(
      id = ns("auth-mod"), class = "panel-auth",
      tags$br(), tags$div(style = "height: 70px;"), tags$br(),
      fluidRow(
        column(
          width = 4, offset = 4,
          tags$div(
            class = paste0("panel panel-", status),
            tags$div(
              class = "panel-body",
              {
                
                choices = lan$get_language()
                lan_registered <- lan$get_language_registered()
                if(is.logical(choose_language) && choose_language){
                  choices = unname(lan$get_language_registered())
                } else if(is.character(choose_language)){
                  choices = unique(c(intersect(choose_language, unname(lan$get_language_registered())), lan$get_language()))
                }
                
                names(choices) <- choices
                for(i in 1:length(choices)){
                  ind <- which(lan_registered %in% choices[i])
                  if(length(ind) > 0){
                    names(choices)[i] <- names(lan_registered)[ind]
                  }
                }
                selected = ifelse(lan$get_language() %in% choices,
                                  lan$get_language(),
                                  choices[1])
                if(length(choices) == 1){
                  style = "display:none"
                } else {
                  style = "margin-bottom:-50px;"
                }
                tags$div(style = style,
                         fluidRow(
                           column(width = 4, offset = 4, uiOutput(ns("label_language"))),
                           column(4,
                                  tags$div(
                                    style = "text-align: left; font-size: 12px;",
                                    selectInput(
                                      inputId = ns("language"),
                                      label = NULL,
                                      choices = choices,
                                      selected = selected,
                                      width = "100%"
                                    )
                                  )
                           )
                         )
                )
              },
              tags$div(
                style = "text-align: center;",
                if (!is.null(tags_top)) tags_top,
                tags$h3(lan$get("Please authenticate"), id = ns("shinymanager-auth-head"))
              ),
              tags$br(),
              textInput(
                inputId = ns("user_id"),
                label = lan$get("Username:"),
                width = "100%"
              ),
              passwordInput(
                inputId = ns("user_pwd"),
                label = lan$get("Password:"),
                width = "100%"
              ),
              tags$br(),
              tags$div(
                id = ns("container-btn-ok"),
                actionButton(
                  inputId = ns("go_auth"),
                  label = lan$get("Login"),
                  width = "100%",
                  class = paste0("btn-", status)
                ),
                tags$br(), tags$br()
              ),
              tags$br(),
              tags$script(
                sprintf("bindEnter('%s');", ns(""))
              ),
              tags$div(id = ns("result_auth")),
              if (!is.null(tags_bottom)) tags$div(style = "margin-top:-10px;", tags$hr()), tags_bottom,
              uiOutput(ns("update_shinymanager_language"))
            )
          )
        )
      )
    )
  )
}



#' @param input,output,session Standard Shiny server arguments.
#' @param check_credentials Function with two arguments (\code{user},
#'  the username provided by the user and \code{password}, his/her password).
#'  Must return a \code{list} with at least 2 (or 4 in case of sqlite) slots :
#'  \itemize{
#'   \item \strong{result} : logical, result of authentication.
#'   \item \strong{user_info} : \code{list}. What you want about user ! (sqlite : the line in \code{db} corresponding to the user).
#'   \item \strong{expired} : logical, is user has expired ? Always \code{FALSE} if \code{db} doesn't have a \code{expire} column. Optional.
#'   \item \strong{authorized} : logical, is user can access to his app ? Always \code{TRUE} if \code{db} doesn't have a \code{applications} column. Optional.
#'  }
#'
#' @param use_token Add a token in the URL to check authentication. Should not be used directly.
#' @param lan A language object. See  \code{\link{use_language}}
#'
#' @export
#'
#' @rdname module-authentication
#'
#' @return A \code{reactiveValues} with 3 slots :
#'  \itemize{
#'   \item \strong{result} : logical, result of authentication.
#'   \item \strong{user} : character, name of connected user.
#'   \item \strong{user_info} : information about the user.
#'  }
#'
#' @importFrom htmltools tags
#' @importFrom shiny reactiveValues observeEvent removeUI updateQueryString insertUI is.reactive icon updateActionButton updateTextInput renderUI
#' @importFrom stats setNames
auth_server <- function(input, output, session,
                        check_credentials,
                        use_token = FALSE, lan = NULL) {
  
  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }
  
  
  if(!is.reactive(lan)){
    if(is.null(lan)){
      lan <- reactiveVal(use_language())
    } else {
      lan <- reactiveVal(lan)
    }
  }
  
  
  observe({
    session$sendCustomMessage(
      type = "focus_input",
      message = list(inputId = ns("user_id"))
    )
  })
  
  observe({
    if(!is.null(input$language)){
      lan()$set_language(input$language)
      updateTextInput(session, inputId = "user_id", label = lan()$get("Username:"))
      updateTextInput(session, inputId = "user_pwd", label = lan()$get("Password:"))
      updateActionButton(session, inputId = "go_auth", label = lan()$get("Login"))
      
      session$sendCustomMessage(
        type = "update_auth_title",
        message = list(
          inputId = ns("shinymanager-auth-head"),
          title = lan()$get("Please authenticate")
        )
      )
      
      output$update_shinymanager_language <- renderUI({
        shinymanager_language(lan()$get_language())
      })
      
      output$label_language <- renderUI({
        tags$p(paste0(lan()$get("Language"), " :"),
               style = "text-align: right; font-style: italic; margin-top:5px")
      })
      
    }
  })
  
  
  authentication <- reactiveValues(result = FALSE, user = NULL, user_info = NULL)
  
  observeEvent(input$go_auth, {
    removeUI(selector = jns("msg_auth"))
    
    insertUI(
      selector = jns("container-btn-ok"),
      ui = tags$div(
        id = ns("spinner_msg_ok"),
        img(src = "shinymanager/1497.gif", style = "height:30px;"), 
        align = "center"
      ),
      immediate = TRUE 
    )
    
    res_auth <- check_credentials(input$user_id, input$user_pwd)
    
    # locked account ?
    locked <- FALSE
    pwd_failure_limit <- as.numeric(get_pwd_failure_limit())
    if(length(pwd_failure_limit) > 0 && !is.na(pwd_failure_limit) && !is.infinite(pwd_failure_limit)){
      locked <- check_locked_account(input$user_id, pwd_failure_limit)
    }
    
    if (isTRUE(res_auth$result) & !locked) {
      removeUI(selector = jns("auth-mod"))
      authentication$result <- TRUE
      authentication$user <- input$user_id
      authentication$user_info <- res_auth$user_info
      # token <- generate_token(input$user_id)
      token <- .tok$generate(input$user_id)
      
      if (isTRUE(use_token)) {
        # add_token(token, as.list(res_auth$user_info))
        .tok$add(token, as.list(res_auth$user_info))
        addAuthToQuery(session, token, lan()$get_language())
        session$reload()
      }
      
    } else if (isTRUE(res_auth$result) & locked) {
      
      save_logs_failed(input$user_id, status = "Locked Account")
      
      insertUI(
        selector = jns("result_auth"),
        ui = tags$div(
          id = ns("msg_auth"), class = "alert alert-danger",
          icon("triangle-exclamation"), lan()$get("Your account is locked")
        )
      )
      
    } else {
      if (is.null(res_auth$user_info)) {
        save_logs_failed(input$user_id, status = "Unknown user")
        insertUI(
          selector = jns("result_auth"),
          ui = tags$div(
            id = ns("msg_auth"), class = "alert alert-danger",
            icon("triangle-exclamation"), lan()$get("Username or password are incorrect")
          )
        )
      } else if (isTRUE(res_auth$expired)) {
        save_logs_failed(input$user_id, status = "Expired")
        insertUI(
          selector = jns("result_auth"),
          ui = tags$div(
            id = ns("msg_auth"), class = "alert alert-danger",
            icon("triangle-exclamation"), lan()$get("Your account has expired")
          )
        )
      } else {
        if (!isTRUE(res_auth$authorized)) {
          save_logs_failed(input$user_id, status = "Unauthorized")
          insertUI(
            selector = jns("result_auth"),
            ui = tags$div(
              id = ns("msg_auth"), class = "alert alert-danger",
              icon("triangle-exclamation"), lan()$get("You are not authorized for this application")
            )
          )
        } else {
          
          save_logs_failed(input$user_id, status = "Wrong pwd")
          
          if(!locked){
            insertUI(
              selector = jns("result_auth"),
              ui = tags$div(
                id = ns("msg_auth"), class = "alert alert-danger",
                icon("triangle-exclamation"), lan()$get("Username or password are incorrect")
              )
            )
          } else {
            insertUI(
              selector = jns("result_auth"),
              ui = tags$div(
                id = ns("msg_auth"), class = "alert alert-danger",
                icon("triangle-exclamation"), lan()$get("Your account is locked")
              )
            )
          }
        }
      }
    }
    
    removeUI(selector = jns("spinner_msg_ok"))
    
  }, ignoreInit = TRUE)
  
  return(authentication)
}




