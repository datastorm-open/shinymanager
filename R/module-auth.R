
#' Authentication module
#'
#' @param id Module's id.
#' @param labels Labels for text field, see \code{\link{auth_labels}}.
#' @param tag_img A \code{tags$img} to be displayed on the authentication module.
#' @param status Bootstrap status to use for the panel and the button.
#'  Valid status are: \code{"default"}, \code{"primary"}, \code{"success"},
#'  \code{"warning"}, \code{"danger"}.
#'
#' @export
#'
#' @name module-authentication
#'
#' @importFrom htmltools tagList tags singleton
#' @importFrom shiny NS fluidRow column textInput passwordInput actionButton
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinymanager)
#'
#'   # data.frame with credentials info
#'   credentials <- data.frame(
#'     user = c("fanny", "victor"),
#'     password = c("azerty", "12345"),
#'     comment = c("alsace", "auvergne"),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   # Function to authenticate user
#'   check_credentials_p <- purrr::partial(
#'     check_credentials_df,
#'     credentials_df = credentials # set default df to use
#'   )
#'
#'   # app
#'   ui <- fluidPage(
#'
#'     # authentication module
#'     auth_ui(
#'       id = "auth",
#'       tag_img = tags$img(
#'         src = "https://www.r-project.org/logo/Rlogo.png", width = 100
#'       )
#'     ),
#'
#'     # result of authentication
#'     verbatimTextOutput(outputId = "res_auth"),
#'
#'     # classic app
#'     headerPanel('Iris k-means clustering'),
#'     sidebarPanel(
#'       selectInput('xcol', 'X Variable', names(iris)),
#'       selectInput('ycol', 'Y Variable', names(iris),
#'                   selected=names(iris)[[2]]),
#'       numericInput('clusters', 'Cluster count', 3,
#'                    min = 1, max = 9)
#'     ),
#'     mainPanel(
#'       plotOutput('plot1')
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     # authentication module
#'     auth <- callModule(
#'       module = auth_server,
#'       id = "auth",
#'       check_credentials = check_credentials_p
#'     )
#'
#'     output$res_auth <- renderPrint({
#'       reactiveValuesToList(auth)
#'     })
#'
#'     # classic app
#'     selectedData <- reactive({
#'
#'       req(auth$result)  # <---- dependency on authentication result
#'
#'       iris[, c(input$xcol, input$ycol)]
#'     })
#'
#'     clusters <- reactive({
#'       kmeans(selectedData(), input$clusters)
#'     })
#'
#'     output$plot1 <- renderPlot({
#'       palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#'                 "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
#'
#'       par(mar = c(5.1, 4.1, 0, 1))
#'       plot(selectedData(),
#'            col = clusters()$cluster,
#'            pch = 20, cex = 3)
#'       points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
auth_ui <- function(id, labels = auth_labels(), tag_img = NULL, status = "primary") {

  ns <- NS(id)

  .globals$labels_auth <- labels
  tagList(
    singleton(tags$head(
      tags$link(href="shinymanager/styles-auth.css", rel="stylesheet"),
      tags$script(src = "shinymanager/bindEnter.js")
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
              tags$div(
                style = "text-align: center;",
                if (!is.null(tag_img)) tag_img,
                tags$h3(labels$please_authenticate)
              ),
              tags$br(),
              textInput(
                inputId = ns("user_id"),
                label = labels$username,
                width = "100%"
              ),
              passwordInput(
                inputId = ns("user_pwd"),
                label = labels$password,
                width = "100%"
              ),
              tags$br(),
              actionButton(
                inputId = ns("go_auth"),
                label = labels$login,
                width = "100%",
                class = paste0("btn-", status)
              ),
              tags$br(), tags$br(),
              tags$script(
                sprintf("bindEnter('%s');", ns(""))
              ),
              tags$div(id = ns("result_auth"))
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
#'  Must return \code{TRUE} or \code{FALSE}.
#'  To use additionnals arguments, set them with \code{purrr::partial} (see examples).
#' @param use_token Add a token in the URL to check authentication. Should not be used directly.
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
#' @importFrom shiny reactiveValues observeEvent removeUI updateQueryString insertUI icon
#' @importFrom stats setNames
auth_server <- function(input, output, session, check_credentials, use_token = FALSE) {

  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }

  authentication <- reactiveValues(result = FALSE, user = NULL, user_info = NULL)

  observeEvent(input$go_auth, {
    removeUI(selector = jns("msg_auth"))
    res_auth <- check_credentials(input$user_id, input$user_pwd)
    if (isTRUE(res_auth$result)) {
      removeUI(selector = jns("auth-mod"))
      authentication$result <- TRUE
      authentication$user <- input$user_id
      authentication$user_info <- res_auth$user_info
      token <- generate_token(input$user_id)

      if (isTRUE(use_token)) {
        add_token(setNames(list(token), input$user_id))
        store_user_info(setNames(list(res_auth$user_info), input$user_id))
        updateQueryString(queryString = paste0("?token=", token), session = session)
        session$reload()
      }

    } else {
      if (isTRUE(res_auth$expired)) {
        insertUI(
          selector = jns("result_auth"),
          ui = tags$div(
            id = ns("msg_auth"), class = "alert alert-danger",
            icon("exclamation-triangle"), .globals$labels_auth$user_expired
          )
        )
      } else {
        insertUI(
          selector = jns("result_auth"),
          ui = tags$div(
            id = ns("msg_auth"), class = "alert alert-danger",
            icon("exclamation-triangle"), .globals$labels_auth$invalid_usr_pwd
          )
        )
      }
    }
  })

  return(authentication)
}


#' Labels for authentication module
#'
#' @param please_authenticate Text to be displayed as a header.
#' @param username Label for username input field.
#' @param password Label for password input field.
#' @param login Text displayed on the button.
#' @param invalid_usr_pwd Error message displayed if authentication is unsuccesful.
#' @param user_expired Error message displayed if user's account has expired.
#'
#' @export
#'
auth_labels <- function(please_authenticate = "Please authenticate",
                        username = "Username:",
                        password = "Password:",
                        login = "Login",
                        invalid_usr_pwd = "Username or password are incorrect",
                        user_expired = "Your account has expired") {
  list(
    please_authenticate = please_authenticate,
    username = username,
    password = password,
    login = login,
    invalid_usr_pwd = invalid_usr_pwd,
    user_expired = user_expired
  )
}


