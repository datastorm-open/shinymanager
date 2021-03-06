% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/secure-app.R
\name{secure-app}
\alias{secure-app}
\alias{secure_app}
\alias{secure_server}
\title{Secure a Shiny application and manage authentication}
\usage{
secure_app(
  ui,
  ...,
  enable_admin = FALSE,
  head_auth = NULL,
  theme = NULL,
  language = "en",
  fab_position = "bottom-right"
)

secure_server(
  check_credentials,
  timeout = 15,
  inputs_list = NULL,
  max_users = NULL,
  fileEncoding = "",
  keep_token = FALSE,
  session = shiny::getDefaultReactiveDomain()
)
}
\arguments{
\item{ui}{UI of the application.}

\item{...}{Arguments passed to \code{\link{auth_ui}}.}

\item{enable_admin}{Enable or not access to admin mode, note that
admin mode is only available when using SQLite backend for credentials.}

\item{head_auth}{Tag or list of tags to use in the \code{<head>}
of the authentication page (for custom CSS for example).}

\item{theme}{Alternative Bootstrap stylesheet, default is to use \code{readable},
you can use themes provided by \code{shinythemes}.
It will affect the authentication panel and the admin page.}

\item{language}{Language to use for labels, supported values are : "en", "fr", "pt-BR", "es", "de", "pl".}

\item{fab_position}{Position for the FAB button, see \code{\link{fab_button}} for options.}

\item{check_credentials}{Function passed to \code{\link{auth_server}}.}

\item{timeout}{Timeout session (minutes) before logout if sleeping. Defaut to 15. 0 to disable.}

\item{inputs_list}{\code{list}. If database credentials, you can configure inputs for editing users information. See Details.}

\item{max_users}{\code{integer}. If not NULL, maximum of users in sql credentials.}

\item{fileEncoding}{character string: Encoding of logs downloaded file. See \code{\link{write.table}}}

\item{keep_token}{Logical, keep the token used to authenticate in the URL, it allow to refresh the
application in the browser, but careful the token can be shared between users ! Default to \code{FALSE}.}

\item{session}{Shiny session.}
}
\value{
A \code{reactiveValues} containing informations about the user connected.
}
\description{
Secure a Shiny application and manage authentication
}
\details{
If database credentials, you can configure inputs with \code{inputs_list} for editing users information
from the admin console. \code{start}, \code{expire}, \code{admin} and \code{password} are not configurable.
The others columns are rendering by defaut using a \code{textInput}. You can modify this using \code{inputs_list}.
\code{inputs_list} must be a named list. Each name must be a column name, and then we must have the function
 shiny to call \code{fun} and the arguments \code{args} like this :
 \code{
 list(group = list(
     fun = "selectInput",
     args = list(
         choices = c("all", "restricted"),
         multiple = TRUE,
         selected = c("all", "restricted")
      )
     )
)
}
}
\note{
A special input value will be accessible server-side with \code{input$shinymanager_where}
 to know in which step user is : authentication, application, admin or password.
}
\examples{
if (interactive()) {

  # define some credentials
  credentials <- data.frame(
    user = c("shiny", "shinymanager"),
    password = c("azerty", "12345"),
    stringsAsFactors = FALSE
  )

  library(shiny)
  library(shinymanager)

  ui <- fluidPage(
    tags$h2("My secure application"),
    verbatimTextOutput("auth_output")
  )

  # Wrap your UI with secure_app
  ui <- secure_app(ui, choose_language = TRUE)

  # change auth ui background ?
  # ui <- secure_app(ui,
  #                  background  = "linear-gradient(rgba(0, 0, 255, 0.5), 
  #                  rgba(255, 255, 0, 0.5)),
  #                  url('https://www.r-project.org/logo/Rlogo.png')  no-repeat center fixed;")

  server <- function(input, output, session) {

    # call the server part
    # check_credentials returns a function to authenticate users
    res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
    )

    output$auth_output <- renderPrint({
      reactiveValuesToList(res_auth)
    })

    observe({
      print(input$shinymanager_where)
      print(input$shinymanager_language)
    })
    
    # your classic server logic

  }

  shinyApp(ui, server)

}
}
