
#' @title Create a FAB button
#'
#' @description Create a fixed button in bottom right corner with additional button(s) in it
#'
#' @param ... HTML tags 'a' or 'button' or \code{actionButton} (with \code{NULL} labels).
#' @param inputId Id for the FAB button (act like an \code{actionButton}).
#' @param icon An \code{icon} for the main button.
#' @param status Bootstra^p status to apply to the main button.
#'
#' @export
#'
#' @importFrom shiny icon
#' @importFrom htmltools tagList singleton tags
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinymanager)
#'
#'   ui <- fluidPage(
#'
#'     tags$h1("FAB button"),
#'
#'     tags$p("FAB button:"),
#'     verbatimTextOutput(outputId = "res_fab"),
#'
#'     tags$p("Logout button:"),
#'     verbatimTextOutput(outputId = "res_logout"),
#'
#'     tags$p("Info button:"),
#'     verbatimTextOutput(outputId = "res_info"),
#'
#'     fab_button(
#'       actionButton(
#'         inputId = "logout",
#'         label = NULL,
#'         tooltip = "Logout",
#'         icon = icon("sign-out")
#'       ),
#'       actionButton(
#'         inputId = "info",
#'         label = NULL,
#'         tooltip = "Information",
#'         icon = icon("info")
#'       ),
#'       inputId = "fab"
#'     )
#'
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     output$res_fab <- renderPrint({
#'       input$fab
#'     })
#'
#'     output$res_logout <- renderPrint({
#'       input$logout
#'     })
#'
#'     output$res_info <- renderPrint({
#'       input$info
#'     })
#'
#'   }
#'
#'   shinyApp(ui, server)
#' }
fab_button <- function(..., inputId = NULL, icon = NULL, status = "default") {
  if (is.null(icon))
    icon <- icon("plus fa-lg")
  args <- list(...)
  for (i in seq_along(args)) {
    if (!is.null(args[[i]]$attribs$class)) {
      args[[i]]$attribs$class <- paste(args[[i]]$attribs$class, "buttons-fab")
    } else {
      args[[i]]$attribs$class <- "buttons-fab"
    }
  }
  tagList(
    htmltools::singleton(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "shinymanager/fab-button.css")
      )
    ),
    tags$nav(
      class = "container-fab", args,
      tags$button(
        class = paste0("btn btn-", status, " buttons-fab btn-fab-main action-button"),
        id = inputId, icon
      )
    )
  )
}
