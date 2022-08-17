
#' @title Create a FAB button
#'
#' @description Create a fixed button in page corner with additional button(s) in it
#'
#' @param ... \code{actionButton}s to be used as floating buttons.
#' @param position Position for the button.
#' @param animation Animation when displaying floating buttons.
#' @param toggle Display floating buttons when main button is clicked or hovered.
#' @param inputId Id for the FAB button (act like an \code{actionButton}).
#' @param label Label for main button.
#'
#' @export
#'
#' @importFrom shiny icon
#' @importFrom htmltools tagList tags tagAppendAttributes
#'
#' @example examples/fab_button.R
fab_button <- function(...,
                       position = c("bottom-right", "top-right", "bottom-left", "top-left", "none"),
                       animation = c("slidein", "slidein-spring", "fountain", "zoomin"),
                       toggle = c("hover", "click"),
                       inputId = NULL,
                       label = NULL) {
  args <- list(...)
  if (!is.null(args$status)) {
    warning("fab_button: status argument is deprecated.", call. = FALSE)
    args$status <- NULL
  }
  if (!is.null(args$icon)) {
    warning("fab_button: icon argument is deprecated.", call. = FALSE)
    args$icon <- NULL
  }
  toggle <- match.arg(toggle)
  animation <- match.arg(animation)
  position <- match.arg(position)
  if (position == "none") return(NULL)
  position <- switch(
    position,
    "bottom-right" = "br",
    "top-right" = "tr",
    "bottom-left" = "bl",
    "top-left" = "tl"
  )
  tagList(
    tags$ul(
      class = paste("mfb-component", position, sep = "--"),
      class = paste("mfb", animation, sep = "-"),
      `data-mfb-toggle` = toggle,
      tags$li(
        class = "mfb-component__wrap",
        tags$a(
          id = inputId,
          `data-mfb-label` = label,
          class = "mfb-component__button--main action-button",
          icon("plus", class = "mfb-component__main-icon--resting"),
          icon("xmark", class = "mfb-component__main-icon--active")
        ),
        tags$ul(
          class = "mfb-component__list",
          lapply(
            X = args,
            FUN = function(x) {
              if (inherits(x, "list")) {
                id <- x$inputId
                label <- x$label
                tagIcon <- x$icon
              } else if (inherits(x, "shiny.tag")) {
                id <- x$attribs$id
                label <- x$children[[1]][[2]]
                tagIcon <- x$children[[1]][[1]]
              } else {
                stop("Arguments in `...` must be lists or actionButtons")
              }
              if (!is.null(tagIcon) && inherits(tagIcon, "shiny.tag")) {
                tagIcon <- htmltools::tagAppendAttributes(
                  tagIcon,
                  class = "mfb-component__child-icon"
                )
              }
              tags$li(
                tags$a(
                  `data-mfb-label` = label,
                  id = id,
                  class = "mfb-component__button--child action-button",
                  tagIcon
                )
              )
            }
          )
        )
      )
    ),
    html_dependency_fab()
  )
}


#' @importFrom htmltools htmlDependency
html_dependency_fab <- function() {
  htmlDependency(
    name = "fab-button",
    version = "0.3.7",
    src = c(
      href = "shinymanager/fab-button",
      file = "assets/fab-button"
    ),
    package = "shinymanager",
    stylesheet = "fab-button.min.css",
    all_files = FALSE
  )
}

