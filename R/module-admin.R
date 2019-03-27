

admin_UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 10, offset = 1,
      tags$h2("Administrator mode"),
      tags$br(), tags$br(),
      actionButton(
        inputId = ns("add_user"),
        label = "Add a user",
        icon = icon("plus"),
        width = "100%",
        class = "btn-primary"
      ),
      tags$br(), tags$br(), tags$br(),
      DT::DTOutput(outputId = ns("table_users"))
    )
  )
}

admin <- function(input, output, session) {

  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }

  users <- reactive({
    data.frame(
      user = c("fanny", "victor", "benoit"),
      password = c("azerty", "12345", "azerty"),
      comment = c("alsace", "auvergne", "bretagne"),
      admin = c(FALSE, TRUE, FALSE),
      stringsAsFactors = FALSE
    )
  })


  output$table_users <- DT::renderDT({
    users <- users()
    users <- users[, setdiff(names(users), "password"), drop = FALSE]
    users$Edit <- edit_btns(ns("edit_user"), users$user)
    users$Remove <- remove_btns(ns("remove_user"), users$user)
    DT::datatable(
      data = users,
      colnames = R.utils::capitalize(names(users)),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({\'background-color\': \'#222222\', \'color\': \'#fff\'});",
          "}"),
        # autoWidth = TRUE,
        columnDefs = list(
          list(width = "50px", targets = (ncol(users)-2):(ncol(users)-1))
        )
      )
    )
  })

  observeEvent(input$edit_user, {
    data <- isolate(users())
    showModal(modalDialog(
      title = "Edit user",
      edit_user_UI(ns("edit_user"), data, input$edit_user)
    ))
  })

  observeEvent(input$add_user, {
    data <- isolate(users())
    showModal(modalDialog(
      title = "Add user",
      edit_user_UI(ns("add_user"), data, NULL)
    ))
  })

}


edit_btns <- function(inputId, users) {
  tag <- lapply(
    X = users,
    FUN = function(x) {
      res <- tags$button(
        class = "btn btn-primary",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s',  {priority: 'event'})",
          inputId, x
        ),
        shiny::icon("pencil-square-o"), tooltip = "Edit user"
      )
      htmltools::doRenderTags(res)
    }
  )
  unlist(tag, use.names = FALSE)
}

remove_btns <- function(inputId, users) {
  tag <- lapply(
    X = users,
    FUN = function(x) {
      res <- tags$button(
        class = "btn btn-danger",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s',  {priority: 'event'})",
          inputId, x
        ),
        shiny::icon("trash-o "), tooltip = "Remove user"
      )
      htmltools::doRenderTags(res)
    }
  )
  unlist(tag, use.names = FALSE)
}





