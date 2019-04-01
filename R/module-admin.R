

#' @importFrom DT DTOutput
#' @importFrom htmltools tags
#' @importFrom shiny NS fluidRow column actionButton icon
admin_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 10, offset = 1,
      htmltools::tags$h2("Administrator mode"),
      htmltools::tags$br(), htmltools::tags$br(),
      shiny::actionButton(
        inputId = ns("add_user"),
        label = "Add a user",
        icon = shiny::icon("plus"),
        width = "100%",
        class = "btn-primary"
      ),
      htmltools::tags$br(), htmltools::tags$br(), htmltools::tags$br(),
      DT::DTOutput(outputId = ns("table_users"))
    )
  )
}

#' @importFrom DT renderDT datatable JS
#' @importFrom R.utils capitalize
#' @importFrom shiny reactive observeEvent isolate showModal modalDialog
admin <- function(input, output, session, sqlite_path, passphrase) {

  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }

  update_read_db <- shiny::reactiveValues(x = NULL)

  users <- shiny::reactive({
    update_read_db$x
    db <- try({
      conn <- dbConnect(SQLite(), dbname = sqlite_path)
      on.exit(dbDisconnect(conn))
      read_db_decrypt(conn = conn, name = "credentials", passphrase = passphrase)
    }, silent = TRUE)
    if (inherits(db, "try-error")) {
      shiny::showModal(shiny::modalDialog("An error occurs when connecting or reading the database."))
      return(NULL)
    } else {
      return(db)
    }
  })


  output$table_users <- DT::renderDT({
    shiny::req(users())
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

  shiny::observeEvent(input$edit_user, {
    data <- shiny::isolate(users())
    shiny::showModal(shiny::modalDialog(
      title = "Edit user",
      edit_user_UI(ns("edit_user"), data, input$edit_user)
    ))
  })

  shiny::observeEvent(input$add_user, {
    data <- shiny::isolate(users())
    shiny::showModal(shiny::modalDialog(
      title = "Add user",
      edit_user_UI(ns("add_user"), data, NULL)
    ))
  })

  shiny::observeEvent(input$remove_user, {
    remove_modal(ns, input$remove_user)
  })

  observeEvent(input$delete_user, {
    users <- users()
    users <- users[!users$user %in% input$remove_user, , drop = FALSE]
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))
    write_db_encrypt(conn = conn, value = users, name = "credentials", passphrase = passphrase)
    update_read_db$x <- Sys.time()
  })

}


#' @importFrom htmltools tags doRenderTags
#' @importFrom shiny icon
edit_btns <- function(inputId, users) {
  tag <- lapply(
    X = users,
    FUN = function(x) {
      res <- htmltools::tags$button(
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

#' @importFrom htmltools tags doRenderTags
#' @importFrom shiny icon
remove_btns <- function(inputId, users) {
  tag <- lapply(
    X = users,
    FUN = function(x) {
      res <- htmltools::tags$button(
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



remove_modal <- function(ns, user) {
  shiny::showModal(shiny::modalDialog(
    tags$p("Are you sure to remove user: ", tags$b(user), " from the database ?"),
    fade = FALSE,
    footer = tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(
        inputId = ns("delete_user"),
        label = "Delete user",
        class = "btn-danger",
        `data-dismiss` = "modal"
      )
    )
  ))
}

