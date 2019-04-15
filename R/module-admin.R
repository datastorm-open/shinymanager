

#' @importFrom DT DTOutput
#' @importFrom htmltools tags singleton tagList
#' @importFrom shiny NS fluidRow column actionButton icon
admin_UI <- function(id) {

  ns <- NS(id)

  lan <- use_language()

  tagList(
    singleton(tags$head(
      tags$link(href="shinymanager/styles-admin.css", rel="stylesheet"),
      tags$script(src = "shinymanager/shiny-utils.js")
    )),
    fluidRow(
      column(
        width = 8, offset = 2,
        # tags$h2(lan$get("Administrator mode")),
        # tags$br(), tags$br(),

        tags$h3(icon("users"), "Users", class = "text-primary"),
        tags$hr(),

        actionButton(
          inputId = ns("add_user"),
          label = lan$get("Add a user"),
          icon = icon("plus"),
          width = "100%",
          class = "btn-primary"
        ),
        tags$br(), tags$br(), tags$br(),
        DTOutput(outputId = ns("table_users")),

        tags$br(),

        tags$h3(icon("key"), "Passwords", class = "text-primary"),
        tags$hr(),

        DTOutput(outputId = ns("table_pwds")),

        tags$br()

      )
    )
  )
}

#' @importFrom DT renderDT datatable JS
#' @importFrom shiny reactive observeEvent isolate showModal modalDialog
#'  removeUI insertUI reactiveValues showNotification callModule req
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
admin <- function(input, output, session, sqlite_path, passphrase) {

  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }

  lan <- use_language()

  token_start <- isolate(getToken(session = session))

  update_read_db <- reactiveValues(x = NULL)

  # read users table from database
  users <- reactive({
    update_read_db$x
    db <- try({
      conn <- dbConnect(SQLite(), dbname = sqlite_path)
      on.exit(dbDisconnect(conn))
      read_db_decrypt(conn = conn, name = "credentials", passphrase = passphrase)
    }, silent = TRUE)
    if (inherits(db, "try-error")) {
      showModal(modalDialog("An error occurs when connecting or reading the database."))
      return(NULL)
    } else {
      return(db)
    }
  })

  # read password management table from database
  pwds <- reactive({
    update_read_db$x
    db <- try({
      conn <- dbConnect(SQLite(), dbname = sqlite_path)
      on.exit(dbDisconnect(conn))
      read_db_decrypt(conn = conn, name = "pwd_mngt", passphrase = passphrase)
    }, silent = TRUE)
    if (inherits(db, "try-error")) {
      showModal(modalDialog("An error occurs when connecting or reading the database."))
      return(NULL)
    } else {
      return(db)
    }
  })


  # displaying users table
  output$table_users <- renderDT({
    req(users())
    users <- users()
    users <- users[, setdiff(names(users), "password"), drop = FALSE]
    users$Edit <- input_btns(ns("edit_user"), users$user, "Edit user", icon("pencil-square-o"), status = "primary")
    users$Remove <- input_btns(ns("remove_user"), users$user, "Delete user", icon("trash-o"), status = "danger")
    datatable(
      data = users,
      colnames = make_title(names(users)),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(
        language = lan$get_DT(),
        # initComplete = JS(
          # "function(settings, json) {",
          # "$(this.api().table().header()).css({\'background-color\': \'#fff\', \'color\': \'#4582ec\'});",
          # "}"),
        scrollX = TRUE,
        columnDefs = list(
          list(width = "50px", targets = (ncol(users)-2):(ncol(users)-1))
        )
      )
    )
  })

  # displaying password management table
  output$table_pwds <- renderDT({
    req(pwds())
    pwds <- pwds()
    pwds$`Change password` <- input_btns(ns("change_pwd"), pwds$user, "Ask to change password", icon("key"), status = "primary")
    datatable(
      data = pwds,
      colnames = make_title(names(pwds)),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(
        language = lan$get_DT(),
        # initComplete = JS(
        #   "function(settings, json) {",
        #   "$(this.api().table().header()).css({\'background-color\': \'#fff\', \'color\': \'#4582ec\'});",
        #   "}"),
        scrollX = TRUE,
        columnDefs = list(
          list(width = "50px", targets = ncol(pwds)-1)
        )
      )
    )
  })

  # launch modal to edit informations about a user
  observeEvent(input$edit_user, {
    users <- users()
    showModal(modalDialog(
      title = "Edit user",
      edit_user_UI(ns("edit_user"), credentials = users, username = input$edit_user),
      footer = tagList(
        modalButton(lan$get("Cancel")),
        actionButton(
          inputId = ns("edited_user"),
          label = lan$get("Confirm change"),
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })

  value_edited <- callModule(module = edit_user, id = "edit_user")

  # Write in database edited values for the user
  observeEvent(input$edited_user, {
    users <- users()
    newval <- value_edited$user
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))

    res_edit <- try({
      users <- update_user(users, newval, input$edit_user)
      write_db_encrypt(conn = conn, value = users, name = "credentials", passphrase = passphrase)
    }, silent = FALSE)
    if (inherits(res_edit, "try-error")) {
      showNotification(ui = lan$get("Fail to update user"), type = "error")
    } else {
      showNotification(ui = lan$get("User successfully updated"), type = "message")
      update_read_db$x <- Sys.time()
    }
  })


  # launch modal to add a new user
  observeEvent(input$add_user, {
    users <- users()
    showModal(modalDialog(
      title = "Add user",
      edit_user_UI(ns("add_user"), users, NULL),
      tags$div(id = ns("placeholder-user-exist")),
      footer = tagList(
        modalButton(lan$get("Cancel")),
        actionButton(
          inputId = ns("added_user"),
          label = lan$get("Confirm new user"),
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })

  value_added <- callModule(module = edit_user, id = "add_user")

  # warning message if user already exist in database
  observeEvent(value_added$user, {
    req(value_added$user$user)
    removeUI(selector = jns("alert-user-exist"), immediate = TRUE)
    new <- value_added$user$user
    existing <- users()$user
    if (new %in% existing) {
      insertUI(
        selector = jns("placeholder-user-exist"),
        ui = tags$div(
          id = ns("alert-user-exist"),
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          lan$get("User already exist!")
        ),
        immediate = TRUE
      )
      toggleBtn(session = session, inputId = ns("added_user"), type = "disable")
    } else {
      toggleBtn(session = session, inputId = ns("added_user"), type = "enable")
    }
  })

  # write in database the new user and display his password
  observeEvent(input$added_user, {
    users <- users()
    newuser <- value_added$user

    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))

    password <- generate_pwd()
    newuser$password <- password

    res_add <- try({
      users <- rbind(users, as.data.frame(newuser))
      write_db_encrypt(conn = conn, value = users, name = "credentials", passphrase = passphrase)
      resetpwd <- read_db_decrypt(conn = conn, name = "pwd_mngt", passphrase = passphrase)
      resetpwd <- rbind(resetpwd, data.frame(
        user = newuser$user,
        must_change = as.character(FALSE),
        have_changed = as.character(FALSE),
        date_change = character(1),
        stringsAsFactors = FALSE
      ))
      write_db_encrypt(conn = conn, value = resetpwd, name = "pwd_mngt", passphrase = passphrase)
    }, silent = FALSE)
    if (inherits(res_add, "try-error")) {
      showNotification(ui = lan$get("Failed to update user"), type = "error")
    } else {
      showModal(modalDialog(
        tags$p(lan$get("New user succesfully created!")),
        tags$p(lan$get("Password:"), tags$b(password)),
        footer = modalButton(lan$get("Dismiss"))
      ))
      update_read_db$x <- Sys.time()
    }
  })


  # launch modal to force a user to change password
  observeEvent(input$change_pwd, {
    users <- users()
    showModal(modalDialog(
      title = "Change password",
      tags$p(
        sprintf(lan$get("Ask %s to change password on next connection?"), input$change_pwd)
      ),
      footer = tagList(
        modalButton(lan$get("Cancel")),
        actionButton(
          inputId = ns("changed_password"),
          label = lan$get("Confirm"),
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })

  # store in database that the user must change password on next connection
  observeEvent(input$changed_password, {
    res_chg <- try(force_chg_pwd(input$change_pwd), silent = TRUE)
    if (inherits(res_chg, "try-error")) {
      showNotification(ui = lan$get("Failed to update the database"), type = "error")
    } else {
      showNotification(ui = lan$get("Change saved!"), type = "message")
      update_read_db$x <- Sys.time()
    }
  })


  # launch modal to remove a user from the database
  observeEvent(input$remove_user, {
    current_user <- .tok$get_user(token_start)
    if (identical(current_user, input$remove_user)) {
      showModal(modalDialog(
        lan$get("You can't remove yourself!"),
        footer = modalButton(lan$get("Cancel")),
        easyClose = TRUE
      ))
    } else {
      remove_modal(ns, input$remove_user)
    }
  })

  # delete the user
  observeEvent(input$delete_user, {
    users <- users()
    users <- users[!users$user %in% input$remove_user, , drop = FALSE]
    pwds <- pwds()
    pwds <- pwds[!pwds$user %in% input$remove_user, , drop = FALSE]
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))
    write_db_encrypt(conn = conn, value = users, name = "credentials", passphrase = passphrase)
    write_db_encrypt(conn = conn, value = pwds, name = "pwd_mngt", passphrase = passphrase)
    update_read_db$x <- Sys.time()
  })

}


#' @importFrom htmltools HTML tags tagList
#' @importFrom shiny showModal modalDialog modalButton actionButton
remove_modal <- function(ns, user) {
  lan <- use_language()
  showModal(modalDialog(
    tags$p(HTML(sprintf(
      lan$get("Are you sure to remove user: %s from the database ?"), tags$b(user)
    ))),
    fade = FALSE,
    footer = tagList(
      actionButton(
        inputId = ns("delete_user"),
        label = lan$get("Delete user"),
        class = "btn-danger",
        `data-dismiss` = "modal"
      ),
      modalButton(lan$get("Cancel"))
    )
  ))
}

