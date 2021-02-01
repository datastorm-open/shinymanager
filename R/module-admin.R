

#' @importFrom DT DTOutput
#' @importFrom htmltools tags singleton tagList
#' @importFrom shiny NS fluidRow column actionButton icon
admin_ui <- function(id, lan = NULL) {

  ns <- NS(id)

  if(is.null(lan)){
    lan <- use_language()
  }

  tagList(
    singleton(tags$head(
      tags$link(href="shinymanager/styles-admin.css", rel="stylesheet"),
      tags$script(src = "shinymanager/shiny-utils.js"),
      tags$script(src = "shinymanager/timeout.js")
    )),
    fluidRow(
      column(
        width = 10, offset = 1,
        # tags$h2(lan$get("Administrator mode")),
        # tags$br(), tags$br(),

        tags$h3(icon("users"), lan$get("Users"), class = "text-primary"),
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

        actionButton(
          inputId = ns("remove_selected_allusers"),
          # label = lan$get("Select all shown users"),
          label = "",
          class = "btn-secondary pull-right",
          style = "margin-left: 5px",
          icon = icon("check-square")
        ),

        actionButton(
          inputId = ns("remove_selected_users"),
          label = lan$get("Remove selected users"),
          class = "btn-danger pull-right disabled",
          icon = icon("trash-o")
        ),

        tags$br(),

        tags$h3(icon("key"), lan$get("Passwords"), class = "text-primary"),
        tags$hr(),

        DTOutput(outputId = ns("table_pwds")),

        tags$br(),

        actionButton(
          inputId = ns("change_selected_allusers"),
          # label = lan$get("Select all shown users"),
          label = "",
          class = "btn-secondary pull-right",
          style = "margin-left: 5px",
          icon = icon("check-square")
        ),

        actionButton(
          inputId = ns("change_selected_pwds"),
          label = lan$get("Force selected users to change password"),
          class = "btn-primary pull-right disabled",
          icon = icon("key")
        ),

        tags$br(),tags$br(), tags$br(), tags$hr(),

        downloadButton(
          outputId = ns("download_sql_database"),
          label = lan$get("Download SQL database"),
          class = "btn-primary center-block",
          icon = icon("download")
        ),

        tags$br(),tags$br()

      )
    )
  )
}

#' @importFrom DT renderDT datatable JS
#' @importFrom shiny reactive observeEvent isolate showModal modalDialog reactiveFileReader
#'  removeUI insertUI reactiveValues showNotification callModule req updateCheckboxInput
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
admin <- function(input, output, session, sqlite_path, passphrase, lan,
                  inputs_list = NULL, max_users = NULL) {

  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }

  token_start <- isolate(getToken(session = session))

  update_read_db <- reactiveValues(x = NULL)

  # read users table from database
  users <- reactiveVal(NULL)

  observe({
    unbindDT(ns("table_users"))
    update_read_db$x
    db <- try({
      conn <- dbConnect(SQLite(), dbname = sqlite_path)
      on.exit(dbDisconnect(conn))
      read_db_decrypt(conn = conn, name = "credentials", passphrase = passphrase)
    }, silent = TRUE)
    if (inherits(db, "try-error")) {
      showModal(modalDialog("An error occurs when connecting or reading the database."))
      users(NULL)
    } else {
      users(db)
    }
  })

  # prevent bug having multiple admin session
  users_update <- reactiveFileReader(1000, session, sqlite_path, filelReaderDB,  passphrase = passphrase, name = "credentials")
  observe({
    if(!is.null(users_update())) users(users_update())
  })

  # read password management table from database
  pwds <- reactiveVal(NULL)

  observe({
    unbindDT(ns("table_pwds"))
    update_read_db$x
    db <- try({
      conn <- dbConnect(SQLite(), dbname = sqlite_path)
      on.exit(dbDisconnect(conn))
      read_db_decrypt(conn = conn, name = "pwd_mngt", passphrase = passphrase)
    }, silent = TRUE)
    if (inherits(db, "try-error")) {
      showModal(modalDialog("An error occurs when connecting or reading the database."))
      pwds(NULL)
    } else {
      pwds(db)
    }
  })

  # prevent bug having multiple admin session
  pwds_update <- reactiveFileReader(1000, session, sqlite_path, filelReaderDB,  passphrase = passphrase, name = "pwd_mngt")
  observe({
    if(!is.null(pwds_update())) pwds(pwds_update())
  })


  # displaying users table
  output$table_users <- renderDT({
    req(users())
    users <- users()
    users <- users[,-which(names(users) %in% c("expire","start"))]
    users <- users[, setdiff(names(users), c("password", "is_hashed_password")), drop = FALSE]
    users$Edit <- input_btns(ns("edit_user"), users$user, "Edit user", icon("pencil-square-o"), status = "primary", lan = lan())
    users$Remove <- input_btns(ns("remove_user"), users$user, "Delete user", icon("trash-o"), status = "danger", lan = lan())
    users$Select <- input_checkbox_ui(ns("remove_mult_users"), users$user)
    names <- base::do.call(base::c,lapply(names(users), function(x) lan()$get(x)))
    change <- as.logical(users$admin)
    users[change, "admin"] <- "Sí"
    users[!change, "admin"] <- "No"
    datatable(
      data = users,
      colnames = make_title(names),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      # extensions = 'FixedColumns', # bug using FixedColumns on checkbox + update table...
      options = list(
        language = lan()$get_DT(),
        drawCallback = JS("function() {Shiny.bindAll(this.api().table().node());}"),
        # initComplete = JS(
        # "function(settings, json) {",
        # "$(this.api().table().header()).css({\'background-color\': \'#fff\', \'color\': \'#4582ec\'});",
        # "}"),
        scrollX = TRUE,
        # columnDefs = list(
        #   list(width = "50px", targets = (ncol(users)-3):(ncol(users)-1))
        # )
        fixedColumns = list(leftColumns = 1)
      )
    )
  }, server = FALSE)


  observeEvent(input$remove_selected_allusers, {
    input_names <- names(input)
    remove_mult_users_input <- input_names[grep("^remove_mult_users", input_names)]
    if(length(remove_mult_users_input) > 0){
      ctrl <- lapply(remove_mult_users_input, function(x){
        updateCheckboxInput(session, x, value = TRUE)
      })
    }
  })

  # displaying password management table
  output$table_pwds <- renderDT({
    req(pwds())
    pwds <- pwds()
    pwds$`Change password` <- input_btns(ns("change_pwd"), pwds$user, "Ask to change password", icon("key"), status = "primary", lan = lan())
    pwds$`Reset password` <- input_btns(ns("reset_pwd"), pwds$user, "Reset password", icon("undo"), status = "warning", lan = lan())
    pwds$Select <- input_checkbox_ui(ns("change_mult_pwds"), pwds$user)
    names <- base::do.call(base::c,lapply(names(pwds), function(x) lan()$get(x)))
    change <- as.logical(pwds$must_change)
    pwds[change, "must_change"] <- "Sí"
    pwds[!change, "must_change"] <- "No"
    change <- as.logical(pwds$have_changed)
    pwds[change,"have_changed"] <- "Sí"
    pwds[!change,"have_changed"] <- "No"
    datatable(
      data = pwds,
      colnames = make_title(names),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(
        language = lan()$get_DT(),
        drawCallback = JS("function() {Shiny.bindAll(this.api().table().node());}"),
        # initComplete = JS(
        #   "function(settings, json) {",
        #   "$(this.api().table().header()).css({\'background-color\': \'#fff\', \'color\': \'#4582ec\'});",
        #   "}"),
        scrollX = TRUE#,
        # columnDefs = list(
        #   list(width = "50px", targets = (ncol(pwds)-3):(ncol(pwds)-1))
        # )
      )
    )
  }, server = FALSE)

  observeEvent(input$change_selected_allusers, {
    input_names <- names(input)
    chge_mult_pwds_input <- input_names[grep("^change_mult_pwds", input_names)]
    if(length(chge_mult_pwds_input) > 0){
      ctrl <- lapply(chge_mult_pwds_input, function(x){
        updateCheckboxInput(session, x, value = TRUE)
      })
    }
  })

  # Remove all selected users
  r_selected_users <- callModule(module = input_checkbox, id = "remove_mult_users")
  observeEvent(r_selected_users(), {
    selected_users <- r_selected_users()
    if (length(selected_users) > 0) {
      toggleBtn(session = session, inputId = ns("remove_selected_users"), type = "enable")
    } else {
      toggleBtn(session = session, inputId = ns("remove_selected_users"), type = "disable")
    }
  })

  observeEvent(input$remove_selected_users, {
    remove_modal(ns("delete_selected_users"), r_selected_users(), lan())
  })

  observeEvent(input$delete_selected_users, {
    users <- users()
    to_delete <- r_selected_users()
    users <- users[!users$user %in% to_delete, , drop = FALSE]
    pwds <- pwds()
    pwds <- pwds[!pwds$user %in% to_delete, , drop = FALSE]
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))
    write_db_encrypt(conn = conn, value = users, name = "credentials", passphrase = passphrase)
    write_db_encrypt(conn = conn, value = pwds, name = "pwd_mngt", passphrase = passphrase)
    update_read_db$x <- Sys.time()
  })


  # Force change password all selected users
  r_selected_pwds <- callModule(module = input_checkbox, id = "change_mult_pwds")
  observeEvent(r_selected_pwds(), {
    selected_pwds <- r_selected_pwds()
    if (length(selected_pwds) > 0) {
      toggleBtn(session = session, inputId = ns("change_selected_pwds"), type = "enable")
    } else {
      toggleBtn(session = session, inputId = ns("change_selected_pwds"), type = "disable")
    }
  })

  observeEvent(input$change_selected_pwds, {
    change_pwd_modal(ns("changed_password_users"), r_selected_pwds(), lan())
  })

  observeEvent(input$changed_password_users, {
    res_chg <- try(force_chg_pwd(r_selected_pwds()), silent = TRUE)
    if (inherits(res_chg, "try-error")) {
      showNotification(ui = lan()$get("Failed to update the database"), type = "error")
    } else {
      showNotification(ui = lan()$get("Change saved!"), type = "message")
      update_read_db$x <- Sys.time()
    }
  })




  # launch modal to edit informations about a user
  observeEvent(input$edit_user, {
    users <- users()
    showModal(modalDialog(
      title = lan()$get("Edit user"),
      edit_user_ui(ns("edit_user"), credentials = users, username = input$edit_user, inputs_list = inputs_list, lan = lan()),
      tags$div(id = ns("placeholder-edituser-exist")),
      footer = tagList(
        modalButton(lan()$get("Cancel")),
        actionButton(
          inputId = ns("edited_user"),
          label = lan()$get("Confirm change"),
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })

  value_edited <- callModule(module = edit_user, id = "edit_user")

  # warning message if user already exist in database
  observeEvent(value_edited$user, {
    req(!is.null(value_edited$user$user))
    removeUI(selector = jns("alert-edituser-exist"), immediate = TRUE)
    new <- value_edited$user$user
    existing <- setdiff(users()$user, input$edit_user)
    if (new %in% existing) {
      insertUI(
        selector = jns("placeholder-edituser-exist"),
        ui = tags$div(
          id = ns("alert-edituser-exist"),
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          lan()$get("User already exist!")
        ),
        immediate = TRUE
      )
      toggleBtn(session = session, inputId = ns("edited_user"), type = "disable")
    } else if(new %in% "") {
      toggleBtn(session = session, inputId = ns("edited_user"), type = "disable")
    } else {
      toggleBtn(session = session, inputId = ns("edited_user"), type = "enable")
    }
  })

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
      showNotification(ui = lan()$get("Fail to update user"), type = "error")
    } else {
      showNotification(ui = lan()$get("User successfully updated"), type = "message")
      update_read_db$x <- Sys.time()
    }
  })


  # launch modal to add a new user
  observeEvent(input$add_user, {
    users <- users()
    if(!is.null(max_users) && is.numeric(max_users) && nrow(users) >= max_users){
      showModal(
        modalDialog(
        title = lan()$get("Too many users"),
        sprintf(lan()$get("Maximum number of users : %s"), max_users)
        )
      )
    } else {
      showModal(modalDialog(
        title = lan()$get("Add a user"),
        edit_user_ui(ns("add_user"), users, NULL, inputs_list = inputs_list, lan = lan()),
        tags$div(id = ns("placeholder-user-exist")),
        footer = tagList(
          modalButton(lan()$get("Cancel")),
          actionButton(
            inputId = ns("added_user"),
            label = lan()$get("Confirm new user"),
            class = "btn-primary",
            `data-dismiss` = "modal"
          )
        )
      ))
    }
  })

  value_added <- callModule(module = edit_user, id = "add_user")

  # warning message if user already exist in database
  observeEvent(value_added$user, {
    req(!is.null(value_added$user$user))
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
          lan()$get("User already exist!")
        ),
        immediate = TRUE
      )
      toggleBtn(session = session, inputId = ns("added_user"), type = "disable")
    } else if(new %in% ""){
      toggleBtn(session = session, inputId = ns("added_user"), type = "disable")
    } else {
      toggleBtn(session = session, inputId = ns("added_user"), type = "enable")
    }
  })

  # write in database the new user and display his password
  observeEvent(input$added_user, {
    users <- users()
    newuser <- value_added$user
    if("must_change" %in% names(newuser)){
      must_change <- as.character(newuser$must_change)
      newuser$must_change <- NULL
    } else {
      must_change <- as.character(TRUE)
    }
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))

    # password <- generate_pwd()
    # newuser$password <- password
    res_add <- try({
      newuser <- as.data.frame(newuser)
      newuser$is_hashed_password <- FALSE
      if(!"is_hashed_password" %in% colnames(users)){
        users$is_hashed_password <- FALSE
      }
      newuser <- newuser[, colnames(users)]
      users <- rbind(users, newuser)
      write_db_encrypt(conn = conn, value = users, name = "credentials", passphrase = passphrase)
      resetpwd <- read_db_decrypt(conn = conn, name = "pwd_mngt", passphrase = passphrase)
      resetpwd <- rbind(resetpwd, data.frame(
        user = newuser$user,
        must_change = must_change,
        have_changed = as.character(FALSE),
        date_change = character(1),
        stringsAsFactors = FALSE
      ))
      write_db_encrypt(conn = conn, value = resetpwd, name = "pwd_mngt", passphrase = passphrase)
    }, silent = FALSE)
    if (inherits(res_add, "try-error")) {
      showNotification(ui = lan()$get("Failed to update user"), type = "error")
    } else {
      showModal(modalDialog(
        tags$p(HTML(
          sprintf(lan()$get("New user %s succesfully created!"), tags$b(newuser$user))
        )),
        tags$p(lan()$get("Password:"), tags$b(newuser$password)),
        footer = modalButton(lan()$get("Dismiss"))
      ))
      update_read_db$x <- Sys.time()
    }
  })


  # launch modal to force a user to change password
  observeEvent(input$change_pwd, {
    change_pwd_modal(ns("changed_password"), input$change_pwd, lan())
  })

  # store in database that the user must change password on next connection
  observeEvent(input$changed_password, {
    res_chg <- try(force_chg_pwd(input$change_pwd), silent = TRUE)
    if (inherits(res_chg, "try-error")) {
      showNotification(ui = lan()$get("Failed to update the database"), type = "error")
    } else {
      showNotification(ui = lan()$get("Change saved!"), type = "message")
      update_read_db$x <- Sys.time()
    }
  })


  # launch modal to reset password
  observeEvent(input$reset_pwd, {
    reset_pwd_modal(ns("reseted_password"), input$reset_pwd, lan())
  })
  observeEvent(input$reseted_password, {
    password <- generate_pwd()
    users <- users()
    if(!"character" %in% class(users$password)){
      users$password <- as.character(users$password)
    }
    users$password[users$user %in% input$reset_pwd] <- password
    if(!"is_hashed_password" %in% colnames(users)){
      users$is_hashed_password <- FALSE
    } else {
      users$is_hashed_password[users$user %in% input$reset_pwd] <- FALSE
    }
    write_db_encrypt(conn = sqlite_path, value = users, name = "credentials", passphrase = passphrase)
    res_chg <- try(force_chg_pwd(input$reset_pwd), silent = TRUE)
    if (inherits(res_chg, "try-error")) {
      showNotification(ui = lan()$get("Failed to update user"), type = "error")
    } else {
      showModal(modalDialog(
        tags$p(lan()$get("Password succesfully reset!")),
        tags$p(lan()$get("Temporary password:"), tags$b(password)),
        footer = modalButton(lan()$get("Dismiss"))
      ))
      update_read_db$x <- Sys.time()
    }
  })


  # launch modal to remove a user from the database
  observeEvent(input$remove_user, {
    current_user <- .tok$get_user(token_start)
    if (identical(current_user, input$remove_user)) {
      showModal(modalDialog(
        lan()$get("You can't remove yourself!"),
        footer = modalButton(lan()$get("Cancel")),
        easyClose = TRUE
      ))
    } else {
      remove_modal(ns("delete_user"), input$remove_user, lan())
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

  # download SQL Database
  output$download_sql_database <- downloadHandler(
    filename = function() {
      paste('shinymanager-sql-', Sys.Date(), '.sqlite', sep = '')
    },
    content = function(con) {
      file.copy(sqlite_path, con)
    }
  )
}


#' @importFrom htmltools HTML tags tagList
#' @importFrom shiny showModal modalDialog modalButton actionButton
remove_modal <- function(inputId, user, lan) {
  showModal(modalDialog(
    tags$p(HTML(sprintf(
      lan$get("Are you sure to remove user(s): %s from the database ?"), tags$b(paste(user, collapse = ", "))
    ))),
    fade = FALSE,
    footer = tagList(
      actionButton(
        inputId = inputId,
        label = lan$get("Delete user(s)"),
        class = "btn-danger",
        `data-dismiss` = "modal"
      ),
      modalButton(lan$get("Cancel"))
    )
  ))
}

change_pwd_modal <- function(inputId, user, lan) {
  showModal(modalDialog(
    title = lan$get("Ask to change password"),
    tags$p(HTML(
      sprintf(lan$get("Ask %s to change password on next connection?"), tags$b(paste(user, collapse = ", ")))
    )),
    footer = tagList(
      modalButton(lan$get("Cancel")),
      actionButton(
        inputId = inputId,
        label = lan$get("Confirm"),
        class = "btn-primary",
        `data-dismiss` = "modal"
      )
    )
  ))
}

reset_pwd_modal <- function(inputId, user, lan) {
  showModal(modalDialog(
    title = lan$get("Reset password"),
    tags$p(HTML(
      sprintf(lan$get("Reset password for %s?"), tags$b(paste(user, collapse = ", ")))
    )),
    footer = tagList(
      modalButton(lan$get("Cancel")),
      actionButton(
        inputId = inputId,
        label = lan$get("Confirm"),
        class = "btn-primary",
        `data-dismiss` = "modal"
      )
    )
  ))
}
