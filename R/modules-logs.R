

#' @importFrom billboarder billboarderOutput
#' @importFrom shiny NS fluidRow column icon selectInput dateRangeInput downloadButton downloadHandler conditionalPanel uiOutput
#' @importFrom htmltools tagList tags
#' @importFrom DT DTOutput
logs_ui <- function(id, lan = NULL) {
  
  ns <- NS(id)
  
  if(is.null(lan)){
    lan <- use_language()
  }
  
  tagList(
    fluidRow(
      column(
        width = 10, offset = 1,
        
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("user"),
              label = lan$get("User:"),
              choices = lan$get("All users"),
              selected = lan$get("All users"),
              multiple = TRUE,
              width = "100%"
            )
          ),
          column(
            width = 3,
            dateRangeInput(
              inputId = ns("overview_period"),
              label = lan$get("Period:"),
              start = Sys.Date() - 31,
              end = Sys.Date(),
              width = "100%"
            )
          ),
          column(
            width = 6,
            actionButton(
              inputId = ns("last_week"),
              label = lan$get("Last week"),
              class = "btn-primary btn-sm btn-margin"
            ),
            actionButton(
              inputId = ns("last_month"),
              label = lan$get("Last month"),
              class = "btn-primary btn-sm btn-margin"
            ),
            actionButton(
              inputId = ns("all_period"),
              label = lan$get("All period"),
              class = "btn-primary btn-sm btn-margin"
            )
          )
        ),
        
        conditionalPanel(condition = paste0("output['",ns("print_app_input_js"),"']"),
                         fluidRow(
                           column(
                             width = 12,
                             selectInput(
                               inputId = ns("app"),
                               label = "Application :",
                               choices = get_appname(),
                               selected = get_appname(),
                               multiple = TRUE,
                               width = "100%"
                             )
                           )
                         )
        ),
        
        tags$h3(icon("users"), lan$get("Number of connections per user"), class = "text-primary"),
        tags$hr(),
        billboarderOutput(outputId = ns("graph_conn_users"), height = "600px"),
        
        tags$br(),
        
        tags$h3(icon("calendar-days"), lan$get("Number of connections per day"), class = "text-primary"),
        tags$hr(),
        billboarderOutput(outputId = ns("graph_conn_days")),

        tags$br(),

        tags$h3(icon("address-card"), lan$get("Users overview"), class = "text-primary"),
        tags$hr(),
        DTOutput(outputId = ns("table_users_overview")),

        tags$br(),

        tags$h3(icon("shield-halved"), lan$get("Security events"), class = "text-primary"),
        tags$hr(),
        uiOutput(outputId = ns("security_kpis")),
        billboarderOutput(outputId = ns("graph_security_days")),
        
        if("logs" %in% get_download()){
          list(tags$br(), tags$br(),
               
               downloadButton(
                 outputId = ns("download_logs"),
                 label = lan$get("Download logs database"),
                 class = "btn-primary center-block",
                 icon = icon("download")
               ))
        },
        
        tags$br()
      )
    )
  )
}

#' @importFrom billboarder renderBillboarder billboarder bb_barchart
#'  bb_y_grid bb_data bb_legend bb_labs bb_linechart bb_colors_manual
#'  bb_x_axis bb_zoom %>% bb_bar_color_manual
#' @importFrom shiny reactiveValues observe req updateSelectInput updateDateRangeInput reactiveVal outputOptions renderUI
#' @importFrom utils write.table
#' @importFrom stats setNames
#' @importFrom DT renderDT datatable
logs <- function(input, output, session, sqlite_path, passphrase, config_db,
                 fileEncoding = "", lan = NULL) {
  
  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }

  token_start <- isolate(getToken(session = session))

  logs_rv <- reactiveValues(logs = NULL, logs_period = NULL, users = NULL,
                            logs_all = NULL, logs_all_period = NULL, pwd_mngt = NULL)
  print_app_input <- reactiveVal(FALSE)
  
  observe({
    if(show_logs_enabled() & "overview_period" %in% isolate(names(input))){
      if(!is.null(sqlite_path)){
        conn <- dbConnect(SQLite(), dbname = sqlite_path)
        on.exit(dbDisconnect(conn))
        logs_rv$logs <- read_db_decrypt(conn = conn, name = "logs", passphrase = passphrase)
        logs_rv$users <- read_db_decrypt(conn = conn, name = "credentials", passphrase = passphrase)
        logs_rv$pwd_mngt <- tryCatch(
          read_db_decrypt(conn = conn, name = "pwd_mngt", passphrase = passphrase),
          error = function(e) NULL
        )
      } else {
        conn <- connect_sql_db(config_db)
        on.exit(disconnect_sql_db(conn, config_db))
        logs_rv$logs <- db_read_table_sql(conn, config_db$tables$logs$tablename)
        logs_rv$users <- read_db_decrypt(conn = conn, name = config_db$tables$credentials$tablename, passphrase = passphrase)
        logs_rv$pwd_mngt <- tryCatch(
          db_read_table_sql(conn, config_db$tables$pwd_mngt$tablename),
          error = function(e) NULL
        )
        
      }
      
      isolate({
        ctrl_log <- isolate({logs_rv$logs})
        # treat old bad admin log (failed attempts have no token: keep them)
        if(any(duplicated(ctrl_log$token))){
          ctrl_log$date_days <- substring(ctrl_log$server_connected, 1, 10)
          ind_dup <- duplicated(ctrl_log[, c("user", "token", "date_days")])
          ctrl_log <- ctrl_log[is.na(ctrl_log$token) | !ind_dup, ]
          ctrl_log$date_days <- NULL
          logs_rv$logs <- ctrl_log
        }

        logs_rv$logs_all <- logs_rv$logs
        if("status" %in% colnames(isolate({logs_rv$logs}))){
          logs_rv$logs <- logs_rv$logs[logs_rv$logs$status %in% "Success", ]
        }
      })
      
   
      updateSelectInput(
        session = session,
        inputId = "user",
        choices = c(lan()$get("All users"), as.character(logs_rv$users$user)),
        selected = lan()$get("All users")
      )
      
      app_choices <- unique(c(isolate(logs_rv$logs_all$app), get_appname()))
      updateSelectInput(
        session = session,
        inputId = "app",
        choices = c("All applications", as.character(app_choices)),
        selected = get_appname()
      )
      if(length(app_choices) <= 1){
        print_app_input(FALSE)
      } else {
        print_app_input(TRUE)
      }
    }
  })
  
  output$print_app_input_js <- reactive({
    print_app_input()
  })
  outputOptions(output, "print_app_input_js", suspendWhenHidden = FALSE)
  
  observe({
    req(logs_rv$logs)
    req(input$overview_period)
    req(input$user)
    filter_logs <- function(logs) {
      logs$date <- as.Date(substr(logs$server_connected, 1, 10))
      logs <- logs[logs$date >= input$overview_period[1] & logs$date <= input$overview_period[2], ]
      if (!lan()$get("All users") %in% input$user) {
        logs <- logs[logs$user %in% input$user, ]
      }
      if (length(input$app) > 0 && !"All applications" %in% input$app) {
        logs <- logs[logs$app %in% input$app, ]
      }
      logs
    }
    logs_rv$logs_period <- filter_logs(isolate(logs_rv$logs))
    logs_rv$logs_all_period <- filter_logs(isolate(logs_rv$logs_all))
  })

  # users overview: last connection / application on all the successful
  # connections (all period, all applications), counts on the selected period
  output$table_users_overview <- renderDT({
    req(logs_rv$users)
    req(logs_rv$logs_all_period)
    req(length(input$user) > 0)

    users <- as.character(logs_rv$users$user)
    if (!lan()$get("All users") %in% input$user) {
      users <- users[users %in% input$user]
    }
    logs_period <- logs_rv$logs_all_period
    if (!"status" %in% colnames(logs_period)) {
      logs_period$status <- rep("Success", nrow(logs_period))
    }

    overview <- users_overview(
      users = users,
      logs = logs_rv$logs,
      logs_period = logs_period,
      pwd_mngt = logs_rv$pwd_mngt,
      pwd_failure_limit = get_pwd_failure_limit()
    )
    if ("locked" %in% colnames(overview)) {
      overview$locked <- ifelse(overview$locked, lan()$get("Yes"), lan()$get("No"))
    }
    col_labels <- c(
      user = lan()$get("user"),
      last_connection = lan()$get("Last connection"),
      last_app = lan()$get("Last application"),
      n_success = lan()$get("Nb logged"),
      n_wrong_pwd = lan()$get("Wrong passwords"),
      n_reset = lan()$get("Password resets"),
      locked = lan()$get("Locked")
    )

    datatable(
      data = overview,
      colnames = unname(col_labels[colnames(overview)]),
      rownames = FALSE,
      selection = "none",
      style = "bootstrap",
      options = list(
        scrollY = if (nrow(overview) > 10) "500px",
        lengthChange = FALSE,
        paging = FALSE,
        scrollX = TRUE,
        order = list(list(1, "desc")),
        language = lan()$get_DT()
      )
    )
  })

  # security events on the selected period. Unknown usernames are only counted,
  # never displayed: users sometimes type their password in the username field.
  output$security_kpis <- renderUI({
    req(logs_rv$logs_all_period)
    logs <- logs_rv$logs_all_period
    status <- if ("status" %in% colnames(logs)) logs$status else character(0)
    category <- security_category(status)

    kpis <- list(
      "Wrong passwords" = sum(category %in% "Wrong passwords"),
      "Unknown users" = sum(category %in% "Unknown users"),
      "Password resets" = sum(category %in% "Password resets"),
      "Failed password resets" = sum(category %in% "Failed password resets"),
      "Reset errors" = sum(status %in% security_mail_status())
    )
    limit <- suppressWarnings(as.numeric(get_pwd_failure_limit()))
    pwd_mngt <- logs_rv$pwd_mngt
    if (is_finite_limit(limit) &&
        !is.null(pwd_mngt) && "n_wrong_pwd" %in% colnames(pwd_mngt)) {
      # current state, not limited to the period
      kpis <- c(list("Locked accounts" = sum(pwd_mngt$n_wrong_pwd >= limit, na.rm = TRUE)), kpis)
    }

    fluidRow(
      lapply(names(kpis), function(x) {
        column(
          width = 2,
          tags$div(
            class = "well well-sm text-center",
            tags$h3(kpis[[x]], style = "margin-top: 5px;"),
            tags$span(lan()$get(x))
          )
        )
      })
    )
  })

  output$graph_security_days <- renderBillboarder({
    req(logs_rv$logs_all_period)
    logs <- logs_rv$logs_all_period
    req("status" %in% colnames(logs))
    logs$category <- security_category(logs$status)
    logs <- logs[!is.na(logs$category), ]
    req(nrow(logs) > 0)

    nb_day <- security_events_per_day(logs)
    categories <- setdiff(colnames(nb_day), "day")
    names_lan <- setNames(lapply(categories, function(x) lan()$get(x)), categories)

    billboarder() %>%
      bb_barchart(data = nb_day, stacked = TRUE) %>%
      bb_data(names = names_lan) %>%
      bb_y_grid(show = TRUE) %>%
      bb_labs(title = lan()$get("Number of security events per day")) %>%
      bb_zoom(
        enabled = list(type = "drag"),
        resetButton = list(text = "Unzoom")
      )
  })
  
  output$graph_conn_users <- renderBillboarder({
    req(logs_rv$logs_period)
    req(nrow(logs_rv$logs_period) > 0)
    req(length(input$user) > 0)
    
    logs <- logs_rv$logs_period
    
    nb_log <- as.data.frame(table(user = logs$user), stringsAsFactors = FALSE)
    nb_log <- nb_log[order(nb_log$Freq, decreasing = TRUE), ]
    
    billboarder() %>%
      bb_barchart(data = nb_log, rotated = TRUE) %>%
      bb_bar_color_manual(list(Freq = "#4582ec")) %>%
      bb_y_grid(show = TRUE) %>%
      bb_data(names = list(Freq = lan()$get("Nb logged"))) %>%
      bb_legend(show = FALSE) %>%
      bb_x_axis(tick = list(width = 10000)) %>%
      bb_labs(
        # title = "Number of connection by user",
        y = lan()$get("Total number of connection")
      ) %>%
      bb_zoom(
        enabled = list(type = "drag"),
        resetButton = list(text = "Unzoom")
      )
  })
  
  
  output$graph_conn_days <- renderBillboarder({
    req(logs_rv$logs_period)
    req(nrow(logs_rv$logs_period) > 0)
    req(length(input$user) > 0)
    
    logs <- logs_rv$logs_period
    
    nb_log_day <- as.data.frame(table(day = substr(logs$server_connected, 1, 10)), stringsAsFactors = FALSE)
    
    nb_log_day$day <- as.Date(nb_log_day$day)
    nb_log_day <- merge(
      x = data.frame(day = seq(
        from = min(nb_log_day$day) - 1, to = max(nb_log_day$day) + 1, by = "1 day"
      )),
      y = nb_log_day, by = "day", all.x = TRUE
    )
    nb_log_day$Freq[is.na(nb_log_day$Freq)] <- 0
    
    billboarder() %>%
      bb_linechart(data = nb_log_day, type = "area-step") %>%
      bb_colors_manual(list(Freq = "#4582ec")) %>%
      bb_x_axis(type = "timeseries", tick = list(fit = FALSE)) %>%
      bb_y_grid(show = TRUE) %>%
      bb_data(names = list(Freq = lan()$get("Nb logged"))) %>%
      bb_legend(show = FALSE) %>%
      bb_labs(
        # title = "Number of connection by user",
        y = lan()$get("Total number of connection")
      ) %>%
      # bb_bar(width = list(ratio = 1, max = 30)) %>%
      bb_zoom(
        enabled = list(type = "drag"),
        resetButton = list(text = "Unzoom")
      )
  })
  
  
  observeEvent(input$last_week, {
    updateDateRangeInput(
      session = session,
      inputId = "overview_period",
      start = Sys.Date() - 7,
      end = Sys.Date()
    )
  })
  
  observeEvent(input$last_month, {
    updateDateRangeInput(
      session = session,
      inputId = "overview_period",
      start = Sys.Date() - 31,
      end = Sys.Date()
    )
  })
  
  observeEvent(input$all_period, {
    updateDateRangeInput(
      session = session,
      inputId = "overview_period",
      start = min(substr(logs_rv$logs$server_connected, 1, 10), na.rm = TRUE),
      end = Sys.Date()
    )
  })
  
  output$download_logs <- downloadHandler(
    
    filename = function() {
      paste('shinymanager-logs-', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      req("logs" %in% get_download())
      # the token must still be valid and belong to an admin (as in module-admin.R)
      req(.tok$is_active(token_start) && .tok$is_admin(token_start))
      if(!is.null(sqlite_path)){
        conn <- dbConnect(SQLite(), dbname = sqlite_path)
        on.exit(dbDisconnect(conn))
        logs <- read_db_decrypt(conn = conn, name = "logs", passphrase = passphrase)
        users <- read_db_decrypt(conn = conn, name = "credentials", passphrase = passphrase)
        
        # treat old bad admin log
        if(any(duplicated(logs$token))){
          logs$date_days <- substring(logs$server_connected, 1, 10)
          logs$ind_dup <- duplicated(logs[, c("user", "token", "date_days")])
          logs <- logs[is.na(logs$token) | (!is.na(logs$token) & !logs$ind_dup), ]
          logs$date_days <- NULL
          logs$ind_dup <- NULL
        }
      } else {
        conn <- connect_sql_db(config_db)
        on.exit(disconnect_sql_db(conn, config_db))
        
        logs <- db_read_table_sql(conn, config_db$tables$logs$tablename)
        users <-  db_read_table_sql(conn, config_db$tables$credentials$tablename)
      }
      
      logs$token <- NULL
      
      users$password <- NULL
      users$is_hashed_password <- NULL
      
      if(all(is.na(users$start))) users$start <- NULL
      if(all(is.na(users$expire))) users$expire <- NULL
      logs <- merge(logs, users, by = "user", all.x = TRUE, sort = FALSE)
      logs <- logs[order(logs$server_connected, decreasing = TRUE), ]
      write.table(logs, con, sep = ";", row.names = FALSE, na = '', fileEncoding = fileEncoding)
    }
  )
}


# Security events of the logs
# ---------------------------
# Failed statuses written by save_logs_failed() / save_reset_logs(), grouped in
# categories (the names are also the labels to translate). NA for the other
# statuses (successful connections).
security_categories <- function() {
  c("Wrong passwords", "Unknown users", "Locked accounts", "Expired / unauthorized",
    "Password resets", "Failed password resets")
}

security_category <- function(status) {
  status <- as.character(status)
  category <- rep(NA_character_, length(status))
  category[status %in% "Wrong pwd"] <- "Wrong passwords"
  category[status %in% "Unknown user"] <- "Unknown users"
  category[status %in% c("Locked Account", "Account locked")] <- "Locked accounts"
  category[status %in% c("Expired", "Unauthorized", "Reset password: expired")] <- "Expired / unauthorized"
  category[status %in% "Reset password"] <- "Password resets"
  category[is.na(category) & grepl("^Reset password: ", status)] <- "Failed password resets"
  category
}

# Statuses of a reset that could not send the email (or save the password)
security_mail_status <- function() {
  c("Reset password: mail failed", "Reset password: db error", "Reset password: config error")
}

# Number of events per day (rows, every day of the range) and category
# (columns, only the categories present), from logs with a 'category' column.
security_events_per_day <- function(logs) {
  dates <- as.Date(substr(logs$server_connected, 1, 10))
  days <- as.character(seq(from = min(dates), to = max(dates), by = "1 day"))
  categories <- security_categories()
  categories <- categories[categories %in% logs$category]
  nb_day <- as.data.frame.matrix(table(
    factor(as.character(dates), levels = days),
    factor(logs$category, levels = categories)
  ))
  nb_day <- cbind(data.frame(day = days, stringsAsFactors = FALSE), nb_day)
  rownames(nb_day) <- NULL
  nb_day
}

# One row per user: last connection and application (on 'logs', successful
# connections of all periods and applications), number of connections / wrong
# passwords / resets (on 'logs_period', all statuses), and locked status when a
# failure limit is set.
users_overview <- function(users, logs, logs_period, pwd_mngt = NULL, pwd_failure_limit = Inf) {
  logs <- logs[order(logs$server_connected, decreasing = TRUE), , drop = FALSE]
  last <- logs[!duplicated(logs$user), , drop = FALSE]
  ind_last <- match(users, last$user)

  count_status <- function(status) {
    as.integer(table(factor(logs_period$user[logs_period$status %in% status], levels = users)))
  }

  overview <- data.frame(
    user = users,
    last_connection = as.character(last$server_connected[ind_last]),
    last_app = as.character(last$app[ind_last]),
    n_success = count_status("Success"),
    n_wrong_pwd = count_status("Wrong pwd"),
    n_reset = count_status("Reset password"),
    stringsAsFactors = FALSE
  )

  limit <- suppressWarnings(as.numeric(pwd_failure_limit))
  if (is_finite_limit(limit) &&
      !is.null(pwd_mngt) && "n_wrong_pwd" %in% colnames(pwd_mngt)) {
    n_wrong <- pwd_mngt$n_wrong_pwd[match(users, pwd_mngt$user)]
    overview$locked <- !is.na(n_wrong) & n_wrong >= limit
  }
  overview
}
