

#' @importFrom billboarder billboarderOutput
#' @importFrom shiny NS fluidRow column icon selectInput dateRangeInput downloadButton downloadHandler conditionalPanel
#' @importFrom htmltools tagList tags
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
#' @importFrom shiny reactiveValues observe req updateSelectInput updateDateRangeInput reactiveVal outputOptions
#' @importFrom utils write.table
logs <- function(input, output, session, sqlite_path, passphrase, config_db,
                 fileEncoding = "", lan = NULL) {
  
  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }
  
  logs_rv <- reactiveValues(logs = NULL, logs_period = NULL, users = NULL)
  print_app_input <- reactiveVal(FALSE)
  
  observe({
    if(show_logs_enabled() & "overview_period" %in% isolate(names(input))){
      if(!is.null(sqlite_path)){
        conn <- dbConnect(SQLite(), dbname = sqlite_path)
        on.exit(dbDisconnect(conn))
        logs_rv$logs <- read_db_decrypt(conn = conn, name = "logs", passphrase = passphrase)
        logs_rv$users <- read_db_decrypt(conn = conn, name = "credentials", passphrase = passphrase)
      } else {
        conn <- connect_sql_db(config_db)
        on.exit(disconnect_sql_db(conn, config_db))
        logs_rv$logs <- db_read_table_sql(conn, config_db$tables$logs$tablename)
        logs_rv$users <- read_db_decrypt(conn = conn, name = config_db$tables$credentials$tablename, passphrase = passphrase)
        
      }
      
      isolate({
        ctrl_log <- isolate({logs_rv$logs})
        # treat old bad admin log
        if(any(duplicated(ctrl_log$token))){
          ctrl_log$date_days <- substring(ctrl_log$server_connected, 1, 10)
          ctrl_log <- ctrl_log[!duplicated(ctrl_log[, c("user", "token", "date_days")]), ]
          ctrl_log$date_days <- NULL
          logs_rv$logs <- ctrl_log
        }
        
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
      
      app_choices <- unique(unique(logs_rv$logs$app), get_appname())
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
    logs <- isolate(logs_rv$logs)
    logs$date <- as.Date(substr(logs$server_connected, 1, 10))
    logs <- logs[logs$date >= input$overview_period[1] & logs$date <= input$overview_period[2], ]
    if (!lan()$get("All users") %in% input$user) {
      logs <- logs[logs$user %in% input$user, ]
    }
    if (length(input$app) > 0 && !"All applications" %in% input$app) {
      logs <- logs[logs$app %in% input$app, ]
    }
    logs_rv$logs_period <- logs
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


