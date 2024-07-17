#' @title Create credentials SQL database
#'
#' @description Create a SQL (not SQLite but Postgres, MSSQL, MySQL...) database with credentials data using DBI interface.
#'
#' @param credentials_data A \code{data.frame} with information about users, \code{user} and \code{password} are required.
#' @param config_path Path to the yaml configuration. You can find a template for Posgres in package \code{system.file("sql_config/pg_template.yml", package = "shinymanager")}
#' 
#' @export
#'
#' @details The credentials \code{data.frame} can have the following columns:
#'  \itemize{
#'   \item \strong{user (mandatory)} : the user's name.
#'   \item \strong{password (mandatory)} : the user's password.
#'   \item \strong{admin (optional)} : logical, is user have admin right ? If so,
#'    user can access the admin mode (only available using a SQLite database). Initialize to FALSE if missing. 
#'   \item \strong{start (optional)} : the date from which the user will have access to the application. Initialize to NA if missing. 
#'   \item \strong{expire (optional)} : the date from which the user will no longer have access to the application. Initialize to NA if missing. 
#'   \item \strong{applications (optional)} : the name of the applications to which the user is authorized,
#'    separated by a semicolon. The name of the application corresponds to the name of the directory,
#'    or can be declared using : \code{options("shinymanager.application" = "my-app")}
#'   \item \strong{additional columns} : add others columns to retrieve the values server-side after authentication
#'  }
#'
#' @importFrom DBI dbConnect dbDisconnect dbSendQuery dbClearResult dbListTables dbAppendTable dbGetQuery dbExecute dbListFields
#' @importFrom scrypt hashPassword
#' @importFrom glue glue_sql
#' @import yaml
#'
#' @examples
#' \dontrun{
#'
#' library(shiny)
#' library(shinymanager)
#' 
#' #### init the SQL Database
#' # first edit the .yml configuration file
#' system.file("sql_config/pg_template.yml", package = "shinymanager")
#' 
#' 
#' # Init Credentials data
#' credentials <- data.frame(
#'   user = c("shiny", "shinymanager"),
#'   password = c("azerty", "12345"), # password will automatically be hashed
#'   stringsAsFactors = FALSE
#' )
#'
#' # Create SQL database
#' create_sql_db(
#'   credentials_data = credentials,
#'   config_path = "path/to/your_sql_configuration.yml"
#' )
#'
#' ### Use in shiny
#' ui <- fluidPage(
#'   tags$h2("My secure application"),
#'   verbatimTextOutput("auth_output")
#' )
#' 
#' # Wrap your UI with secure_app
#' ui <- secure_app(ui, choose_language = TRUE)
#' 
#'
#'server <- function(input, output, session) {
#'  
#'  # call the server part
#'  # check_credentials returns a function to authenticate users
#'  res_auth <- secure_server(
#'    check_credentials = check_credentials(db = "path/to/your_sql_configuration.yml")
#'  )
#'  
#'  output$auth_output <- renderPrint({
#'    reactiveValuesToList(res_auth)
#'  })
#'  
#'  observe({
#'    print(input$shinymanager_where)
#'    print(input$shinymanager_language)
#'  })
#'  
#'  # your classic server logic
#'  
#'}
#'
#'shinyApp(ui, server)
#'
#'}
#' 
#' @seealso \code{\link{create_db}}, \code{\link{create_sql_db}}, \code{\link{check_credentials}}
create_sql_db <- function(credentials_data, config_path) {
  if (!all(c("user", "password") %in% names(credentials_data))) {
    stop("credentials_data must contains columns: 'user', 'password'", call. = FALSE)
  }
  if(any(duplicated(credentials_data$user))){
    stop("Duplicated users in credentials_data", call. = FALSE)
  }
  if(!"admin" %in% names(credentials_data)){
    credentials_data$admin <- FALSE
  }
  if(!"start" %in% names(credentials_data)){
    credentials_data$start <- as.Date(NA)
  }
  if(!"expire" %in% names(credentials_data)){
    credentials_data$expire <- as.Date(NA)
  }
  
  default_col <- c("user", "password", "start", "expire", "admin")
  credentials_data <- credentials_data[, c(default_col,
                                           setdiff(colnames(credentials_data), default_col))]
  
  config_db <- tryCatch({
    yaml::yaml.load_file(config_path, eval.expr = TRUE)
  }, error = function(e) stop("Error reading 'config_path' SQL DB configuration :", e$message))
  
  verify_sql_config(config_db)
  
  ## init table
  conn <- connect_sql_db(config_db)
  on.exit(disconnect_sql_db(conn, config_db))
  
  init_user <- !config_db$tables$pwd_mngt$tablename %in% dbListTables(conn)
  
  for(t in config_db$tables){
    tablename <- t$tablename
    if(tablename %in% dbListTables(conn)){
      warning(tablename, " already exists in database. Please remove it if wanted / needed")
    } else {
      tablename <- SQL(tablename)
      request <- glue_sql(t$init, .con = conn)
      check_create_db <- dbClearResult(dbSendQuery(conn, request))
    }
  }
  
  if(init_user){
    # init credentials data
    write_sql_db(
      config_db = config_db, 
      value = credentials_data, 
      name = config_db$tables$credentials$tablename
    )
    
    
    write_sql_db(
      config_db = config_db, 
      value = data.frame(
        user = credentials_data$user,
        must_change = FALSE,
        have_changed = FALSE,
        date_change = Sys.Date(),
        n_wrong_pwd = 0,
        stringsAsFactors = FALSE
      ),
      name = config_db$tables$pwd_mngt$tablename
    )
  }
  
  invisible(TRUE)
}

write_sql_db <- function(config_db, value, name = "credentials") {
  ## init table
  conn <- connect_sql_db(config_db)
  on.exit(disconnect_sql_db(conn, config_db))
  name <- SQL(name)
  
  if("password" %in% colnames(value)){
    # store hashed password
    value$password <- sapply(value$password, function(x) scrypt::hashPassword(x))
  }
  
  if("MariaDBConnection" %in% class(conn)){
    is_logical <- sapply(value, function(x) "logical" %in% class(x) || all(x %in% c("TRUE", "FALSE")))
    if(any(is_logical)){
      for(i in which(is_logical)){
        value[[i]] <- as.integer(as.logical(value[[i]]))
      }
    }
  }
  
  if(!"spark_connection" %in% class(conn)){
    dbAppendTable(conn = conn, name = name, value = value)
  } else {
    col_date <- c("start", "expire", "date_change")
    for(i in col_date){
      if(i %in% colnames(value)){
        value[[i]] <- as.Date(value[[i]])
      }
    }
    if (!requireNamespace("sparklyr", quietly = TRUE)){
      stop("Needed sparklyr package")
    }
    
    shinymanager_tmp_table_1324354657 <- sparklyr::copy_to(conn, value, overwrite = TRUE)
    sparklyr::sdf_copy_to(conn, shinymanager_tmp_table_1324354657, overwrite = TRUE)
    request <- glue_sql("INSERT INTO TABLE {name} BY NAME SELECT * FROM shinymanager_tmp_table_1324354657", .con = conn)
    dbClearResult(dbSendQuery(conn, request))
  }
}

db_read_table_sql <- function(conn, tablename){
  if(!"spark_connection" %in% class(conn)){
    res <- dbReadTable(conn, tablename)
  } else {
    tablename <- SQL(tablename)
    request <- glue_sql("SELECT *  FROM {tablename}", .con = conn)
    res <- dbGetQuery(conn, request)
  }
  if("tbl" %in% class(res)) res <- as.data.frame(res)
  res
}

db_list_fields_sql <-function(conn, tablename){
  if(!"spark_connection" %in% class(conn)){
    dbListFields(conn, tablename) 
  } else {
    tablename <- SQL(tablename)
    request <- glue_sql("SELECT *  FROM {tablename} LIMIT 1", .con = conn)
    colnames(dbGetQuery(conn, request))
  }
}


verify_sql_config <- function(config_db){
  
  # all needed info
  stopifnot(all(c("r_packages", "connect", "tables") %in% names(config_db)))
  
  stopifnot(all(c("credentials", "pwd_mngt", "logs") %in% names(config_db$tables)))
  
  stopifnot(all(c("tablename", "init", "select", "update", "delete") %in% names(config_db$tables$credentials)))
  
  stopifnot(all(c("tablename", "init", "select", "update", "check_token") %in% names(config_db$tables$logs)))
  
  stopifnot(all(c("tablename", "init", "select", "update", "delete") %in% names(config_db$tables$pwd_mngt)))
  
  # require packages
  for(pck in config_db$r_packages){
    check <- suppressPackageStartupMessages({
      suppressWarnings({
        requireNamespace(pck, quietly = T)
      })
    })
    
    if(!check){
      stop("Can't load ", pck, " package")
    }
  }
  
  # connect / disconnect
  con <- connect_sql_db(config_db, force_connect = TRUE)
  disconnect_sql_db(con, config_db)
  
  
  invisible(TRUE)
}

get_fun <- function(x) {
  if(length(grep("::", x)) > 0) {
    parts <- strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    x
  }
}

connect_sql_db <- function(config_db, force_connect = FALSE){
  if(((length(config_db$connect_every_request) == 0) | (length(config_db$connect_every_request) > 0 &&  !config_db$connect_every_request && exists("con", envir = shinymanager_con))) && !force_connect){
    return(get("con", envir = shinymanager_con))
  }

  if("fun" %in% names(config_db$connect)){
    fun <- config_db$connect$fun
    params <- config_db$connect
    params$fun <- NULL
  } else {
    fun <- "dbConnect"
    params <- config_db$connect
  }
  con <- tryCatch({
    do.call(get_fun(fun), params)
  }, error = function(e) stop("Can't connect to database : ", e$message, ". Verify configuration file"))
  
  if((length(config_db$connect_every_request) == 0) | (length(config_db$connect_every_request) > 0 && !config_db$connect_every_request)){
    assign("con", con, envir = shinymanager_con)
  }
  
  con
}

disconnect_sql_db <- function(con, config_db = NULL){
  if(length(config_db$connect_every_request) > 0 && config_db$connect_every_request){
    if("fun" %in% names(config_db$disconnect)){
      fun <- config_db$disconnect$fun
    } else {
      fun <- "dbDisconnect"
    }
    tryCatch({
      do.call(get_fun(fun), list(con))
    }, error = function(e) stop("Can't disconnect to database : ", e$message, ". Verify configuration file"))
  }
}