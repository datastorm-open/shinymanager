
is_sqlite <- function(path) {
  is.character(path) && file.exists(path) && grepl(pattern = "\\.sqlite$", x = path)
}

is_yaml <- function(path) {
  is.character(path) && file.exists(path) && (grepl(pattern = "\\.yaml$", x = tolower(path)) | grepl(pattern = "\\.yml$", x = tolower(path)))
}

hasName <- function(x, name) {
  match(name, names(x), nomatch = 0L) > 0L
}

write_logs_enabled <- function() {
  getOption("shinymanager.write_logs", default = TRUE)
}

show_logs_enabled <- function() {
  getOption("shinymanager.show_logs", default = TRUE)
}

get_auto_sql_reader <- function() {
  getOption("shinymanager.auto_sql_reader", default = Inf)
}

get_auto_sqlite_reader <- function() {
  getOption("shinymanager.auto_sqlite_reader", default = 1000)
}

get_appname <- function() {
  getOption("shinymanager.application", default = basename(getwd()))
}

get_download <- function(){
  getOption("shinymanager.download", default = c("db", "logs", "users"))
}

get_pwd_validity <- function(){
  getOption("shinymanager.pwd_validity", default = Inf)
}

get_pwd_failure_limit <- function(){
  getOption("shinymanager.pwd_failure_limit", default = Inf)
}


get_args <- function(..., fun) {
  args_fun <- names(formals(fun))
  args <- list(...)
  args[names(args) %in% args_fun]
}

#' @importFrom R.utils capitalize
make_title <- function(x) {
  capitalize(gsub(
    pattern = "_", replacement = " ", x = x
  ))
}

dropFalse <- function(x) {
  isFALSE <- Negate(isTRUE)
  x[!vapply(x, isFALSE, FUN.VALUE = logical(1))]
}

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
is_force_chg_pwd <- function(token) {
  user_info <- .tok$get(token)
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
  config_db  <- .tok$get_sql_config_db()
  
  # sqlite
  if (!is.null(sqlite_path)) {
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))
    resetpwd <- read_db_decrypt(conn, name = "pwd_mngt", passphrase = passphrase)
    ind_user <- resetpwd$user %in% user_info$user
    # first check must change
    res <- identical(resetpwd$must_change[ind_user], "TRUE")
    # then pwd_validity
    if(!res){
      pwd_validity <- as.numeric(get_pwd_validity())
      if(length(pwd_validity) > 0 && !is.na(pwd_validity)){
        user_date <- as.Date(resetpwd$date_change[ind_user])
        if(length(user_date) > 0 && !is.na(user_date)){
          res <- as.numeric(difftime(Sys.Date(), user_date, units = "days")) > pwd_validity
        }
      }
    }
    return(res)
  } else  if (!is.null(config_db)) {
    conn <- connect_sql_db(config_db)
    on.exit(disconnect_sql_db(conn, config_db))
    
    user <- user_info$user
    tablename <- SQL(config_db$tables$pwd_mngt$tablename)
    request <- glue_sql(config_db$tables$pwd_mngt$select, .con = conn)
    resetpwd <- dbGetQuery(conn, request)
    
    # first check must change
    res <- resetpwd$must_change[1]
    
    # then pwd_validity
    if(!res){
      pwd_validity <- as.numeric(get_pwd_validity())
      if(length(pwd_validity) > 0 && !is.na(pwd_validity)){
        user_date <- as.Date(resetpwd$date_change[1])
        if(length(user_date) > 0 && !is.na(user_date)){
          res <- as.numeric(difftime(Sys.Date(), user_date, units = "days")) > pwd_validity
        }
      }
    }
    return(res)
    
  } else {
    return(FALSE)
  }
}

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
force_chg_pwd <- function(user, change = TRUE) {
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
  config_db  <- .tok$get_sql_config_db()
  
  # sqlite
  if (!is.null(sqlite_path)) {
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))
    resetpwd <- read_db_decrypt(conn, name = "pwd_mngt", passphrase = passphrase)
    if(nrow(resetpwd) > 0){
      if(!"n_wrong_pwd" %in% colnames(resetpwd)){
        resetpwd$n_wrong_pwd <- 0
      }
    }
    
    ind_user <- resetpwd$user %in% user
    resetpwd$must_change[ind_user] <- change
    resetpwd$n_wrong_pwd[ind_user] <- 0
    
    if (!isTRUE(change)) {
      resetpwd$have_changed[ind_user] <- TRUE
      resetpwd$date_change[ind_user] <- as.character(Sys.Date())
    }
    
    write_db_encrypt(conn, value = resetpwd, name = "pwd_mngt", passphrase = passphrase)
    
  } else  if (!is.null(config_db)) {
    conn <- connect_sql_db(config_db)
    on.exit(disconnect_sql_db(conn, config_db))
    
    tablename <- SQL(config_db$tables$pwd_mngt$tablename)
    
    udpate_users <- user
    
    name <- "must_change"
    value <- change
    request <- glue_sql(config_db$tables$pwd_mngt$update, .con = conn)
    dbExecute(conn, request)
    
    name <- "n_wrong_pwd"
    value <- 0
    request <- glue_sql(config_db$tables$pwd_mngt$update, .con = conn)
    dbExecute(conn, request)
    
    if (!isTRUE(change)) {
      name <- "have_changed"
      value <- TRUE
      request <- glue_sql(config_db$tables$pwd_mngt$update, .con = conn)
      dbExecute(conn, request)
      
      name <- "date_change"
      value <- Sys.Date()
      request <- glue_sql(config_db$tables$pwd_mngt$update, .con = conn)
      dbExecute(conn, request)
      
    }
  }
}

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
update_pwd <- function(user, pwd) {
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
  config_db  <- .tok$get_sql_config_db()
  
  # sqlite
  if (!is.null(sqlite_path)) {
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))
    
    res_pwd <- try({
      users <- read_db_decrypt(conn, name = "credentials", passphrase = passphrase)
      ind_user <- users$user %in% user
      if(!"character" %in% class(users$password)){
        users$password <- as.character(users$password)
      }
      if(!"is_hashed_password" %in% colnames(users)){
        users$is_hashed_password <- FALSE
      }
      users$password[ind_user] <- pwd
      users$is_hashed_password[ind_user] <- FALSE
      write_db_encrypt(conn, value = users, name = "credentials", passphrase = passphrase)
      force_chg_pwd(user, FALSE)
    }, silent = TRUE)
    return(list(result = !inherits(res_pwd, "try-error")))
  } else  if (!is.null(config_db)) {
    res_pwd <- try({
      
      conn <- connect_sql_db(config_db)
      on.exit(disconnect_sql_db(conn, config_db))
      
      value <- scrypt::hashPassword(pwd)
      name <- "password"
      udpate_users <- user
      
      tablename <- SQL(config_db$tables$credentials$tablename)
      request <- glue_sql(config_db$tables$credentials$update, .con = conn)
      dbExecute(conn, request)
      
      force_chg_pwd(user, FALSE)
    })
    return(list(result = !inherits(res_pwd, "try-error")))
  } else {
    return(list(result = FALSE))
  }
}

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
check_new_pwd <- function(user, pwd) {
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
  config_db  <- .tok$get_sql_config_db()
  
  # sqlite
  if (!is.null(sqlite_path)) {
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))
    res_pwd <- try({
      users <- read_db_decrypt(conn, name = "credentials", passphrase = passphrase)
      ind_user <- users$user %in% user
      if("is_hashed_password" %in% colnames(users)){
        if(users$is_hashed_password[ind_user]){
          return(!scrypt::verifyPassword(users$password[ind_user], pwd))
        }
      } else {
        return(!identical(users$password[ind_user], pwd))
      }
    }, silent = TRUE)
    if("try-error" %in% class(res_pwd)) res_pwd <- TRUE
    return(res_pwd)
    
  } else  if (!is.null(config_db)) {
    
    res_pwd <- try({
      conn <- connect_sql_db(config_db)
      on.exit(disconnect_sql_db(conn, config_db))
      
      tablename <- SQL(config_db$tables$credentials$tablename)
      request <- glue_sql(config_db$tables$credentials$select, .con = conn)
      user_info <- dbGetQuery(conn, request)
      
      !scrypt::verifyPassword(user_info$password[1], pwd)
    }, silent = TRUE)
    
    if("try-error" %in% class(res_pwd)) res_pwd <- TRUE
    return(res_pwd)
  } else {
    return(TRUE)
  }
}


#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
save_logs <- function(token) {
  if(write_logs_enabled()){
    sqlite_path <- .tok$get_sqlite_path()
    passphrase <- .tok$get_passphrase()
    user <- .tok$get_user(token)
    config_db  <- .tok$get_sql_config_db()
    
    # sqlite ?
    if (!is.null(sqlite_path)) {
      conn <- dbConnect(SQLite(), dbname = sqlite_path)
      on.exit(dbDisconnect(conn))
      res_logs <- try({
        logs <- read_db_decrypt(conn = conn, name = "logs", passphrase = passphrase)
        # patch for old logs database
        if(!"status" %in% colnames(logs)){
          if(nrow(logs) > 0){
            logs$status <- "Success"
          } else {
            logs$status <- character(0)
          }
        }
        
        # check if current admin user
        date_time <- as.character(Sys.time())
        date_day <- substring(date_time, 1, 10)
        logs_day <- substring(logs$server_connected, 1, 10)
        already_user_token <- any(logs$user %in% user & logs_day %in% date_day & logs$token %in% token)
        
        if(!already_user_token){
          logs <- rbind(logs, data.frame(
            user = user,
            server_connected = date_time,
            token = token,
            logout = NA_character_,
            app = get_appname(),
            status = "Success",
            stringsAsFactors = FALSE
          ))
          write_db_encrypt(conn = conn, value = logs, name = "logs", passphrase = passphrase)
          
          # update pwd_management
          pwd_mngt <- read_db_decrypt(conn = conn, name = "pwd_mngt", passphrase = passphrase)
          if(nrow(pwd_mngt) > 0){
            if(!"n_wrong_pwd" %in% colnames(pwd_mngt)){
              pwd_mngt$n_wrong_pwd <- 0
            } else {
              pwd_mngt$n_wrong_pwd[pwd_mngt$user %in% user] <- 0
            }
            write_db_encrypt(conn = conn, value = pwd_mngt, name = "pwd_mngt", passphrase = passphrase)
          }
          
        }
      }, silent = TRUE)
      if (inherits(res_logs, "try-error")) {
        warning(paste(
          "shinymanager: unable to save logs | error:", attr(res_logs, "condition")$message
        ), call. = FALSE)
      }
    } else if(!is.null(config_db)){
      
      conn <- connect_sql_db(config_db)
      on.exit(disconnect_sql_db(conn, config_db))
      
      # check if current admin user
      tablename <- SQL(config_db$tables$logs$tablename)
      request <- glue_sql(config_db$tables$logs$check_token, .con = conn)
      already_user_token <- dbGetQuery(conn, request)
      
      if(nrow(already_user_token) == 0){
        
        write_sql_db(
          config_db = config_db, 
          value = data.frame(
            user = user,
            server_connected = as.character(Sys.time()),
            token = token,
            logout = NA_character_,
            app = get_appname(),
            status = "Success",
            stringsAsFactors = FALSE
          ), 
          name = config_db$tables$logs$tablename
        )
        
        # update pwd_management
        tablename <- SQL(config_db$tables$pwd_mngt$tablename)
        request <- glue_sql(config_db$tables$pwd_mngt$select, .con = conn)
        pwd_mngt_user <- dbGetQuery(conn, request)
        
        if(nrow(pwd_mngt_user) > 0){
          if("n_wrong_pwd" %in% colnames(pwd_mngt_user)){
            value <- 0
            name <- "n_wrong_pwd"
            udpate_users <- user
            request <- glue_sql(config_db$tables$pwd_mngt$update, .con = conn)
            db <- dbExecute(conn, request)
          } 
        }
      }
    }
  }
}

check_locked_account <- function(user, pwd_failure_limit) {
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
  config_db  <- .tok$get_sql_config_db()
  
  # sqlite
  if (!is.null(sqlite_path)) {
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))
    res_lock <- try({
      pwd_mngt <- read_db_decrypt(conn = conn, name = "pwd_mngt", passphrase = passphrase)
      if(nrow(pwd_mngt) > 0 && "n_wrong_pwd" %in% colnames(pwd_mngt)){
        ind_user <- which(pwd_mngt$user %in% user)
        if(length(ind_user) > 0){
          pwd_mngt$n_wrong_pwd[ind_user] >= pwd_failure_limit
        } else {
          FALSE
        }
      } else {
        FALSE
      }
    }, silent = TRUE)
    if (inherits(res_lock, "try-error")) res_lock <- FALSE
    return(res_lock)
  } else  if (!is.null(config_db)) {
    
    res_lock <- try({
      
      conn <- connect_sql_db(config_db)
      on.exit(disconnect_sql_db(conn, config_db))
      
      tablename <- SQL(config_db$tables$pwd_mngt$tablename)
      request <- glue_sql(config_db$tables$pwd_mngt$select, .con = conn)
      pwd_mngt <- dbGetQuery(conn, request)
      
      if(nrow(pwd_mngt) == 1 && "n_wrong_pwd" %in% colnames(pwd_mngt)){
        pwd_mngt$n_wrong_pwd[1] >= pwd_failure_limit
      } else {
        FALSE
      }
    }, silent = TRUE)
    
    if (inherits(res_lock, "try-error")) res_lock <- FALSE
    return(res_lock)
    
  } else {
    return(FALSE)
  }
}

save_logs_failed <- function(user, status = "Failed") {
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
  
  config_db <- .tok$get_sql_config_db()
  
  # sqlite ?
  if (!is.null(sqlite_path)) {
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))
    res_logs <- try({
      logs <- read_db_decrypt(conn = conn, name = "logs", passphrase = passphrase)
      # patch for old logs database
      if(!"status" %in% colnames(logs)){
        if(nrow(logs) > 0){
          logs$status <- "Success"
        } else {
          logs$status <- character(0)
        }
      }
      logs <- rbind(logs, data.frame(
        user = user,
        server_connected = as.character(Sys.time()),
        token = NA_character_,
        logout = NA_character_,
        app = get_appname(),
        status = status,
        stringsAsFactors = FALSE
      ))
      write_db_encrypt(conn = conn, value = logs, name = "logs", passphrase = passphrase)
      
      if(status %in% "Wrong pwd"){
        # update pwd_management
        pwd_mngt <- read_db_decrypt(conn = conn, name = "pwd_mngt", passphrase = passphrase)
        if(nrow(pwd_mngt) > 0){
          if(!"n_wrong_pwd" %in% colnames(pwd_mngt)){
            pwd_mngt$n_wrong_pwd <- 0
          } 
          
          ind_user <- which(pwd_mngt$user %in% user)
          pwd_mngt$n_wrong_pwd[ind_user] <- pwd_mngt$n_wrong_pwd[ind_user] + 1
          write_db_encrypt(conn = conn, value = pwd_mngt, name = "pwd_mngt", passphrase = passphrase)
        }
      }
      
    }, silent = TRUE)
    
    if (inherits(res_logs, "try-error")) {
      warning(paste(
        "shinymanager: unable to save logs | error:", attr(res_logs, "condition")$message
      ), call. = FALSE)
    }
  } else if(!is.null(config_db)){
    res_logs <- try({
      
      conn <- connect_sql_db(config_db)
      on.exit(disconnect_sql_db(conn, config_db))
      
      if(write_logs_enabled()){
        write_sql_db(
          config_db = config_db, 
          value = data.frame(
            user = user,
            server_connected = as.character(Sys.time()),
            token = NA_character_,
            logout = NA_character_,
            app = get_appname(),
            status = status
          ), 
          name = config_db$tables$logs$tablename
        )
      }
      
      if(status %in% "Wrong pwd"){
        # update pwd_management
        
        tablename <- SQL(config_db$tables$pwd_mngt$tablename)
        request <- glue_sql(config_db$tables$pwd_mngt$select, .con = conn)
        pwd_mngt_user <- dbGetQuery(conn, request)
        
        if(nrow(pwd_mngt_user) > 0){
          if("n_wrong_pwd" %in% colnames(pwd_mngt_user)){
            value <- pwd_mngt_user$n_wrong_pwd + 1
            name <- "n_wrong_pwd"
            udpate_users <- user
            request <- glue_sql(config_db$tables$pwd_mngt$update, .con = conn)
            db <- dbExecute(conn, request)
          } 
        }
      }
    }, silent = TRUE)
    
    if (inherits(res_logs, "try-error")) {
      warning(paste(
        "shinymanager: unable to save logs | error:", attr(res_logs, "condition")$message
      ), call. = FALSE)
    }
    
  }
}

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
logout_logs <- function(token) {
  
  if(write_logs_enabled()){
    sqlite_path <- .tok$get_sqlite_path()
    passphrase <- .tok$get_passphrase()
    config_db <- .tok$get_sql_config_db()
    
    # sqlite ?
    if (!is.null(sqlite_path)) {
      conn <- dbConnect(SQLite(), dbname = sqlite_path)
      on.exit(dbDisconnect(conn))
      res_logs <- try({
        logs <- read_db_decrypt(conn = conn, name = "logs", passphrase = passphrase)
        logs$logout[logs$token  %in% token] <- as.character(Sys.time())
        write_db_encrypt(conn = conn, value = logs, name = "logs", passphrase = passphrase)
      }, silent = TRUE)
      if (inherits(res_logs, "try-error")) {
        warning(paste(
          "shinymanager: unable to save logs | error:", attr(res_logs, "condition")$message
        ), call. = FALSE)
      }
    } else if(!is.null(config_db)){
      
      conn <- connect_sql_db(config_db)
      on.exit(disconnect_sql_db(conn, config_db))
      
      tablename <- SQL(config_db$tables$logs$tablename)
      request <- glue_sql(config_db$tables$logs$check_token, .con = conn)
      logs_user <- dbGetQuery(conn, request)
      
      if(nrow(logs_user) > 0){
        if("logout" %in% colnames(logs_user)){
          value <-  as.character(Sys.time())
          name <- "logout"
          token <- logs_user$token
          request <- glue_sql(config_db$tables$logs$update, .con = conn)
          db <- dbExecute(conn, request)
        } 
      }
    }
  }
}


#' Simple password generation
#'
#' @param n Number of password(s)
#'
#' @return a \code{character}
#' @export
#'
#' @importFrom openssl base64_encode rand_bytes
#'
#' @examples
#' generate_pwd()
#'
#' generate_pwd(3)
generate_pwd <- function(n = 1) {
  replicate(n = n, base64_encode(rand_bytes(6)))
}


validate_pwd <- function(pwd) {
  all(vapply(
    X = c("[0-9]+", "[a-z]+", "[A-Z]+", ".{6,}"),
    FUN = grepl, x = pwd, FUN.VALUE = logical(1)
  ))
}




