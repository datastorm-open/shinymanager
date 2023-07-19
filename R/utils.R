
is_sqlite <- function(path) {
  is.character(path) && file.exists(path) && grepl(pattern = "\\.sqlite$", x = path)
}

hasName <- function(x, name) {
  match(name, names(x), nomatch = 0L) > 0L
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
  } else if (class(conn)=="PqConnection") { 
    ## Condition for SQL connection
    resetpwd <- read_sql_decrypt(conn, name = "pwd_mngt", passphrase = passphrase)
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
  } else {
    return(FALSE)
  }
}

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
force_chg_pwd <- function(user, change = TRUE) {
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
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
  }
  
  ## Condition for SQL connection
  if (class(conn)=="PqConnection") {
    resetpwd <- read_sql_decrypt(conn, name = "pwd_mngt", passphrase = passphrase)
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
    write_sql_encrypt(conn, value = resetpwd, name = "pwd_mngt", passphrase = passphrase)
  }
}

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
update_pwd <- function(user, pwd) {
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
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
  } else if (class(conn)=="PqConnection") { 
    ## Condition for SQL connection
    res_pwd <- try({
      users <- read_sql_decrypt(conn, name = "credentials", passphrase = passphrase)
      ind_user <- users$user %in% user
      if(!"character" %in% class(users$password)){
        users$password <- as.character(users$password)
      }
      if(!"is_hashed_password" %in% colnames(users)){
        users$is_hashed_password <- FALSE
      }
      users$password[ind_user] <- pwd
      users$is_hashed_password[ind_user] <- FALSE
      write_sql_encrypt(conn, value = users, name = "credentials", passphrase = passphrase)
      force_chg_pwd(user, FALSE)
    }, silent = TRUE)
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
  } else if (class(conn)=="PqConnection") { 
    ## Condition for SQL connection
    res_pwd <- try({
      users <- read_sql_decrypt(conn, name = "credentials", passphrase = passphrase)
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
    
  } else {
    return(TRUE)
  }
}

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
save_logs <- function(token) {
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
  user <- .tok$get_user(token)
  
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
  } 
  
  ## Condition for SQL connection
  if (class(conn)=="PqConnection") { 
    res_logs <- try({
      logs <- read_sql_decrypt(conn, name = "logs", passphrase = passphrase)
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
        write_sql_encrypt(conn = conn, value = logs, name = "logs", passphrase = passphrase)
        
        # update pwd_management
        pwd_mngt <- read_sql_decrypt(conn = conn, name = "pwd_mngt", passphrase = passphrase)
        if(nrow(pwd_mngt) > 0){
          if(!"n_wrong_pwd" %in% colnames(pwd_mngt)){
            pwd_mngt$n_wrong_pwd <- 0
          } else {
            pwd_mngt$n_wrong_pwd[pwd_mngt$user %in% user] <- 0
          }
          write_sql_encrypt(conn = conn, value = pwd_mngt, name = "pwd_mngt", passphrase = passphrase)
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

check_locked_account <- function(user, pwd_failure_limit) {
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
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
  } else if (class(conn)=="PqConnection") { 
    ## Condition for SQL connection
    res_lock <- try({
      pwd_mngt <- read_sql_decrypt(conn = conn, name = "pwd_mngt", passphrase = passphrase)
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
  } else {
    return(FALSE)
  }
}

save_logs_failed <- function(user, status = "Failed") {
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
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
  }
  
  ## Condition for SQL connection
  if (class(conn)=="PqConnection") { 
    res_logs <- try({
      logs <- read_sql_decrypt(conn = conn, name = "logs", passphrase = passphrase)
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
      write_sql_encrypt(conn = conn, value = logs, name = "logs", passphrase = passphrase)
      
      if(status %in% "Wrong pwd"){
        # update pwd_management
        pwd_mngt <- read_sql_decrypt(conn = conn, name = "pwd_mngt", passphrase = passphrase)
        if(nrow(pwd_mngt) > 0){
          if(!"n_wrong_pwd" %in% colnames(pwd_mngt)){
            pwd_mngt$n_wrong_pwd <- 0
          } 
          
          ind_user <- which(pwd_mngt$user %in% user)
          pwd_mngt$n_wrong_pwd[ind_user] <- pwd_mngt$n_wrong_pwd[ind_user] + 1
          write_sql_encrypt(conn = conn, value = pwd_mngt, name = "pwd_mngt", passphrase = passphrase)
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
  sqlite_path <- .tok$get_sqlite_path()
  passphrase <- .tok$get_passphrase()
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
  }
  
  ## Condition for SQL connection
  if (class(conn)=="PqConnection") { 
    res_logs <- try({
      logs <- read_sql_decrypt(conn = conn, name = "logs", passphrase = passphrase)
      logs$logout[logs$token  %in% token] <- as.character(Sys.time())
      write_sql_encrypt(conn = conn, value = logs, name = "logs", passphrase = passphrase)
    }, silent = TRUE)
    if (inherits(res_logs, "try-error")) {
      warning(paste(
        "shinymanager: unable to save logs | error:", attr(res_logs, "condition")$message
      ), call. = FALSE)
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




