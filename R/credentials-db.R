#' @title Create credentials database
#'
#' @description Create a SQLite database with credentials data protected by a password.
#'
#' @param credentials_data A \code{data.frame} with information about users, \code{user} and \code{password} are required.
#' @param sqlite_path Path to the SQLite database.
#' @param passphrase A password to protect the data inside the database.
#' @param flags \code{RSQLite::SQLITE_RWC:} open the database in read/write mode and create the database file if it does not already exist; 
#' \code{RSQLite::SQLITE_RW:} open the database in read/write mode. Raise an error if the file does not already exist; 
#' \code{RSQLite::SQLITE_RO:} open the database in read only mode. Raise an error if the file does not already exist
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
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite
#' @importFrom scrypt hashPassword
#'
#'
#' @examples
#' \dontrun{
#'
#' library(shiny)
#' library(shinymanager)
#' 
#' #### init the Sqlite Database
#' # Credentials data
#' credentials <- data.frame(
#'   user = c("shiny", "shinymanager"),
#'   password = c("azerty", "12345"), # password will automatically be hashed
#'   stringsAsFactors = FALSE
#' )
#'
#' # you can use keyring package to set database key
#' library(keyring)
#' key_set("R-shinymanager-key", "obiwankenobi")
#'
#' # Create the database
#' create_db(
#'   credentials_data = credentials,
#'   sqlite_path = "/path/to/database.sqlite", # will be created
#'    passphrase = key_get("R-shinymanager-key", "obiwankenobi")
#'    # passphrase = "secret"  # or just a word, without keyring
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
#'    check_credentials = check_credentials(
#'        db = "/path/to/database.sqlite", 
#'        passphrase = key_get("R-shinymanager-key", "obiwankenobi")
#'    )
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
#' }
#'
#' shinyApp(ui, server)
#'
#'}
#' 
#' @seealso \code{\link{create_db}}, \code{\link{create_sql_db}}, \code{\link{check_credentials}}, \code{\link{read_db_decrypt}}
#' 
create_db <- function(credentials_data, sqlite_path, passphrase = NULL, flags = RSQLite::SQLITE_RWC) {
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
    credentials_data$start <- NA
  }
  if(!"expire" %in% names(credentials_data)){
    credentials_data$expire <- NA
  }

  default_col <- c("user", "password", "start", "expire", "admin")
  credentials_data <- credentials_data[, c(default_col,
                                           setdiff(colnames(credentials_data), default_col))]
  conn <- dbConnect(SQLite(), dbname = sqlite_path, flags = flags)
  on.exit(dbDisconnect(conn))
  credentials_data[] <- lapply(credentials_data, as.character)

  write_db_encrypt(
    conn = conn,
    name = "credentials",
    value = credentials_data,
    passphrase = passphrase
  )
  write_db_encrypt(
    conn = conn,
    name = "pwd_mngt",
    value = data.frame(
      user = credentials_data$user,
      must_change = as.character(FALSE),
      have_changed = as.character(FALSE),
      date_change = as.character(Sys.Date()),
      n_wrong_pwd = 0,
      stringsAsFactors = FALSE
    ),
    passphrase = passphrase
  )
  write_db_encrypt(
    conn = conn,
    name = "logs",
    value = data.frame(
      user = character(0),
      server_connected = character(0),
      token = character(0),
      logout = character(0),
      app = character(0),
      stringsAsFactors = FALSE
    ),
    passphrase = passphrase
  )
}


#' Read / Write crypted table from / to a SQLite database
#'
#' @param conn A DBIConnection object, as returned by \code{\link[DBI]{dbConnect}}.
#' @param value A \code{data.frame}.
#' @param name A character string specifying the unquoted DBMS table name.
#' @param passphrase A secret passphrase to crypt the table inside the database
#'
#' @return a \code{data.frame} for \code{read_db_decrypt}.
#' @export
#'
#' @name db-crypted
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite
#' @importFrom openssl sha256 aes_cbc_encrypt
#'
#' @seealso \code{\link{create_db}}
#'
#' @examples
#' # connect to database
#' conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
#'
#' # write to database
#' write_db_encrypt(conn, value = head(iris), name = "iris", passphrase = "supersecret")
#'
#' # read
#' read_db_decrypt(conn = conn, name = "iris", passphrase = "supersecret")
#'
#' 
#' # with wrong passphrase
#' \dontrun{
#' read_db_decrypt(conn = conn, name = "iris", passphrase = "forgotten")
#' }
#'
#' # with DBI method you'll get a crypted blob
#' DBI::dbReadTable(conn = conn, name = "iris")
#'
#' # add some users to database
#' \dontrun{
#' conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = "path/to/database.sqlite")
#'
#' # update "credentials" table
#' current_user <- read_db_decrypt(
#'   conn,
#'   name = "credentials",
#'   passphrase = key_get("R-shinymanager-key", "obiwankenobi")
#' )
#'
#' add_user <- data.frame(user = "new", password = "pwdToChange",
#'                       start = NA, expire = NA, admin = TRUE)
#'
#' new_users <- rbind.data.frame(current_user, add_user)
#'
#' write_db_encrypt(
#'   conn,
#'   value = new_users,
#'   name = "credentials",
#'   key_get("R-shinymanager-key", "obiwankenobi")
#' )
#'
#' # update "pwd_mngt" table
#' pwd_mngt <- read_db_decrypt(
#'   conn,
#'   name = "pwd_mngt",
#'   passphrase = key_get("R-shinymanager-key", "obiwankenobi")
#' )
#'
#' pwd_mngt <- rbind.data.frame(
#'   pwd_mngt,
#'   data.frame(user = "new", must_change = T, have_changed = F, date_change = "")
#' )
#'
#' write_db_encrypt(
#'   conn,
#'   value = pwd_mngt,
#'   name = "pwd_mngt",
#'   passphrase = key_get("R-shinymanager-key", "obiwankenobi")
#' )
#' }
#' 
#' DBI::dbDisconnect(conn)
#'
write_db_encrypt <- function(conn, value, name = "credentials", passphrase = NULL) {
  if (is.character(conn)) {
    conn <- dbConnect(RSQLite::SQLite(), dbname = conn)
    on.exit(dbDisconnect(conn))
  }

  if(name == "credentials" && "password" %in% colnames(value)){
    if(!"is_hashed_password" %in% colnames(value)){
      value$is_hashed_password <- FALSE
    }
    to_hash <- which(!as.logical(value$is_hashed_password))
    if(length(to_hash) > 0){
      # store hashed password
      value$password[to_hash] <- sapply(value$password[to_hash], function(x) scrypt::hashPassword(x))
      value$is_hashed_password[to_hash] <- TRUE
    }
  }

  if (!is.null(passphrase)) {
    passphrase <- as.character(passphrase)
    passphrase <- charToRaw(passphrase)
    key <- sha256(passphrase)
    value_serialized <- serialize(value, NULL)
    value_encrypted <- aes_cbc_encrypt(data = value_serialized, key = key)
    value <- data.frame(value = I(list(value_encrypted)), iv = I(list(attr(value_encrypted, "iv"))))
  }
  dbWriteTable(conn = conn, name = name, value = value, overwrite = TRUE)
}


#' @export
#'
#' @rdname db-crypted
#'
#' @importFrom DBI dbConnect dbDisconnect dbReadTable
#' @importFrom RSQLite SQLite
#' @importFrom openssl sha256 aes_cbc_decrypt
#'
read_db_decrypt <- function(conn, name = "credentials", passphrase = NULL) {
  if (is.character(conn)) {
    conn <- dbConnect(RSQLite::SQLite(), dbname = conn)
    on.exit(dbDisconnect(conn))
  }
  out <- db_read_table_sql(conn, name)
  if (!is.null(passphrase)) {
    passphrase <- as.character(passphrase)
    passphrase <- charToRaw(passphrase)
    key <- sha256(passphrase)
    value <- out$value[[1]]
    attr(value, "iv") <- out$iv[[1]]
    out <- aes_cbc_decrypt(value, key = key)
    out <- unserialize(out)
  }
  return(out)
}






