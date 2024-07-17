
#' Enable / Disable a Button
#'
#' @param session shiny session.
#' @param inputId Input's id to enable / disable.
#' @param type 'enable' or 'disable'.
#'
#' @noRd
toggleBtn <- function(session, inputId, type = "disable") {
  session$sendCustomMessage(
    type = "togglewidget",
    message = list(inputId = inputId, type = type)
  )
}


# Send input value to know where we are (what ui is displayed: auth, admin, app, ...)

shinymanager_where <- function(where) {
  # tags$script(sprintf(
  #   paste0("$(document).ready(function(){Shiny.setInputValue('shinymanager_where', '", where, "');})")
  # ))
  # humm, little hack ^^
  tags$div(
    style = "display: none;",
    selectInput(inputId = "shinymanager_where", label = NULL, 
               choices = where, selected = where, multiple = FALSE)
  )
}

shinymanager_language <- function(lan) {
  tags$div(
    style = "display: none;",
    selectInput(inputId = "shinymanager_language", label = NULL, 
                choices = lan, selected = lan, multiple = FALSE)
  )
}


# Remove the whole query string
#' @importFrom shiny updateQueryString getDefaultReactiveDomain
clearQueryString <- function(session = getDefaultReactiveDomain()) {
  updateQueryString(session$clientData$url_pathname, mode = "replace", session = session)
}

# Retrieve token from the query string
#' @importFrom shiny getQueryString getDefaultReactiveDomain
getToken <- function(session = getDefaultReactiveDomain()) {
  query <- getQueryString(session = session)
  if(!is.null(query$token)){
    gsub('\"', "", query$token)
  } else {
    NULL
  }
}

# Retrieve language from the query string
#' @importFrom shiny getQueryString getDefaultReactiveDomain
getLanguage <- function(session = getDefaultReactiveDomain()) {
  query <- getQueryString(session = session)
  if(!is.null(query$language)){
    gsub('\"', "", query$language)
  } else {
    NULL
  }
}

# Remove the token from the query string
#' @importFrom shiny updateQueryString getQueryString getDefaultReactiveDomain
resetQueryString <- function(session = getDefaultReactiveDomain()) {
  query <- getQueryString(session = session)
  query$token <- NULL
  query$language <- NULL
  if (length(query) == 0) {
    clearQueryString(session = session)
  } else {
    query <- paste(names(query), query, sep = "=", collapse="&")
    query <- gsub("_inputs_=", "_inputs_", query, fixed = T) # shiny bookmark
    updateQueryString(queryString = paste0("?", query), mode = "replace", session = session)
  }
}

#  Add token to query string but leave rest of query (if any) in tact
#' @importFrom shiny updateQueryString getQueryString getDefaultReactiveDomain
addAuthToQuery <- function(session = getDefaultReactiveDomain(), token, language ) {
  query <- getQueryString(session = session)
  query$token <- paste0('"', token, '"') # shiny bookmark
  query$language <- paste0('"', language, '"') # shiny bookmark
  query <- paste(names(query), query, sep = "=", collapse="&")
  query <- gsub("_inputs_=", "_inputs_", query, fixed = T) # shiny bookmark
  updateQueryString(queryString = paste0("?", query), mode = "replace", session = session)
}

#' @importFrom htmltools tags doRenderTags
#' @importFrom shiny icon
input_btns <- function(inputId, users, tooltip, icon, status = "primary", lan = NULL) {

  if(is.null(lan)){
    lan <- use_language()
  }
  
  tag <- lapply(
    X = users,
    FUN = function(x) {
      res <- tags$button(
        class = paste0("btn btn-", status),
        style = "float: right;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s',  {priority: 'event'})",
          inputId, x
        ),
        icon,
        `data-toggle` = "tooltip",
        `data-title` = lan$get(tooltip),
        `data-container` = "body"
      )
      res <- tagList(res, tags$script(HTML("$('[data-toggle=\"tooltip\"]').tooltip();")))
      doRenderTags(res)
    }
  )
  unlist(tag, use.names = FALSE)
}


remove_input <- function(id, session){
  shiny::removeUI(paste0("#", id), immediate = TRUE)
  session$sendCustomMessage(
    type = "rmInputSM",
    message = list(id = id)
  )
}

input_checkbox_ui <- function(id, users, session, checked = FALSE) {
  ns <- NS(id)
  inputs <- isolate({names(session$input)})
  rm_inputs <- inputs[grepl(paste0("^", gsub("^(admin-)", "", ns("check_"))), inputs)]
  if(length(rm_inputs) > 0){
    for(i in rm_inputs){
      remove_input(paste0("admin-", i), session)
    }
  }
  tag <- lapply(
    X = users,
    FUN = function(x) {
      # res <- checkboxInput(inputId = ns(paste0("check_", x)), label = NULL, value = FALSE)
      res <- tags$input(id = ns(paste0("check_", x)), type = "checkbox", style = "float: right;")
      if(checked) res$attribs$checked <- "checked"
      doRenderTags(res)
    }
  )
  unlist(tag, use.names = FALSE)
}

input_checkbox <- function(input, output, session) {
  
  reac <- reactive({
    inputs <- reactiveValuesToList(input)
    inputs <- dropFalse(inputs)
    gsub(pattern = "^check_", replacement = "", x = names(inputs))
  })
  
  return(reac)
}

unbindDT <- function(id, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "unbindDT",
    message = list(id = id)
  )
}

fileReaderSqlite <- function(sqlite_path, passphrase, name){
    conn <- dbConnect(SQLite(), dbname = sqlite_path)
    on.exit(dbDisconnect(conn))
    res <- tryCatch(read_db_decrypt(conn = conn, name = name, passphrase = passphrase), 
             error = function(e) NULL)
}

fileReaderSQL <- function(config_db, name){
    conn <- connect_sql_db(config_db)
    on.exit(disconnect_sql_db(conn, config_db))
    res <- tryCatch(db_read_table_sql(conn, config_db$tables[[name]]$tablename), 
                    error = function(e) NULL)
}