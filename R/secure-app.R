
#' Secure a Shiny application and manage authentication
#'
#' @param ui UI of the application.
#' @param ... Arguments passed to \code{\link{auth_ui}}.
#' @param enable_admin Enable or not access to admin mode, note that
#'  admin mode is only available when using SQLite backend for credentials.
#' @param head_auth Tag or list of tags to use in the \code{<head>}
#'  of the authentication page (for custom CSS for example).
#' @param theme Alternative Bootstrap stylesheet, default is to use \code{readable},
#'  you can use themes provided by \code{shinythemes}.
#'  It will affect the authentication panel and the admin page.
#' @param language Language to use for labels, supported values are : "en", "fr", "pt-BR", "es", "de", "pl", "ja", "el", "id", "zh-CN".
#' @param fab_position Position for the FAB button, see \code{\link{fab_button}} for options.
#'
#' @note A special input value will be accessible server-side with \code{input$shinymanager_where}
#'  to know in which step user is : authentication, application, admin or password.
#'
#' @return A \code{reactiveValues} containing informations about the user connected.
#'
#' @export
#'
#' @importFrom shiny parseQueryString fluidPage actionButton icon navbarPage tabPanel
#' @importFrom htmltools tagList
#'
#' @name secure-app
#'
#' @example examples/secure_app.R
secure_app <- function(ui,
                       ...,
                       enable_admin = FALSE,
                       head_auth = NULL,
                       theme = NULL,
                       language = "en",
                       fab_position = "bottom-right") {
  if (!language %in% c("en", "fr", "pt-BR", "es", "de", "pl", "ja", "el", "id", "zh-CN")) {
    warning("Only supported language for the now are: en, fr, pt-BR, es, de, pl, ja, el, id, zh-CN", call. = FALSE)
    language <- "en"
  }

  lan <- use_language(language)
  ui <- force(ui)
  enable_admin <- force(enable_admin)
  head_auth <- force(head_auth)
  if (is.null(theme)) {
    theme <- "shinymanager/css/readable.min.css"
  }

  function(request) {
    query <- parseQueryString(request$QUERY_STRING)
    token <- gsub('\"', "", query$token)
    admin <- query$admin
    language <- query$language
    if (!is.null(language)) {
      lan <- use_language(gsub('\"', "", language))
    }
    if (.tok$is_valid(token)) {
      is_forced_chg_pwd <- is_force_chg_pwd(token = token)
      if (is_forced_chg_pwd) {
        args <- get_args(..., fun = pwd_ui)
        args$id <- "password"
        args$lan <- lan
        pwd_ui <- fluidPage(
          theme = theme,
          tags$head(head_auth),
          do.call(pwd_ui, args),
          shinymanager_where("password"),
          shinymanager_language(lan$get_language())
        )
        return(pwd_ui)
      }
      if (isTRUE(enable_admin) && .tok$is_admin(token) & identical(admin, "true") && (!is.null(.tok$get_sqlite_path()) | !is.null(.tok$get_sql_config_db()))) {
        navbarPage(
          title = "Admin",
          id = "sm_admin_nv",
          theme = theme,
          header = tagList(
            tags$style(".navbar-header {margin-left: 16.66% !important;}"),
            fab_button(
              position = fab_position,
              actionButton(
                inputId = ".shinymanager_logout",
                label = lan$get("Logout"),
                icon = icon("right-from-bracket")
              ),
              actionButton(
                inputId = ".shinymanager_app",
                label = lan$get("Go to application"),
                icon = icon("share")
              )
            ),
            shinymanager_where("admin")
          ),
          tabPanel(
            title = tagList(icon("house"), lan$get("Home")),
            value = "home",
            admin_ui("admin", lan),
            shinymanager_language(lan$get_language())
          ),
          if(show_logs_enabled()){
            tabPanel(
              title = lan$get("Logs"),
              logs_ui("logs", lan),
              shinymanager_language(lan$get_language())
            )
          }
        )
      } else {
        if (isTRUE(enable_admin) && .tok$is_admin(token) && (!is.null(.tok$get_sqlite_path()) | !is.null(.tok$get_sql_config_db()))) {
          menu <- fab_button(
            position = fab_position,
            actionButton(
              inputId = ".shinymanager_logout",
              label = lan$get("Logout"),
              icon = icon("right-from-bracket")
            ),
            actionButton(
              inputId = ".shinymanager_admin",
              label = lan$get("Administrator mode"),
              icon = icon("gears")
            )
          )
        } else {
          if (isTRUE(enable_admin) && .tok$is_admin(token) && is.null(.tok$get_sqlite_path()) && is.null(.tok$get_sql_config_db())) {
            warning("Admin mode is only available when using a SQLite / SQL database!", call. = FALSE)
          }
          menu <- fab_button(
            position = fab_position,
            actionButton(
              inputId = ".shinymanager_logout",
              label = lan$get("Logout"),
              icon = icon("right-from-bracket")
            )
          )
        }
        save_logs(token)
        if (is.function(ui)) {
          ui <- ui(request)
        }
        tagList(
          ui, menu, shinymanager_where("application"),
          shinymanager_language(lan$get_language()),
          singleton(tags$head(tags$script(src = "shinymanager/timeout.js")))
        )
      }
    } else {
      args <- get_args(..., fun = auth_ui)
      # patch / message changing tag_img & tag_div
      deprecated <- list(...)
      if ("tag_img" %in% names(deprecated)) {
        args$tags_top <- deprecated$tag_img
        warning("'tag_img' (auth_ui, secure_app) is now deprecated. Please use 'tags_top'", call. = FALSE)
      }
      if ("tag_div" %in% names(deprecated)) {
        args$tags_bottom <- deprecated$tag_div
        warning("'tag_div' (auth_ui, secure_app) is now deprecated. Please use 'tags_bottom'", call. = FALSE)
      }
      args$id <- "auth"
      args$lan <- lan
      fluidPage(
        theme = theme,
        tags$head(head_auth),
        do.call(auth_ui, args),
        shinymanager_where("authentication"),
        shinymanager_language(lan$get_language())
      )
    }
  }
}


#' @param check_credentials Function passed to \code{\link{auth_server}}.
#' @param timeout Timeout session (minutes) before logout if sleeping. Defaut to 15. 0 to disable.
#' @param inputs_list \code{list}. If database credentials, you can configure inputs for editing users information. See Details.
#' @param max_users \code{integer}. If not NULL, maximum of users in sql credentials.
#' @param fileEncoding 	character string: Encoding of logs downloaded file. See \code{\link{write.table}}
#' @param keep_token Logical, keep the token used to authenticate in the URL, it allow to refresh the
#'  application in the browser, but careful the token can be shared between users ! Default to \code{FALSE}.
#' @param validate_pwd A \code{function} to validate the password enter by the user.
#'  Default is to check for the password to have at least one number, one lowercase,
#'  one uppercase and be of length 6 at least.
#' @param session Shiny session.
#'
#' @details
#'
#' If database credentials, you can configure inputs with \code{inputs_list} for editing users information
#' from the admin console. \code{start}, \code{expire}, \code{admin} and \code{password} are not configurable.
#' The others columns are rendering by defaut using a \code{textInput}. You can modify this using \code{inputs_list}.
#' \code{inputs_list} must be a named list. Each name must be a column name, and then we must have the function
#'  shiny to call \code{fun} and the arguments \code{args} like this :
#'  \code{
#'  list(group = list(
#'      fun = "selectInput",
#'      args = list(
#'          choices = c("all", "restricted"),
#'          multiple = TRUE,
#'          selected = c("all", "restricted")
#'       )
#'      )
#' )
#' }
#'
#' You can specify if you want to allow downloading users file,  sqlite database and logs from within
#' the admin panel by invoking \code{options("shinymanager.download")}. It defaults
#' to \code{c("db", "logs", "users")}, that allows downloading all. You can specify
#' \code{options("shinymanager.download" = "db"} if you want allow admin to download only
#' sqlite database, \code{options("shinymanager.download" = "logs")} to allow logs download
#' or \code{options("shinymanager.download" = "")} to disable all.
#'
#' Using \code{options("shinymanager.pwd_validity")}, you can set password validity period. It defaults
#' to \code{Inf}. You can specify for example
#' \code{options("shinymanager.pwd_validity" = 90)} if you want to force user changing password each 90 days.
#'
#' Using \code{options("shinymanager.pwd_failure_limit")}, you can set password failure limit. It defaults
#' to \code{Inf}. You can specify for example
#' \code{options("shinymanager.pwd_failure_limit" = 5)} if you want to lock user account after 5 wrong password.
#' 
#' Using \code{options("shinymanager.auto_sqlite_reader")}, you can set reactiveFileReader time (milliseconds) used to look at sqlite db only. 
#' Used and useful in admin panel to prevent bug having potentially multiple admin session. It defaults to \code{1000}
#'  
#' Using \code{options("shinymanager.auto_sql_reader")}, you can set reactiveTimer SQL (not sqlite) admin reader. It defaults
#' to \code{Inf} (disabled). It's only needed to prevent potential bug if two ore more admin are updated users
#' at the same time.
#'   
#' Using \code{options("shinymanager.write_logs")}, you can activate or not writing users connection logs. Default to \code{TRUE}
#' 
#' Using \code{options("shinymanager.show_logs")}, you can activate or not showing users connection logs in admin panel. Default to \code{TRUE}
#' 
#' @export
#'
#' @importFrom shiny callModule getQueryString parseQueryString
#'  updateQueryString observe getDefaultReactiveDomain isolate invalidateLater
#'
#' @rdname secure-app
secure_server <- function(check_credentials,
                          timeout = 15,
                          inputs_list = NULL,
                          max_users = NULL,
                          fileEncoding = "",
                          keep_token = FALSE,
                          validate_pwd = NULL,
                          session = shiny::getDefaultReactiveDomain()) {

  session$setBookmarkExclude(c(session$getBookmarkExclude(),
                               "shinymanager_language",
                               ".shinymanager_timeout",
                               ".shinymanager_admin",
                               ".shinymanager_logout",
                               "shinymanager_where"))

  token_start <- isolate(getToken(session = session))
  if (isTRUE(keep_token)) {
    .tok$reset_count(token_start)
  } else {
    isolate(resetQueryString(session = session))
  }


  lan <- reactiveVal(use_language())
  observe({
    lang <- getLanguage(session = session)
    if (!is.null(lang)) {
      lan(use_language(lang))
    }
  })

  callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials,
    use_token = TRUE,
    lan = lan
  )

  callModule(
    module = pwd_server,
    id = "password",
    user = reactiveValues(user = .tok$get(token_start)$user),
    update_pwd = update_pwd,
    validate_pwd = validate_pwd,
    use_token = TRUE,
    lan = lan
  )

  .tok$set_timeout(timeout)

  path_sqlite <- .tok$get_sqlite_path()
  config_db <- .tok$get_sql_config_db()
  
  if (!is.null(path_sqlite) | !is.null(config_db)) {
    callModule(
      module = admin,
      id = "admin",
      sqlite_path = path_sqlite,
      passphrase = .tok$get_passphrase(),
      config_db = config_db,
      inputs_list = inputs_list,
      max_users = max_users,
      lan = lan
    )
    
    if(show_logs_enabled()){
      callModule(
        module = logs,
        id = "logs",
        sqlite_path = path_sqlite,
        passphrase = .tok$get_passphrase(),
        config_db = config_db,
        fileEncoding = fileEncoding,
        lan = lan
      )
    }
  }

  user_info_rv <- reactiveValues()

  observe({
    token <- getToken(session = session)
    if (!is.null(token)) {
      user_info <- .tok$get(token)
      for (i in names(user_info)) {
        value <- user_info[[i]]
        if (i %in% "applications") {
          value <- strsplit(x = as.character(value), split = ";")
          value <- unlist(x = value, use.names = FALSE)
        } else if (!is.null(inputs_list)) {
          if (i %in% names(inputs_list) && !is.null(inputs_list[[i]]$args$multiple) && inputs_list[[i]]$args$multiple) {
            value <- strsplit(x = as.character(value), split = ";")
            value <- unlist(x = value, use.names = FALSE)
          }
        }
        user_info_rv[[i]] <- value
      }
    }
  })

  observeEvent(session$input$.shinymanager_admin, {
    token <- getToken(session = session)
    updateQueryString(queryString = sprintf("?token=\"%s\"&admin=true&language=\"%s\"", token, lan()$get_language()), session = session, mode = "replace")
    .tok$reset_count(token)
    session$reload()
  }, ignoreInit = TRUE)

  observeEvent(session$input$.shinymanager_app, {
    token <- getToken(session = session)
    updateQueryString(queryString = sprintf("?token=\"%s\"&language=\"%s\"", token, lan()$get_language()), session = session, mode = "replace")
    .tok$reset_count(token)
    session$reload()
  }, ignoreInit = TRUE)

  observeEvent(session$input$.shinymanager_logout, {
    token <- getToken(session = session)
    logout_logs(token)
    .tok$remove(token)
    clearQueryString(session = session)
    session$reload()
  }, ignoreInit = TRUE)



  if (timeout > 0) {

    observeEvent(session$input$.shinymanager_timeout, {
      token <- getToken(session = session)
      if (!is.null(token)) {
        valid_timeout <- .tok$is_valid_timeout(token, update = TRUE)
        if (!valid_timeout) {
          .tok$remove(token)
          clearQueryString(session = session)
          session$reload()
        }
      }
    })

    observe({
      invalidateLater(30000, session)
      token <- getToken(session = session)
      if (!is.null(token)) {
        valid_timeout <- .tok$is_valid_timeout(token, update = FALSE)
        if(!valid_timeout){
          .tok$remove(token)
          clearQueryString(session = session)
          session$reload()
        }
      }
    })

  }

  return(user_info_rv)
}


