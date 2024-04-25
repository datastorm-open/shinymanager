
#' @importFrom shiny NS dateInput checkboxInput textInput
#' @importFrom htmltools tagList
#' @importFrom R.utils capitalize
edit_user_ui <- function(id, credentials, username = NULL, inputs_list = NULL, lan = NULL) {

  ns <- NS(id)

  if (is.null(lan)) {
    lan <- use_language()
  }

  if (!is.null(username) && all(username %in% credentials$user)) {
    data_user <- credentials[credentials$user %in% username, ]
    if (length(username) > 1) {
      data_user$user <- NULL
    }
  } else {
    data_user <- credentials[0, ]
  }

  input_list <- lapply(
    X = names(data_user),
    FUN = function(x) {
      if (identical(x, "start")) {
        value <- unique(data_user[[x]])
        if (is.null(value) || length(value) != 1) {
          value <- NA
        }
        suppressWarnings({
          dateInput(
            inputId = ns(x),
            label = R.utils::capitalize(lan$get("start")),
            value = value,
            language = lan$get_dateInput(),
            width = "100%"
          )
        })
      } else if (identical(x, "expire")) {
        value <- unique(data_user[[x]])
        if (is.null(value) || length(value) != 1) {
          # value <- Sys.Date() + 60
          value <- NA
        }
        suppressWarnings({
          dateInput(
            inputId = ns(x),
            label = R.utils::capitalize(lan$get("expire")),
            value = value,
            language = lan$get_dateInput(),
            width = "100%"
          )
        })
      } else if (identical(x, "user") && length(username) > 1) {
        NULL # MULTIPLE USERS: dont modify user name

      } else if (identical(x, "password")) {
        NULL
      } else if (identical(x, "is_hashed_password")) {
        NULL
      } else if (identical(x, "admin")) {
        if (length(username) > 1) {
          NULL # MULTIPLE USERS: dont allow to set all users admin
        } else {
          checkboxInput(
            inputId = ns(x),
            label = R.utils::capitalize(lan$get("admin")),
            value = isTRUE(all(as.logical(data_user[[x]]))) & length(data_user[[x]])
          )
        }
      } else {
        if (!is.null(inputs_list) && x %in% names(inputs_list) &&
           all(c("fun", "args") %in% names(inputs_list[[x]])) && exists(inputs_list[[x]]$fun)) {

          fun <- inputs_list[[x]]$fun
          fun_args <- names(formals(fun))
          list_args <- inputs_list[[x]]$args

          list_args$inputId <- ns(x)

          if (!"value" %in% fun_args) {
            list_args$value <- NULL
          } else {
            if (!is.null(username)) {
              list_args$value <- data_user[[x]]
            }
          }
          if (!"selected" %in% fun_args) {
            list_args$selected <- NULL
          } else {
            if (!is.null(username)){
              if (isTRUE(list_args$multiple) && is.character(data_user[[x]])) {
                list_args$selected <- unlist(strsplit(data_user[[x]], ";"))
              } else {
                list_args$selected <- data_user[[x]]
              }
            }
          }
          if (!"label" %in% fun_args) {
            list_args$label <- NULL
          } else if (is.null(list_args$label)) {
            list_args$label <- R.utils::capitalize(x)
          }

          if (!"width" %in% fun_args) {
            list_args$width <- NULL
          } else if (is.null(list_args$width)) {
            list_args$width <- "100%"
          }

          tryCatch(do.call(fun, list_args), error = function(e) {
            warning("Error building custom input for column '", x,
                    "'. (fun : '", fun, "'). Verify 'inputs_list' argument.", call. = FALSE)
            textInput(inputId = ns(x), label = R.utils::capitalize(lan$get(x)), value = data_user[[x]], width = "100%")
          })

        } else {
          value <- unique(data_user[[x]])
          if (length(value) > 1) {
            value <- ""
          }
          textInput(
            inputId = ns(x),
            label = R.utils::capitalize(lan$get(x)),
            value = value,
            width = "100%"
          )
        }
      }
    }
  )

  # add new user
  if (is.null(username)) {
    input_list[[length(input_list) + 1]] <- textInput(
      inputId = ns("password"),
      label = lan$get("Password"),
      value = generate_pwd(),
      width = "100%"
    )
    input_list[[length(input_list) + 1]] <- checkboxInput(
      inputId = ns("must_change"),
      label = lan$get("Ask to change password"),
      value = TRUE
    )
  } else  if (length(username) == 1) {
    # add checkbox to authorized NULL value
    input_list[[length(input_list) + 1]] <- checkboxInput(
      inputId = ns("_sm_enabled_null"),
      label = lan$get("Allowed null values"),
      value = TRUE,
      width = "100%"
    )
  }

  tagList(
    input_list
  )
}

#' @importFrom shiny reactiveValues observe reactiveValuesToList
edit_user <- function(input, output, session) {

  rv <- reactiveValues(user = NULL)

  observe({
    rv$user <- lapply(
      X = reactiveValuesToList(input),
      FUN = function(x){
        x <- as.character(x)
        ifelse(length(x) == 0  | (length(x) == 1 && is.na(x)), NA_character_, paste(x, collapse = ";"))
      }
    )
  })

  return(rv)
}


#' @importFrom utils modifyList
#' @importFrom shiny isTruthy
update_user <- function(df, value, username) {

  check_isTruthy <- TRUE
  if("_sm_enabled_null" %in% names(value)){
    check_isTruthy <- !as.logical(value$`_sm_enabled_null`)
  }
  value <- value[intersect(names(value), names(df))]
  users_order <- factor(df$user, levels = unique(df$user))
  df <- split(df, f = users_order)
  user <- as.list(df[[username]])
  value <- lapply(value, function(x) {
    ifelse(length(x) == 0 | (length(x) == 1 && is.na(x)), NA_character_, paste(x, collapse = ";"))
  })
  if(check_isTruthy) {
    value <- value[vapply(value, isTruthy, logical(1))]
  }
  new <-  modifyList(x = user, val = value)
  df[[username]] <- as.data.frame(new, stringsAsFactors = FALSE)
  do.call(rbind, c(df, list(make.row.names = FALSE)))
}

update_user_sql <- function(config_db, list_value, username) {
  
  conn <- connect_sql_db(config_db)
  on.exit(disconnect_sql_db(conn, config_db))
  
  col_names <- db_list_fields_sql(conn, config_db$tables$credentials$tablename) 
  
  check_isTruthy <- TRUE
  if("_sm_enabled_null" %in% names(list_value)){
    check_isTruthy <- !as.logical(list_value$`_sm_enabled_null`)
  }
  list_value <- list_value[intersect(names(list_value), col_names)]

  list_value <- lapply(list_value, function(x) {
    ifelse(length(x) == 0 | (length(x) == 1 && is.na(x)), NA_character_, paste(x, collapse = ";"))
  })
  if(check_isTruthy) {
    list_value <- list_value[vapply(list_value, isTruthy, logical(1))]
  }

  if(length(list_value) > 0){
    udpate_users <- username
    for(i in 1:length(list_value)){
      name <- names(list_value)[i]
      value <- list_value[[i]]
      if(any(c("start", "expire") %in% name) && is.na(value)){
        value <- as.Date(NA)
      }

      if("admin" %in% name){
        write_logical <- try({
          tablename <- SQL(config_db$tables$credentials$tablename)
          request <- glue_sql(config_db$tables$credentials$update, .con = conn)
          dbExecute(conn, request)
        }, silent = TRUE)
        
        if("try-error" %in% class(write_logical)){
          value <- as.integer(as.logical(value))
          tablename <- SQL(config_db$tables$credentials$tablename)
          request <- glue_sql(config_db$tables$credentials$update, .con = conn)
          dbExecute(conn, request)
        }
      } else {
        tablename <- SQL(config_db$tables$credentials$tablename)
        request <- glue_sql(config_db$tables$credentials$update, .con = conn)
        dbExecute(conn, request)
      }

    }
  }

}