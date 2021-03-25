
#' @importFrom shiny NS dateInput checkboxInput textInput
#' @importFrom htmltools tagList
#' @importFrom R.utils capitalize
edit_user_ui <- function(id, credentials, username = NULL, inputs_list = NULL, lan = NULL) {

  ns <- NS(id)

  if(is.null(lan)){
    lan <- use_language()
  }

  if (!is.null(username) && username %in% credentials$user) {
    data_user <- credentials[credentials$user == username, ]
  } else {
    data_user <- list()
  }

  input_list <- lapply(
    X = names(credentials),
    FUN = function(x) {
      if (x %in% "start") {
        value <- data_user[[x]]
        if (is.null(value)) {
          # value <- Sys.Date()
          value <- NA
        }
        # warning on shiny if setting to NA / NULL
        suppressWarnings({
          dateInput(inputId = ns(x), label = R.utils::capitalize(lan$get("start")), value = value, width = "100%")
        })
      } else if (x %in% "expire") {
        value <- data_user[[x]]
        if (is.null(value)) {
          # value <- Sys.Date() + 60
          value <- NA
        }
        suppressWarnings({
          dateInput(inputId = ns(x), label = R.utils::capitalize(lan$get("expire")), value = value, width = "100%")
        })
      } else if (identical(x, "password")) {
        NULL
      } else if (identical(x, "is_hashed_password")) {
        NULL
      } else if (identical(x, "admin")) {
        checkboxInput(inputId = ns(x), label = R.utils::capitalize(lan$get("admin")), value = isTRUE(as.logical(data_user[[x]])))
      } else {
        if(!is.null(inputs_list) && x %in% names(inputs_list) && 
           all(c("fun", "args") %in% names(inputs_list[[x]])) && exists(inputs_list[[x]]$fun)){
          
          fun <- inputs_list[[x]]$fun
          fun_args <- names(formals(fun))
          list_args <- inputs_list[[x]]$args
          
          list_args$inputId <- ns(x)
          
          if(!"value" %in% fun_args){
            list_args$value <- NULL
          } else {
            if(!is.null(username)) list_args$value <- data_user[[x]]
          }
          if(!"selected" %in% fun_args){
            list_args$selected <- NULL
          } else {
            if(!is.null(username)){
              if(list_args$multiple && is.character(data_user[[x]])){
                list_args$selected <- unlist(strsplit(data_user[[x]], ";"))
              } else {
                list_args$selected <- data_user[[x]]
              }
            }
          }
          if(!"label" %in% fun_args){
            list_args$label <- NULL
          } else if(is.null(list_args$label)) list_args$label <- R.utils::capitalize(x)
          
          if(!"width" %in% fun_args){
            list_args$width <- NULL
          } else if(is.null(list_args$width)) list_args$width <- "100%"
          
          tryCatch(do.call(fun, list_args), error = function(e){
            warning("Error building custom input for column '", x, 
                    "'. (fun : '", fun, "'). Verify 'inputs_list' argument.", call. = FALSE)
            textInput(inputId = ns(x), label = R.utils::capitalize(lan$get(x)), value = data_user[[x]], width = "100%")
          })
          
        } else {
          textInput(inputId = ns(x), label = R.utils::capitalize(lan$get(x)), value = data_user[[x]], width = "100%")
        }
      }
    }
  )

  if(is.null(username)){
    input_list[[length(input_list) + 1]] <- textInput(inputId = ns("password"), label = lan$get("Password"), value = generate_pwd(), width = "100%")
    input_list[[length(input_list) + 1]] <- checkboxInput(inputId = ns("must_change"), label = lan$get("Ask to change password"), value = TRUE)
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
update_user <- function(df, value, username) {
  value <- value[intersect(names(value), names(df))]
  users_order <- factor(df$user, levels=unique(df$user))
  df <- split(df, f = users_order)
  user <- as.list(df[[username]])
  value <- lapply(value, function(x) ifelse(length(x) == 0 | (length(x) == 1 && is.na(x)), NA_character_, paste(x, collapse = ";")))
  new <-  modifyList(x = user, val = value)
  df[[username]] <- as.data.frame(new, stringsAsFactors = FALSE)
  do.call(rbind, c(df, list(make.row.names = FALSE)))
}
