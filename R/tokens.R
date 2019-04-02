
.globals <- new.env(parent = emptyenv())

validate_token <- function(token) {
  if (is.null(token))
    return(FALSE)
  isTRUE(token %in% names(.globals$tokens))
}

#' @importFrom openssl sha256 rand_bytes
generate_token <- function(user = "user") {
  sha256(paste0(user, Sys.time()), key = rand_bytes(32))
}

remove_token <- function(token) {
  .globals$tokens[[token]] <- NULL
}

add_token <- function(token, ...) {
  .globals$tokens <- append(
    x = .globals$tokens,
    values = setNames(
      object = list(...),
      nm = token
    )
  )
}

get_user <- function(token) {
  .globals$tokens[[token]]$user
}

get_user_info <- function(token) {
  .globals$tokens[[token]]
}

is_token_admin <- function(token) {
  isTRUE(.globals$tokens[[token]]$admin)
}


#' @importFrom R6 R6Class
#' @importFrom openssl sha256 rand_bytes
.tokens <- R6::R6Class(
  classname = "tokens",
  public = list(
    initialize = function() {
      invisible(self)
    },
    generate = function(user) {
      sha256(paste0(user, Sys.time()), key = rand_bytes(32))
    },
    add = function(token, ...) {
      private$queue <- append(
        x = private$queue,
        values = setNames(
          object = list(...),
          nm = token
        )
      )
      invisible(self)
    },
    get_user = function(token) {
      private$queue[[token]]$user
    },
    is_valid = function(token) {
      isTRUE(token %in% names(private$queue))
    },
    is_admin = function(token) {
      isTRUE(private$queue[[token]]$admin)
    },
    get = function(token) {
      private$queue[[token]]
    },
    remove = function(token) {
      if (private$length() == 0) return(NULL)
      private$queue[[token]] <- NULL
      invisible()
    }
  ),
  private = list(
    queue = list(),
    length = function() base::length(private$queue)
  )
)
.tok <- .tokens$new()

