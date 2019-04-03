
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
      private$tokens <- append(
        x = private$tokens,
        values = setNames(
          object = list(...),
          nm = token
        )
      )
      invisible(self)
    },
    get_user = function(token) {
      private$tokens[[token]]$user
    },
    is_valid = function(token) {
      isTRUE(token %in% names(private$tokens))
    },
    is_admin = function(token) {
      isTRUE(as.logical(private$tokens[[token]]$admin))
    },
    get = function(token) {
      private$tokens[[token]]
    },
    remove = function(token) {
      if (private$length() == 0) return(NULL)
      private$tokens[[token]] <- NULL
      invisible()
    },
    set_sqlite_path = function(path) {
      private$sqlite_path <- path
      invisible(private$sqlite_path)
    },
    get_sqlite_path = function() {
      private$sqlite_path
    },
    set_passphrase = function(passphrase) {
      private$passphrase <- passphrase
      invisible()
    },
    get_passphrase = function() {
      private$passphrase
    }
  ),
  private = list(
    tokens = list(),
    sqlite_path = NULL,
    passphrase = NULL,
    length = function() base::length(private$tokens)
  )
)
.tok <- .tokens$new()

