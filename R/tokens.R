

#' @importFrom R6 R6Class
#' @importFrom openssl sha256 rand_bytes
.tokens <- R6::R6Class(
  classname = "shinymanager_tokens",
  public = list(
    initialize = function() {
      invisible(self)
    },
    generate = function(user) {
      sha256(paste0(user, Sys.time()), key = rand_bytes(32))
    },
    add = function(token, ...) {
      args <- list(...)
      if(length(args) > 0){
        args[[1]]$shinymanager_datetime <- Sys.time()
      } else {
        args <- list(list(shinymanager_datetime = Sys.time()))
      }
      private$tokens <- union(private$tokens, token)
      if (length(args) > 0) {
        private$tokens_user <- append(
          x = private$tokens_user,
          values = setNames(
            object = args,
            nm = token
          )
        )
      }
      invisible(self)
    },
    is_valid_timeout = function(token, update = TRUE) {
      datetime <- private$tokens_user[[token]]$shinymanager_datetime
      if(!is.null(datetime) && private$timeout  > 0){
        valid <- difftime(Sys.time(), datetime, units = "mins") <= private$timeout
      } else {
        valid <- TRUE
      }
      if(valid && update) private$tokens_user[[token]]$shinymanager_datetime <- Sys.time()
      valid
    },
    get_user = function(token) {
      private$tokens_user[[token]]$user
    },
    is_valid = function(token) {
      valid <- token %in% private$tokens
      count <- sum(private$tokens_count %in% token, na.rm = TRUE)
      private$tokens_count <- c(private$tokens_count, token)
      isTRUE(valid) & isTRUE(count < 1)
    },
    is_valid_server = function(token) {
      isTRUE(token %in% private$tokens)
    },
    # valid on the server side: not revoked and not timed out (no side effect)
    is_active = function(token) {
      if (is.null(token) || !self$is_valid_server(token)) return(FALSE)
      isTRUE(self$is_valid_timeout(token, update = FALSE))
    },
    is_admin = function(token) {
      isTRUE(as.logical(private$tokens_user[[token]]$admin))
    },
    get = function(token) {
      info <- private$tokens_user[[token]]
      if("shinymanager_datetime" %in% names(info)) info$shinymanager_datetime <- NULL
      info
    },
    remove = function(token) {
      if (private$length() == 0) return(NULL)
      private$tokens <- setdiff(private$tokens, token)
      # also drop the user info, so a revoked token gives no identity anymore
      if (!is.null(token)) private$tokens_user[[token]] <- NULL
      invisible()
    },
    reset_count = function(token) {
      private$tokens_count <- setdiff(private$tokens_count, token)
    },
    set_sqlite_path = function(path) {
      private$sqlite_path <- path
      invisible(private$sqlite_path)
    },
    get_sqlite_path = function() {
      private$sqlite_path
    },
    set_sql_config_db = function(config) {
      private$sql_config_db <- config
      invisible(private$sql_config_db)
    },
    get_sql_config_db = function() {
      private$sql_config_db 
    },
    set_passphrase = function(passphrase) {
      private$passphrase <- passphrase
      invisible()
    },
    get_passphrase = function() {
      private$passphrase
    },
    set_timeout = function(timeout) {
      private$timeout <- timeout
      invisible()
    },
    get_timeout = function() {
      private$timeout
    },
    set_send_mail = function(send_mail) {
      private$send_mail <- send_mail
      invisible()
    },
    get_send_mail = function() {
      private$send_mail
    },
    # time of the last successful self-service reset of each user (in memory,
    # shared by the sessions of the R process, lost on restart)
    set_reset_time = function(user) {
      private$reset_times[[user]] <- Sys.time()
      invisible()
    },
    is_reset_too_soon = function(user, delay) {
      last <- private$reset_times[[user]]
      !is.null(last) && as.numeric(difftime(Sys.time(), last, units = "secs")) < delay
    }
  ),
  private = list(
    tokens = character(0),
    tokens_count = character(0),
    tokens_user = list(),
    sqlite_path = NULL,
    passphrase = NULL,
    sql_config_db = NULL,
    timeout = 0,
    send_mail = NULL,
    reset_times = list(),
    length = function() base::length(private$tokens)
  )
)
.tok <- .tokens$new()

