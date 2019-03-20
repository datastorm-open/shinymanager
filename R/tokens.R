
validate_token <- function(token) {
  if (is.null(token))
    return(FALSE)
  isTRUE(token %in% .globals$tokens)
}

#' @importFrom openssl sha256 rand_bytes
generate_token <- function(user = "user") {
  sha256(paste0(user, Sys.time()), key = rand_bytes(32))
}
