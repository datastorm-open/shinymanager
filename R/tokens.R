
validate_token <- function(token) {
  if (is.null(token))
    return(FALSE)
  isTRUE(token %in% unlist(.globals$tokens, use.names = FALSE))
}

#' @importFrom openssl sha256 rand_bytes
generate_token <- function(user = "user") {
  sha256(paste0(user, Sys.time()), key = rand_bytes(32))
}

remove_token <- function(token) {
  tokeep <- !unlist(.globals$tokens, use.names = FALSE) %in% token
  .globals$tokens <- .globals$tokens[tokeep]
}

add_token <- function(token) {
  .globals$tokens <- append(.globals$tokens, token)
}

get_user <- function(token) {
  tok <- which(token == unlist(.globals$tokens, use.names = FALSE))
  names(.globals$tokens)[tok]
}

store_user_info <- function(info) {
  .globals$user_infos <- append(
    .globals$user_infos, info
  )
}

get_user_info <- function(user) {
  .globals$user_infos[[user]]
}

# add_user <- function(user) {
#   .globals$users <- append(.globals$users, user)
# }
#
# remove_user <- function(user) {
#   .globals$users <- setdiff(.globals$users, user)
# }
#
# get_user <- function(token) {
#   tok <- which(token == .globals$tokens)
#   .globals$users[tok]
# }
