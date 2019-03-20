
#' Check credentials
#'
#' @param user User to authenticate.
#' @param password Password of user to authenticate.
#' @param credentials_df A \code{data.frame} containing credentials,
#'  must contain columns \code{user} and \code{password}.
#'
#' @return A \code{list} with 2 slots :
#'  \itemize{
#'   \item \strong{result} : logical, result of authentication.
#'   \item \strong{user_info} : the line in \code{credentials_df} corresponding to the user.
#'  }
#'
#' @export
#'
#' @name check-credentials
#'
#' @examples
#' # data.frame with credentials info
#' credentials <- data.frame(
#'   user = c("fanny", "victor"),
#'   password = c("azerty", "12345"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # check a user
#' check_credentials_df("fanny", "azerty", credentials)
#' check_credentials_df("fanny", "azert", credentials)
#' check_credentials_df("fannyyy", "azerty", credentials)
#'
#' check_credentials_p <- purrr::partial(
#'   check_credentials_df,
#'   credentials_df = credentials # set default df to use
#' )
#' check_credentials_p("fanny", "azerty")
check_credentials_df <- function(user, password, credentials_df) {
  if (!user %in% credentials_df$user) {
    return(FALSE)
  }
  pwd <- credentials_df$password[credentials_df$user == user]
  if (isTRUE(pwd == password)) {
    auth <- list(
      result = TRUE,
      user_info = credentials_df[credentials_df$user == user, setdiff(names(credentials_df), "password"), drop = FALSE]
    )
  } else {
    auth <- list(
      result = FALSE,
      user_info = NULL
    )
  }
  return(auth)
}

check_credentials_sqlite <- function(user, password, credentials_dbname) {

}
