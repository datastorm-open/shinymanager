
#' Send an email through an SMTP server
#'
#' @description Optional helper to send an email using the \code{emayili} package.
#'  This is only a convenience wrapper: \code{shinymanager} never requires \code{emayili}
#'  to be installed. You can use this function inside the \code{send_mail} callback of
#'  \code{\link{secure_server}}, or provide your own sending function using any other
#'  backend (\code{blastula}, \code{sendmailR}, \code{gmailr}, system \code{sendmail}, ...).
#'
#' @param to Recipient email address.
#' @param subject Subject of the email.
#' @param body Body of the email (plain text, or HTML if \code{html = TRUE}).
#' @param from Sender email address.
#' @param host SMTP server host.
#' @param port SMTP server port. Default to \code{587}.
#' @param username Username used to authenticate on the SMTP server. Optional.
#' @param password Password used to authenticate on the SMTP server. Optional.
#' @param html Logical, is \code{body} an HTML content ? Default to \code{FALSE}.
#' @param timeout Maximum time, in seconds, allowed for each sending attempt
#'  (curl \code{timeout} option). Default to \code{30}. Sending blocks the R process
#'  (and so every user served by it), so keep it short.
#' @param max_times Maximum number of sending attempts (\code{emayili} retries with an
#'  exponential backoff). Default to \code{1} (no retry): the user can simply ask again.
#'
#' @return \code{TRUE} invisibly on success. An error is raised if \code{emayili}
#'  is not installed or if sending fails.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # inside secure_server()
#' secure_server(
#'   check_credentials = check_credentials("credentials.sqlite", passphrase = "supersecret"),
#'   send_mail = function(user, email, temp_password) {
#'     send_smtp_mail(
#'       to = email,
#'       subject = "Password reset",
#'       body = paste0("Hello ", user, ", your temporary password is: ", temp_password),
#'       from = "no-reply@example.com",
#'       host = "smtp.example.com",
#'       port = 587,
#'       username = "no-reply@example.com",
#'       password = "smtp_secret"
#'     )
#'   }
#' )
#'
#' }
send_smtp_mail <- function(to, subject, body, from,
                           host, port = 587,
                           username = NULL, password = NULL,
                           html = FALSE, timeout = 30, max_times = 1) {
  if (!requireNamespace("emayili", quietly = TRUE)) {
    stop(
      "Package 'emayili' is required to use send_smtp_mail(). ",
      "Install it with install.packages('emayili'), or provide your own ",
      "'send_mail' function to secure_server().",
      call. = FALSE
    )
  }

  msg <- emayili::envelope()
  msg <- emayili::from(msg, from)
  msg <- emayili::to(msg, to)
  msg <- emayili::subject(msg, subject)
  if (isTRUE(html)) {
    msg <- emayili::html(msg, body)
  } else {
    msg <- emayili::text(msg, body)
  }

  smtp <- emayili::server(
    host = host,
    port = port,
    username = username,
    password = password,
    # bound the time the R process can be blocked by an unresponsive server:
    # no retry by default, and 'timeout' is passed to curl through '...'
    max_times = max_times,
    timeout = timeout
  )
  smtp(msg, verbose = FALSE)

  invisible(TRUE)
}
