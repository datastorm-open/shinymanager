
#' @importFrom R6 R6Class
#' @importFrom utils modifyList
language <- R6::R6Class(
  classname = "language",
  public = list(
    initialize = function() {
      .globals$language <- self
      invisible(self)
    },
    add = function(...) {
      args <- list(...)
      if (!all(nzchar(names(args)))) {
        stop("All arguments must be named!", call. = FALSE)
      }
      private$labels <- modifyList(
        x = private$labels, val = args
      )
      invisible(self)
    },
    set_language = function(lan) {
      if (!lan %in% names(private$labels_lan)) {
        stop("Unsupported language !", call. = FALSE)
      }
      private$labels <- private$labels_lan[[lan]]
    },
    get = function(label) {
      private$labels[[label]]
    },
    get_all = function() {
      private$labels
    }
  ),
  private = list(
    labels = list(
      "Please authenticate" = "Please authenticate",
      "Username:" = "Username:",
      "Password:" = "Password:",
      "Login" = "Login",
      "Username or password are incorrect" = "Username or password are incorrect",
      "Your account has expired" = "Your account has expired",
      "Please change your password" = "Please change your password",
      "New password:" = "New password:",
      "Confirm password:" = "Confirm password:",
      "Update new password" = "Update new password",
      "Password successfully updated! Please re-login" = "Password successfully updated! Please re-login",
      "The two passwords are different" = "The two passwords are different",
      "Failed to update password" = "Failed to update password"
    ),
    labels_lan = list(
      fr = list(
        "Please authenticate" = "Veuillez vous authentifier",
        "Username:" = "Nom d\'utilisateur :",
        "Password:" = "Mot de passe :",
        "Login" = "Se connecter",
        "Username or password are incorrect" = "Nom d\'utilisateur ou mot de passe incorrect",
        "Your account has expired" = "Votre compte a expir\u00e9",
        "Please change your password" = "Veuillez changer votre mot de passe",
        "New password:" = "Nouveau mot de passe :",
        "Confirm password:" = "Confirmez le mot de passe :",
        "Update new password" = "Mettre \u00e0 jour",
        "Password successfully updated! Please re-login" = "Mot de passe modifi\u00e9! Veuillez vous reconnecter",
        "The two passwords are different" = "Les deux mots de passe sont diff\u00e9rents",
        "Failed to update password" = "Echec de la mise \u00e0 jour du mot de passe"
      )
    ),
    length = function() base::length(private$labels)
  )
)



use_language <- function() {
  if (is.null(.globals$language))
    language$new()
  .globals$language
}


