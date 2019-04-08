
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
      private$language <- lan
      private$labels <- private$labels_lan[[lan]]
    },
    get = function(label) {
      private$labels[[label]]
    },
    get_all = function() {
      private$labels
    },
    get_DT = function() {
      private$DT_lan[[private$language]]
    }
  ),
  private = list(
    language = "en",
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
      "Failed to update password" = "Failed to update password",
      "Logout" = "Logout",
      "Go to application" = "Go to application",
      "Administrator mode" = "Administrator mode",
      "Add a user" = "Add a user",
      "Failed to update user" = "Failed to update user",
      "User successfully updated" = "User successfully updated",
      "Cancel" = "Cancel",
      "Confirm new user" = "Confirm new user",
      "Confirm change" = "Confirm change",
      "Are you sure to remove user: %s from the database ?" = "Are you sure to remove user: %s from the database ?",
      "Delete user" = "Delete user",
      "Edit user" = "Edit user",
      "User already exist!" = "User already exist!",
      "Dismiss" = "Dismiss",
      "New user succesfully created!" = "New user succesfully created!",
      "Ask to change password" = "Ask to change password",
      "Confirm" = "Confirm",
      "Ask %s to change password on next connection?" = "Ask %s to change password on next connection?",
      "Change saved!" = "Change saved!",
      "Failed to update the database" = "Failed to update the database"
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
        "Failed to update password" = "Echec de la mise \u00e0 jour du mot de passe",
        "Logout" = "Se d\u00e9connecter",
        "Go to application" = "Aller \u00e0 l'application",
        "Administrator mode" = "Mode administrateur",
        "Add a user" = "Ajouter un utilisateur",
        "Failed to update user" = "Echec de la mise \u00e0 jour de l'utilisateur",
        "User successfully updated" = "Mise \u00e0 jour r\u00e9ussie",
        "Cancel" = "Annuler",
        "Confirm new user" = "Valider l'ajout",
        "Confirm change" = "Valider les modifications",
        "Are you sure to remove user: %s from the database ?" = "Etes-vous s\u00fbr de vouloir supprimer %s de la base de donn\u00e9es ?",
        "Delete user" = "Supprimer l'utilisateur",
        "Edit user" = "Modifier l'utilisateur",
        "User already exist!" = "L'utilisateur existe d\u00e9j\u00e0",
        "Dismiss" = "Fermer",
        "New user succesfully created!" = "Nouvel utilisateur cr\u00e9\u00e9 avec succ\u00e8s !",
        "Ask to change password" = "Demander \u00e0 changer le mot de passe",
        "Confirm" = "Confirmer",
        "Ask %s to change password on next connection?" = "Demander \u00e0 de changer son mot de passe \u00e0 la prochaine connexion ?",
        "Change saved!" = "Changements sauvegard\u00e9s !",
        "Failed to update the database" = "Une erreur s'est produite"
      )
    ),
    DT_lan = list(
      fr = list(
        sProcessing = "Traitement en cours...", sSearch = "Rechercher&nbsp;:",
        sLengthMenu = "Afficher _MENU_ &eacute;l&eacute;ments",
        sInfo = "Affichage de l'&eacute;l&eacute;ment _START_ &agrave; _END_ sur _TOTAL_ &eacute;l&eacute;ments",
        sInfoEmpty = "Affichage de l'&eacute;l&eacute;ment 0 &agrave; 0 sur 0 &eacute;l&eacute;ment",
        sInfoFiltered = "(filtr&eacute; de _MAX_ &eacute;l&eacute;ments au total)",
        sInfoPostFix = "", sLoadingRecords = "Chargement en cours...",
        sZeroRecords = "Aucun &eacute;l&eacute;ment &agrave; afficher",
        sEmptyTable = "Aucune donn&eacute;e disponible dans le tableau",
        oPaginate = list(
          sFirst = "Premier", sPrevious = "Pr&eacute;c&eacute;dent",
          sNext = "Suivant", sLast = "Dernier"
        ),
        oAria = list(
          sSortAscending = ": activer pour trier la colonne par ordre croissant",
          sSortDescending = ": activer pour trier la colonne par ordre d&eacute;croissant"
        )
      ),
      en = list(
        sEmptyTable = "No data available in table",
        sInfo = "Showing _START_ to _END_ of _TOTAL_ entries",
        sInfoEmpty = "Showing 0 to 0 of 0 entries",
        sInfoFiltered = "(filtered from _MAX_ total entries)",
        sInfoPostFix = "",
        sInfoThousands = ",",
        sLengthMenu = "Show _MENU_ entries",
        sLoadingRecords = "Loading...",
        sProcessing = "Processing...",
        sSearch = "Search:",
        sZeroRecords = "No matching records found",
        oPaginate = list(
          sFirst = "First",
          sLast = "Last",
          sNext = "Next",
          sPrevious = "Previous"
        ),
        oAria = list(
          sSortAscending = ": activate to sort column ascending",
          sSortDescending = ": activate to sort column descending"
        )
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


