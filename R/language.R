
.globals <- new.env(parent = emptyenv())

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
      if (!lan %in% private$language_registered) {
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
    language_registered = c("en", "fr", "br"),
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
      "Are you sure to remove user(s): %s from the database ?" = "Are you sure to remove user(s): %s from the database ?",
      "Delete user(s)" = "Delete user(s)",
      "Edit user" = "Edit user",
      "User already exist!" = "User already exist!",
      "Dismiss" = "Dismiss",
      "New user %s succesfully created!" = "New user %s succesfully created!",
      "Ask to change password" = "Ask to change password",
      "Confirm" = "Confirm",
      "Ask %s to change password on next connection?" = "Ask %s to change password on next connection?",
      "Change saved!" = "Change saved!",
      "Failed to update the database" = "Failed to update the database",
      "Password does not respect safety requirements" = "Password does not respect safety requirements",
      "Password must contain at least one number, one lowercase, one uppercase and must be at least length 6." = "Password must contain at least one number, one lowercase, one uppercase and must be at least length 6.",
      "Number of connections per user" = "Number of connections per user",
      "Number of connections per day" = "Number of connections per day",
      "Total number of connection" = "Total number of connection",
      "You can't remove yourself!" = "You can't remove yourself!",
      "User:" = "User:",
      "Period:" = "Period:",
      "Last week" = "Last week",
      "Last month" = "Last month",
      "All period" = "All period",
      "Home" = "Home",
      "Select all shown users" = "Select all shown users",
      "Remove selected users" = "Remove selected users",
      "Force selected users to change password" = "Force selected users to change password",
      "Users" = "Users",
      "Passwords" = "Passwords",
      "Download logs database" = "Download logs database",
      "Download SQL database" = "Download SQL database",
      "Reset password for %s?" = "Reset password for %s?",
      "Temporary password:" = "Temporary password:",
      "Password succesfully reset!" = "Password succesfully reset!",
      "You are not authorized for this application" = "You are not authorized for this application"
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
        "Are you sure to remove user(s): %s from the database ?" = "Etes-vous s\u00fbr de vouloir supprimer %s de la base de donn\u00e9es ?",
        "Delete user(s)" = "Supprimer l'/les utilisateur(s)",
        "Edit user" = "Modifier l'utilisateur",
        "User already exist!" = "L'utilisateur existe d\u00e9j\u00e0",
        "Dismiss" = "Fermer",
        "New user %s succesfully created!" = "Nouvel utilisateur %s cr\u00e9\u00e9 avec succ\u00e8s !",
        "Ask to change password" = "Demander \u00e0 changer le mot de passe",
        "Confirm" = "Confirmer",
        "Ask %s to change password on next connection?" = "Demander \u00e0 %s de changer son mot de passe \u00e0 la prochaine connexion ?",
        "Change saved!" = "Changements sauvegard\u00e9s !",
        "Failed to update the database" = "Une erreur s'est produite",
        "Password does not respect safety requirements" = "Le mot de passe ne respecte les r\u00e8gles de s\u00e9curit\u00e9",
        "Password must contain at least one number, one lowercase, one uppercase and must be at least length 6." = "Le mot de passe doit contenir au minimum un chiffre, une lettre majuscule, une lettre minuscule et doit \u00eatre au moins de longueur 6.",
        "Number of connections per user" = "Nombre de connexions par utilisateur",
        "Number of connections per day" = "Nombre de connexions par jour",
        "Total number of connection" = "Nombre total de connexions",
        "You can't remove yourself!" = "Vous ne pouvez pas supprimer votre propre compte!",
        "User:" = "Utilisateur :",
        "Period:" = "P\u00e9riode :",
        "Last week" = "Semaine derni\u00e8re",
        "Last month" = "Mois dernier",
        "All period" = "P\u00e9riode enti\u00e8re",
        "Home" = "Accueil",
        "Select all shown users" = "S\u00e9lectionner tous les utilisateurs affich\u00e9s",
        "Remove selected users" = "Supprimer les utilisateurs s\u00e9lectionn\u00e9s",
        "Force selected users to change password" = "Forcer les utilisateurs s\u00e9l\u00e9ctionn\u00e9s \u00e0 changer de mot de passe",
        "Users" = "Utilisateurs",
        "Passwords" = "Mots de passe",
        "Download logs database" = "T\u00e9l\u00e9charger les logs",
        "Download SQL database" = "T\u00e9l\u00e9charger la base SQL",
        "Reset password for %s?" = "R\u00e9initialiser le mot de passe de %s ?",
        "Temporary password:" = "Mot de passe temporaire",
        "Password succesfully reset!" = "Mot de passe r\u00e9initialis\u00e9",
        "You are not authorized for this application" = "Vous n'\u00eates pas habilit\u00e9 pour cette application"
      ),
      br = list(
        "Please authenticate" = "Autenticação",
        "Username:" = "Usuário:",
        "Password:" = "Senha:",
        "Login" = "Conectar",
        "Username or password are incorrect" = "Usuário ou senha incorreto",
        "Your account has expired" = "Sua conta expirou",
        "Please change your password" = "Por favor, mude sua senha",
        "New password:" = "Nova senha:",
        "Confirm password:" = "Confirmar nova senha:",
        "Update new password" = "Atualizar nova senha",
        "Password successfully updated! Please re-login" = "Senha alterada com sucesso! Por favor, autentique-se novamente",
        "The two passwords are different" = "As duas senhas são diferentes",
        "Failed to update password" = "Falha em atualizar a senha",
        "Logout" = "Desconectar",
        "Go to application" = "Ir à aplicação",
        "Administrator mode" = "Modo administrador",
        "Add a user" = "Adicionar usuário",
        "Failed to update user" = "Falha em atualizar usuário",
        "User successfully updated" = "Usuário atualizado com sucesso",
        "Cancel" = "Cancelas",
        "Confirm new user" = "Confirmar novo usuário",
        "Confirm change" = "Confirmar mudança",
        "Are you sure to remove user(s): %s from the database ?" = "Tem certeza que deseja remover o(s) usuário(s) %s do banco de dados?",
        "Delete user(s)" = "Deletar usuário(s)",
        "Edit user" = "Modificar usuário",
        "User already exist!" = "O usuário já existe!",
        "Dismiss" = "Fechar",
        "New user %s succesfully created!" = "Novo usuário %s criado com sucesso!",
        "Ask to change password" = "Pedir para alterar a senha",
        "Confirm" = "Confirmar",
        "Ask %s to change password on next connection?" = "Pedir a %s para alterar a senha na próxima conexão?",
        "Change saved!" = "Mudanças salvas!",
        "Failed to update the database" = "Erro em atualizar o banco de dados",
        "Password does not respect safety requirements" = "Senha não conforme com as exigências de segurança",
        "Password must contain at least one number, one lowercase, one uppercase and must be at least length 6." = "A senha deve conter pelo menos um número, uma letra minúscula, uma letra maiúscula e deve ter pelo menos 6 caracteres",
        "Number of connections per user" = "Número de conexões por usuário",
        "Number of connections per day" = "Número de conexões por dia",
        "Total number of connection" = "Número total de conexões",
        "You can't remove yourself!" = "Você não pode se remover!",
        "User:" = "Usuário:",
        "Period:" = "Período:",
        "Last week" = "Semana passada",
        "Last month" = "Mês passado",
        "All period" = "Todo período",
        "Home" = "Início",
        "Select all shown users" = "Selecionar todos os usuários mostrados",
        "Remove selected users" = "Remover usuários selecionados",
        "Force selected users to change password" = "Forçar usuário selecionado a mudar a senha",
        "Users" = "Usuários",
        "Passwords" = "Senhas",
        "Download logs database" = "Fazer download dos logs do banco de dados",
        "Download SQL database" = "Fazer download do banco de dados SQL",
        "Reset password for %s?" = "Resetar a senha de %s?",
        "Temporary password:" = "Senha temporária",
        "Password succesfully reset!" = "Senha resetada com sucesso!",
        "You are not authorized for this application" = "Você não está autorizado a utilizar esse aplicativo"
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
      ),
      br = list(
        sEmptyTable = "Nenhum dado disponível na tabela",
        sInfo = "Mostrando entrada de _START_ até _END_ de um total de _TOTAL_ entradas",
        sInfoEmpty = "Mostrando entrada de 0 até 0 de um total de 0 entradas",
        sInfoFiltered = "(filtrado de um total de _MAX_ entradas)",
        sInfoPostFix = "",
        sInfoThousands = ",",
        sLengthMenu = "Mostrar _MENU_ entradas",
        sLoadingRecords = "Carregando...",
        sProcessing = "Processando...",
        sSearch = "Busca:",
        sZeroRecords = "Nenhum registro compatível encontrado",
        oPaginate = list(
          sFirst = "Primeiro",
          sLast = "Último",
          sNext = "Próximo",
          sPrevious = "Anterior"
        ),
        oAria = list(
          sSortAscending = ": ativar para ordenar coluna de forma ascendente",
          sSortDescending = ": ativar para ordenar coluna de forma descendente"
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
set_language <- function(lan) {
  if (!identical(lan, "en")) {
    lan_set <- language$new()
    lan_set$set_language(lan)
  }
}

