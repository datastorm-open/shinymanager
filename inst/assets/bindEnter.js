

function bindEnter(ns) {
  $('#' + ns + 'user_pwd').on('keypress',function(e) {
    if(e.which == 13) {
      $('#' + ns + 'go_auth').click();
    }
  });
  $('#' + ns + 'user_id').on('keypress',function(e) {
    if(e.which == 13) {
      $('#' + ns + 'go_auth').click();
    }
  });
}


