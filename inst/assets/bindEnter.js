

function bindEnter(ns) {
  $('#' + ns + 'user_pwd').on('keyup',function(e) {
    if(e.which == 13) {
      $('#' + ns + 'go_auth').click();
    }
  });
  $('#' + ns + 'user_id').on('keyup',function(e) {
    if(e.which == 13) {
      $('#' + ns + 'go_auth').click();
    }
  });
  
  $('#' + ns + 'pwd_one').on('keyup',function(e) {
    if(e.which == 13) {
      $('#' + ns + 'update_pwd').click();
    }
  });
  $('#' + ns + 'pwd_two').on('keyup',function(e) {
    if(e.which == 13) {
      $('#' + ns + 'update_pwd').click();
    }
  });
}

Shiny.addCustomMessageHandler('focus_input', function(data) {
  $('#' + data.inputId).focus();
});

Shiny.addCustomMessageHandler('update_auth_title', function(data) {
  $('#' + data.inputId).html(data.title);
});
