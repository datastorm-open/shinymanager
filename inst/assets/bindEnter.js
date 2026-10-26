// #190
window.onload = function() { 
  val = $('#auth-language').find("option[selected]").val();
  if(val === ''){
    location.reload();
  }
};

// Shiny sends text inputs 250 ms after the last key (debounce): send the
// current values right away, so the click does not use outdated ones
function sendInputs(ns, ids) {
  ids.forEach(function(id) {
    Shiny.setInputValue(ns + id, $('#' + ns + id).val());
  });
}

function bindEnter(ns) {
  $('#' + ns + 'user_pwd').on('keyup',function(e) {
    if(e.which == 13) {
      sendInputs(ns, ['user_id', 'user_pwd']);
      $('#' + ns + 'go_auth').click();
    }
  });
  $('#' + ns + 'user_id').on('keyup',function(e) {
    if(e.which == 13) {
      sendInputs(ns, ['user_id', 'user_pwd']);
      $('#' + ns + 'go_auth').click();
    }
  });
  
  $('#' + ns + 'pwd_one').on('keyup',function(e) {
    if(e.which == 13) {
      sendInputs(ns, ['pwd_one', 'pwd_two']);
      $('#' + ns + 'update_pwd').click();
    }
  });
  $('#' + ns + 'pwd_two').on('keyup',function(e) {
    if(e.which == 13) {
      sendInputs(ns, ['pwd_one', 'pwd_two']);
      $('#' + ns + 'update_pwd').click();
    }
  });
}

Shiny.addCustomMessageHandler('update_auth_title', function(data) {
  $('#' + data.inputId).html(data.title);
});
