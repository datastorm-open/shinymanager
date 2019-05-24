$(function() {
  timeout_cpt = 1;
  $(document).on({
    'shiny:inputchanged': function(event) {
      if (event.name !== '.shinymanager_timeout') {
        Shiny.onInputChange(".shinymanager_timeout", timeout_cpt);
        timeout_cpt = timeout_cpt + 1;
      }
    },
    
    'shiny:recalculating': function(event) {
      if (event.name !== '.shinymanager_timeout') {
        Shiny.onInputChange(".shinymanager_timeout", timeout_cpt);
        timeout_cpt = timeout_cpt + 1;
      }
    }
    
  });
});
