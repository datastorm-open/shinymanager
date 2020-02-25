// add assign polyfill (billboarder / old browser)
if (typeof Object.assign !== 'function') {
  // Must be writable: true, enumerable: false, configurable: true
  Object.defineProperty(Object, "assign", {
    value: function assign(target, varArgs) { // .length of function is 2
      'use strict';
      if (target === null || target === undefined) {
        throw new TypeError('Cannot convert undefined or null to object');
      }

      var to = Object(target);

      for (var index = 1; index < arguments.length; index++) {
        var nextSource = arguments[index];

        if (nextSource !== null && nextSource !== undefined) { 
          for (var nextKey in nextSource) {
            // Avoid bugs when hasOwnProperty is shadowed
            if (Object.prototype.hasOwnProperty.call(nextSource, nextKey)) {
              to[nextKey] = nextSource[nextKey];
            }
          }
        }
      }
      return to;
    },
    writable: true,
    configurable: true
  });
}


$(function() {
  timeout_cpt = 1;
  
  $(document).on({
    'shiny:inputchanged': function(event) {
      if (event.name !== '.shinymanager_timeout' && event.name.indexOf(".clientdata") === -1) {
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
