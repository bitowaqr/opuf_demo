(function() {
var output_states2 = {};

function escapeSelector2(s) {
    return s.replace(/([!"#$%&'()*+,-./:;<=>?@\[\\\]^`{|}~])/g, "\\$1");
}

function show_spinner2(id) {
    var selector = "#" + escapeSelector2(id);
    var parent = $(selector).closest(".shiny-spinner-output-container");
    $(selector).siblings(".load-container, .shiny-spinner-placeholder").removeClass('shiny-spinner-hidden');
    
    if (parent.hasClass("shiny-spinner-hideui")) {
      $(selector).siblings(".load-container").siblings('.shiny-bound-output, .shiny-output-error').css('visibility', 'hidden');
      // if there is a proxy div, hide the previous output
      $(selector).siblings(".shiny-spinner-placeholder").siblings('.shiny-bound-output, .shiny-output-error').addClass('shiny-spinner-hidden');      
    }
}

function hide_spinner2(id) {
    var selector = "#" + escapeSelector2(id);
    var parent = $(selector).closest(".shiny-spinner-output-container");
    $(selector).siblings(".load-container, .shiny-spinner-placeholder").addClass('shiny-spinner-hidden');
    if (parent.hasClass("shiny-spinner-hideui")) {
      $(selector).siblings(".load-container").siblings('.shiny-bound-output').css('visibility', 'visible');
      // if there is a proxy div, show the previous output in case it was hidden
      $(selector).siblings(".shiny-spinner-placeholder").siblings('.shiny-bound-output').removeClass('shiny-spinner-hidden');
    }
}

function update_spinner2(id) {
  if (!(id in output_states2)) {
    /*show_spinner2(id);*/
  }
  if (output_states2[id] <= 0) {
    show_spinner2(id);
  } else {
    setTimeout(() => {hide_spinner2(id)}, 1250); //wait 1.2 extra seconds.
  }
}

$(document).on('shiny:bound', function(event) { 
  var id = event.target.id;
  if (id === undefined || id == "") {
    return;
  }
  
  /* if not bound before, then set the value to 0 */
  if (!(id in output_states2)) {
    output_states2[id] = 1;  /* !!! I set this to 1 !!! */
  }
  update_spinner2(id);
});

/* When recalculating starts, show the spinner container & hide the output */
$(document).on('shiny:outputinvalidated', function(event) {
  var id = event.target.id;
  if (id === undefined) {
    return;
  }
  output_states2[id] = 0;
  update_spinner2(id);
});

/* When new value or error comes in, hide spinner container (if any) & show the output */
$(document).on('shiny:value shiny:error', function(event) {
  var id = event.target.id;
  if (id === undefined) {
    return;
  }
  output_states2[id] = 1;
  update_spinner2(id);
});
}());