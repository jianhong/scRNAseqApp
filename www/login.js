$(document).keyup(function(event) {
    if ($("#passwd").is(":focus") && (event.key == "Enter")) {
        $("#Login").click();
    }
});

$(document).on('shiny:sessioninitialized', function(){
  $.getJSON("https://api.ipify.org/?format=json", function(e) {
    Shiny.onInputChange("remote_addr", e.ip);
  });
});
