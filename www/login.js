$(document).keyup(function(event) {
    if ($("#passwd").is(":focus") && (event.key == "Enter")) {
        $("#Login").click();
    }
});