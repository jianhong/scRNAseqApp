$(document).on('shiny:sessioninitialized', function(){
  $.getJSON("https://api.ipify.org/?format=json", function(e) {
    Shiny.setInputValue("remote_addr", e.ip);
  });
  Shiny.addCustomMessageHandler("save_key", function(value){
    let arr = value.split("|");
    localStorage.setItem(arr[0], arr[1]);
  });
  Shiny.addCustomMessageHandler("load_key", function(key){
    let value = localStorage.getItem(key);
    if(typeof(value)=="undefined") return false;
    if(value === null) return false;
    Shiny.setInputValue('default_'+key, value);
  });
});
