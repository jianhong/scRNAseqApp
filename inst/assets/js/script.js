(function(){
  function show_loader(event){
    var id = event.target.id;
    if (id === undefined) {
      return;
    }
    $('#scRNAseqAppLoader-'+id).show();
  }
  function hide_loader(event){
    var id = event.target.id;
    if (id === undefined) {
      return;
    }
    $('#scRNAseqAppLoader-'+id).hide();
  }
  $(document).on('shiny:outputinvalidated', show_loader);
  $(document).on('shiny:bound', show_loader);
  $(document).on('shiny:value shiny:error', hide_loader);
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
    $('a[data-value="home"]').on("click", function(e) {
          e.preventDefault();
          $('a[data-value="about"]').trigger('click');
          return false;
        });
    $('#about-search').on('keyup', function(e) {
        if(e.keyCode == 13){//search box, when enter pressed
            $("#about-sbtn").trigger('click');
            $("#about-sbtn").prop('disabled', true); //disable clicked button
            var intr = setInterval(function(){
                if($("#about-s_res_flag").prop("checked")){
                    $("#about-sbtn").prop('disabled', false);
                    clearInterval(intr);
                }
            }, 1000);
        }
    });
    // Get mouse coordinates
    var mouseX, mouseY;
    $(document).mousemove(function(e){
        mouseX = e.pageX;
        mouseY = e.pageY;
    }).mouseover();
    Shiny.addCustomMessageHandler("placeGeneExproupInfoEditorBox", function(id){
        Shiny.setInputValue(id+'-current_mouseX', mouseX);
        Shiny.setInputValue(id+'-current_mouseY', mouseY);
    })
    // editorStatus
    Shiny.addCustomMessageHandler("updateEditorStatus", function(id){
        Shiny.setInputValue('editorStatus', Date.now());
        Shiny.setInputValue(id.id+'-editorStatus'+id.postfix, Date.now());
    })
    // explorer when subset group is selected
    Shiny.addCustomMessageHandler("click_subset_btn", function(id){
        $("#"+id).trigger('click');
    })
  });
}())
