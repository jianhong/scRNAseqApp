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
    $('#about-privateDatasets').hide();
    $('div.about-left-border').on('click', function(e){
        // Append the new div to the container
        $('#informationBox>h4').text($(this).attr('info'));
        $('#informationBox>div#infolist').html($(this).attr('details'));

        // Optional: Add animation for a smoother expansion
        if($(this).attr('info')!='Public dataset list:'){
            $('#about-privateDatasets').hide();
        }else{
            $('#about-privateDatasets').show();
        }
        $('#informationBox').hide().slideDown('slow');
    });
    // activate the wheel-zoomable-plot 
    Shiny.addCustomMessageHandler("updatePanStatus", function(id){
        if(id.value){
            $("#"+id.id+'-GeneExproupDIV'+id.postfix).addClass("wheel-zoomable-plot");
        }else{
            $("#"+id.id+'-GeneExproupDIV'+id.postfix).removeClass("wheel-zoomable-plot");
        }
        
    })
    // handle mouse wheel
    var scrollTimeout;
    var delta = 0;
    document.addEventListener('wheel', function(e) {
        var plot = e.target.closest('.wheel-zoomable-plot');
        if (!plot) return;
        e.preventDefault();
        delta += e.deltaY;
        clearTimeout(scrollTimeout);
        scrollTimeout = setTimeout(() => {
            Shiny.setInputValue(
                plot.id.replace(/DIV/, '.scroll'), 
                delta, {priority: 'event'});
            delta = 0;
        }, 200);
    }, { passive: false });   // ← THIS is the key to prevent parents scroll events
    // set pan and selecting
    var startX = 0;
    var startY = 0;
    var activePlot = null;
    var isPanning = false;
    let dragThreshold = 3;   // pixels
    var activePlotID = '';
    document.addEventListener('mousedown', function(e) {
      isPanning = false;
      const plot = e.target.closest('.wheel-zoomable-plot > div');
      if (!plot) return;
      activePlotID = plot.parentElement.id;
      Shiny.setInputValue(activePlotID.replace(/DIV/, '.isPanning'), false);
      // normal drag = pan
      startX = e.clientX;
      startY = e.clientY;
      activePlot = plot;
    }, true);   // 🔥 CRITICAL: capture phase
    document.addEventListener('mousemove', function(e) {
      if (!activePlot) return;
      const dx = e.clientX - startX;
      const dy = e.clientY - startY;
      if (!isPanning) {
        if ( (Math.abs(dx) > dragThreshold && Math.abs(dy) < dragThreshold) ||
             (Math.abs(dy) > dragThreshold && Math.abs(dx) < dragThreshold) ) {
        // pan only when move in x direction or y direction
          isPanning = true;
          activePlot.style.cursor = "grabbing";
          Shiny.setInputValue(activePlotID.replace(/DIV/, '.isPanning'), true);
          e.preventDefault();
          e.stopPropagation();
          e.stopImmediatePropagation();
        } else {
          return;  // small movement → allow click/dblclick
        }
      }
    }, true);
    document.addEventListener('mouseup', function(e) {
        if (!activePlot) return;
        if (isPanning) {
            // clean all brush
            $("#"+activePlotID.replace(/DIV/, '')+"_brush").remove();
            const dx = e.clientX - startX;
            const dy = e.clientY - startY;
            Shiny.setInputValue(activePlotID.replace(/DIV/, '.pan'), {
              dx: dx,
              dy: dy,
              width: e.target.offsetWidth,
              height: e.target.offsetHeight,
              nonce: Math.random()
            }, {priority: 'event'});
        
            e.preventDefault();
            e.stopPropagation();
            e.stopImmediatePropagation();
          }
          activePlot.style.cursor = "auto";
          activePlot = null;
          isPanning = false;
        }, true);

    // Get mouse coordinates
    var mouseX, mouseY, clientX, clientY;
    $(document).mousemove(function(e){
        mouseX = e.pageX;
        mouseY = e.pageY;
        clientX = e.clientX;
        clientY = e.clientY;
    }).mouseover();
    
    Shiny.addCustomMessageHandler("placeGeneExproupInfoEditorBox", function(id){
        Shiny.setInputValue(id+'-current_mouseX', mouseX);
        Shiny.setInputValue(id+'-current_mouseY', mouseY);
        Shiny.setInputValue(id+'-current_clientX', clientX);
        Shiny.setInputValue(id+'-current_clientY', clientY);
    })
    // editorStatus
    Shiny.addCustomMessageHandler("updateEditorStatus", function(id){
        Shiny.setInputValue('editorStatus', Date.now());
        Shiny.setInputValue(id.id+'-editorStatus'+id.postfix, Date.now());
    })
    // explorer when subset group is selected
    Shiny.addCustomMessageHandler("click_btn", function(id){
        $("#"+id).trigger('click');
    })
    // show duplicated botton
    Shiny.addCustomMessageHandler("show_div", function(id){
        $("#"+id).css('visibility', 'visible');
    })
    // hide div
    Shiny.addCustomMessageHandler("hide_div", function(id){
        $("#"+id).css('visibility', 'hidden');
    })
    // switch div
    Shiny.addCustomMessageHandler("toggle_div", function(id){
        if($("#"+id).css('visibility') === 'hidden'){
            $("#"+id).css('visibility', 'visible');
        }else{
            $("#"+id).css('visibility', 'hidden');
        }
    })
    // resizable-container
    function initResizable() {
          document.querySelectorAll('.resizable-container').forEach(function (container) {
            // Skip already-initialised containers
            if (container.dataset.resizableInit) return;
            container.dataset.resizableInit = 'true';
            // Each container has exactly ONE divider
            var divider    = container.querySelector('.divider');
            var leftPanel  = divider.previousElementSibling;
            var rightPanel = divider.nextElementSibling;
            var isResizing   = false;
            var startX, startLeftWidth, startRightWidth;
            divider.addEventListener('mousedown', function (e) {
              isResizing     = true;
              startX         = e.clientX;
              startLeftWidth  = leftPanel.getBoundingClientRect().width;
              startRightWidth = rightPanel.getBoundingClientRect().width;
              divider.classList.add('dragging');
              document.body.style.cursor     = 'col-resize';
              document.body.style.userSelect = 'none';
              e.preventDefault();
            });
            document.addEventListener('mousemove', function (e) {
              if (!isResizing) return;
              var dx       = e.clientX - startX;
              var newLeft  = startLeftWidth  + dx;
              var newRight = startRightWidth - dx;
              var minW     = 150;
              if (newLeft  < minW) { dx = minW - startLeftWidth;  newLeft  = minW; newRight = startRightWidth - dx; }
              if (newRight < minW) { dx = startRightWidth - minW; newLeft  = startLeftWidth + dx; newRight = minW; }
              leftPanel.style.width  = newLeft  + 'px';
              rightPanel.style.width = newRight + 'px';
            });

            document.addEventListener('mouseup', function () {
              if (!isResizing) return;
              isResizing = false;
              divider.classList.remove('dragging');
              document.body.style.cursor     = '';
              document.body.style.userSelect = '';
            });
          });
        }
    // Wait for Shiny to render panels
    $(document).on('shiny:idle', function(){
        initResizable();
    });
  });
}())
