// custom handler for deleting shapes drawn with leafletDraw.
// adapted from https://github.com/bhaskarvk/leaflet.extras/issues/96
Shiny.addCustomMessageHandler('removeSelectionPolygon', function(x){
   console.log('deleting', x);
   // get leaflet map
   var map = HTMLWidgets.find('#' + x.elid).getMap();
   // remove the layer
   map.removeLayer(map._layers[x.layerid]);
});

// To get access to the dom right after a widget is created and has been placed into the dom:
// 1) Need to catch shiny in the act of creating the widget, by catching on('shiny:value') events
// 2) Since the widget hasn't been inserted into the dom yet at this point, it can't be modified yet.
//    Instead, set an input for shiny (using Shiny.onInputChange('myWidgetCreated'...)), then set an event handler in 
//    Shiny for this input (observeEvent(input$myWidgetCreated, {...}))
// 3) Add a custom message hander in javascript (using Shiny.addCustomMessageHandler('modifyMyWidget', function(params){...}))
// 4) in observeEvent(input$myWidgetCreated, ...), add a call to the custom message handler
//    session$sendCustomMessage("modifyMyWidget", params)
//    The custom message handler will be called after the widget is inserted into the dom and can modify any element there
//    e.g., use $('.myClass').attr(...) to get/set attributes for dom elements of class myClass 

Shiny.addCustomMessageHandler('fixDrawButtonTitles', function(x){
   //console.log('fixing draw buttons for map ' + x);
   $('.leaflet-draw-draw-rectangle').attr('title', 'Rectangular selection');
   $('.leaflet-draw-draw-polygon').attr('title', 'Polygon selection');
});


$(document).on('shiny:value', function(event) {
  // show event
  console.log('shiny value set:');
  console.log(event);
  if (event.target.id == 'CUmap') {
     Shiny.onInputChange('CUmapCreated', Math.random());
  }
});
       
