// collapse sidebar into buttons
$(function() {
  var $el2 = $(".skin-red");
  $el2.addClass("sidebar-mini");
  
  
  var $sidebarInput = $("#val_date");
  var $logo = $(".logo");
  $(".sidebar-toggle").click(function() {
    $sidebarInput.toggle(400);
    $logo.toggle(400);
  });
});

