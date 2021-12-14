
function copyToClipboard(text) {
  var $temp = $("<input>");
  $("body").append($temp);
  $temp.val(text).select();
  document.execCommand("copy");
  $temp.remove();
}


function vexpand(e) {
  $h = $(e);
  $h.siblings('div.expand-content').slideToggle(100,
      function() {
          $h.children('i').attr('class',
              function() {
                  return $h.siblings('div.expand-content').is(':visible') ? 'fa fa-chevron-down' :
                      'fa fa-chevron-right';
              });
      });
}
   
   
jQuery(document).ready(function() {
//h1~h2,h1~h3,h1~h4,h1~h5,h1~h6
    $("#body-inner h1,h2,h3,h4,h5,h6").append(function(index, html){
      var element = $(this);     
      var link = window.location.href + "#"+element[0].id;
      return " <span class='anchor'>" +
        "<i class='fas fa-link fa-lg'></i>" +
        "</span>"
      ;
    });
/*
    $(".locallab").each(function(i,e){
      if (window.location.hostname != "localhost"){
        $(e).hide();
      }

    });
*/

    $( ".anchor" ).on("click",
    function() {
      var ele=$(this).parent();
      //encodeURI取代escape而且只能放ID部分
      var link = window.location.href + "#"+encodeURI(ele[0].id);
      copyToClipboard(link);
    });

    $("#sidebar-toggle-span").on("click",
    function(e){
       $("#sidebar").toggle();
       if( $("#sidebar").is(":visible"))
         $("#page-guide").show();
       else
       $("#page-guide").hide();
      e.stopPropagation (); // indicate prevent bubbling to the parent element
      e.preventDefault ();      

    }

  );

 /*
  $("#sidebar .highlightable #topics-toggle").on("click",
  function(e){
     $(".topics li li").toggle();
    e.stopPropagation (); // indicate prevent bubbling to the parent element
    e.preventDefault ();      

  });
*/
$("#topics-toggle").on("click",
function(e){
   $("#footer").toggleClass("up");
  e.stopPropagation (); // indicate prevent bubbling to the parent element
  e.preventDefault ();      

});

var images = $("#body-inner img").not(".inline");
// Wrap image inside a featherlight (to get a full size view in a popup)
images.wrap(function(){
  var image =$(this);
  //var o = getUrlParameter(image[0].src);
  //var f = o['featherlight'];
  // IF featherlight is false, do not use feather light
  //if (f != 'false') {
    if (!image.parent("a").length) {
      return "<a href='" + image[0].src + "' data-featherlight='image'></a>";
    }
  //}
});


});  

