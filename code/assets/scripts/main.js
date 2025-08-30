(function($) {

  $(document).on('shiny:connected', function(event) {
    //console.log("Shiny connected");
    $('#showcase-code-tabs .nav-tabs > .active').removeClass('active');
    $('#showcase-code-tabs .nav-tabs li').each(function(){
      var a_text = $(this).children().text();
      var a_split = a_text.split('.R');
      //console.log(a_split[0]);
      $(this).addClass(a_split[0]);
    });
    $('#showcase-code-tabs .nav-tabs >').remove('.ui, .server');
    $('#showcase-code-tabs .nav-tabs > :first-child').addClass('active');
    
    $('#showcase-code-content .active').removeClass('active');
    $('#showcase-code-content >').remove('#ui_R_code, #server_R_code');
    $('#showcase-code-content > :first-child').addClass('active');
  });

})(jQuery); // Fully reference jQuery after this point.