// This script just listens for "enter"s on the text input and simulates
// clicking the "send" button when that occurs. Totally optional.
jQuery(document).ready(function(){
  jQuery('#login_pw').keypress(function(evt){
    if (evt.keyCode == 13){
      // Enter, simulate clicking send
      jQuery('#login_button').click();
    }
  });
});

