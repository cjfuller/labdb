function loginViaEmail() {
  navigator.id.get(function(assertion) {
    if (assertion) {
      $('input[name=assertion]').val(assertion);
      $('#persona_form').submit();
    } else {
      window.location = "#{failure_path}"
    }
  });  
}

$(function() {
  $('#persona_form button').click(function() {
    loginViaEmail();
    return false;
  });
});

