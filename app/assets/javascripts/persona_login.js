$(function() {
  $('#persona-login-button').click(function() {
    navigator.id.get(verifyAssertion, {
      backgroundColor: "#095826",
      siteName: "Labdb"
    });
  });

  function verifyAssertion(assertion) {
    $.ajax({
      type: 'POST',
      url: '/auth/persona/callback',
      data: {assertion: assertion},
      success: function(res, status, xhr) {window.location.reload();},
      error: function(xhr, status, err) {
        window.location.reload();
      }
    });
  }
});
