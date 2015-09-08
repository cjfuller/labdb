$(function() {

  function renderLoginItems() {
    $('.login-area').show();
    $('.waiting-area').hide();
  }

  function renderWaitingSpinner() {
    $('.login-area').hide();
    $('.waiting-area').show();
  }

  $.ready(renderLoginItems());

  $('#persona-login-button').click(function() {
    navigator.id.get(verifyAssertion, {
      backgroundColor: "#095826",
      siteName: "Labdb"
    });
    renderWaitingSpinner();
  });

  function verifyAssertion(assertion) {
    $.ajax({
      type: 'POST',
      url: '/_i/persona/verify_assertion',
      data: {assertion: assertion},
      success: function(res, status, xhr) {window.location.reload();},
      error: function(xhr, status, err) {
        window.location.reload();
      }
    });
  }

  $('#logout-button').click(function() {
    navigator.id.logout();
  });
});
