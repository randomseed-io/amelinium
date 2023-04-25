var amelinium = {
  config: ameliniumConfig,

  openMainModal: function() {
    var el = document.getElementById('main-modal');
    if (el) {
      var mm = bootstrap.Modal.getOrCreateInstance(el);
      if (mm) {
        mm.show();
      }
    }
  },

  closeMainModal: function() {
    var el = document.getElementById('main-modal');
    if (el) {
      var mm = bootstrap.Modal.getOrCreateInstance(el);
      if (mm) {
        mm.hide();
      }
    }
  },

  init: function() {
    var session_id_key = amelinium.config.session_id_key;
    var session_id = null;

    if (session_id_key) {
       document.body.addEventListener('htmx:afterOnLoad', function(evt) {
        var sid = evt.detail.xhr.getResponseHeader(session_id_key);
        if (sid) {
          session_id = sid;
        } else {
          session_id = null;
        }
      });

      document.body.addEventListener('htmx:configRequest', function(evt) {
        if (session_id) {
          evt.detail.headers[session_id_key] = session_id;
        }
      });
    }
  }
}

if (window.attachEvent)           { window.attachEvent('onload', amelinium.init); }
else if (window.addEventListener) { window.addEventListener('load', amelinium.init, false); }
else { document.addEventListener('load', amelinium.init, false); }
