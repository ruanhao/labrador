/**
 * File     : labrador.js
 * Author   : ryan.ruan@ericsson.com
 * Purpose  : labrador websocket connection library
 * Created  : 2013-4-4 6:35
 */

var LABRADOR = (function(){
    var connect = function(path, callback) {
        var host = document.location.host;
        var sock = new WebSocket("ws://"+host+path);
        sock.onopen = function() {};
        sock.onclose = function() {isPaused = false;};
        sock.onmessage = callback;
        return sock;
    };
    return {
        connect: connect
    };
})();
