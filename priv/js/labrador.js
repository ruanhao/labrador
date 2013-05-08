/**
 * This is a common javascript file
 * Author: Hao Ruan <ryan.ruan@ericsson.com>
 * Date  : 13-4-4
 * Time  : 6:35
 */

var DALLAS = (function(){
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