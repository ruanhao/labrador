/**
 * File   : pid.js
 * Author : ryan.ruan@ericsson.com
 * Purpose: linked in pid.html
 * Date   : 13-4-9 PM 8:51
 */

function render_json_val(obj) {
    if (obj        === true)    return "true";
    if (obj        === false)   return "false";
    if (typeof obj ==  'number') return obj;
    if (typeof obj ==  'string') return obj;
    if ($.isArray(obj))         return render_list(obj, false);
    if (typeof obj ==  'object') return render_object(obj);
}

function render_list(list, isTuple) {
    if (list.length == 0) {
        return isTuple ? "{}" : "[]";
    } else {
        var tmp = isTuple ? "{" : "[";
        $.each(list, function (idx, value) {
                if (idx == list.length - 1) {
                    var prefix = (list.length == 1) ? "" : ", ";
                    var suffix = isTuple ? "}" : "]";
                    tmp += prefix + render_json_val(value) + suffix;
                } else {
                    var prefix = (idx != 0) ? ", " : "";
                    tmp += prefix + render_json_val(value);
                }});
        return tmp;
    }
}

function render_object(obj) {
    switch (obj._type) {
        case "tuple":
            if (obj.data.length == 3 && (typeof obj.data[2] == 'number')) {
                return obj.data[0] + ':' + obj.data[1] + "/" + obj.data[2];
            }
            return render_list(obj.data, true);
        case "pid":
            var Pid = obj.data.replace(/[<>]/g, '');
            return "<a id='pid' href='/static/html/pid.html?pid=" + obj.data + "'>" + obj.data + "</a>";
        case "port":
            return obj.data;
        case "fun":
            return obj.data;
        default: /*just a normal Object*/
            if (typeof obj == "object") {
                var tmp = "";
                $.each(obj, function(k, v) {
                    tmp += k + ": " + render_json_val(v) + "; ";
                });
                return tmp;
            } else {
                return "undef";
            }
    }
}

function gen_pid_html(json) {
    var tmpHtml = "";
    $.each(json, function (k, v) {
        tmpHtml += "<div>" + "<b>" + k + ": " + "</b>" + render_json_val(v) + "</div>";
    });
    return tmpHtml;
}
