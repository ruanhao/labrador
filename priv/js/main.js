/**
 * File   : main.js
 * Author : ryan.ruan@ericsson.com
 * Purpose: linked in index.html
 * Date   : 13-4-9 PM 8:51
 */

var globalRef       = null;
/* current process info websocket */
var currPiWebsocket = null;
var isPaused        = false;

$(document).ready(function()
{
    setCnodeInfo();
    setNodesMenu();
    setSpkLine();
});

function setSpkLine() {
    // Bar + line composite charts
    var cpuArr    = [];
    var memArr    = [];
    var etsArr    = [];
    var ARRMAXLEN = 35;
    LABRADOR.connect("/cnis", function(msg) {
        var jsonObj = $.parseJSON(msg.data);
        drawSparkLine(cpuArr, jsonObj.nprocs, '#99B3FF', 'red', ARRMAXLEN, '#nprocssl');
        drawSparkLine(memArr, jsonObj.memtot, '#2E8B57', 'red', ARRMAXLEN, '#memsl');
        drawSparkLine(etsArr, jsonObj.ets,    '#B8860A', 'red', ARRMAXLEN, '#etssl');
    });
}

function drawSparkLine(buffer, point, barColor, lineColor, maxPoints, jqyId) 
{
    buffer.push(point);
    if (buffer.length > maxPoints) {
        buffer.splice(0, 1);
    }
    $(jqyId).sparkline(buffer, {type: 'bar', barColor: barColor});
    $(jqyId).sparkline(buffer, {composite: true, fillColor: false, lineColor: lineColor});
}

function setCnodeInfo() {
    $.getJSON("/cni", function(data) {
        $("#cn")  .text(data.cnode);
        $("#lp")  .text(data.lp);
        $("#otpr").text(data.otpr);
        $("#sa")  .text(data.sa);
    });
}

function setNodesMenu() {
    $.getJSON("/ni", function(data)
    {
        $root = $("ul.menu");
        $.each(data, function(host, nodeList)
        {   $rootLi = $("<li></li>");
            $rootLi.append("<span>" + host + "</span>");
            $rootUl = $("<ul></ul>");
            $.each(nodeList, function(i, node)
            {
                $rootUl.append("<li><span>" + node + "</span></li>");
            });
            $rootLi.append($rootUl);
            $root  .append($rootLi);
        });

        $(".menu>li ul li span").click(function()   /*node exchange*/
        {
            $(".menu>li ul li span").removeClass("current");
            $(this).addClass("current");
            var rHost = $(this).parent().parent().prev().html();
            var rNode = $(this).html();
            var fullNodeName = rNode + "@" + rHost;
            if (!(fullNodeName === globalRef)) {
                if (currPiWebsocket) {
                    currPiWebsocket.close();
                }
                $("#rside").load("/static/html/tpl.html", function() {
                    onLoadTplComplete();
                });
                globalRef = fullNodeName;
            }
        });

        $("ul.menu>li>span").click(function()   /*toggle choice*/
        {
            $(this).next("ul").toggle();
        });
    });
}

function onLoadTplComplete() {
    setSniChk();
    setPuCheck();
    setAccCheck();
    setSearchFn();
    setSort();
    currPiWebsocket = LABRADOR.connect("/etop?node=" + globalRef, function(msg) {
        //show_debug($.dump($.parseJSON(msg.data)));
        doTabUpdate(msg);
        $("#search").trigger("keyup");
    });
}

function setSearchFn() {
    $("#search").keyup(function() {
        if ($(this).val() == '') {
            $("tbody#pib tr").show();
        } else {
            $("tbody#pib tr").hide().filter(":contains('" + $(this).val() + "')").show();
        }
    });
}

function doTabUpdate(msg) {
    if (!isPaused) {
        $("#pib").empty();
        var bodyStr   = "";
        var jsonObf   = $.parseJSON(msg.data);
        var procInfos = jsonObf.proc_info;
        var nodeInfo  = jsonObf.node_info;
        $("#cpuutil").text(nodeInfo[0]);
        $("#procnum").text(nodeInfo[1]);
        $("#runq")   .text(nodeInfo[2]);
        $("#totmem") .text(nodeInfo[3]);
        $("#binmem") .text(nodeInfo[4]);
        $("#procmem").text(nodeInfo[5]);
        $("#codemem").text(nodeInfo[6]);
        $("#atommem").text(nodeInfo[7]);
        $("#etsmem") .text(nodeInfo[8]);
        $.each(procInfos, function(idx, value) {
            var piArr   = value.data;
            var pid     = "<td><a id='pid' href='" + 
                          "/static/html/pid.html?pid=" + 
                          piArr[1].data + "'>" + piArr[1].data + "</a></td>";
            var mem     = "<td>" + piArr[2] + "</td>";
            var reds    = "<td>" + piArr[3] + "</td>";
            if (typeof(piArr[4]) == "object") {
                var name = "<td>" + 
                           piArr[4].data[0] + ":" + 
                           piArr[4].data[1] + "/" + 
                           piArr[4].data[2] + "</td>";
            } else {
                var name = "<td>" + piArr[4] + "</td>";
            }
            var cf      = "<td>" + 
                          piArr[6].data[0] + ":" + 
                          piArr[6].data[1] + "/" + 
                          piArr[6].data[2] + "</td>";
            var mq      = "<td>" + piArr[7] + "</td>";
            bodyStr += "<tr>" + pid + mem + reds + name + cf + mq + "</tr>";
        });
        $("#pib").append(bodyStr);
    }
}

function setSort() {
    $(".sort").click(function() {
        currPiWebsocket.send($(this).text());
    });
}

/* Show Node Info */
function setSniChk() {
    $("#nodeinfo").hide();
    var $sniChk = $('[name=sni]:checkbox');
    $sniChk.click(function() {
        if ($(this).is(":checked")) {
            $("#nodeinfo").show();
        } else {
            $("#nodeinfo").hide();
        }
    });
}

function setAccCheck() {
    var  $accChk = $('[name=acc]:checkbox');
    $accChk.click(function() {
        currPiWebsocket.send($(this).is(":checked") ? "accum" : "no_accum");
    });
}

function setPuCheck() {
    var $puChk = $('[name=pu]:checkbox');
    $puChk.click(function() {
        isPaused = $(this).is(":checked") ? true : false;
    });
}
