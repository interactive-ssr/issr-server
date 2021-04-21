if (socket == undefined) var socket;
if (wsurl == undefined) var wsurl;
if (previousdata == undefined) var previousdata = {};

/**
 * attr
 * Return the ATTRIBUTE member, attribute, or undefined of OBJ
 */
function attr (obj, attribute) {
    if (obj) {
        if (obj[attribute] || obj[attribute] == "") {
            return obj[attribute];
        } else if (obj.getAttribute) {
            return obj.getAttribute(attribute);
        }
    }
    return undefined
}

/**
 * update
 * Modify the dom to be up to date with the server.
 * INSTRUCTIONS: An array containing objects like such:
 * - ["mod", id, [key, value]...]: modifiy attributes or properties
 * - ["delete", id, child-index]: delete node (child-index is optional)
 * - ["insert", id, text, position, html-string]: insert html-string as a text or html node  either "before", "after", or "prepend" indexes.
 * - ["cookie", cookies...]: set cookies
 * - ["redirect", target]: redirect to target
 * - ["reconnect"]: reset the websocket and send the previousdata.
 * - ["error", message]: display server error to console.error
 */
function update (instructions) {
    for (let instruction of instructions) {
        switch (instruction[0]) {
        case "mod": {
            let node = document.getElementById(instruction[1]);
            if (node) {
                for (let i = 2; i < instruction.length; ++i) {
                    if (instruction[i][0].toString().startsWith("on")) {
                        node[instruction[i][0]] = Function("event", instruction[i][1]);
                    } else {
                        node[instruction[i][0]] = instruction[i][1];
                    }
                    if (instruction[i][0].indexOf("HTML") < 0) {
                        if ("" == instruction[i][1]) {
                            node.removeAttribute([instruction[i][0]]);
                        } else {
                            node.setAttribute([instruction[i][0]], instruction[i][1]);
                        }
                    }
                }
            }
            break;}
        case "delete": {
            let node = document.getElementById(instruction[1]);
            if (node && instruction[2]) {
                node.childNodes[instruction[2]].remove();
            } else if (node) {
                node.remove();
            }
            break;}
        case "insert": {
            let parent = document.getElementById(instruction[1]);
            if (parent) {
                let node = instruction[2]?
                    document.createTextNode(instruction[4]):
                    document.createElement('nil');
                parent[instruction[3]](node);
                if (!instruction[2]) {
                    node.outerHTML = instruction[4];
                }
            }
            break;}
        case "cookie": {
            for (let i = 1; i < instruction.length; ++i) {
                document.cookie = instruction[i];
            }
            break;}
        case "redirect": {
            document.location = instruction[1];
            break;}
        case "reconnect": {
            instruction[1]();
            break;}
        case "error": {
            console.error(instruction[1]);
            break;}
        }
    }
}

/**
 * connect
 * Connect to the websocket on the server.
 * ID: the unique server generated id for identifying with the websocket.
 * PORT (optional): The port to connect to. (443 or 80 by default based on PROTOCOL).
 * PROTOCOL (optional): Either "wss" (default) or "ws".
 */

// connect calls setup
function connect (id, protocol, port) {
    if (!window.WebSocket) {
        alert("Your browser doesn't support websockets. This website might not work properly.");
        return;
    }
    if (!protocol) {
        protocol = "wss";
    }
    if (!port) {
        port = (protocol == "ws" ? 80 : 443);
    }
    wsurl = `${protocol}://${location.hostname}:${port}`;
    if (socket && socket.readyState == 1) {
        socket.close();
    } else {
        socket = undefined;
    }
    socket = new WebSocket(wsurl);
    socket.onmessage = function (event) {
        update(JSON.parse(event.data));
    };
    socket.onopen = function (event) {
        socket.send(`id:${id}`);
    };
}

async function getvalue (obj) {
    let value = attr(obj, "value");
    if (obj.type === "radio" ||
        obj.type === "checkbox") {
        if (!obj.checked) {
            value = "";
        }
    } else if (obj.type === "file") {
        value = obj.files;
        if (value.length > 0) {
            for (let file of value) {
                if (!file.content) {
                    let arrayBuffer = await new Response(file).arrayBuffer();
                    file.content = btoa(
                        new Uint8Array(arrayBuffer).reduce(function(data, byte) {
                            return data + String.fromCharCode(byte);
                        }, "")
                    );
                }
            }
        }
    }
    return value;
}

/**
 * rr - re-render
 * Generate the url parameter list and send it over the server throught the socket.
 * Any element that has a "name" attribute will be put in the parameter list.
 * OBJS (optional) (variadic): Make OBJ.action be the only one of its kind in the parameter list.
 *
 * Usually, you would want to call rr as rr() or rr(this) from something like onclick="rr(this)", but it can be called as rr({action:"custom-name",value:"custom-value"}...) for custom results.
 */
async function rr (...objs) {
    let elements = document.querySelectorAll("[name]"),
        data = {},
        actions = document.querySelectorAll("[action]");
    for (let element of elements) {
        let name = attr(element, "name");
        if (element.disable) {
            continue;
        }
        let value = await getvalue(element);
        if (typeof data[name] === "undefined") {
            // set value
            data[name] = value;
        } else if (data[name] && data[name].constructor === Array) {
            // append to array
            data[name].push(value);
        } else {
            // become array
            data[name] = [data[name], value];
        }
    }
    for (let element of actions) {
        let name = attr(element, "action");
        if (!data[name]) {
            data[name] = "";
        }
    }

    // generate params based on new and previous data
    let changed = keepchanged(previousdata, data);
    for (let obj of objs) {     // always ensure the data of objs gets sent
        let name = attr(obj, "action") ||
            attr(obj, "name");
        changed[name] = data[name] = await getvalue(obj);
    }
    let params = jsonfiles(changed)?
        "post:" + JSON.stringify(changed):
        "?" + querystring(changed);
    previousdata = data;
    if (!socket || socket.readyState != 1) {
        reconnect();
    } else {
        socket.send(params);
    }
    return true;
}

function reconnect () {
    let xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
            let newhtml = document.open("text/html", "replace");
            newhtml.write(this.responseText);
            newhtml.close();
            clean(document);
            Array.from(document.getElementsByTagName("script"), function (scripttag) {
                eval(scripttag.text);
            });
        }
    };
    xhttp.open("POST", location.pathname, true);
    xhttp.setRequestHeader("content-type", "application/json");
    xhttp.send(JSON.stringify(previousdata));
}

File.prototype.content = "";
File.prototype.toString = function () {
    return this.content;
}
FileList.prototype.toString = function () {
    return Array.from(this, function (file) {
        return file? file.toString() : "";
    }).join(",");
}
function keepchanged (olddata, newdata) {
    let updated = {};
    for (let name of Object.keys(newdata)) {
        if (typeof olddata[name] === "undefined" ||
            (olddata[name]? olddata[name].toString() : "")
            !==
            (newdata[name]? newdata[name].toString() : "")) {
            updated[name] = newdata[name];
        }
    }
    return updated;
}

function jsonfiles (data) {
    let containsfiles = false;
    for (let key of Object.keys(data)) {
        if (data[key]? data[key].constructor === FileList : null) {
            containsfiles = true;
            data[key] = Array.from(data[key], function (file) {
                return {content: file.content,
                        name: file.name,
                        type: file.type}
            });
        } else if (data[key] && data[key].name) {
            containsfiles = true;
        }
    }
    return containsfiles;
}

function querystring (data) {
    return Object.keys(data).map(function (name) {
        if (typeof data[name] === "object") {
            return (data[name]? data[name].map(function (value) {
                return `${encodeURIComponent(name)}=${encodeURIComponent(value)}`;
            }) : []).join("&");
        } else {
            return `${encodeURIComponent(name)}=${encodeURIComponent(data[name])}`;
        }
    }).join("&");
}

/**
 * clean
 * Remove all pure whitespace text nodes.
 */
function clean (node) {
    for(let n = node.childNodes.length - 1; n >= 0; --n) {
        let child = node.childNodes[n];
        if (child.nodeType == 8 || (child.nodeType == 3 && !/\S/.test(child.nodeValue))) {
            node.removeChild(child);
        } else if (child.nodeType == 1 && child.tagName !== "PRE") {
            clean(child);
        }
    }
}
document.addEventListener("DOMContentLoaded", function () { clean(document); });

