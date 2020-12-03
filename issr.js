/**
 * descendant
 * Return the node that is the indexed descendant of document.
 * INDEXES: a list of indexes.
 */
function descendant (indexes) {
    let node = document;
    for (let i of indexes) {
        node = node.childNodes[i];
    }
    return node;
}

/**
 * update
 * Modify the dom to be up to date with the server.
 * INSTRUCTIONS: An array containing objects like such:
 * - ["mod", [indexes], [key, value]...]: modifiy attributes or properties
 * - ["delete", indexes...]: delete node
 * - ["insert", [indexes], text, position, html-string]: insert html-string as a text or html node  either "before", "after", or "prepend" indexes.
 * - ["cookie", cookies...]: set cookies
 * - ["session", [key, value]...]: set session variables
 * - ["redirect", target]: redirect to target
 */
function update (instructions) {
    for (let instruction of instructions) {
        switch (instruction[0]) {
        case "mod": {
            let node = descendant(instruction[1]);
            for (let i = 2; i < instruction.length; ++i) {
                node[instruction[i][0]] = instruction[i][1];
                if (instruction[i][0].indexOf("HTML") < 0) {
                    if ("" == instruction[i][1]) {
                        node.removeAttribute([instruction[i][0]]);
                    } else {
                        node.setAttribute([instruction[i][0]], instruction[i][1]);
                    }
                }
            }
            break;}
        case "delete": {
            descendant(instruction.slice(1)).remove();
            break;}
        case "insert": {
            let node = instruction[2]?
                document.createTextNode(instruction[4]):
                document.createElement('nil');
            descendant(instruction[1])[instruction[3]](node);
            if (!instruction[2]) {
                node.outerHTML = instruction[4];
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
        }
    }
}

let socket, wsurl;
/**
 * setup
 * Connect to the websocket on the server.
 * ID: the unique server generated id for identifying with the websocket.
 * PORT (optional): The port to connect to. 443 (default).
 * PROTOCOL (optional): Either "wss" or "ws" (default).
 */
function setup (id, port, protocol) {
    if (!window.WebSocket) {
        alert("Your browser doesn't support websockets. This website might not work properly.");
        return;
    }
    if (!protocol) {
        protocol = "ws";
    }
    if (!port) {
        port = 443;
    }
    wsurl = `${protocol}://${location.hostname}:${port}`;
    socket = new WebSocket(wsurl);
    socket.onmessage = function (event) {
        update(JSON.parse(event.data));
    };
    socket.onopen = function (event) {
        socket.send(`id:${id}`);
    };
}

async function getvalue (obj) {
    let value = obj.value || (obj.value == ""? "" : obj.getAttribute("value"));
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
    return value
}

let previousdata = {};
/**
 * rr - re-render
 * Generate the url parameter list and send it over the server throught the socket.
 * Any element that has a "name" attribute will be put in the parameter list.
 * OBJS (optional) (variadic): Make OBJ.name be the only one of its kind in the parameter list.
 *
 * Usually, you would want to call rr as rr() or rr(this) from something like onclick="rr(this)", but it can be called as rr({name:"custom-name",value:"custom-value"}...) for custom results.
 */
async function rr (...objs) {
    let elements = document.querySelectorAll("[name]"),
        data = {},
        taken = function (name) {
            for (let obj of objs) {
                if (name === (obj.name || (obj.value == ""? "" : obj.getAttribute("name")))) {
                    return true;
                }
            }
            return false;
        };
    for (let element of elements) {
        let name = element.name || element.getAttribute("name");
        if (element.disabled || taken(name)) {
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
    // generate params based on new and previous data
    let changed = keepchanged(previousdata, data);
    for (let obj of objs) {     // always ensure the data of objs gets sent
        let name = (obj.name) || obj.getAttribute("name");
        changed[name] = data[name] = await getvalue(obj);
    }
    let params = jsonfiles(changed)?
        "post:" + JSON.stringify(changed):
        "?" + querystring(changed);
    previousdata = data;
    if (!socket) {
        console.error("Socket is not set up yet; try calling setup before calling rr.");
        return false;
    } else if (socket.readyState > 1) {
        socket = new WebSocket(wsurl);
        socket.onmessage = function (event) {
            update(JSON.parse(event.data));
        };
        jsonfiles(previousdata);
        let loc = location.href.toString(), host = location.host,
            state = {uri: loc.substring(loc.indexOf(host) + host.length, loc.indexOf("?")),
                     page: "<!doctype html>" + document.documentElement.outerHTML,
                     params: previousdata,
                     query: loc.substring(loc.indexOf("?") + 1)};
        socket.onopen = function (event) {
            socket.send("http:" + JSON.stringify(state));
            socket.send(params);
        };
    } else {
        socket.send(params);
    }
    return true;
}

File.prototype.content = "";
File.prototype.toString = function () {
    return this.content;
}
FileList.prototype.toString = function () {
    return Array.from(this, function (file) {
        return file.toString();
    }).join(",");
}
function keepchanged (olddata, newdata) {
    let updated = {};
    for (let name of Object.keys(newdata)) {
        if (typeof olddata[name] === "undefined" ||
            olddata[name].toString() !== newdata[name].toString()) {
            updated[name] = newdata[name];
        }
    }
    return updated;
}

function jsonfiles (data) {
    let containsfiles = false;
    for (let key of Object.keys(data)) {
        if (data[key].constructor === FileList) {
            containsfiles = true;
            data[key] = Array.from(data[key], function (file) {
                return {content: file.content,
                        name: file.name,
                        type: file.type}
            });
        }
    }
    return containsfiles;
}

function querystring (data) {
    return Object.keys(data).map(function (name) {
        if (typeof data[name] === "object") {
            return data[name].map(function (value) {
                return `${encodeURIComponent(name)}=${encodeURIComponent(value)}`;
            }).join("&");
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

