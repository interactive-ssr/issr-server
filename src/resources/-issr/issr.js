if (wsurl == undefined) {
    var socket;
    var wsurl;
    var previousData = {};
    var textNodes = {};
    var issrId;
    var drrs = {};
    var delay = 800;
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
 * elementById
 * like document.getElementById, but also consults textNodes
 */
function elementById (id) {
    return document.getElementById(id) || textNodes[id];
}

/**
 * update
 * Modify the dom to be up to date with the server.
 * INSTRUCTIONS: An array containing objects like such:
 * - ["mod", id, [key, value]...]: modifiy attributes or properties
 * - ["delete", ids...]: delete node (child-index is optional)
 * - ["insert", id, text, position, html-string]: insert html-string as a text or html node  either "before", "after", or "prepend" indexes.
 * - ["cookie"]: fetch cookies
 * - ["redirect", target]: redirect to target
 * - ["reconnect"]: reset the websocket and send the previousData.
 * - ["error", message]: display server error to console.error
 */
function update (instructions) {
    for (let instruction of instructions) {
        switch (instruction[0]) {
        case "mod": {
            let node = elementById(instruction[1]);
            if (node) {
                for (let i = 2; i < instruction.length; ++i) {
                    if (instruction[i][0].toString().startsWith("on")) {
                        // set event value
                        node[instruction[i][0]] = Function("event", instruction[i][1]);
                    } else {
                        // set value
                        node[instruction[i][0]] = instruction[i][1];
                    }
                    // set attribute value
                    if (!instruction[i][0].includes("textContent")
                        && !instruction[i][0].includes("HTML")) {
                        if ("" == instruction[i][1]) {
                            node.removeAttribute([instruction[i][0]]);
                        } else {
                            node.setAttribute(instruction[i][0], instruction[i][1]);
                        }
                    }
                }
            }
            break;}
        case "delete": {
            for (let i = 1; i < instruction.length; ++i) {
                let node = elementById(instruction[i]);
                freeTextNodes(node);
                node.remove();
            }
            break;}
        case "insert": {
            let parent = elementById(instruction[1]);
            if (parent) {
                let node = document.createElement('nil');
                parent[instruction[2]](node);
                node.outerHTML = instruction[3];
                trackTextNodes(parent);
            }
            break;}
        case "cookie": {
            fetch("-issr/cookie", {
                method:"POST",
                headers: {
                    "content-type": "application/x-www-form-urlencoded"
                },
                body: "id=" + issrId
            });
            break;}
        case "redirect": {
            document.location = instruction[1];
            break;}
        case "reconnect": {
            reconnect();
            break;}
        case "error": {
            let newhtml = open("about:blank")
                .document.open("text/html","replace");
            newhtml.write(instruction[1]);
            newhtml.close()
            break;}
        }
    }
}

/**
 * connect
 * Connect to the websocket on the server.
 * ID: the unique server generated id for identifying with the websocket.
 */
function connect (id) {
    issrId = id;
    if (!window.WebSocket) {
        alert("Your browser doesn't support websockets. This website might not work properly.");
        return;
    }
    secure = (location.protocol == "https:"? "s" : "");
    wsurl = `ws${secure}://${location.hostname}:${location.port}/-issr`;
    socket = new WebSocket(wsurl);
    socket.onmessage =
        event => update(JSON.parse(event.data));
    socket.onopen =
        event => socket.send("id:" + id);
}

async function getValue (obj) {
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
        let value = await getValue(element);
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
        if (element.type === "radio"
            && data[name].constructor === Array) {
            data[name] = data[name]
                .filter((value) => value)[0]
                || "";
        }
    }
    for (let element of actions) {
        let name = attr(element, "action");
        if (!data[name]) {
            data[name] = "";
        }
    }

    // generate params based on new and previous data
    jsonFiles(data);
    let changed = keepChanged(previousData, data);
    for (let obj of objs) {     // always ensure the data of objs gets sent
        let action = attr(obj, "action"),
            name = attr(obj, "name");
        if (action) {
            changed[action] = data[action] = await getValue(obj) || "T";
        }
        if (name) {
            changed[name] = data[name] = await getValue(obj);
        }
    }
    let params = JSON.stringify(changed);
    previousData = data;
    if (!socket || socket.readyState != 1) {
        reconnect();
    } else {
        socket.send(params);
    }
    return true;
}

function drr (id, delay = this.delay) {
    let debounce = (func, delay) => {
        let timeout;
        return (...args) => {
            clearTimeout(timeout);
            timeout = setTimeout(() => func(args), delay);
        };
    }, debouncer = drrs[id];
    // do not add newline after return
    return (debouncer?
            debouncer:
            drrs[id] = debounce(objs => {
                rr(...objs);
                delete drrs[id];
            }, delay));
}

async function reconnect () {
    let response = await fetch("/-issr/reconnect", {
        method: "POST",
        headers: {
            "accept": "application/html",
            "content-type": "application/json",
            "issr-uri": location.pathname
        },
        body: JSON.stringify(previousData)
    }), text = await response.text(),
        newhtml = document.open("text/html", "replace");
    if (300 <= response.status && response.status <= 399) {
        location.href = response.headers.get("Location");
    }
    newhtml.write(text);
    newhtml.close();
    trackTextNodes(document);
    // Array.from(document.getElementsByTagName("script"), script => {
    //     if (!script.type || script.type.includes("javascript")) {
    //         eval(script.text);
    //     }
    // });
}

function keepChanged (olddata, newdata) {
    let updated = {};
    for (let name of Object.keys(newdata)) {
        if (olddata[name] == undefined ||
            (olddata[name]? olddata[name].toString() : "")
            !==
            (newdata[name]? newdata[name].toString() : "")) {
            updated[name] = newdata[name];
        }
    }
    return updated;
}

function jsonFiles (data) {
    for (let key of Object.keys(data)) {
        if (data[key]? data[key].constructor === FileList : null) {
            data[key] = Array.from(data[key], function (file) {
                return [true, file.content, file.name, file.type];
            });
        }
    }
}

function elseTraverse (node, condition, action) {
    for(let child of node.childNodes) {
        if (condition(child)) {
            action(child);
        } else if (child.childNodes) {
            elseTraverse(child, condition, action);
        }
    }
}

/**
 * trackTextNodes
 * Add text nodes to textNodes using the data in the parent TN tag.
 */
function trackTextNodes (node) {
    elseTraverse(
        node,
        // if
        child => child.nodeType == Node.ELEMENT_NODE
            && child.tagName == "TN"
            && child.id,
        // then
        child => {
            let textNode = child.childNodes[0];
            textNode.id = child.id;
            textNodes[child.id] = textNode;
            child.parentNode.replaceChild(textNode, child);
        });
}

/**
 * freeTextNodes
 * Garbage collect text nodes that are children of NODE removing them from
 * textNodes.
 */
async function freeTextNodes (node) {
    elseTraverse(
        node,
        // if
        child => [Node.TEXT_NODE, Node.COMMENT_NODE]
            .includes(child.nodeType)
            && child.id,
        // then
        child => delete textNodes[child.id]
    );
}
document.addEventListener("DOMContentLoaded", () => { trackTextNodes(document) });