if (socket == undefined) var socket;
if (wsurl == undefined) var wsurl;
if (previousData == undefined) var previousData = {};
if (textNodes == undefined) var textNodes = {};
if (issrId == undefined) var issrId;

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
 */
function connect (id) {
    issrId = id;
    if (!window.WebSocket) {
        alert("Your browser doesn't support websockets. This website might not work properly.");
        return;
    }
    secure = (location.protocol == "https:"? "s" : "");
    wsurl = `ws${secure}://${location.hostname}:${location.port}/-issr`;
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
    let changed = keepChanged(previousData, data);
    for (let obj of objs) {     // always ensure the data of objs gets sent
        let action = attr(obj, "action"),
            name = attr(obj, "name");
        if (action) {
            changed[action] = data[action] = await getvalue(obj) || "T";
        }
        if (name) {
            changed[name] = data[name] = await getvalue(obj);
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

        }
    }
}

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

