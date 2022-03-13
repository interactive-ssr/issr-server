if (wsurl == undefined) {
    var socket,
        wsurl = `ws${location.protocol == 'https:'? 's' : ''}://${location.hostname}:${location.port}/-issr`,
        previousData = {},
        registeredActions = new Set(),
        textNodes = {},
        issrId,
        drrs = {},
        delay = 800
}

File.prototype.content = ""
File.prototype.toString = () => this.content
FileList.prototype.toString = () => Array.from(this, file => file? file.toString() : "").join(",")

/**
 * attr
 * Return the ATTRIBUTE member, attribute, or undefined of OBJ
 */
attr = (obj, attribute) => obj?
    (obj[attribute] || obj[attribute] == "" ?
     obj[attribute]:
     (obj.getAttribute && obj.getAttribute(attribute))):
    undefined

/**
 * elementById
 * like document.getElementById, but also consults textNodes
 */
elementById = id => document.getElementById(id) || textNodes[id]

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
update = instructions => {
    for (let instruction of instructions) {
        switch (instruction[0]) {
        case "mod":
            let node = elementById(instruction[1])
            if (node) {
                for (let i = 2; i < instruction.length; ++i) {
                    if (instruction[i][0].toString().startsWith("on")) {
                        // set event value
                        node[instruction[i][0]] = Function("event", instruction[i][1])
                    } else {
                        // set value
                        node[instruction[i][0]] = instruction[i][1]
                    }
                    // set attribute value
                    if (!instruction[i][0].includes("textContent")
                        && !instruction[i][0].includes("HTML")) {
                        if ("" == instruction[i][1]) {
                            node.removeAttribute([instruction[i][0]])
                        } else {
                            node.setAttribute(instruction[i][0], instruction[i][1])
                        }
                    }
                }
            }
            break
        case "delete":
            for (let i = 1; i < instruction.length; ++i) {
                let node = elementById(instruction[i])
                freeTextNodes(node)
                node.remove()
            }
            break
        case "insert":
            let parent = elementById(instruction[1])
            if (parent) {
                let node = document.createElement('nil')
                parent[instruction[2]](node)
                node.outerHTML = instruction[3]
                trackTextNodes(parent)
            }
            break
        case "cookie":
            fetch("-issr/cookie", {
                method:"POST",
                headers: {
                    "content-type": "application/x-www-form-urlencoded"
                },
                body: "id=" + issrId
            })
            break
        case "redirect":
            document.location = instruction[1]
            break
        case "reconnect":
            reconnect()
          break
        case "script":
          if (allowScript)
            eval(instruction[1])
          break
        case "error":
            let newhtml = open("about:blank")
                .document.open("text/html","replace")
            newhtml.write(instruction[1])
            newhtml.close()
            break
        }
    }
}

/**
 * connect
 * Connect to the websocket on the server.
 * ID: the unique server generated id for identifying with the websocket.
 */
connect = id => {
    issrId = id
    if (!window.WebSocket) {
        alert("Your browser doesn't support websockets. This website might not work properly.")
        return
    }
    socket = new WebSocket(wsurl)
    socket.onmessage =
        event => update(JSON.parse(event.data))
    socket.onopen =
        event => socket.send("id:" + id)
}


getValue = async obj => {
    let value = attr(obj, "value")
    if (obj.type === "radio" ||
        obj.type === "checkbox") {
        if (!obj.checked) {
            value = ""
        }
    } else if (obj.type === "file") {
        value = obj.files
        if (value.length > 0) {
            for (let file of value) {
                if (!file.content) {
                    let arrayBuffer = await new Response(file).arrayBuffer()
                    file.content = btoa(
                        new Uint8Array(arrayBuffer).reduce((data, byte) =>
                            data + String.fromCharCode(byte), "")
                    )
                }
            }
        }
    }
    return value
}

computeArgs = async () => {
  let elements = document.querySelectorAll("[name]"),
      data = {}
  for (let element of elements) {
    let name = attr(element, "name")
    if (element.disable || element.value == null) {
      continue
    }
    let value = await getValue(element)
    if (typeof data[name] === "undefined") {
      // set value
      data[name] = value
    } else if (data[name] && data[name].constructor === Array) {
      // append to array
      data[name].push(value)
    } else {
      // become array
      data[name] = [data[name], value]
    }
    if (element.type === "radio"
        && data[name].constructor === Array) {
      data[name] = data[name]
        .filter((value) => value)[0]
        || ""
    }
  }
  return data
}

/**
 * rr - re-render
 * Generate the url parameter list and send it over the server throught the socket.
 * Any element that has a "name" attribute will be put in the parameter list.
 * OBJS (optional): Make OBJ.action be the only one of its kind in the parameter list.
 *
 * Usually, you would want to call rr as rr() or rr(this) from something like onclick="rr(this)", but it can be called as rr({action:"custom-name",value:"custom-value"}...) for custom results.
 */
rr = async (actionElement) => {
  // generate params based on new and previous data
    let data = await computeArgs(),
        changed = keepChanged(previousData, data),
        action = {}
    if (actionElement)
        action[attr(actionElement, "action")] = await getValue(actionElement) || "T"
    jsonFiles(data)
    previousData = data
    if (!socket || socket.readyState != 1) {
        reconnect()
    } else {
        socket.send(JSON.stringify([action, changed]))
    }
    return true
}

drr = (id, delay = this.delay) => {
    let debounce = (func, delay) => {
        let timeout
        return (...args) => {
            clearTimeout(timeout)
            timeout = setTimeout(() => func(args), delay)
        }
    }, debouncer = drrs[id]
    // do not add newline after return
    return (debouncer?
            debouncer:
            drrs[id] = debounce(objs => {
                rr(...objs)
                delete drrs[id]
            }, delay))
}

reconnect = async () => {
    let response = await fetch("/-issr/reconnect", {
        method: "POST",
        headers: {
            "accept": "application/html",
            "content-type": "application/json",
            "issr-uri": location.pathname
        },
        body: JSON.stringify(previousData)
    }), text = await response.text(),
        newhtml = document.open("text/html", "replace")
    if (300 <= response.status && response.status <= 399) {
        location.href = response.headers.get("Location")
    }
    newhtml.write(text)
    newhtml.close()
    trackTextNodes(document)
}

keepChanged = (olddata, newdata) => {
    let updated = {},
        key = (data, name)=> data[name]? data[name].toString() : ""
    for (let name of Object.keys(newdata)) {
        if (olddata[name] == undefined
            || key(olddata, name) !== key(newdata, name)) {
            updated[name] = newdata[name]
        }
    }
    return updated
}

jsonFiles = data => {
    for (let key of Object.keys(data)) {
        if (data[key]? data[key].constructor === FileList : null) {
            data[key] = Array.from(data[key], file =>
                [true, file.content, file.name, file.type]
            )
        }
    }
}

elseTraverse = (node, condition, action) => {
    for(let child of node.childNodes) {
        if (condition(child)) {
            action(child)
        } else if (child.childNodes) {
            elseTraverse(child, condition, action)
        }
    }
}

/**
 * trackTextNodes
 * Add text nodes to textNodes using the data in the parent TN tag.
 */
trackTextNodes = node =>
elseTraverse(
    node,
    // if
    child => child.nodeType == Node.ELEMENT_NODE
        && child.tagName == "TN"
        && child.id,
    // then
    child => {
        let textNode = child.childNodes[0]
        textNode.id = child.id
        textNodes[child.id] = textNode
        child.parentNode.replaceChild(textNode, child)
    })

/**
 * freeTextNodes
 * Garbage collect text nodes that are children of NODE removing them from
 * textNodes.
 */
freeTextNodes = async node =>
elseTraverse(
    node,
    // if
    child => [Node.TEXT_NODE, Node.COMMENT_NODE]
        .includes(child.nodeType)
        && child.id,
    // then
    child => delete textNodes[child.id]
)

document.addEventListener("DOMContentLoaded", () => { trackTextNodes(document) })
