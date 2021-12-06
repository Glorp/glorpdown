
const send = (verb, url, str, cont) => {
  const xhr = new XMLHttpRequest();
  xhr.open(verb, url, true);
  xhr.setRequestHeader("Content-Type", "text/plain");
  xhr.onreadystatechange = () => {
    if (xhr.readyState === XMLHttpRequest.DONE) {
      if (xhr.status === 200) {
        cont(xhr.responseText);
      } else {
        console.log({ status: xhr.status, statusText: xhr.statusText, text: xhr.responseText });
      }
    } 
  };
  xhr.send(str);
};

const listFiles = () =>
  send("GET", "notes", null, res => {
    const list = document.getElementById("list");
    list.innerHTML = "";
    list.appendChild(filelist(JSON.parse(res)));
  });

window.addEventListener("load", function () {
  document.getElementById("add-stuff").onclick = () => {
    document.getElementById("stuff").appendChild(editor("???", "<>\\\n<beep boop>\n\ntrallalala\n\n<code>\nmlep")._result);
  };
  document.getElementById("refresh-list").onclick = listFiles;
});

const element = (name, ...classes) => {
  const el = document.createElement(name);
  classes.forEach(c => el.classList.add(c));
  return el;
};

const filelist = files => {
  const select = element("select");
  select.setAttribute("name", "files");
  select.setAttribute("size", "" + Math.max(2, files.length));
  files.forEach(f => {
    const opt = element("option");
    opt.setAttribute("value", f);
    opt.innerText = f;
    opt.ondblclick = () => send(
      "GET",
      "notes/" + f,
      null,
      str => {
        const js = JSON.parse(str);
        document.getElementById("stuff").appendChild(editor(js.filename, js.content)._result);
    });
    select.appendChild(opt);
  });
  return select;
};

const elem = arr => {
  const resObj = {};
  const halp = arr => {
    const res = document.createElement(arr[0]);
    var idx = 1;
    if (typeof arr[idx] === "string") {
      resObj[arr[idx]] = res;
      idx++;
    }
    for (const a of arr[idx]) {
      const k = a[0];
      const v = a[1];
      if (k.startsWith("a:")) {
        res.setAttribute(k.substring(2), a[1]);
      } else if (k === "value") {
        res.value = a[1];
      } else if (k === "class") {
        for (var idy = 1; idy < a.length; idy++) {
          res.classList.add(a[idy]);
        }
      } else if (k === "edit") {
        res.setAttribute("contenteditable", "true");
        res.addEventListener("input", () => a[1](() => res.innerText), false);
      }

    }
    for (idx++; idx < arr.length; idx++) {
      const x = arr[idx];
      if (typeof x === "string") {
        res.append(x);
      } else if (Array.isArray(x)) {
        res.appendChild(halp(x));
      } else {
        res.appendChild(x);
      }
    }
    return res;
  }
  const res = halp(arr);
  resObj._result = res;
  return resObj;
};

const elemReplace = (obj, key, elem) => {
  const old = obj[key];
  old.insertAdjacentElement('afterend', elem);
  obj[key] = elem;
  old.remove();
};


const editor = (filename, str) => {
  const res = elem(
    [
      "div",
      [["class", "post-edit-container"]],
      [
        "div",
        [["class", "panel"]],
        ["input", "filename", []],
        ["br", []],
        ["button", "previewButton", [], "Preview"],
        ["button", "saveButton", [], "💾"],
        ["button", "closeButton", [], "X"],
      ],
      [
        "div",
        [["class", "edit-preview-container"]],
        ["pre", "edit", [["class", "post-editor"], ["edit", f => console.log(f())]], str],
        ["div", "preview", [["class", "post-preview"]]]
      ]
    ]);
  res.filename.value = filename;
  res.saveButton.onclick = () => send("POST", "notes/" + res.filename.value, res.edit.innerText, console.log);
  res.previewButton.onclick = () => send("POST", "preview", res.edit.innerText, str => res.preview.innerHTML = str);
  res.closeButton.onclick = () => res._result.remove();
  return res;
};


const soon = (() => {

  const timed = new Map();

  const timer = () => {
    for (const k of timed.keys()) {
      const t = timed.get(k);
      if (t.time === 0) {
        console.log(k);
        timed.delete(k);
      } else {
        t.time = t.time - 1;
      }
    }
    setTimeout(timer, 100);
  };
  
  timer();
  return str => timed.set(str, { time: 20 });
})();
