"use strict";
const app = Elm.Main.init({
  node: document.getElementById("root")
});

app.ports.requestGetSvgBoundingBox.subscribe(function (id) {
  try {
    const svg = document.getElementById(id);
    const bbox = svg.getBBox();
    app.ports.receiveGotSvgBoundingBox.send({
      id: id,
      boundingBox: bbox
    });
  } catch {}
});
