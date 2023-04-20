"use strict";
const app = Elm.Main.init({
  node: document.getElementById("root")
});

app.ports.changeSvgTextContent.subscribe(function (val) {
  try {
    const [newText, id] = val;
    const svg = document.getElementById(id);
    svg.innerHTML = newText;
    const bbox = svg.getBBox();
    app.ports.receiveChangedSvgTextContent.send({
      updatedText: newText,
      id: id,
      boundingBox: {
        width: bbox.width,
        height: bbox.height,
        x: bbox.x,
        y: bbox.y
      }
    });
  } catch {
    // TODO: send error to Elm
  }
});
