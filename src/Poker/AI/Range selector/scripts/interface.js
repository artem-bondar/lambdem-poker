function createGrid(width, height) {
  var field = "";
  var text  = "AKQJT98765432";
  var handName = "";
  var array = []

  for (var i = 0; i < height; i++) {
    field += "<tr>";
    array.push([]);
    for (var j = 0; j < width; j++) {
      array[i].push(false);
      if (i < j)
        handName = text[i] + text[j] + "s"
      else if (i > j)
        handName = text[j] + text[i] + "o"
      else
        handName = text[i] + text[j]
      field += "<td class='primitive' id='" + indexToID(i, j) +
        "' onclick='handleClick(event)'>" + handName + "</td>";
    }
    field += "</tr>";
  }

  selectorGrid.innerHTML = field;
  field.range = array
}

function loadSelectorGrid() {
  calculateFieldPosition();
  calculateZoom();
  createGrid(field.width, field.height);
  changePrimitiveSize(field.primitiveSize);
}

function changePrimitiveSize(size) {
  var tiles = document.getElementsByClassName("primitive");
  for (var i = 0; i < tiles.length; i++) {
    tiles[i].style.width = size + "px";
    tiles[i].style.height = size + "px";
  }
}
