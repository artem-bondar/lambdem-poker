function createGrid(width, height) {
  var html = "";
  var text = "AKQJT98765432";
  var handName = "";
  var array = []

  for (var i = 0; i < height; i++) {
    html += "<tr>";
    array.push([]);
    for (var j = 0; j < width; j++) {
      array[i].push(false);
      if (i < j)
        handName = text[i] + text[j] + "s"
      else if (i > j)
        handName = text[j] + text[i] + "o"
      else
        handName = text[i] + text[j]
      html += "<td class='primitive' id='" + indexToID(i, j) +
        "' onclick='handleClick(event)'>" + handName + "</td>";
    }
    html += "</tr>";
  }

  selectorGrid.innerHTML = html;
  field.range = array

  var tiles = document.getElementsByClassName("primitive");
  for (var i = 0; i < tiles.length; i++)
    tiles[i].style.backgroundColor = "white"
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

function printRange() {
  var suitedRange = "[";
  var pairedRange = "[";
  var offsuitedRange = "[";

  var cardRank =
    ["Deuce",
      "Three",
      "Four",
      "Five",
      "Six",
      "Seven",
      "Eight",
      "Nine",
      "Ten",
      "Jack",
      "Queen",
      "King",
      "Ace"].reverse()

  for (var i = 0; i < field.height; i++)
    for (var j = 0; j < field.width; j++) {
      if (field.range[i][j]) {
        if (i < j)
          suitedRange += "(" + cardRank[i] + ", " + cardRank[j] + "), "
        else if (i > j)
          offsuitedRange += "(" + cardRank[j] + ", " + cardRank[i] + "), "
        else
          pairedRange += cardRank[i] + ", "
      }
    }

  document.getElementById("text-field").innerHTML = `
  range :: CardRange<br>
  range = CardRange<br>
  &nbsp;{ suitedRange = ` + suitedRange + `]<br>
  &nbsp;&nbsp;, pairedRange = ` + pairedRange + `]<br>
  &nbsp;&nbsp;, offsuitedRange = ` + offsuitedRange + "]<br>&nbsp;}";
}
