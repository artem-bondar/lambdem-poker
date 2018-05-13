function indexToID(i, j) {
    return ((i < 10) ? "0" : "") + i +
        ((j < 10) ? "0" : "") + j;
}

function IDtoIndex(id) {
    return { i: parseInt(id.slice(0, 2)), j: parseInt(id.slice(2)) };
}

function calculateFieldPosition() {
    field.pageX = selectorView.offsetLeft;
    field.pageY = selectorView.offsetTop;
    field.margin = window.getComputedStyle(selectorGrid).getPropertyValue("margin").slice(0, -2);
}

function calculateZoom() {
    var zoomToWidthSize = Math.round((selectorView.offsetWidth - 2 * field.margin) / field.width);
    var zoomToHeightSize = Math.round((selectorView.offsetHeight - 2 * field.margin) / field.height);
    field.primitiveSize = Math.min(zoomToWidthSize, zoomToHeightSize);
}
