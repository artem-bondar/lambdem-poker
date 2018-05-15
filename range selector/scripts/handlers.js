function handleResize(event) {
    calculateFieldPosition();
    calculateZoom();
    changePrimitiveSize(field.primitiveSize);
}

function handleClick(event) {
    var coords = IDtoIndex(event.target.id);

    if (event.target.style.backgroundColor == "white") {
        event.target.style.backgroundColor = "steelblue";
        field.range[coords.i][coords.j] = true;
    }
    else {
        event.target.style.backgroundColor = "white";
        field.range[coords.i][coords.j] = false;
    }
    printRange();
}
