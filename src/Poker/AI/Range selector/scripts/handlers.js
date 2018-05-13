function handleResize(event) {
    calculateFieldPosition();
    calculateZoom();
    changePrimitiveSize(field.primitiveSize);
}

function handleClick(event) {
    if (field.partitioning && event.target.style.backgroundColor != colors[field.color]) {
        var coords = IDtoIndex(event.target.id);
        var index = tabProperties.selectedPuzzle.value;
        field.figure.array[coords.i][coords.j] = field.color;
        event.target.style.backgroundColor = colors[field.color];
    }
}
