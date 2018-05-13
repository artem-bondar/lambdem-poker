var selectorView = document.getElementById('selector-view');
var selectorGrid = document.getElementById('selector-grid');

var field = {
    pageX: undefined,
    pageY: undefined,
    margin: undefined,
    figure: undefined,
    primitiveSize: undefined,
    width: 13,
    height: 13,
    range: []
};

/* Initialization of events listeners */

// For whole window
window.addEventListener('resize', handleResize);

// Loading of grid
loadSelectorGrid();