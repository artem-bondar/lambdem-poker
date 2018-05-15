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
    range: undefined
};

// Loading of grid
loadSelectorGrid();

// Initialization of events listeners
window.addEventListener('resize', handleResize);
