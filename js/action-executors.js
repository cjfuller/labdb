let $ = require("jquery");

let Actions = require("./actions.js");

let _injected = {};
export function injectDispatch(disp) {
    _injected.dispatch = (action) => {console.log(action); disp(action);};
}

function dispatch(action) {
    return _injected.dispatch(action);
}

const FETCH_BASE = "/api/v1/m";

function fetchItem(resourcePath) {
    return $.ajax({
        url: `${FETCH_BASE}${resourcePath}`,
        method: "GET",
    });
}

function fetchItemList(resourceType, start, end) {
    return $.ajax({
        url: `${FETCH_BASE}/${resourceType}/list?start=${start}&end=${end}`,
        method: "GET",
    });
}

export function maybeFetchThenDisplay(fetchType, resource) {
    if (fetchType === "item") {
        return fetchItem(resource.resourcePath).done((data) => {
            dispatch(Actions.updateItemCache(resource.type, data));
            dispatch(Actions.displayItem(resource.type, resource.id));
        });
        //TODO: error handling
    } else if (fetchType === "table") {
        return fetchItemList(resource.type, resource.id[0], resource.id[1]).done((data) => {
            dispatch(Actions.updateTableCache(resource.type, data));
            dispatch(Actions.displayTable(resource.type, resource.id, resource.resourcePath));
        });
        //TODO: error handling
    }

    throw `Unknown fetch type ${fetchType}`;
}

export function editField(resourceType, idx, fieldName, value) {
    let action = Actions.editItem(resourceType, idx, fieldName, value);
    dispatch(action);
}

export function saveEdits(resource, edits) {
    $.ajax({
        url: `${FETCH_BASE}${resource.resourcePath}`,
        method: "PUT",
        data: edits,
        dataType: "json",
    }).then(() => {
        clearEdits(resource);
    });
}

export function clearEdits(resource) {
    dispatch(Actions.invalidateItemCache(resource.type, resource.id));
    maybeFetchThenDisplay("item", resource).then(() => {
        dispatch(Actions.clearEdits(resource.type, resource.id));
        dispatch(Actions.setEditMode(false));
    });
}
