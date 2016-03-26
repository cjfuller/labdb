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

function fetchItemList(resourceType, start, sortOrder) {
    return $.ajax({
        url: `${FETCH_BASE}/${resourceType}/list?start=${start}&sort_order=${sortOrder}`,
        method: "GET",
    });
}

export function maybeFetchThenDisplay(fetchType, resource, sortOrder="DESC") {
    if (fetchType === "item") {
        return fetchItem(resource.resourcePath).then((data) => {
            dispatch(Actions.updateItemCache(resource.type, data));
            dispatch(Actions.displayItem(resource.type, resource.id));
            if (resource.type === "plasmid" && !resource.plasmid_map) {
                loadPlasmidMapData(resource);
            }
        });
        //TODO: error handling
    } else if (fetchType === "table") {
        return fetchItemList(resource.type, resource.start, sortOrder).then((data) => {
            window.location.href = data.resourcePath;
            // TODO: don't require a reload
            // dispatch(Actions.updateTableCache(resource.type, data.items));
            // dispatch(Actions.displayTable(resource.type, [resource.start, resource.start - 99], resource.resourcePath));
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

export function newItem(itemType) {
    return $.ajax({
        url: `${FETCH_BASE}/${itemType}/new`,
        method: "POST"
    }).then((data) => {
        return maybeFetchThenDisplay('item', data).then(() => {
            dispatch(Actions.setEditMode(true));
            return Promise.resolve();
        });
    });
}

export function copyItem(resource) {
    return $.ajax({
        url: `${FETCH_BASE}/${resource.type}/${resource.id}/copy`,
        method: "POST"
    }).then((data) => {
        return maybeFetchThenDisplay('item', data).then(() => {
            dispatch(Actions.setEditMode(true));
            return Promise.resolve();
        });
    });
}

export function deleteItem(resource) {
    if (window.confirm(`Are you sure you want to delete ${resource.name}?  (This cannot be undone.)`)) {
        return $.ajax({
            url: `${FETCH_BASE}/${resource.type}/${resource.id}`,
            method: "DELETE",
        }).then(() => {
            // TODO: delete item from cache
            return maybeFetchThenDisplay('table', {type: resource.type, start: resource.id});
        });
    }
}

export function loadPlasmidMapData(resource) {
    return $.ajax({
        url: `${FETCH_BASE}/plasmid_map/${resource.id}`,
        method: "GET",
    }).then((data) => {
        dispatch(Actions.setPlasmidMapData(resource.id, data));
    });
}

export function showPlasmidMap(resource) {
    let promise = Promise.resolve();
    if (!resource.plasmid_map) {
        promise = loadPlasmidMapData(resource);
    }
    promise.then(() => {
        dispatch(Actions.displayItem("plasmid", resource.id));
        dispatch(Actions.mapVisibility(true));
    });
}
