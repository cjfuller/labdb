export const UPDATE_CACHE = "UPDATE_CACHE";
export const DISPLAY = "DISPLAY";
export const EDIT_MODE = "EDIT_MODE";
export const INVALIDATE_CACHE = "INVALIDATE_CACHE";
export const CLEAR_UNSAVED = "CLEAR_UNSAVED";
export const UPDATE_ITEM = "UPDATE_ITEM";
export const ROUTING_UPDATE = "ROUTING";
export const HAMBURGER_VISIBILITY = "HAMBURGER";
export const USER = "USER";
export const SEARCH_VISIBILITY = "SEARCH";
export const SEARCH_DATA = "SEARCH_DATA";
export const MAP_VISIBILITY = "MAP_VISIBILITY";
export const MAP_DATA = "MAP_DATA";

export function updateItemCache(itemType, data) {
    return {
        type: UPDATE_CACHE,
        cacheType: "item",
        itemType: itemType,
        data: data,
    };
}

export function displayItem(itemType, idx) {
    return {
        type: DISPLAY,
        displayType: "item",
        itemType: itemType,
        idx: idx,
    };
}


export function updateTableCache(itemType, data) {
    return {
        type: UPDATE_CACHE,
        cacheType: "table",
        itemType: itemType,
        data: data,
    };
}

export function displayTable(itemType, range, resourcePath,
                             numberFieldName) {
    return {
        type: DISPLAY,
        displayType: "table",
        resourcePath: resourcePath,
        itemType: itemType,
        idx: range,
        numberFieldName: numberFieldName,
    };
}

export function editItem(itemType, idx, field, value) {
    return {
        type: UPDATE_ITEM,
        itemType: itemType,
        idx: idx,
        field: field,
        value: value,
    };
}

export function invalidateItemCache(itemType, idx) {
    return {
        type: INVALIDATE_CACHE,
        cacheType: "item",
        itemType: itemType,
        idx: idx,
    };
}

export function setEditMode(val) {
    return {
        type: EDIT_MODE,
        value: val,
    };
}

export function clearEdits(itemType, idx) {
    return {
        type: CLEAR_UNSAVED,
        cacheType: "item",
        itemType: itemType,
        idx: idx,
    };
}

export function hamburgerVisibility(visible) {
    return {
        type: HAMBURGER_VISIBILITY,
        visible: visible,
    };
}

export function setUserAndAuth(user, auth) {
    return {
        type: USER,
        name: user,
        auth: auth,
    };
}

export function searchVisibility(visible) {
    return {
        type: SEARCH_VISIBILITY,
        visible: visible,
    };
}

export function searchData(data) {
    return {
        type: SEARCH_DATA,
        data: data,
    };
}

export function displaySearch() {
    // TODO: include the notion of what was searched for.
    return {
        type: DISPLAY,
        displayType: "search",
    };
}

export function mapVisibility(value) {
    return {
        type: MAP_VISIBILITY,
        visible: value,
    };
}

export function setPlasmidMapData(id, data) {
    return {
        type: MAP_DATA,
        data: data,
        id: id,
    };
};

export function doSearchAndRedirect(searchTerm, includeSequence, person, types) {
    let url = `/search?term=${searchTerm}&seq=${includeSequence ? 1 : 0}`;
    if (person) {
        url += `&person=${person}`
    }
    url += `&types=${JSON.stringify(types)}`
    window.location = url;
}
