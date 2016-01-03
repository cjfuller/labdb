export const UPDATE_CACHE = "UPDATE_CACHE";
export const DISPLAY = "DISPLAY";
export const EDIT_MODE = "EDIT_MODE";
export const INVALIDATE_CACHE = "INVALIDATE_CACHE";
export const CLEAR_UNSAVED = "CLEAR_UNSAVED";
export const UPDATE_ITEM = "UPDATE_ITEM";
export const ROUTING_UPDATE = "ROUTING";

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
