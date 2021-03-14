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

// TODO(colin): proper union type
export type Action = {
  type: string;
  [k: string]: any;
};

export function updateItemCache(itemType: any, data: any): Action {
  return {
    type: UPDATE_CACHE,
    cacheType: "item",
    itemType: itemType,
    data: data,
  };
}

export function displayItem(itemType: string, idx: any): Action {
  return {
    type: DISPLAY,
    displayType: "item",
    itemType: itemType,
    idx: idx,
  };
}

export function updateTableCache(itemType: any, data: any[]): Action {
  return {
    type: UPDATE_CACHE,
    cacheType: "table",
    itemType: itemType,
    data: data,
  };
}

export function displayTable(
  itemType: any,
  range: any[],
  resourcePath: any,
  numberFieldName: any
): Action {
  return {
    type: DISPLAY,
    displayType: "table",
    resourcePath: resourcePath,
    itemType: itemType,
    idx: range,
    numberFieldName: numberFieldName,
  };
}

export function editItem(
  itemType: any,
  idx: any,
  field: any,
  value: any
): Action {
  return {
    type: UPDATE_ITEM,
    itemType: itemType,
    idx: idx,
    field: field,
    value: value,
  };
}

export function invalidateItemCache(itemType: any, idx: any): Action {
  return {
    type: INVALIDATE_CACHE,
    cacheType: "item",
    itemType: itemType,
    idx: idx,
  };
}

export function setEditMode(val: boolean): Action {
  return {
    type: EDIT_MODE,
    value: val,
  };
}

export function clearEdits(itemType: any, idx: any): Action {
  return {
    type: CLEAR_UNSAVED,
    cacheType: "item",
    itemType: itemType,
    idx: idx,
  };
}

export function hamburgerVisibility(visible: boolean): Action {
  return {
    type: HAMBURGER_VISIBILITY,
    visible: visible,
  };
}

export function setUserAndAuth(user: any, auth: any): Action {
  return {
    type: USER,
    name: user,
    auth: auth,
  };
}

export function searchVisibility(visible: boolean): Action {
  return {
    type: SEARCH_VISIBILITY,
    visible: visible,
  };
}

export function searchData(data: any): Action {
  return {
    type: SEARCH_DATA,
    data: data,
  };
}

export function displaySearch(): Action {
  // TODO: include the notion of what was searched for.
  return {
    type: DISPLAY,
    displayType: "search",
  };
}

export function mapVisibility(value: boolean): Action {
  return {
    type: MAP_VISIBILITY,
    visible: value,
  };
}

export function setPlasmidMapData(id: any, data: any): Action {
  return {
    type: MAP_DATA,
    data: data,
    id: id,
  };
}

export function doSearchAndRedirect(
  searchTerm: any,
  includeSequence: any,
  person: any,
  types: any
): void {
  let url = `/search?term=${searchTerm}&seq=${includeSequence ? 1 : 0}`;
  if (person) {
    url += `&person=${person}`;
  }
  url += `&types=${JSON.stringify(types)}`;
  (window.location as any) = url;
}
