import $ from "jquery";

import * as Actions from "./actions";
import type { Action } from "./actions";

let _injected: { dispatch: (a: Action) => void } = { dispatch: () => null };

export function injectDispatch(disp: (a: Action) => void) {
  _injected.dispatch = (action: Action) => {
    console.log(action);
    disp(action);
  };
}

function dispatch(action: Action) {
  return _injected.dispatch(action);
}

const FETCH_BASE = "/api/v1";
const MODEL_FETCH_BASE = FETCH_BASE + "/m";

function fetchItem(resourcePath: any) {
  return $.ajax({
    url: `${MODEL_FETCH_BASE}${resourcePath}`,
    method: "GET",
  });
}

function fetchItemList(resourceType: any, start: any, sortOrder: string) {
  return $.ajax({
    url: `${MODEL_FETCH_BASE}/${resourceType}/list?start=${start}&sort_order=${sortOrder}`,
    method: "GET",
  });
}

export function maybeFetchThenDisplay(
  fetchType: string,
  resource: any,
  sortOrder = "DESC"
) {
  if (fetchType === "item") {
    return fetchItem(resource.resourcePath).then((data) => {
      dispatch(Actions.updateItemCache(resource.type, data));
      dispatch(Actions.displayItem(resource.type, resource.id));
      if (resource.type === "plasmid" && !resource.plasmid_map) {
        loadPlasmidMapData(data);
      }
    });
    //TODO: error handling
  } else if (fetchType === "table") {
    return fetchItemList(resource.type, resource.start, sortOrder).then(
      (data) => {
        window.location.href = data.resourcePath;
        // TODO: don't require a reload
        // dispatch(Actions.updateTableCache(resource.type, data.items));
        // dispatch(Actions.displayTable(resource.type, [resource.start, resource.start - 99], resource.resourcePath));
      }
    );
    //TODO: error handling
  }

  throw `Unknown fetch type ${fetchType}`;
}

export function editField(
  resourceType: any,
  idx: any,
  fieldName: any,
  value: any
) {
  let action = Actions.editItem(resourceType, idx, fieldName, value);
  dispatch(action);
}

export function saveEdits(
  resource: Pick<any, "type" | "resourcePath" | "id">,
  edits: any
) {
  $.ajax({
    url: `${MODEL_FETCH_BASE}${resource.resourcePath}`,
    method: "PUT",
    data: JSON.stringify(edits),
    dataType: "json",
    contentType: "application/json; charset=utf-8",
  }).then(() => {
    clearEdits(resource);
  });
}

export function clearEdits(
  resource: Pick<any, "type" | "resourcePath" | "id">
) {
  dispatch(Actions.invalidateItemCache(resource.type, resource.id));
  maybeFetchThenDisplay("item", resource).then(() => {
    dispatch(Actions.clearEdits(resource.type, resource.id));
    dispatch(Actions.setEditMode(false));
  });
}

export function newItem(itemType: any) {
  return $.ajax({
    url: `${MODEL_FETCH_BASE}/${itemType}/new`,
    method: "POST",
  }).then((data) => {
    return maybeFetchThenDisplay("item", data).then(() => {
      dispatch(Actions.setEditMode(true));
      return Promise.resolve();
    });
  });
}

export function copyItem(resource: { type: any; id: any }) {
  return $.ajax({
    url: `${MODEL_FETCH_BASE}/${resource.type}/${resource.id}/copy`,
    method: "POST",
  }).then((data) => {
    return maybeFetchThenDisplay("item", data).then(() => {
      dispatch(Actions.setEditMode(true));
      return Promise.resolve();
    });
  });
}

export function deleteItem(resource: { name: any; type: any; id: any }) {
  if (
    window.confirm(
      `Are you sure you want to delete ${resource.name}?  (This cannot be undone.)`
    )
  ) {
    return $.ajax({
      url: `${MODEL_FETCH_BASE}/${resource.type}/${resource.id}`,
      method: "DELETE",
    }).then(() => {
      // TODO: delete item from cache
      return maybeFetchThenDisplay("table", {
        type: resource.type,
        start: resource.id,
      });
    });
  }
}

export function loadPlasmidMapData(resource: any): Promise<any> {
  return ($.ajax({
    url: `${FETCH_BASE}/plasmid_map`,
    method: "POST",
    contentType: "text/plain",
    data: resource.fieldData.sequence,
  }).then((data) => {
    dispatch(Actions.setPlasmidMapData(resource.id, data));
  }) as unknown) as Promise<any>;
}

export function showPlasmidMap(resource: { plasmid_map: any; id: any }) {
  let promise = Promise.resolve();
  if (!resource.plasmid_map) {
    promise = loadPlasmidMapData(resource);
  }
  promise.then(() => {
    dispatch(Actions.displayItem("plasmid", resource.id));
    dispatch(Actions.mapVisibility(true));
  });
}

export function strainFromPlasmid(resource: { id: any }) {
  return $.ajax({
    url: `/bacteria/create_from_plasmid?plasmid_id=${resource.id}`,
    method: "POST",
  }).then((data) => {
    maybeFetchThenDisplay("item", {
      type: "bacterium",
      resourcePath: `/bacteria/${data}`,
      id: data,
    });
  });
}
