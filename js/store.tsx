import React from "react";
import ReactDOM from "react-dom";
import { connect, Provider } from "react-redux";
import { createStore } from "redux";
import _ from "underscore";
import $ from "jquery";
import icepick from "icepick";

import * as Actions from "./actions";
import * as ActionExecutors from "./action-executors";
import LandingPage from "./landing";
import Page from "./page";

const Data = (window as any)._labdbPrefetch;

type StateKeys =
  | "editMode"
  | "itemCache"
  | "tableCache"
  | "displayedResource"
  | "invertTable"
  | "mapVisible"
  | "scrollTop"
  | "updateUrl"
  | "unsavedChanges";

type State = { [k in StateKeys]: any };

const initialState: State = {
  editMode: false,
  itemCache: {},
  tableCache: {},
  displayedResource: {},
  invertTable: true,
  mapVisible: false,
  scrollTop: true,
  updateUrl: false,
  unsavedChanges: {},
};

function extractTableRows(
  state: State,
  itemType: string | number,
  start: number,
  end: number
) {
  if (start > end) {
    const temp = start;
    start = end;
    end = temp;
  }
  const inRange = (k: number) => {
    return k >= start && k <= end;
  };
  const keys = Object.keys(state.tableCache[itemType])
    .map((i) => parseInt(i))
    .filter(inRange);
  keys.sort((a, b) => a - b);
  if (state.invertTable) {
    keys.reverse();
  }
  return keys.map((k) => state.tableCache[itemType][k]);
}

function invalidateCacheHandler(
  state: State,
  action: { itemType: string | number; idx: string | number }
) {
  const cache = { ...state.itemCache };
  cache[action.itemType] = { ...cache[action.itemType] };
  delete cache[action.itemType][action.idx];
  return { ...state, itemCache: cache };
}

function clearUnsavedChanges(
  state: State,
  action: { itemType: string | number; idx: string | number }
) {
  const unsavedChanges = { ...state.unsavedChanges };
  unsavedChanges[action.itemType] = { ...unsavedChanges[action.itemType] };
  delete unsavedChanges[action.itemType][action.idx];
  return { ...state, unsavedChanges: unsavedChanges };
}

function displayHandler(
  state: State,
  action: {
    displayType: string;
    itemType: string | number;
    resourcePath: any;
    idx: string | number | any[];
    numberFieldName: any;
  }
) {
  if (action.displayType === "table") {
    return {
      ...state,
      displayedResource: {
        type: "table",
        itemType: action.itemType,
        resourcePath: action.resourcePath,
        data: extractTableRows(
          state,
          action.itemType,
          (action.idx as any[])[0],
          (action.idx as any[])[1]
        ),
        idx: action.idx,
        numberFieldName: action.numberFieldName,
      },
      updateUrl: true,
    };
  } else if (action.displayType === "item") {
    return {
      ...state,
      displayedResource: {
        type: "item",
        resourcePath:
          state.itemCache[action.itemType][action.idx as any].resourcePath,
        itemType: action.itemType,
        data: state.itemCache[action.itemType][action.idx as any],
        idx: action.idx,
      },
      updateUrl: true,
    };
  } else if (action.displayType === "search") {
    return {
      ...state,
      displayedResource: {
        type: "search",
      },
      updateUrl: true,
    };
  }

  throw new Error(`Unknown displayType: ${action.displayType}`);
}

function routingHandler(state: State, action: { resource: any }) {
  const fullResource = { ...action.resource };
  if (action.resource.type === "item") {
    fullResource.data =
      state.itemCache[fullResource.itemType][fullResource.idx];
  } else if (action.resource.type === "table") {
    fullResource.data = extractTableRows(
      state,
      fullResource.itemType,
      fullResource.idx[0],
      fullResource.idx[1]
    );
  } else if (action.resource.type === "search") {
    // TODO: handle the notion of what was searched for.
  } else {
    throw new Error(`Unknown resource type ${action.resource.type}`);
  }
  return {
    ...state,
    displayedResource: fullResource,
    updateUrl: false,
  };
}

function cacheHandler(
  state: { tableCache: any; itemCache: any },
  action: {
    cacheType: string;
    itemType: string | number;
    data: { map: (arg0: (item: any) => void) => void; id: string | number };
  }
) {
  if (action.cacheType === "table") {
    const newCache = { ...state.tableCache };
    const newItemCache = { ...newCache[action.itemType] };
    action.data.map((item: { id: string | number }) => {
      newItemCache[item.id] = item;
    });
    newCache[action.itemType] = newItemCache;
    return { ...state, tableCache: newCache };
  } else if (action.cacheType === "item") {
    const newCache = { ...state.itemCache };
    const newItemCache = { ...newCache[action.itemType] };
    newItemCache[action.data.id] = action.data;
    newCache[action.itemType] = newItemCache;
    return { ...state, itemCache: newCache };
  }

  throw new Error(`Unknown cache type ${action.cacheType}`);
}

function updateHandler(
  state: { unsavedChanges: any },
  action: {
    itemType: string | number;
    idx: string | number;
    field: string | number;
    value: any;
  }
) {
  const newChanges = { ...state.unsavedChanges };
  if (newChanges[action.itemType]) {
    newChanges[action.itemType] = { ...newChanges[action.itemType] };
  } else {
    newChanges[action.itemType] = {};
  }

  if (newChanges[action.itemType][action.idx]) {
    newChanges[action.itemType][action.idx] = {
      ...newChanges[action.itemType][action.idx],
    };
  } else {
    newChanges[action.itemType][action.idx] = {};
  }

  newChanges[action.itemType][action.idx][action.field] = action.value;
  return { ...state, unsavedChanges: newChanges };
}

function editHandler(state: any, action: { value: any }) {
  return { ...state, editMode: action.value };
}

function hamburgerHandler(state: any, action: { visible: any }) {
  return { ...state, showHamburger: action.visible };
}

function userHandler(state: any, action: { name: any; auth: any }) {
  return { ...state, user: { name: action.name, auth: action.auth } };
}

function searchBarHandler(state: any, action: { visible: any }) {
  return { ...state, showSearch: action.visible };
}

function searchDataHandler(state: any, action: { data: any }) {
  return { ...state, searchResults: action.data };
}

function mapVisibilityHandler(state: any, action: { visible: any }) {
  return { ...state, mapVisible: action.visible };
}

function mapDataHandler(state: any, action: { id: any; data: any }) {
  const { id, data } = action;
  return icepick.assocIn(
    state,
    ["itemCache", "plasmid", id, "plasmid_map"],
    data
  );
}

const actionHandlers: { [k: string]: any } = {};

actionHandlers[Actions.INVALIDATE_CACHE] = invalidateCacheHandler;
actionHandlers[Actions.CLEAR_UNSAVED] = clearUnsavedChanges;
actionHandlers[Actions.DISPLAY] = displayHandler;
actionHandlers[Actions.ROUTING_UPDATE] = routingHandler;
actionHandlers[Actions.UPDATE_CACHE] = cacheHandler;
actionHandlers[Actions.UPDATE_ITEM] = updateHandler;
actionHandlers[Actions.EDIT_MODE] = editHandler;
actionHandlers[Actions.HAMBURGER_VISIBILITY] = hamburgerHandler;
actionHandlers[Actions.USER] = userHandler;
actionHandlers[Actions.SEARCH_VISIBILITY] = searchBarHandler;
actionHandlers[Actions.SEARCH_DATA] = searchDataHandler;
actionHandlers[Actions.MAP_VISIBILITY] = mapVisibilityHandler;
actionHandlers[Actions.MAP_DATA] = mapDataHandler;

function stateReducer(state: any, action: { type: string | number }) {
  if (typeof state === "undefined") {
    return initialState;
  }
  const handler = actionHandlers[action.type];
  return handler(state, action);
}

const store = createStore(stateReducer);

function currentResourceURI() {
  const resource = store.getState().displayedResource;
  if (resource.type === "table") {
    return (
      `${resource.resourcePath}?start=${resource.idx[0]}` +
      `&end=${resource.idx[1]}`
    );
  } else if (resource.type === "item") {
    return resource.data.resourcePath;
  }
  return null;
}

function dataForCurrentResource() {
  const resource = store.getState().displayedResource;
  if (resource.type === "search") {
    return {
      type: "search",
      items: store.getState().searchResults,
    };
  }
  if (resource.type === "table") {
    return {
      type: "collection",
      items: resource.data,
      numberFieldName: resource.numberFieldName,
      itemType: resource.itemType,
    };
  } else {
    return resource.data;
  }
}

function unsavedForCurrentResource() {
  const resource = store.getState().displayedResource;
  if (resource.type === "item") {
    return (
      (store.getState().unsavedChanges[resource.itemType] || {})[
        resource.idx
      ] || {}
    );
  }
  return {};
}

function loadPrefetchedData() {
  if (
    (window as any)._labdbSearchResults &&
    (window as any)._labdbSearchResults.length > 0
  ) {
    store.dispatch(Actions.searchData((window as any)._labdbSearchResults));
    (window as any)._labdbSearchResults.forEach((result: { type: any }) => {
      store.dispatch(Actions.updateTableCache(result.type, [result]));
    });
    store.dispatch(Actions.displaySearch());
  } else if (
    (window as any)._labdbSearchResults.length === 0 &&
    (window as any)._labdbPrefetch.length === 0
  ) {
    store.dispatch(Actions.displaySearch());
  } else if (Data.type === "collection") {
    store.dispatch(Actions.updateTableCache(Data.objectType, Data.items));
    const start = _.min(Data.items.map((i: { id: any }) => i.id));
    const end = _.max(Data.items.map((i: { id: any }) => i.id));
    store.dispatch(
      Actions.displayTable(
        Data.objectType,
        [start, end],
        Data.resourcePath,
        Data.numberFieldName
      )
    );
  } else {
    store.dispatch(Actions.updateItemCache(Data.type, Data));
    store.dispatch(Actions.displayItem(Data.type, Data.id));
  }
  store.dispatch(
    Actions.setUserAndAuth(
      (window as any)._labdbUser,
      (window as any)._labdbAuth
    )
  );
}

type Props = {
  dispatch: Function;
  displayedResource: any;
  editMode: boolean;
  mapVisible: boolean;
  scrollTop: boolean;
  updateUrl: boolean;
};

class Application extends React.Component<Props> {
  componentDidMount() {
    window.onpopstate = (event: { state: any }) => {
      if (event.state) {
        this.props.dispatch({
          type: Actions.ROUTING_UPDATE,
          resource: event.state,
        });
      }
    };
    window.history.replaceState(
      this.compactResource(this.props.displayedResource),
      (window as any).title,
      window.location.pathname + window.location.search
    );
  }

  componentDidUpdate() {
    if (this.props.scrollTop) {
      window.scrollTo(0, 0);
    }
    if (this.props.updateUrl) {
      const newState = this.compactResource(this.props.displayedResource);
      if (!_.isEqual(newState, window.history.state)) {
        window.history.pushState(newState, "", currentResourceURI());
      }
    }
  }

  compactResource(res: any) {
    return { ...res, data: null };
  }

  render() {
    return (
      <Page
        data={dataForCurrentResource()}
        dispatch={this.props.dispatch}
        editMode={this.props.editMode}
        mapVisible={this.props.mapVisible}
        showHamburger={store.getState().showHamburger}
        showSearch={store.getState().showSearch}
        unsavedChanges={unsavedForCurrentResource()}
        user={store.getState().user}
      />
    );
  }
}

const App: any = connect((s) => s)(Application);

window.onload = () => {
  (window as any).getCSRFToken = function () {
    return $("meta[name='csrf-token']").attr("content");
  };

  $.ajaxPrefilter((options, originalOptions, jqXHR) => {
    jqXHR.setRequestHeader("X-CSRF-Token", (window as any).getCSRFToken());
  });
  if (document.getElementById("landing")) {
    ReactDOM.render(<LandingPage />, document.getElementById("landing"));
  } else {
    (window as any).gapi.load("auth2", () => (window as any).gapi.auth2.init());
    loadPrefetchedData();
    ActionExecutors.injectDispatch(store.dispatch);
    ReactDOM.render(
      <Provider store={store as any}>
        <App />
      </Provider>,
      document.getElementById("body")
    );
  }
};
