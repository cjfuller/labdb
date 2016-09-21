const icepick = require("icepick");
const React = require("react");
const ReactDOM = require("react-dom");
const ReactRedux = require("react-redux");
const Redux = require("redux");
const _ = require("underscore");
const $ = require("jquery");

const Actions = require("./actions.js");
const ActionExecutors = require("./action-executors.js");
const LandingPage = require("./landing.jsx");
const Page = require("./page.jsx");

const Data = window._labdbPrefetch;
const Provider = ReactRedux.Provider;

const initialState = {
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

function extractTableRows(state, itemType, start, end) {
    if (start > end) {
        const temp = start;
        start = end;
        end = temp;
    }
    const inRange = (k) => {
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

function invalidateCacheHandler(state, action) {
    const cache = {...state.itemCache};
    cache[action.itemType] = {...cache[action.itemType]};
    delete cache[action.itemType][action.idx];
    return {...state, itemCache: cache};
}

function clearUnsavedChanges(state, action) {
    const unsavedChanges = {...state.unsavedChanges};
    unsavedChanges[action.itemType] = {...unsavedChanges[action.itemType]};
    delete unsavedChanges[action.itemType][action.idx];
    return {...state, unsavedChanges: unsavedChanges};
}

function displayHandler(state, action) {
    if (action.displayType === "table") {
        return {
            ...state,
            displayedResource: {
                type: "table",
                itemType: action.itemType,
                resourcePath: action.resourcePath,
                data: extractTableRows(
                    state, action.itemType, action.idx[0], action.idx[1]),
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
                resourcePath: state.itemCache[action.itemType][action.idx]
                    .resourcePath,
                itemType: action.itemType,
                data: state.itemCache[action.itemType][action.idx],
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

function routingHandler(state, action) {
    const fullResource = {...action.resource};
    if (action.resource.type === "item") {
        fullResource.data = (
            state.itemCache[fullResource.itemType][fullResource.idx]);
    } else if (action.resource.type === "table") {
        fullResource.data = extractTableRows(
            state, fullResource.itemType, fullResource.idx[0],
            fullResource.idx[1]);
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

function cacheHandler(state, action) {
    if (action.cacheType === "table") {
        const newCache = {...state.tableCache};
        const newItemCache = {...newCache[action.itemType]};
        action.data.map((item) => {
            newItemCache[item.id] = item;
        });
        newCache[action.itemType] = newItemCache;
        return {...state, tableCache: newCache};
    } else if (action.cacheType === "item") {
        const newCache = {...state.itemCache};
        const newItemCache = {...newCache[action.itemType]};
        newItemCache[action.data.id] = action.data;
        newCache[action.itemType] = newItemCache;
        return {...state, itemCache: newCache};
    }

    throw new Error(`Unknown cache type ${action.cacheType}`);
}

function updateHandler(state, action) {
    const newChanges = {...state.unsavedChanges};
    if (newChanges[action.itemType]) {
        newChanges[action.itemType] = {...newChanges[action.itemType]};
    } else {
        newChanges[action.itemType] = {};
    }

    if (newChanges[action.itemType][action.idx]) {
        newChanges[action.itemType][action.idx] = {
            ...newChanges[action.itemType][action.idx]};

    } else {
        newChanges[action.itemType][action.idx] = {};
    }

    newChanges[action.itemType][action.idx][action.field] = action.value;
    return {...state, unsavedChanges: newChanges};
}

function editHandler(state, action) {
    return {...state, editMode: action.value};
}

function hamburgerHandler(state, action) {
    return {...state, showHamburger: action.visible};
}

function userHandler(state, action) {
    return {...state, user: {name: action.name, auth: action.auth}};
}

function searchBarHandler(state, action) {
    return {...state, showSearch: action.visible};
}

function searchDataHandler(state, action) {
    return {...state, searchResults: action.data};
}

function mapVisibilityHandler(state, action) {
    return {...state, mapVisible: action.visible};
}

function mapDataHandler(state, action) {
    const {id, data} = action;
    return icepick.assocIn(
        state, ["itemCache", "plasmid", id, "plasmid_map"], data);
}

const actionHandlers = {};

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

function stateReducer(state, action) {
    if (typeof state === 'undefined') {
        return initialState;
    }
    const handler = actionHandlers[action.type];
    return handler(state, action);
}

const store = Redux.createStore(stateReducer);

function currentResourceURI() {
    const resource = store.getState().displayedResource;
    if (resource.type === "table") {
        return (
            `${resource.resourcePath}?start=${resource.idx[0]}`
            + `&end=${resource.idx[1]}`);
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
            (store.getState().unsavedChanges[resource.itemType] ||
             {})[resource.idx] || {});
    }
    return {};
}

function loadPrefetchedData() {
    if (window._labdbSearchResults && window._labdbSearchResults.length > 0) {
        store.dispatch(
            Actions.searchData(window._labdbSearchResults));
        window._labdbSearchResults.forEach((result) => {
            store.dispatch(
                Actions.updateTableCache(
                    result.type, [result]));
        });
        store.dispatch(Actions.displaySearch());
    } else if (window._labdbSearchResults.length === 0 &&
               window._labdbPrefetch.length === 0) {
        store.dispatch(Actions.displaySearch([]));
        store.dispatch(Actions.displaySearch());
    } else if (Data.type === "collection") {
        store.dispatch(
            Actions.updateTableCache(
                Data.objectType, Data.items));
        const start = _.min(Data.items.map((i) => i.id));
        const end = _.max(Data.items.map((i) => i.id));
        store.dispatch(
            Actions.displayTable(
                Data.objectType, [start, end], Data.resourcePath,
                Data.numberFieldName));
    } else {
        store.dispatch(
            Actions.updateItemCache(
                Data.type, Data));
        store.dispatch(
            Actions.displayItem(
                Data.type, Data.id));
    }
    store.dispatch(
        Actions.setUserAndAuth(window._labdbUser, window._labdbAuth));
}


const Application = React.createClass({
    propTypes: {
        dispatch: React.PropTypes.func,
        displayedResource: React.PropTypes.any,
        editMode: React.PropTypes.bool,
        mapVisible: React.PropTypes.bool,
        scrollTop: React.PropTypes.bool,
        updateUrl: React.PropTypes.bool,
    },
    componentDidMount: function() {
        window.onpopstate = (event) => {
            if (event.state) {
                this.props.dispatch({
                    type: Actions.ROUTING_UPDATE,
                    resource: event.state,
                });
            }
        };
        window.history.replaceState(
            this.compactResource(this.props.displayedResource),
            window.title,
            window.location.pathname + window.location.search);
    },

    componentDidUpdate: function() {
        if (this.props.scrollTop) {
            window.scrollTo(0, 0);
        }
        if (this.props.updateUrl) {
            const newState = this.compactResource(this.props.displayedResource);
            if (!_.isEqual(newState, window.history.state)) {
                window.history.pushState(
                    newState,
                    null,
                    currentResourceURI());
            }
        }
    },

    compactResource: function(res) {
        return {...res, data: null};
    },

    render: function() {
        return <Page
            data={dataForCurrentResource()}
            dispatch={this.props.dispatch}
            editMode={this.props.editMode}
            mapVisible={this.props.mapVisible}
            showHamburger={store.getState().showHamburger}
            showSearch={store.getState().showSearch}
            unsavedChanges={unsavedForCurrentResource()}
            user={store.getState().user}
        />;
    },
});

const App = ReactRedux.connect((s) => s)(Application);

window.onload = () => {
    window.getCSRFToken = function() {
        return $("meta[name='csrf-token']").attr('content');
    };

    $.ajaxPrefilter((options, originalOptions, jqXHR) => {
        jqXHR.setRequestHeader('X-CSRF-Token', window.getCSRFToken());
    });
    if (document.getElementById("landing")) {
        ReactDOM.render(
            <LandingPage />,
            document.getElementById("landing"));
    } else {
        window.gapi.load('auth2', () => window.gapi.auth2.init());
        loadPrefetchedData();
        ActionExecutors.injectDispatch(store.dispatch);
        ReactDOM.render(
            <Provider store={store} >
                <App />
            </Provider>,
            document.getElementById("body"));
    }
};

module.exports = App;
