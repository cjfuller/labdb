defmodule Labdb.Router do
  use Labdb.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug :fetch_session
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  scope "/", Labdb do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :home

    get "/:type/:id", PageController, :get_item
    get "/:type", PageController, :get_index

  end

  scope "/api", Labdb do
    pipe_through :api

    post "/verify", APIController, :verify
    scope "/v1" do
        get "/m/plasmid_map/:id", APIController, :plasmid_map
        get "/m/:type/list", APIController, :model_list
        post "/m/:type/new", APIController, :model_new
        get "/m/:type/:id", APIController, :model_get
        put "/m/:type/:id", APIController, :model_put
        post "/m/:type/:id", APIController, :model_put
        delete "/m/:type/:id", APIController, :model_delete
        post "/m/:type/:id/copy", APIController, :model_copy
    end
  end
end
