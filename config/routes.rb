Labdb::Application.routes.draw do
  root to: "static#index"

  post "/logout", to: "api#logout"

  match "/auth/:provider/callback", to: "sessions#create", via: [:get, :post]
  match "/auth/failure", to: "sessions#failure", via: [:get, :post]

  get "/quick_search", to: "quick_search#do_quick_search"
  get "/by_name/:name", to: "application#show_by_name"

  common_actions = proc do
    collection do
      get "search"
      post "search"
    end
    member do
      get "export"
      get "next"
      get "previous"
    end
  end

  resources :users, &common_actions

  resources :plasmids, as: "plasmid", &common_actions

  resources :oligos, &common_actions

  resources :antibodies, &common_actions

  resources :bacteria do
    collection do
      get "search"
      post "search"
      post "create_from_plasmid"
    end
    member do
      get "export"
      get "next"
      get "previous"
    end
  end

  resources :yeaststrains, &common_actions

  resources :samples, &common_actions

  resources :lines do
    collection do
      get "search"
      post "search"
    end
    member do
      get "export"
      get "next"
      get "previous"
      put "update_number"
    end
  end

  resources :seq_libs, &common_actions
  resources :rnai_clones, &common_actions

  get "/search", to: "application#search"
  post "/search_result", to: "application#search_result"

  scope "/api/v1/m" do
    get "/plasmid_map/:id", to: "api#plasmid_map"
    post "/:model/new", to: "api#new"
    get "/:model/list", to: "api#list"
    post "/:model/:id/copy", to: "api#copy"
    get "/:model/:id", to: "api#fetch"
    put "/:model/:id", to: "api#update"
    delete "/:model/:id", to: "api#delete"
  end

  scope "/api/v1" do
    post "/import", to: "api#import"
    post "/plasmid_map", to: "api#plasmid_map"
  end

  scope "/api" do
    post "/verify", to: "api#verify"
  end
end
