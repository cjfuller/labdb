#--
# Copyright (C) 2013  Colin J. Fuller
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#++

Labdb::Application.routes.draw do

  root to: "static#index"

  post '/logout', to: 'api#logout'

  match '/auth/:provider/callback', to: 'sessions#create', via: [:get, :post]
  match '/auth/failure', to: 'sessions#failure', via: [:get, :post]

  get '/quick_search', to: 'quick_search#do_quick_search'

  common_actions = Proc.new do
    collection do
      get 'search'
      post 'search'
    end
    member do
      get 'export'
      get 'next'
      get 'previous'
    end
  end

  resources :users, except: [:show] do
    member do
      put 'toggle_auth_read'
      put 'toggle_auth_write'
      put 'toggle_auth_admin'
    end
  end

  resources :plasmids, as: 'plasmid', &common_actions

  resources :oligos, &common_actions

  resources :antibodies, &common_actions

  resources :bacteria do
    collection do
      get 'search'
      post 'search'
      post 'create_from_plasmid'
    end
    member do
      get 'export'
      get 'next'
      get 'previous'
    end
  end

  resources :yeaststrains, &common_actions

  resources :samples, &common_actions

  resources :lines do
    collection do
      get 'search'
      post 'search'
    end
    member do
      get 'export'
      get 'next'
      get 'previous'
      put 'update_number'
    end
  end

  scope '/api/v1/m' do
    post '/:model/new', to: "api#new"
    get '/:model/list', to: "api#list"
    post '/:model/:id/copy', to: "api#copy"
    get '/:model/:id', to: "api#fetch"
    put '/:model/:id', to: "api#update"
    delete '/:model/:id', to: "api#delete"
  end

  scope '/api' do
    post '/verify', to: "api#verify"
  end

end
