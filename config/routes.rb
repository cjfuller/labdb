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


  get '/login', :to => redirect('/auth/google'), :as => :login
  get '/logout', :to => 'sessions#destroy', :as => :logout

  match '/auth/:provider/callback', :to => 'sessions#create'
  match '/auth/failure', :to => 'sessions#failure'

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

  resources :plasmids, &common_actions

  resources :oligos, &common_actions

  resources :antibodies, &common_actions

  resources :bacteria do
    collection do
      get 'search'
      post 'search'
    end
    member do
      get 'export'
      get 'next'
      get 'previous'
      post 'create_from_plasmid'
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

  root :to => "static#index"
    

end
