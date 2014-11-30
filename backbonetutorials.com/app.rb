#!/usr/bin/env ruby

require 'sinatra'
require 'rethinkdb'

include RethinkDB::Shortcuts

set :port, 8080
set :environment, :production

get '/', provides: :html do
  File.open('./index.html', 'r').read
end

# GET	/users	users#index	display a list of all users
# GET	/users/new	users#new	return an HTML form for creating a new user
# POST	/users	users#create	create a new user
# GET	/users/:id	users#show	display a specific user
# GET	/users/:id/edit	users#edit	return an HTML form for editing a user
# PATCH/PUT	/users/:id	users#update	update a specific user
# DELETE	/users/:id	users#destroy	delete a specific user

get '/users', provides: :json do
  content_type :json
end

get '/users/new', provides: :json do
  content_type :json
end

post '/users/', provides: :json do
  content_type :json
end

get '/users/:id', provides: :json do
  content_type :json
end

get '/users/:id/edit', provides: :json do
  content_type :json
end

put '/users/:id', provides: :json do
  content_type :json
end

delete '/users/:id', provides: :json do
  content_type :json
end
