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

require 'psych'
require 'number_assignment'
require 'object_naming'

class ApplicationController < ActionController::Base

  protect_from_forgery

  DATABASE_SYSTEM_NAME = Naming.name_for("database_full")
  DATABASE_SYSTEM_SHORT_NAME = Naming.name_for("database_short")

  ALLOWED_LABEL = "allowedusers"
  NAME_TAG = "name"
  EMAIL_TAG = "email"

  def redirect_https
    redirect_to :protocol => "https://" unless request.protocol == "https://"
  end

  def denied
    render :text => "Access denied."
  end

  before_filter :require_authorization
  prepend_before_filter :redirect_https
  around_filter :clear_temporary_number_assignments

  def clear_temporary_number_assignments
    
    if request.get?
      NumberAssignment.clear_unused_temporaries(session[:session_id])
    end

    yield

    if request.post? or request.put? or request.delete?
      NumberAssignment.clear_unused_temporaries(session[:session_id])
    end

  end

  def require_authorization

    unless authorized? then

      redirect_to "/", notice: "Access denied."

    end

  end

  def load_auth

    @user_data = Rails.configuration.user_data

    @users_loaded = true

  end

  def curr_username

    curr_uid = session[:user_id]

    return false if curr_uid.nil?

    curr_user = User.find_by_uid(curr_uid)

    curr_user.name

  end

  def generate_date(an_obj)
    an_obj.date_entered= Time.now
  end

  def generate_name(an_obj)
    if an_obj.respond_to?(:entered_by) then
      an_obj.entered_by = curr_username
    end

    if an_obj.respond_to?(:enteredby) then
      an_obj.enteredby = curr_username
    end
  end


  def authorized?

    curr_uid = session[:user_id]

    return false if curr_uid.nil?

    curr_user = User.find_by_uid(curr_uid)

    return false if curr_user.nil?

    load_auth unless @users_loaded

    allowed_users = @user_data[ALLOWED_LABEL]

    allowed_users.each do |u|


      if curr_user.email == u[EMAIL_TAG] and curr_user.name == u[NAME_TAG] then
        return true
      end

    end

    false

  end

  def define_ui_variables(opts_hash)

    opts_hash.each do |k, v|

      instance_variable_set("@#{k}", v)

    end

  end


  def self.headings_for(headings_list)

    headings_list.inject({}) do |a, e|
      a[e]= get_heading(e)
      a
    end

  end

  def process_search_query(search_params, search_class)

    preprocessed_conditions = preprocess_search_query(search_params)

    conditions = Hash.new
    regex_conditions = Hash.new
    regex_detection_regex = /\A\/.*\/(i?)\Z/
    search_params.each_key do |k|
      if search_params[k] and search_params[k] != "" and k != "verified" then #TODO: is there a better way to deal with the verified field?

        matched = regex_detection_regex.match(search_params[k])

        if matched then

          case_insensitive = (matched[1].length > 0)

          end_of_regex_offset = 1

          if case_insensitive then
            end_of_regex_offset = 2
          end
          
          regex_conditions[k] = Regexp.new(search_params[k][1...(search_params[k].length-end_of_regex_offset)], case_insensitive)
        
        else
          #if a regex has not been entered:
          #substitute * for .* to turn the old filemaker-style glob syntax into a regex
          search_params[k].gsub!("*", ".*")
          #add start and end of string matchers to avoid, e.g., matching all plasmids with a 1 when searching for #1
          search_params[k]= '\A' + search_params[k] + '\Z'
          #also make it case-insensitive since there's no way to specify one or the other here
          regex_conditions[k] = Regexp.new(search_params[k], true)
        end
      end
    end

    preprocessed_conditions.each do |k,v|
      regex_conditions[k] = Regexp.new(v)
    end

    preliminary_list = search_class.where(conditions)

    final_list = Array.new

    preliminary_list.each do |p|
      include_obj = true
      regex_conditions.each_key do |k|
        val = p.send(k.to_s).to_s
        unless regex_conditions[k].match(val) then
          include_obj = false
          break
        end
      end
      if include_obj then
        final_list << p
      end
    end

    final_list

  end

  def preprocess_search_query(search_params)
    {}
  end

  def generate_object_number(klass, number_field_name)

    NumberAssignment.assignment_for_class(klass, number_field_name, session)

  end

  def do_export(obj)

    send_data(obj.export_to(params["exportformat"].to_sym), obj.get_export_params(params["exportformat"].to_sym))

  end


end
