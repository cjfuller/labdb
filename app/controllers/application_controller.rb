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
require 'standard_actions'
require 'searching'
require 'authorization'

class ApplicationController < ActionController::Base

  include Searching
  include Authorization

  protect_from_forgery

  DATABASE_SYSTEM_NAME = Naming.name_for("database_full")
  DATABASE_SYSTEM_SHORT_NAME = Naming.name_for("database_short")

  def redirect_https
    redirect_to :protocol => "https://" unless request.protocol == "https://"
  end

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


  def curr_username

    curr_uid = session[:user_id]

    return false if curr_uid.nil?

    curr_user = User.find_by_uid(curr_uid)

    curr_user.name

  end

  def curr_user_id

    curr_uid = session[:user_id]

    return false if curr_uid.nil?

    curr_user = User.find_by_uid(curr_uid)

    curr_user.id.to_i

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
  
  def generate_object_number(klass, number_field_name)

    NumberAssignment.assignment_for_class(klass, number_field_name, session)

  end

  def do_export(obj)

    send_data(obj.export_to(params["exportformat"].to_sym), obj.get_export_params(params["exportformat"].to_sym))

  end

  def preprocess_model_object(model_obj)
  end


end
