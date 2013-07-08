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

class QuickSearchController < ApplicationController

  DB_STR_TO_CONTROLLER_MAP = {
      'Plasmids' => 'plasmids',
      'Oligonucleotides' => 'oligos',
      'Bacterial Strains' => 'bacteria',
      'Samples' => 'samples',
      'Antibodies' => 'antibodies',
      'TC' => 'lines',
      'Yeast' => 'yeaststrains'
    }

  def db_str_to_controller_map
    DB_STR_TO_CONTROLLER_MAP
  end

  def failure_location
    '/'
  end

  def do_quick_search
    controller = db_str_to_controller_map[params[:database]]
    redirect_to failure_location and return unless params[:database] and controller
    model = controller.classify.constantize
    search_obj = nil

    if params[:number] and not params[:number].empty? then
      search_obj = {model.to_s.downcase.to_sym => {model.number_field_name => params[:number]}}
    elsif params[:alias] and not params[:alias].empty? then
      search_obj = {model.to_s.downcase.to_sym => {model.info_field_name => params[:alias]}}
    end

    if search_obj then
      redirect_to search_obj.merge({controller: controller, action: :index}) and return
    end

    redirect_to failure_location
  end

end