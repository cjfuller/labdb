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

module Searching

	def delete_old_searches

    all_searches = Search.all

    unless all_searches.nil? then
      all_searches.each { |s| s.destroy if s.expired? }
    end

    searches = Search.find_all_by_user_id(curr_user_id)

    unless searches.nil? then
      searches.each { |s| s.destroy }
    end

  end


  def generate_search_object(search_params, result)

    delete_old_searches

    sec_per_day = 60*60*24

    exp_time = Time.now + 1.5*sec_per_day

    s = Search.new(expires: exp_time, result: Psych.dump(result), searchparams: search_params, user_id: curr_user_id)

    s.save

  end

  def valid_search_requested?

    current_search = find_current_search

    params.has_key?(:search_id) and current_search and current_search.id == params[:search_id].to_i
    
  end

  def find_current_search

    s = Search.find_by_user_id(curr_user_id)

    return nil if s.nil? or s.expired?

    s.loaded_result = Psych.load(s.result) unless s.loaded_result

    s

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

    final_list.sort! { |e0, e1| e0.number_field.to_i <=> e1.number_field.to_i }

    generate_search_object(search_params, final_list)

    final_list

  end

  def preprocess_search_query(search_params)
    {}
  end


end
