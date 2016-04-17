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

  REGEX_DETECTION_REGEX = /\A\/.*\/(i?)\Z/

  def glob_style_search_to_regex(search_params, key)
    search_params[key].gsub!("*", ".*")
    #add start and end of string matchers to avoid, e.g., matching all plasmids with a 1 when searching for #1
    search_params[key]= '\A' + search_params[key] + '\Z'
    Regexp.new(search_params[key], true) #always case insensitive since there's no way to specify
  end

  def parse_search_regex(search_param)
    matched = REGEX_DETECTION_REGEX.match(search_param)
    end_of_regex_offset = 1
    case_insensitive = (matched[1].length > 0)

    if case_insensitive then
      end_of_regex_offset = 2
    end

    Regexp.new(search_param[1...(search_param.length-end_of_regex_offset)], case_insensitive)
  end

  def generate_regex_conditions(search_params)
    regex_conditions = {}
    
    search_params.each_key do |k|
      if search_params[k] and search_params[k] != "" and k != "verified" then #TODO: is there a better way to deal with the verified field?
        matched = REGEX_DETECTION_REGEX.match(search_params[k])

        if matched then
          regex_conditions[k] = parse_search_regex(search_params[k])
        else
          regex_conditions[k] = glob_style_search_to_regex(search_params, k)
        end
      end
    end

    regex_conditions
  end
  
  def process_search_query(search_params, search_class)
    preprocessed_conditions = preprocess_search_query(search_params)
    regex_conditions = generate_regex_conditions(search_params)

    preprocessed_conditions.each do |k,v|
      regex_conditions[k] = Regexp.new(v)
    end

    preliminary_list = search_class.order(index_order)

    final_list = preliminary_list.select do |p|
      regex_conditions.any? do |k, r|
        if p.respond_to? k then
          val = p.send(k.to_s).to_s
          r.match(val)
        else
          nil
        end
      end
    end

    final_list
  end

  def preprocess_search_query(search_params)
    {}
  end
end
