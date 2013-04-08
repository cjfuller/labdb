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

		begin
			s.loaded_result = Psych.load(s.result) unless s.loaded_result
		rescue ArgumentError => e
			logger.warn("Exception encountered while loading stored search result.  Likely a development mode lazy-loading issue: #{e.message}")
		end

		s

	end

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

		preliminary_list = search_class.all

		final_list = preliminary_list.select do |p|
			regex_conditions.all? do |k, r|
				val = p.send(k.to_s).to_s
				r.match(val)
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
