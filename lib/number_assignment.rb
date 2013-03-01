
class NumberAssignment

	@assignment_mutex = Mutex.new
	@temporary_assignments = {}

	def self.clear_unused_temporaries(session_id)

		@assignment_mutex.synchronize do

			@temporary_assignments.each_value do |v|

				v.delete_if { |kk,vv| vv == session_id}

			end

		end

	end


	def self.assign_temporary_number(klass, next_index, requesting_session)

		@temporary_assignments[klass]= {} unless @temporary_assignments[klass]

		while @temporary_assignments[klass].has_key?(next_index) do
			next_index += 1
		end

		@temporary_assignments[klass][next_index]= requesting_session[:session_id]

		next_index

	end


	def self.assignment_for_class(klass, number_field_name, requesting_session)

		assignment = -1


		@assignment_mutex.synchronize do

			max_number = 0

			klass.find_each do |p|
				index = p.send(number_field_name)
				if index and index.to_i > max_number then
					max_number = index.to_i
				end
			end

			assignment = assign_temporary_number(klass, max_number + 1, requesting_session)

		end

		assignment

	end

end