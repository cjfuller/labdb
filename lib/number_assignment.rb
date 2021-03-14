require "logging"

class NumberAssignment
  @assignment_mutex = Mutex.new
  @temporary_assignments = {}
  @session_log = {}
  EXPIRY_TIME_S = 60 * 60

  def self.clear_unused_temporaries(session_id)
    @assignment_mutex.synchronize do
      @temporary_assignments.each_value do |v|
        v.delete_if { |kk, vv| vv == session_id }
        v.delete_if { |kk, vv| Time.now - @session_log[vv] > EXPIRY_TIME_S }
      end
    end
  end

  def self.assign_temporary_number(klass, next_index, requesting_session)
    @temporary_assignments[klass] = {} unless @temporary_assignments[klass]
    while @temporary_assignments[klass].has_key?(next_index)
      Logging.logger.info("Number conflict for #{klass.name} #{next_index}")
      next_index += 1
    end
    @temporary_assignments[klass][next_index] = requesting_session[:session_id]
    @session_log[requesting_session[:session_id]] = Time.now
    next_index
  end

  def self.assignment_for_class(klass, number_field_name, requesting_session)
    assignment = -1
    @assignment_mutex.synchronize do
      max_number = 0
      klass.find_each do |p|
        index = p.send(number_field_name)
        if index and index.to_i > max_number
          max_number = index.to_i
        end
      end
      Logging.logger.info("Trying to assign number #{klass.name} #{max_number + 1}")
      assignment = assign_temporary_number(klass, max_number + 1, requesting_session)
      Logging.logger.info("Assigned number #{klass.name} #{assignment}")
    end
    assignment
  end
end
