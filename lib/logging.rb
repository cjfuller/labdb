class Logging
  @logger = Logger.new(STDOUT)
  @logger.formatter = proc do |severity, datetime, progname, msg|
    "#{severity} #{datetime.iso8601} #{msg}\n"
  end

  def self.logger
    @logger
  end
end
