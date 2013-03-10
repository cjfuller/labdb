ENV["RAILS_ENV"] = "test"
require File.expand_path('../../config/environment', __FILE__)
require 'rails/test_help'

class ActiveSupport::TestCase
  # Setup all fixtures in test/fixtures/*.(yml|csv) for all tests in alphabetical order.
  #
  # Note: You'll currently still have to declare fixtures explicitly in integration tests
  # -- they do not yet inherit this setting
  fixtures :all

  # Add more helper methods to be used by all tests here...

  def log_in(session_obj)

  	session_obj[:user_id]= "0001"

  end


end

module MiniTest::Assertions

	def assert_raises_subcl *exp

		msg = "#{exp.pop}.\n" if String === exp.last

		begin
			yield
		rescue MiniTest::Skip => e
			return e if exp.include? MiniTest::Skip
			raise e
		rescue Exception => e
			expected = exp.any? { |ex|
				e.kind_of?(ex)
			}

			assert expected, proc {
				exception_details(e, "#{msg}#{mu_pp(exp)} exception expected, not")
			}

			return e
		end

		exp = exp.first if exp.size == 1

		flunk "#{msg}#{mu_pp(exp)} expected but nothing was raised."

	end

end
