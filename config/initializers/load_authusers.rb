require 'psych'

AUTH_FN = Rails.root.join('config/authusers.yml')

File.open(AUTH_FN, 'r') do |f|

	Rails.configuration.user_data = Psych.load(f.read)

end
