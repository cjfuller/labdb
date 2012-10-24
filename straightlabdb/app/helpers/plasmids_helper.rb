module PlasmidsHelper

  def truncate_string(str, max_length=20)

    str unless str.length > max_length

    str[0...max_length]

  end



end
