module PlasmidsHelper
  def truncate_string(str, max_length = 20)
    str unless str.length > max_length

    str[0...max_length]
  end

  def readonly_for(property_list)
    property_list.inject({}) { |a, e| a[e] = readonly?; a }
  end

  def show_map?
    @show_map
  end
end
