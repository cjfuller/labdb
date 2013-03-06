
module LinkableModel

  def get_linked_items(klass, field, field_values)
    field_values.delete_if { |e| e == "none" }
    linked = {}
    field_values.each do |n|
      linked[n] = klass.where(field => n)[0]
    end
    linked
  end

  def get_linked_plasmids(plasmid_numbers)
    get_linked_items(Plasmid, :plasmidnumber, plasmid_numbers)
  end

  def get_linked_bacterial_strains(strain_numbers)
    get_linked_items(Bacterium, :strain_number, strain_numbers)
  end


end

