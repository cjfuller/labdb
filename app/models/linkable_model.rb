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
    get_linked_items(Plasmid, :number, plasmid_numbers)
  end

  def get_linked_bacterial_strains(strain_numbers)
    get_linked_items(Bacterium, :strain_number, strain_numbers)
  end

  def get_linked_samples(sample_numbers)
    get_linked_items(Sample, :sample_number, sample_numbers)
  end

  def get_linked_number_fields(property_name)
    return nil unless linked_properties.include? property_name
    return nil if self.send(property_name).nil?
    self.send(property_name).split(",").map! { |e| e.strip }.delete_if { |e| e == "none" }
  end

  def clear_linked
    linked_properties.each do |linked_property|
      setter_name = linked_property.to_s + "="
      self.send(linked_property.to_s + "=", nil) if self.respond_to?(setter_name)
    end
  end

  def linkable?
    true
  end
end
