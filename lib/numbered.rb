module Numbered
  def number_field
    send(number_field_name)
  end

  def number_field_name
    self.class.number_field_name
  end

  def named_number_string
    "#{Naming.name_for(self.class)} #{self.number_field}"
  end
end
