module Described
  def info_field
    send(info_field_name)
  end

  def info_field_name
    self.class.info_field_name
  end
end
