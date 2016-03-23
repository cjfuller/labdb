module ResourceHelpers
  def field(sym, type: :value)
    {name: get_heading(sym), lookup: sym, type: type}
  end

  def fields(lst)
    lst.map { |f| field(f) }
  end

  def as_json
    JSON.generate(as_resource_def)
  end

  def core_alt_field_type
    :value
  end

  def as_resource_def
    field_data = {}
    exportable_fields.each { |f| field_data[f] = self.send f }
    field_data[:id] = id
    # TODO: this doesn't work for items where the core alt field is not a link!
    core_links = if self.respond_to? :core_alt_field and not [:depleted].include? self.core_alt_field_name then
                   {lookup: core_alt_field_name,
                    name: self.get_heading(core_alt_field_name),
                    links: core_alt_field.map(&:item_links).map(&:first)}
                 else
                   nil
                 end
    return {
      type: self.class.name.demodulize.downcase,
      id: id,
      timestamp: self.send(self.timestamp_field_name),
      fieldData: field_data,
      resourcePath: "/#{self.class.name.demodulize.pluralize.downcase}/#{id}",
      name: named_number_string,
      shortDesc: {lookup: info_field_name, inlineValue: (info_field || "").labdb_auto_link.html_safe, name: "Alias"},
      coreLinks: core_links,
      coreInfoSections: core_info,
      sequenceInfo: sequence_info,
      supplementalFields: supplemental_info,
    }
  end
end
