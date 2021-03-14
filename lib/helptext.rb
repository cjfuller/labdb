module Helptext
  HELPTEXT_LOCS = { description_formatting: "lib/assets/description_helptext.md", plasmid_map: "lib/assets/plasmid_map.md" }

  def self.get(name)
    if HELPTEXT_LOCS[name]
      helptext = Labdb::Application::MARKDOWN.render(File.read(HELPTEXT_LOCS[name])).html_safe
      "<div class='helptext-container'>#{helptext}</div>".html_safe
    else
      nil
    end
  end
end
