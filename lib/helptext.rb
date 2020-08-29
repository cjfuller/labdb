#--
# Copyright (C) 2013  Colin J. Fuller
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#++

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
