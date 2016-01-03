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

require 'action_view/helpers/url_helper'

require 'object_naming'

class LinkableString

  include Rails.application.routes.url_helpers

  def controller
    nil
  end

  def initialize(str)
    @str = str
  end

  def scan_matchobjs(regex)
    matches = []
    last_pos = 0
    while m = regex.match(@str, last_pos)
      last_pos = m.end(0)
      yield m
      matches << m
    end
    matches
  end

  def sub_labdb_links
    Naming::NAMES_LOOKUP.each_key do |k|
      matches = {}
      scan_matchobjs(/#{k}N?\W*(\d+)/) do |m|
        cls = Naming.named_class_for(k).constantize
        id = cls.where(cls.number_field_name => m[1]).first
        if id then
          matches[m[0]] =
            "<a href=\"#{url_for(controller: Naming.named_class_for(k).downcase.pluralize,
                                action: :show,
                                id: id,
                                only_path: true)}\">#{m[0]}</a>"

        end
      end
      matches.each do |k,v|
        @str = @str.gsub(k,v)
      end
    end
  end

  def to_s
    @str
  end

end

class String
  def labdb_auto_link
    ls = LinkableString.new(self)
    ls.sub_labdb_links
    ls.to_s
  end
end
