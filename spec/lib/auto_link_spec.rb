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

require "spec_helper"
require "auto_linked"

describe LinkableString do
  fixtures :oligos, :plasmids

  it "should identify and link an object reference" do
    ls = LinkableString.new("ASP1")
    ls.sub_labdb_links
    obj_id = Plasmid.where(Plasmid.number_field_name => 1).first.id
    ls.to_s.should match /href="\/plasmids\/#{obj_id}"/
  end

  it "should identify and link an object reference with optional N" do
    ls = LinkableString.new("ASON1")
    ls.sub_labdb_links
    obj_id = Oligo.where(Oligo.number_field_name => 1).first.id
    ls.to_s.should match /href="\/oligos\/#{obj_id}"/
  end

  it "should fail gracefully when encountering a nonexistent reference" do
    ls = LinkableString.new("ASON3444992")
    expect { ls.sub_labdb_links }.not_to raise_error
  end
end
