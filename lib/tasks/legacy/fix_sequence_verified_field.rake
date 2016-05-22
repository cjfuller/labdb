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

require 'rexml/document'
require 'date'
require 'nokogiri'

require_relative 'import_helpers'


desc "Fix the sequence verified field on plasmids to import correctly."
task :fix_sequence_verified_field => :environment do


  doc = Nokogiri::XML(File.new("db/db_imports/plasmid_database.xml"))

  doc.remove_namespaces!

  plasmids = doc.xpath("//ROW")

  plasmids.each do |plas|

    asp = node_text(plas, "ASP_Number")

    puts asp

    verified = node_text(plas, "Sequence_verified")

    p = Plasmid.where("plasmidnumber = ?", asp).first

    p.verified = (/Yes/i.match(verified) and true)

    p.save

  end

end
