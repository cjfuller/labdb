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


desc "Update bacterial strains."
task :update_bacteria => :environment do


  doc = Nokogiri::XML(File.new("db/db_imports/bacteria_database.xml"))

  doc.remove_namespaces!

  bacts = doc.xpath("//ROW")

  all = Bacterium.all

  puts all.size

  bacts.each do |bact|

    asbs = node_text(bact, "ASBS_Number")

    puts asbs

    bact_obj = all[all.find_index { |e| e.strain_number.to_i == asbs.to_i }]

    strainalias = node_text(bact, "Alias")
    sequence = node_text(bact, "DNA_sequence")

    plasmid = bact_obj.plasmid_number


    options = {
      strainalias: strainalias
    }

    if plasmid == "" then
      options[:sequence] = sequence
    end

    bact_obj.update_attributes(options)

  end

end
