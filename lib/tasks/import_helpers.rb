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

def node_text(plas, name)

  ch = plas.xpath(name)[0].child

  return "" unless ch

  ch.text

end

def parse_date(date)

  if date.length > 0 then
    if /\d{1,2}\/\d{1,2}\/\d{4}/.match(date) then
      date=Date.strptime(date, '%m/%d/%Y')
    elsif /\d{1,2}-\d{1,2}-\d{4}/.match(date) then
      date=Date.strptime(date, '%m-%d-%Y')
    elsif /\d{4}-\d{1,2}-\d{1,2}/.match(date) then
      date=Date.strptime(date, '%Y-%m-%d')
    elsif /\d{4}\/\d{1,2}\/\d{1,2}/.match(date) then
      date=Date.strptime(date, '%Y/%m/%d')
    end
  end

  date

end