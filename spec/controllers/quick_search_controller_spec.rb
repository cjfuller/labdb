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

require 'spec_helper'

describe QuickSearchController do

  fixtures :users

  before :each do
    request.env['HTTPS'] = 'on'
    log_in(request.session)
  end 
  
  it "should redirect to the appropriate search page when searching numbers" do
    get :do_quick_search, database: 'Plasmids', number: '5'
    response.should redirect_to(controller: 'plasmids', action: 'index', plasmid: {plasmidnumber: 5})
  end

  it "should redirect to the appropriate search page when searching aliases" do
    get :do_quick_search, database: 'Oligonucleotides', alias: 'an oligo'
    response.should redirect_to(controller: 'oligos', action: 'index', oligo: {oligoalias: 'an oligo'})
  end

  it "should go back to the quick start page on invalid search" do
    get :do_quick_search, database: '', number: ''
    response.should redirect_to('/')
  end


end
