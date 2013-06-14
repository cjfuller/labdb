###
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
###

persona_login = ->
  navigator.id.get((assertion) ->
    if assertion
      $('input[name=assertion]').val(assertion)
      $('#browser_id_form').submit()
    else
      window.location = "#{failure_path}""
  )

$(->
  $('#browser_id_form_button').click(->
    persona_login()
    false
  )
)
