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

def log_in_address(session_obj, address)
  session_obj[:user_id] = address
end

def log_in(session_obj)
  log_in_admin(session_obj)
end

def log_in_admin(session_obj)
  log_in_address(session_obj, "admin@labdb")
end

def log_in_rw(session_obj)
  log_in_address(session_obj, "rw@labdb")
end

def log_in_ro(session_obj)
  log_in_address(session_obj, "ro@labdb")
end

def log_in_unauthorized(session_obj)
  log_in_address(session_obj, "unauthorized@labdb")
end
