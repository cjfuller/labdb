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

module DNASequence

	SEQ_CHARS_LOWER = "atgcrymkswhbvdn"
	VALID_SEQ_CHARS = SEQ_CHARS_LOWER + SEQ_CHARS_LOWER.upcase

	def formatted_sequence
    self.sequence and self.sequence.gsub(/[^#{VALID_SEQ_CHARS}]/, "").gsub(/\s/, "") #.word_wrap(80)
  end

  def sequence_size
  	if formatted_sequence then
  		formatted_sequence.size
  	else
  		0
  	end
  end

end
