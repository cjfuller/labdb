module DNASequence
  SEQ_CHARS_LOWER = "atgcrymkswhbvdn"
  VALID_SEQ_CHARS = SEQ_CHARS_LOWER + SEQ_CHARS_LOWER.upcase

  def formatted_sequence
    self.sequence and self.sequence.gsub(/[^#{VALID_SEQ_CHARS}]/, "").gsub(/\s/, "") #.word_wrap(80)
  end

  def sequence_size
    if formatted_sequence
      formatted_sequence.size
    else
      0
    end
  end
end
