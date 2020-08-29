module Headings
  def self.included(base)
    base.class_eval do
      def self.get_heading(var_name)
        @headings[var_name]
      end

      def self.obj_tag
        Naming.name_for(self)
      end
    end
  end

  def get_heading(var_name)
    self.class.get_heading(var_name)
  end

  def obj_tag
    Naming.name_for(self.class)
  end
end
