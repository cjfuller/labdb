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

require 'action_view/helpers/tag_helper'
require 'action_view/helpers/url_helper'


class BootstrapFormBuilder < ActionView::Helpers::FormBuilder

  include Rails.application.routes.url_helpers

  include ActionView::Helpers::TagHelper
  include ActionView::Helpers::UrlHelper

  def controller #this appears to be necessary to use link_to or url_for
    nil
  end

  class HTMLString < String

    def initialize(str="")
      super
    end

    def open_row
      self << '<div class="row-fluid" id="map">'
    end
  
    def close_row
      self << '</div>'
    end
    
    def row
      open_row
      yield
      close_row
    end
  
    def col(span_size)
      open_col(span_size)
      yield
      close_col
    end
  
    def open_col(span_size)
      self << "<div class=\"span#{span_size}\">"
    end
  
    def close_col
      self << '</div>'
    end
    
  end
    
    
  def text_field_with_label(html, propertyname, displayname, span_size, emph_label_color=false, readonly=false)

    html << '<div class="control-group">'
    
    class_string = "label control-label"
    
    if emph_label_color then
      class_string << " label-info"
    end
    
    html << label(propertyname, displayname, :class=> class_string)
    html << tag("br")

    linked_items = nil

    if not self.object.nil? and self.object.respond_to? :get_linked then

      linked_items = self.object.get_linked(propertyname)

    end

    if readonly and not linked_items.nil? and not linked_items.size == 0 then

      content = ""

      linked_items.each do |k, lnkd_obj|

        if content.length > 0 then
          content << ", "
        end

        content << link_to(k, lnkd_obj)

      end

      html << content_tag(:div, content.html_safe, class: "span#{span_size}")
    else
      html << text_field(propertyname, readonly: readonly, class: "span#{span_size}", size: nil)
    end

    html << '</div>'
    
  end
  
  def checkbox_with_label(html, propertyname, displayname, span_size, emph_label_color=false, readonly=false)
    
    html << '<div class="control-group">'
    
    class_string = "label control-label"
    
    if emph_label_color then
      class_string << " label-info"
    end
    
    html << label(propertyname, displayname, :class=> class_string)

    html << '<div>'
    
    html << check_box(propertyname, :disabled=>readonly)
        
    html << '</div>'

    html << '</div>'
    
  end
  
  
  def add_span_series(html, sizes, list_of_properties, display_names, emph_hash, readonly_hash, is_checkbox_hash)
    
    list_of_properties.each_with_index do |prop, i|
        
      col_size = sizes[i]
        
      html.col(col_size) do
        
        if is_checkbox_hash[prop] then
            
          checkbox_with_label(html, prop, display_names[prop], 12, emph_hash[prop], readonly_hash[prop])
          
        else
        
          text_field_with_label(html, prop, display_names[prop], 12, emph_hash[prop], readonly_hash[prop])
            
        end
          
      end
        
    end
    
    html
      
  end
  
  def span_series(sizes, list_of_properties, display_names, emph_hash, readonly_hash, is_checkbox_hash)
    
    html = HTMLString.new
    
    add_span_series(html, sizes, list_of_properties, display_names, emph_hash, readonly_hash, is_checkbox_hash)
    
    String.new(html).html_safe
    
  end
  
  def row(sizes, list_of_properties, display_names, emph_hash, readonly_hash, is_checkbox_hash)
    
    html = HTMLString.new
      
    html.row do
      
      add_span_series(html, sizes, list_of_properties, display_names, emph_hash, readonly_hash, is_checkbox_hash)
        
    end
    
    String.new(html).html_safe
    
  end


end


