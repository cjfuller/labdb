class BootstrapFormBuilder < ActionView::Helpers::FormBuilder


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
    
    html << text_field(propertyname, :disabled=>readonly, :class=> "span#{span_size}", :size=> nil)
    
    html << '</div>'
    
  end
  
  def checkbox_with_label(html, propertyname, displayname, span_size, emph_label_color=false, readonly=false)
    
    html << '<div class="control-group">'
    
    class_string = "label control-label"
    
    if emph_label_color then
      class_string << " label-info"
    end
    
    html << label(propertyname, displayname, :class=> class_string)
    
    html << check_box(propertyname, :disabled=>readonly)
        
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


