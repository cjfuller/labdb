class BacteriaController < ApplicationController
  include StandardActions

  def obj_tag
    Bacterium.obj_tag
  end

  def self.get_heading(var_name)
    Bacterium.get_heading(var_name)
  end

  def self.model_class
    Bacterium
  end

  def self.text
    "Bacterial strain"
  end

  def search_path
    "/bacteria/search"
  end

  def define_table_view_vars
    @table_columns = { sort: :strain_number, others: [:date_entered, :entered_by, :strainalias, :plasmid_number] }
    @controller = self.class
    @table_objects = @bacteria
  end

  def append_strain_number(strain, plasmid)
    if /\d+/.match(plasmid.strainnumbers)
      plasmid.strainnumbers = plasmid.strainnumbers + ",#{strain.strain_number}"
    else
      plasmid.strainnumbers = strain.strain_number
    end

    plasmid.save
  end

  def copy_fields_from_plasmid(strain, plasmid)
    strain.strainalias = plasmid.alias
    strain.entered_by = plasmid.creator
    strain.notebook = plasmid.notebook
    strain.comments = plasmid.description
    strain.plasmid_number = plasmid.number
  end

  def create_from_plasmid
    plas_id = params[:plasmid_id]

    if plas_id.nil?
      redirect_to controller: "bacteria", action: :new and return
    end

    plas = Plasmid.find(plas_id)

    @obj = Bacterium.new

    auto_fill_generated_fields

    copy_fields_from_plasmid(@obj, plas)

    append_strain_number(@obj, plas)

    @obj.save
    render json: @obj.id
  end
end
