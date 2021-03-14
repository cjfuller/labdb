class PlasmidsController < ApplicationController
  include StandardActions

  before_action :antibiotic_params_from_form_for_create, only: [:create]
  before_action :antibiotic_params_from_form, only: [:update]

  @@plasmid_number_mutex = Mutex.new

  def obj_tag
    Plasmid.obj_tag
  end

  def self.get_heading(var_name)
    Plasmid.get_heading(var_name)
  end

  def self.model_class
    Plasmid
  end

  def self.text
    "Plasmid"
  end

  def search_path
    "/plasmids/search"
  end

  def define_table_view_vars
    @table_columns = { sort: :number, others: [:date_entered, :enteredby, :alias, :strainnumbers] }
    @controller = self.class
    @table_objects = @plasmids
  end

  def preprocess_model_object(model_obj)
    model_obj.parse_antibiotics
    model_obj.calculate_size
  end

  def antibiotic_params_from_form_for_create
    return nil if params[:plasmid][:antibiotic] and params[:plasmid][:antibiotic].size > 0
    antibiotic_params_from_form
  end

  def antibiotic_params_from_form
    antibiotic_string = generate_antibiotics_string(params[:plasmid])
    params[:plasmid] = fix_antibiotic_params(params[:plasmid])
    params[:plasmid][:antibiotic] = antibiotic_string
  end

  def fix_antibiotic_params(param_hash)
    abs = Plasmid.get_antibiotics
    newparams = Hash.new
    param_hash.each_key do |k|
      unless abs.has_value?(k)
        newparams[k] = param_hash[k]
      end
    end
    newparams
  end

  def generate_antibiotics_string(params_hash)
    antibiotic = ""
    Plasmid.get_antibiotics.each_value do |v|
      if params_hash[v] == "1"
        if antibiotic.length > 0
          antibiotic = antibiotic + ","
        end
        antibiotic = antibiotic + v
      end
    end
    antibiotic
  end

  def preprocess_search_query(search_params)
    antibiotics_string = generate_antibiotics_string(search_params)
    mod_search_params = fix_antibiotic_params(search_params)
    search_params.delete_if { |e| not(mod_search_params.include?(e)) }
    conditions = {}
    if antibiotics_string != ""
      conditions[:antibiotic] = antibiotics_string
    end
    conditions
  end

  def upload
  end
end
