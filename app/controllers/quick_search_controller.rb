class QuickSearchController < ApplicationController
  DB_STR_TO_CONTROLLER_MAP = {
    "Plasmids" => "plasmids",
    "Oligonucleotides" => "oligos",
    "Bacterial Strains" => "bacteria",
    "Samples" => "samples",
    "Antibodies" => "antibodies",
    "TC" => "lines",
    "Yeast" => "yeaststrains",
  }

  def db_str_to_controller_map
    DB_STR_TO_CONTROLLER_MAP
  end

  def failure_location
    "/"
  end

  def do_quick_search
    controller = db_str_to_controller_map[params[:database]]
    redirect_to failure_location and return unless params[:database] and controller
    model = controller.classify.constantize
    search_obj = nil

    if params[:number] and not params[:number].empty?
      search_obj = { model.to_s.downcase.to_sym => { model.number_field_name => params[:number] } }
    elsif params[:alias] and not params[:alias].empty?
      search_obj = { model.to_s.downcase.to_sym => { model.info_field_name => params[:alias] } }
    end

    if search_obj
      redirect_to search_obj.merge({ controller: controller, action: :index }) and return
    end

    redirect_to failure_location
  end
end
