class PlasmidsController < ApplicationController
  
  @@headings = {:plasmidnumber => "ASP Number", :datefrozen => "Date frozen", 
    :enteredby => "Entered by", :notebook => "Notebook", :verified => "Sequence verified?",
    :plasmidalias => "Alias", :antibiotic => "Antibiotic resistances", :plasmidsize => "Size",
    :concentration => "Concentration (ug/mL)", :strainnumbers => "ASBS numbers",
    :description => "Description", :sequence => "Sequence", :vector => "Vector",
    :mapreference => "Map"}
  
  @@plasmid_number_mutex = Mutex.new
  
  def self.get_heading(var_name)
    @@headings[var_name]
  end
  
  def generate_plasmid_number(a_plasmid)
    @@plasmid_number_mutex.synchronize do
    
      max_number = 1
      unless defined? @@max_number then
        Plasmid.find_each do |p|
          if p.plasmidnumber and p.plasmidnumber.to_i > max_number then
            max_number = p.plasmidnumber.to_i
          end
        end
      
        @@max_number = max_number
      end
      @@max_number += 1
      a_plasmid.plasmidnumber = @@max_number    
      
    end
  end
  
  def fix_antibiotic_params(param_hash)
    
    abs = Plasmid.get_antibiotics
    
    newparams = Hash.new
    
    param_hash.each_key do |k|
      unless abs.has_value?(k) then
        newparams[k] = param_hash[k]
      end
    end
    
    newparams
    
  end
  
  def generate_antibiotics_string(params_hash)
    
    antibiotic=""
    
    Plasmid.get_antibiotics.each_value do |v|
      if params_hash[v] == "1" then
      
        if antibiotic.length > 0 then
          antibiotic= antibiotic + ","
        end
        
        antibiotic = antibiotic + v
        
      end
        
    end
    
    antibiotic
    
  end
  
  def process_search_query(searchparams)
    puts "SEARCHING"
    antibiotics_string = generate_antibiotics_string(searchparams)
    searchparams = fix_antibiotic_params(searchparams)
    conditions = Hash.new
    searchparams.each_key do |k|
      if searchparams[k] and searchparams[k] != "" and k != "verified" then #todo: is there a better way to deal with the verified field?
        conditions[k] = searchparams[k]
      end
    end
    if antibiotics_string != "" then
      conditions[:antibiotic] = antibiotics_string
    end
    puts conditions
    Plasmid.where(conditions)
  end
  
  # GET /plasmids
  # GET /plasmids.json
  def index
    puts params
    if params.has_key?(:plasmid) then
      @plasmids = process_search_query(params[:plasmid])
    else
      @plasmids = Plasmid.all
    end
    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @plasmids }
    end
  end

  # GET /plasmids/1
  # GET /plasmids/1.json
  def show
    @plasmid = Plasmid.find(params[:id])
    @plasmid.parse_antibiotics
    respond_to do |format|
      format.html # show.html.erb
      format.json { render json: @plasmid }
    end
  end

  # GET /plasmids/new
  # GET /plasmids/new.json
  def new
    @plasmid = Plasmid.new

    @plasmid.generate_date
    generate_plasmid_number(@plasmid)
    @plasmid.parse_antibiotics
    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @plasmid }
    end
  end

  # GET /plasmids/1/edit
  def edit
    @plasmid = Plasmid.find(params[:id])
    @plasmid.parse_antibiotics
  end

  # POST /plasmids
  # POST /plasmids.json
  def create
    @plasmid = Plasmid.new(fix_antibiotic_params(params[:plasmid]))
    @plasmid.calculate_size
    @plasmid.antibiotic= generate_antibiotics_string(params[:plasmid])
    respond_to do |format|
      if @plasmid.save
        format.html { redirect_to @plasmid, notice: 'Plasmid was successfully created.' }
        format.json { render json: @plasmid, status: :created, location: @plasmid }
      else
        format.html { render action: "new" }
        format.json { render json: @plasmid.errors, status: :unprocessable_entity }
      end
    end
  end

  # PUT /plasmids/1
  # PUT /plasmids/1.json
  def update
    @plasmid = Plasmid.find(params[:id])
    @plasmid.calculate_size
    @plasmid.antibiotic= generate_antibiotics_string(params[:plasmid])
    newparams = fix_antibiotic_params(params[:plasmid])
    respond_to do |format|
      if @plasmid.update_attributes(newparams)
        format.html { redirect_to @plasmid, notice: 'Plasmid was successfully updated.' }
        format.json { head :no_content }
      else
        format.html { render action: "edit" }
        format.json { render json: @plasmid.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /plasmids/1
  # DELETE /plasmids/1.json
  def destroy
    @plasmid = Plasmid.find(params[:id])
    @plasmid.destroy

    respond_to do |format|
      format.html { redirect_to plasmids_url }
      format.json { head :no_content }
    end
  end
  
  def search
    @plasmid = Plasmid.new
    respond_to do |format|
      format.html
      format.json { render json: @plasmid }
    end
  end
    
end
