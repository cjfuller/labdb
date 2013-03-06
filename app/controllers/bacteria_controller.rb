class BacteriaController < ApplicationController

  BACT_TAG = "ASBS"

  @@headings = {strain_number: "ASBS Number", date_entered: "Date entered",
                entered_by: "Entered by", notebook: "Notebook", 
                comments: "Description", plasmid_number: "ASP Number", species_bkg: "Species and background", genotype: "Genotype"}


  def self.get_heading(var_name)
    @@headings[var_name]
  end

  def search_path
    "/bacteria/search"
  end

  def define_ui_variables(params)

    params[:model_class]= Bacterium
    params[:search_path]= search_path

    super(params)

  end

  def define_table_view_vars

    @table_columns = [:strain_number, :date_entered, :entered_by, :species_bkg, :plasmid_number]
    @controller = BacteriaController
    @table_objects = @bacteria

  end


  # GET /bacteria
  # GET /bacteria.json
  def index

    define_ui_variables(status_text: "Bacterial strains")

    if params.has_key?(:bacterium) then
      @bacteria = process_search_query(params[:bacterium], Bacterium)
    else
      @bacteria = Bacterium.all
    end

    @bacteria.sort! { |e0, e1| e0.strain_number.to_i <=> e1.strain_number.to_i }

    define_table_view_vars

    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @bacteria }
    end
  end

  # GET /bacteria/1
  # GET /bacteria/1.json
  def show

    @bacterium = Bacterium.find(params[:id])

    define_ui_variables(status_text: "#{BACT_TAG} #{@bacterium.strain_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @bacterium, readonly: true)


    respond_to do |format|
      format.html # show.html.erb
      format.json { render json: @bacterium }
    end
  end

  # GET /bacteria/new
  # GET /bacteria/new.json
  def new

    @bacterium = Bacterium.new

    define_ui_variables(status_text: "New bacterial strain", readonly: false, submit_text: "Create strain")

    generate_date(@bacterium)
    generate_name(@bacterium)
    @bacterium.strain_number = generate_object_number(Bacterium, :strain_number)


    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @bacterium }
    end
  end

  # GET /bacteria/1/edit
  def edit

    @bacterium = Bacterium.find(params[:id])

     define_ui_variables(status_text: "Editing #{BACT_TAG} #{@bacterium.strain_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @bacterium, readonly: false, submit_text: "Update strain")

  end

  # POST /bacteria
  # POST /bacteria.json
  def create
    @bacterium = Bacterium.new(params[:bacterium])

    respond_to do |format|
      if @bacterium.save
        format.html { redirect_to @bacterium, notice: 'Bacterium was successfully created.' }
        format.json { render json: @bacterium, status: :created, location: @bacterium }
      else
        format.html { render action: "new" }
        format.json { render json: @bacterium.errors, status: :unprocessable_entity }
      end
    end
  end

  # PUT /bacteria/1
  # PUT /bacteria/1.json
  def update
    @bacterium = Bacterium.find(params[:id])

    respond_to do |format|
      if @bacterium.update_attributes(params[:bacterium])
        format.html { redirect_to @bacterium, notice: 'Bacterium was successfully updated.' }
        format.json { head :no_content }
      else
        format.html { render action: "edit" }
        format.json { render json: @bacterium.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /bacteria/1
  # DELETE /bacteria/1.json
  def destroy
    @bacterium = Bacterium.find(params[:id])
    @bacterium.destroy

    respond_to do |format|
      format.html { redirect_to bacteria_url }
      format.json { head :no_content }
    end
  end

  def search
    @bacterium = Bacterium.new

    define_ui_variables(status_text: "Searching bacterial strains", obj: @bacterium, readonly: false, submit_text: "Search")

    respond_to do |format|
      format.html
      format.json { render json: @bacterium }
    end
  end

end
