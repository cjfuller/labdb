class YeaststrainsController < ApplicationController
  
  YEAST_TAG = "ASYS"

  @@headings = {strain_number: "ASYS Number", date_entered: "Date entered",
                entered_by: "Entered by", notebook: "Notebook", 
                comments: "Description", plasmidnumber: "ASP Number", strain_bkg: "Strain background", genotype: "Genotype", antibiotic: "Antibiotics", location: "Location in freezer", sequence: "Sequence", species: "Species", strainalias: "Alias"}


  def self.get_heading(var_name)
    @@headings[var_name]
  end

  def search_path
    "/yeaststrains/search"
  end

  def define_ui_variables(params)

    params[:model_class]= Yeaststrain
    params[:search_path]= search_path

    super(params)

  end

  def define_table_view_vars

    @table_columns = [:strain_number, :date_entered, :entered_by, :species, :strainalias]
    @controller = YeaststrainsController
    @table_objects = @yeaststrains

  end



  # GET /yeaststrains
  # GET /yeaststrains.json
  def index

    define_ui_variables(status_text: "Yeast strains")

    if params.has_key?(:yeaststrain) then
      @yeaststrains = process_search_query(params[:yeaststrain], Yeaststrain)
    else
      @yeaststrains = Yeaststrain.all
    end

    define_table_view_vars

    @yeaststrains = Yeaststrain.all

    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @yeaststrains }
    end
  end

  # GET /yeaststrains/1
  # GET /yeaststrains/1.json
  def show
    @yeaststrain = Yeaststrain.find(params[:id])

    define_ui_variables(status_text: "#{YEAST_TAG} #{@yeaststrain.strain_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @yeaststrain, readonly: true)

    respond_to do |format|
      format.html # show.html.erb
      format.json { render json: @yeaststrain }
    end
  end

  # GET /yeaststrains/new
  # GET /yeaststrains/new.json
  def new
    @yeaststrain = Yeaststrain.new

    define_ui_variables(status_text: "New yeast strain", readonly: false, submit_text: "Create strain")

    generate_date(@yeaststrain)
    generate_name(@yeaststrain)
    @yeaststrain.strain_number = generate_object_number(Yeaststrain, :strain_number)

    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @yeaststrain }
    end
  end

  # GET /yeaststrains/1/edit
  def edit
    @yeaststrain = Yeaststrain.find(params[:id])

    define_ui_variables(status_text: "Editing #{YEAST_TAG} #{@yeaststrain.strain_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @yeaststrain, readonly: false, submit_text: "Update strain")
  end

  # POST /yeaststrains
  # POST /yeaststrains.json
  def create
    @yeaststrain = Yeaststrain.new(params[:yeaststrain])

    respond_to do |format|
      if @yeaststrain.save
        format.html { redirect_to @yeaststrain, notice: 'Yeaststrain was successfully created.' }
        format.json { render json: @yeaststrain, status: :created, location: @yeaststrain }
      else
        format.html { render action: "new" }
        format.json { render json: @yeaststrain.errors, status: :unprocessable_entity }
      end
    end
  end

  # PUT /yeaststrains/1
  # PUT /yeaststrains/1.json
  def update
    @yeaststrain = Yeaststrain.find(params[:id])

    respond_to do |format|
      if @yeaststrain.update_attributes(params[:yeaststrain])
        format.html { redirect_to @yeaststrain, notice: 'Yeaststrain was successfully updated.' }
        format.json { head :no_content }
      else
        format.html { render action: "edit" }
        format.json { render json: @yeaststrain.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /yeaststrains/1
  # DELETE /yeaststrains/1.json
  def destroy
    @yeaststrain = Yeaststrain.find(params[:id])
    @yeaststrain.destroy

    respond_to do |format|
      format.html { redirect_to yeaststrains_url }
      format.json { head :no_content }
    end
  end

  def search
    @yeaststrain = Yeaststrain.new

    define_ui_variables(status_text: "Searching yeast strains", obj: @yeaststrain, readonly: false, submit_text: "Search")

    respond_to do |format|
      format.html
      format.json { render json: @bacterium }
    end
  end

end
