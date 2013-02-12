class OligosController < ApplicationController

  OLIGO_TAG = "ASO"

  @@headings = {:oligo_number => "ASO Number", :date_entered => "Date entered",
                :entered_by => "Entered by", :notebook => "Notebook", :oligoalias => "Alias",
                :purpose => "Description", :sequence => "Sequence", :organism => "Organism", :vendor => "Vendor"}


  def self.get_heading(var_name)
    @@headings[var_name]
  end

  def search_path
    "/oligos/search"
  end

  def define_ui_variables(params)

    params[:model_class]= Oligo
    params[:search_path]= search_path

    super(params)

  end


  # GET /oligos
  # GET /oligos.json
  def index
    @oligos = Oligo.all

    define_ui_variables(status_text: "Oligos")

    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @oligos }
    end
  end

  # GET /oligos/1
  # GET /oligos/1.json
  def show
    @oligo = Oligo.find(params[:id])

    define_ui_variables(status_text: "#{OLIGO_TAG} #{@oligo.oligo_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @oligo, readonly: true)

    respond_to do |format|
      format.html # show.html.erb
      format.json { render json: @oligo }
    end
  end

  # GET /oligos/new
  # GET /oligos/new.json
  def new

    @oligo = Oligo.new

    define_ui_variables(status_text: "New oligo", readonly: false, submit_text: "Create oligo")


    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @oligo }
    end
  end

  # GET /oligos/1/edit
  def edit

    @oligo = Oligo.find(params[:id])

    define_ui_variables(status_text: "Editing #{OLIGO_TAG} #{@oligo.oligo_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @oligo, readonly: false, submit_text: "Update oligo")


  end

  # POST /oligos
  # POST /oligos.json
  def create
    @oligo = Oligo.new(params[:oligo])

    respond_to do |format|
      if @oligo.save
        format.html { redirect_to @oligo, notice: 'Oligo was successfully created.' }
        format.json { render json: @oligo, status: :created, location: @oligo }
      else
        format.html { render action: "new" }
        format.json { render json: @oligo.errors, status: :unprocessable_entity }
      end
    end
  end

  # PUT /oligos/1
  # PUT /oligos/1.json
  def update
    @oligo = Oligo.find(params[:id])

    respond_to do |format|
      if @oligo.update_attributes(params[:oligo])
        format.html { redirect_to @oligo, notice: 'Oligo was successfully updated.' }
        format.json { head :no_content }
      else
        format.html { render action: "edit" }
        format.json { render json: @oligo.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /oligos/1
  # DELETE /oligos/1.json
  def destroy
    @oligo = Oligo.find(params[:id])
    @oligo.destroy

    respond_to do |format|
      format.html { redirect_to oligos_url }
      format.json { head :no_content }
    end
  end
end
