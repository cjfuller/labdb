MAP_STORE = File.join Rails.root, 'public', 'map_store'

class Plasmid < ActiveRecord::Base
  attr_accessible :antibiotic, :concentration, :datefrozen, :description, :enteredby, :mapreference, :notebook, :plasmidalias, :plasmidnumber, :plasmidsize, :sequence, :strainnumbers, :vector, :verified

  after_save :save_tasks
	before_destroy :delete_map

  @@Antibiotics = {"carbenicillin" => "carb", "kanamycin"=>"kan", "chloramphenicol"=>"chlor", "gentamycin"=>"gent", "tetracycline"=>"tet", "streptomycin"=>"strep"}

  def self.get_antibiotics
    return @@Antibiotics
  end
  
  self.get_antibiotics.each_value do |v|
    attr_accessor v.to_sym
  end

  def parse_antibiotics
    @@Antibiotics.each_value do |v|
      self.send((v + "=").to_sym, "0")
    end
    
    if not self.antibiotic or self.antibiotic.length == 0 then
      return
    end
    
    self.antibiotic.split(",").each do |ab|
      if self.respond_to?(ab.to_sym) then
        self.send((ab+"=").to_sym, "1")
      end
    end
  end

  def has_map?
    false
  end
  
  def calculate_size
    self.plasmidsize= self.sequence.chomp.length
  end
  
  def generate_date
    self.datefrozen= Time.now
  end	
	
	def mapreference=(file_data)
		
		unless file_data.blank? then
			@file_data = file_data
		end
		
	end
	
	def has_map?
		File.exists? map_filename
	end
	
	def map_path
		"/map_store/#{id}.png"
	end
	
	private
	
	def save_tasks
		store_map
	end
	
	def store_map
		if @file_data then
			FileUtils.mkdir_p MAP_STORE
			if File.exists?(map_filename) then
			  delete_map
		  end
			File.open(map_filename, 'wb') do |f|
				f.write(@file_data.read)
			end
			
			@file_data = nil
			
		end
		
	end
	
	def map_filename
		File.join MAP_STORE, "#{id}.png"
	end
	
	def delete_map
	  if has_map? then
		  FileUtils.rm(map_filename)
	  end
	end

end
