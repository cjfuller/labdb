# encoding: UTF-8
# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended to check this file into your version control system.

ActiveRecord::Schema.define(:version => 20130414232510) do

  create_table "antibodies", :force => true do |t|
    t.integer  "ab_number"
    t.string   "host"
    t.string   "label"
    t.string   "box"
    t.string   "alias"
    t.string   "fluorophore"
    t.string   "entered_by"
    t.boolean  "good_for_if"
    t.boolean  "good_for_western"
    t.text     "comments"
    t.string   "vendor"
    t.datetime "created_at",       :null => false
    t.datetime "updated_at",       :null => false
    t.date     "date_entered"
  end

  create_table "bacteria", :force => true do |t|
    t.integer  "strain_number"
    t.string   "species_bkg"
    t.date     "date_entered"
    t.string   "entered_by"
    t.integer  "notebook"
    t.text     "genotype"
    t.text     "comments"
    t.string   "plasmid_number"
    t.datetime "created_at",     :null => false
    t.datetime "updated_at",     :null => false
    t.text     "sequence"
    t.string   "strainalias"
  end

  create_table "lines", :force => true do |t|
    t.integer  "line_number"
    t.string   "line_alias"
    t.date     "date_entered"
    t.string   "entered_by"
    t.integer  "notebook"
    t.string   "species"
    t.text     "parent_line"
    t.text     "sequence"
    t.text     "description"
    t.string   "plasmid_numbers"
    t.string   "selectable_markers"
    t.string   "locations"
    t.text     "current_stock_counts"
    t.datetime "created_at",           :null => false
    t.datetime "updated_at",           :null => false
    t.text     "genotype"
  end

  create_table "oligos", :force => true do |t|
    t.integer  "oligo_number"
    t.string   "oligoalias"
    t.date     "date_entered"
    t.string   "entered_by"
    t.integer  "notebook"
    t.string   "vendor"
    t.string   "organism"
    t.text     "sequence"
    t.text     "purpose"
    t.datetime "created_at",   :null => false
    t.datetime "updated_at",   :null => false
  end

  create_table "plasmids", :force => true do |t|
    t.date     "date_entered"
    t.string   "enteredby"
    t.integer  "notebook"
    t.boolean  "verified"
    t.string   "plasmidalias"
    t.string   "antibiotic"
    t.integer  "plasmidsize"
    t.float    "concentration"
    t.string   "strainnumbers"
    t.text     "description"
    t.text     "sequence"
    t.string   "vector"
    t.datetime "created_at",              :null => false
    t.datetime "updated_at",              :null => false
    t.string   "plasmidmap_file_name"
    t.string   "plasmidmap_content_type"
    t.integer  "plasmidmap_file_size"
    t.datetime "plasmidmap_updated_at"
    t.integer  "plasmidnumber"
  end

  create_table "samples", :force => true do |t|
    t.integer  "sample_number"
    t.string   "sample_alias"
    t.string   "storage_type"
    t.date     "date_entered"
    t.string   "entered_by"
    t.integer  "notebook"
    t.string   "sample_type"
    t.boolean  "depleted"
    t.text     "description"
    t.text     "linked_items"
    t.datetime "created_at",    :null => false
    t.datetime "updated_at",    :null => false
  end

  create_table "searches", :force => true do |t|
    t.text     "searchparams"
    t.integer  "user_id"
    t.date     "expires"
    t.text     "result"
    t.datetime "created_at",   :null => false
    t.datetime "updated_at",   :null => false
  end

  create_table "users", :force => true do |t|
    t.string   "provider"
    t.string   "uid"
    t.string   "name"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
    t.string   "email"
  end

  create_table "yeaststrains", :force => true do |t|
    t.string   "strainalias"
    t.string   "antibiotic"
    t.string   "plasmidnumber"
    t.integer  "strain_number"
    t.string   "strain_bkg"
    t.date     "date_entered"
    t.text     "sequence"
    t.string   "entered_by"
    t.text     "comments"
    t.text     "genotype"
    t.string   "location"
    t.string   "species"
    t.datetime "created_at",    :null => false
    t.datetime "updated_at",    :null => false
    t.integer  "notebook"
  end

end
