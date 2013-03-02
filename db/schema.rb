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

ActiveRecord::Schema.define(:version => 20130302211423) do

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
    t.string   "plasmidnumber"
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
  end

  create_table "users", :force => true do |t|
    t.string   "provider"
    t.string   "uid"
    t.string   "name"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
    t.string   "email"
  end

end
