# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# This file is the source Rails uses to define your schema when running `rails
# db:schema:load`. When creating a new database, `rails db:schema:load` tends to
# be faster and is potentially less error prone than running all of your
# migrations from scratch. Old migrations may fail to apply correctly if those
# migrations use external dependencies or application code.
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 2024_01_19_125715) do

  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"

  create_table "antibodies", id: :serial, force: :cascade do |t|
    t.integer "ab_number"
    t.string "host", limit: 255
    t.string "label", limit: 255
    t.string "box", limit: 255
    t.string "alias", limit: 255
    t.string "fluorophore", limit: 255
    t.string "entered_by", limit: 255
    t.boolean "good_for_if"
    t.boolean "good_for_western"
    t.text "comments"
    t.string "vendor", limit: 255
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.date "date_entered"
  end

  create_table "bacteria", id: :serial, force: :cascade do |t|
    t.integer "strain_number"
    t.string "species_bkg", limit: 255
    t.date "date_entered"
    t.string "entered_by", limit: 255
    t.string "notebook", limit: 255
    t.text "genotype"
    t.text "comments"
    t.string "plasmid_number", limit: 255
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.text "sequence"
    t.string "strainalias", limit: 255
    t.string "intrinsic_resistance"
  end

  create_table "dinos", force: :cascade do |t|
    t.integer "number"
    t.string "alias"
    t.string "description"
    t.string "entered_by"
    t.string "notebook"
    t.datetime "created_at", precision: 6, null: false
    t.datetime "updated_at", precision: 6, null: false
    t.string "species"
    t.string "genotype"
    t.string "selectable_markers"
  end

  create_table "lines", id: :serial, force: :cascade do |t|
    t.integer "line_number"
    t.string "line_alias", limit: 255
    t.date "date_entered"
    t.string "entered_by", limit: 255
    t.string "notebook", limit: 255
    t.string "species", limit: 255
    t.text "parent_line"
    t.text "sequence"
    t.text "description"
    t.string "plasmid_numbers", limit: 255
    t.string "selectable_markers", limit: 255
    t.text "locations"
    t.text "current_stock_counts"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.text "genotype"
    t.text "stock_person"
    t.text "stock_date"
  end

  create_table "oligos", id: :serial, force: :cascade do |t|
    t.integer "oligo_number"
    t.string "oligoalias", limit: 255
    t.date "date_entered"
    t.string "entered_by", limit: 255
    t.string "notebook", limit: 255
    t.string "vendor", limit: 255
    t.string "organism", limit: 255
    t.text "sequence"
    t.text "purpose"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "plasmids", id: :serial, force: :cascade do |t|
    t.date "date"
    t.string "creator", limit: 255
    t.string "notebook", limit: 255
    t.boolean "verified"
    t.string "alias", limit: 255
    t.string "antibiotic", limit: 255
    t.integer "plasmidsize"
    t.float "concentration"
    t.string "strainnumbers", limit: 255
    t.text "description"
    t.text "sequence"
    t.string "vector", limit: 255
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "plasmidmap_file_name", limit: 255
    t.string "plasmidmap_content_type", limit: 255
    t.integer "plasmidmap_file_size"
    t.datetime "plasmidmap_updated_at"
    t.integer "number"
  end

  create_table "rnai_clones", id: :serial, force: :cascade do |t|
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer "number"
    t.text "alias"
    t.text "notebook"
    t.text "description"
    t.text "entered_by"
    t.text "sequence_name"
    t.text "library"
    t.text "host_strain"
    t.text "plasmid_backbone"
    t.text "antibiotic"
    t.text "location"
    t.boolean "sequenced"
  end

  create_table "samples", id: :serial, force: :cascade do |t|
    t.integer "sample_number"
    t.string "sample_alias", limit: 255
    t.string "storage_type", limit: 255
    t.date "date_entered"
    t.string "entered_by", limit: 255
    t.string "notebook", limit: 255
    t.string "sample_type", limit: 255
    t.boolean "depleted"
    t.text "description"
    t.text "linked_items"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "searches", id: :serial, force: :cascade do |t|
    t.text "searchparams"
    t.integer "user_id"
    t.date "expires"
    t.text "result"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "seq_libs", id: :serial, force: :cascade do |t|
    t.datetime "created_at"
    t.datetime "updated_at"
    t.text "genome"
    t.text "method"
    t.text "entered_by"
    t.text "project"
    t.text "storage_location"
    t.decimal "concentration"
    t.text "size_distribution"
    t.text "index_id"
    t.text "index_seq"
    t.text "description"
    t.text "linked_items"
    t.text "alias"
    t.text "notebook"
    t.integer "number"
  end

  create_table "users", id: :serial, force: :cascade do |t|
    t.string "name", limit: 255
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "email", limit: 255
    t.boolean "auth_read"
    t.boolean "auth_write"
    t.boolean "auth_admin"
    t.text "notes"
  end

  create_table "yeaststrains", id: :serial, force: :cascade do |t|
    t.string "strainalias", limit: 255
    t.string "antibiotic", limit: 255
    t.string "plasmidnumber", limit: 255
    t.integer "strain_number"
    t.string "strain_bkg", limit: 255
    t.date "date_entered"
    t.text "sequence"
    t.string "entered_by", limit: 255
    t.text "comments"
    t.text "genotype"
    t.string "location", limit: 255
    t.string "species", limit: 255
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "notebook", limit: 255
  end

end
