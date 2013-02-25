require 'psych'

class ApplicationController < ActionController::Base

  protect_from_forgery

  DATABASE_SYSTEM_NAME = "StraightlabDB"
  DATABASE_SYSTEM_SHORT_NAME = "ASDB"

  AUTH_FN = Rails.root.join('config/authusers.yml')
  ALLOWED_LABEL = "allowedusers"
  NAME_TAG = "name"
  EMAIL_TAG = "email"

  def denied
    render :text => "Access denied."
  end

  before_filter :require_authorization

  def require_authorization

    unless authorized? then

      redirect_to "/", notice: "Access denied."

    end

  end

  def load_auth

    @user_data = nil

    File.open(AUTH_FN, 'r') do |f|
      @user_data = Psych.load(f.read)
    end

    @users_loaded = true

  end

  def curr_username

    curr_uid = session[:user_id]

    return false if curr_uid.nil?

    curr_user = User.find_by_uid(curr_uid)

    curr_user.name

  end

  def generate_date(an_obj)
    an_obj.date_entered= Time.now
  end

  def generate_name(an_obj)
    if an_obj.respond_to?(:entered_by) then
      an_obj.entered_by = curr_username
    end

    if an_obj.respond_to?(:enteredby) then
      an_obj.enteredby = curr_username
    end
  end


  def authorized?

    curr_uid = session[:user_id]

    return false if curr_uid.nil?

    curr_user = User.find_by_uid(curr_uid)

    return false if curr_user.nil?

    load_auth unless @users_loaded

    allowed_users = @user_data[ALLOWED_LABEL]

    puts curr_user.inspect

    allowed_users.each do |u|

      puts u


      if curr_user.email == u[EMAIL_TAG] and curr_user.name == u[NAME_TAG] then
        return true
      end

    end

    false

  end

  def define_ui_variables(opts_hash)

    opts_hash.each do |k, v|

      instance_variable_set("@#{k}", v)

    end

  end


  def self.headings_for(headings_list)

    headings_list.inject({}) do |a, e|
      a[e]= get_heading(e)
      a
    end

  end

  def process_search_query(search_params, search_class)

    preprocessed_conditions = preprocess_search_query(search_params)

    conditions = Hash.new
    regex_conditions = Hash.new
    regex_detection_regex = /^\/.*\/$/
    search_params.each_key do |k|
      if search_params[k] and search_params[k] != "" and k != "verified" then #TODO: is there a better way to deal with the verified field?
        unless regex_detection_regex.match(search_params[k]) then
          #if a regex has not been entered:
          #substitute * for .* to turn the old filemaker-style glob syntax into a regex
          search_params[k].gsub!("*", ".*")
          #add start and end of string matchers to avoid, e.g., matching all plasmids with a 1 when searching for #1
          search_params[k]= "\A" + search_params[k] + "\Z"
          regex_conditions[k] = Regexp.new(search_params[k])
        else
          regex_conditions[k] = Regexp.new(search_params[k][1...(search_params[k].length-1)])
        end
      end
    end

    puts regex_conditions

    preprocessed_conditions.each do |k,v|
      regex_conditions[k] = Regexp.new(v)
    end

    preliminary_list = search_class.where(conditions)

    final_list = Array.new

    puts preliminary_list.size

    preliminary_list.each do |p|
      include_obj = true
      regex_conditions.each_key do |k|
        val = p.send(k.to_s).to_s
        unless regex_conditions[k].match(val) then
          include_obj = false
          break
        end
      end
      if include_obj then
        final_list << p
      end
    end

    final_list

  end

  def preprocess_search_query(search_params)
    {}
  end



end
