require 'json'
require 'set'

require 'psych'

require 'authorization'
require 'auto_linked'
require 'helptext'
require 'number_assignment'
require 'object_naming'
require 'searching'
require 'standard_actions'

class ApplicationController < ActionController::Base
  include Searching
  include Authorization

  protect_from_forgery

  DATABASE_SYSTEM_NAME = Naming.name_for('database_full')
  DATABASE_SYSTEM_SHORT_NAME = Naming.name_for('database_short')

  prepend_before_filter :set_user_vars
  prepend_before_filter :set_js_version
  prepend_before_filter :force_https

  def force_https
    if Rails.env.production?
      redirect_to protocol: "https://" unless request.ssl?
    end
  end

  def set_js_version
    @production = Rails.env.production?
    @js_version = Labdb::Application.config.js_version
  end

  def set_user_vars
    @user_name ||= curr_username
    @user_email ||= curr_user_email
    @logged_in = (@user_name && @user_email)
  end

  def curr_user_obj
    return nil if curr_uid.nil?
    User.find_by_email(curr_uid)
  end

  def curr_username
    return nil unless curr_user_obj
    curr_user_obj.name
  end

  def curr_user_id
    return nil unless curr_user_obj
    curr_user_obj.id.to_i
  end

  def curr_user_email
    return nil unless curr_user_obj
    curr_user_obj.email
  end

  def generate_date(an_obj)
    an_obj.date_entered = Time.now
  end

  def generate_name(an_obj)
    if an_obj.class.respond_to? :owner_field_name then
      an_obj.send(an_obj.class.owner_field_name.to_s + '=',
                  curr_username)
    end
  end

  def define_ui_variables(opts_hash)
    opts_hash.each do |k, v|
      instance_variable_set("@#{k}", v)
    end
  end

  def self.headings_for(headings_list)
    headings_list.each_with_object do |field, obj|
      obj[field] = get_heading(field)
    end
  end

  def generate_object_number(cls, number_field_name)
    NumberAssignment.assignment_for_class(cls, number_field_name, session)
  end

  def do_export(obj)
    send_data(
      obj.export_to(params['exportformat'].to_sym),
      obj.get_export_params(params['exportformat'].to_sym)
    )
  end

  def preprocess_model_object(model_obj)
  end

  def index_order
    "#{@model_class.number_field_name} DESC"
  end

  def search
    search_term = params[:term]
    include_seq = params[:seq] == '1'
    types = params[:types] &&
            !params[:types].strip.empty? &&
            JSON.parse(params[:types].strip)
    person = params[:person] &&
             !params[:person].strip.empty? &&
             params[:person].strip
    linked_term = LinkableString.new(search_term)
    results = []
    if linked_term.item_links(items: true) then
      results += linked_term.item_links(items: true)
    end
    # TODO: don't hardcode these
    model_classes = [
      Plasmid, Oligo, Bacterium, Sample, Antibody, Line, Yeaststrain
    ]
    model_classes = model_classes.select { |t| types.include? t.name } if types
    model_classes.map do |cls|
      fields = [cls.description_field_name,
                cls.info_field_name]
      fields += [:sequence] if include_seq
      fields += [:genotype] if cls::Fields.include? :genotype
      seen_ids = Set.new
      @model_class = cls
      fields.each do |f|
        query = { f => search_term }
        partial_results = process_search_query(query, cls)
                          .reject { |r| seen_ids.include? r.id }

        if person then
          partial_results = partial_results.select do |r|
            r.send(r.class.owner_field_name).downcase.include? person.downcase
          end
        end
        partial_results.each do |r|
          seen_ids.add(r.id)
        end
        results += partial_results
      end
    end
    redirect_to results[0] if results.size == 1
    resources = results.map(&:as_resource_def)
    # TODO: icky hack to sort by date then id; fix.
    resources.sort_by! { |r| r[:timestamp] || (Date.new(1800, 1, 1) + r[:id].days) }.reverse!
    @search_results = JSON.generate(resources)
    @content_json = JSON.generate([])
    @user_name = current_user.name
    @user_auth = auth_scope
  end
end
