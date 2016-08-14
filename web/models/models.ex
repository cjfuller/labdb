defmodule Model do
  # TODO: more complete types here
  @callback core_info(Ecto.Model) :: List
  @callback sequence_info() :: Map
  @callback supplemental_info() :: List

  require Ecto.Query

  defmacro __using__(_) do
    mod = __CALLER__.module
    quote do
      use Ecto.Schema

      def number_field_name, do: @number_field_name
      def info_field_name, do: @info_field_name
      def description_field_name, do: @description_field_name
      def core_alt_field_name, do: @core_alt_field_name
      def owner_field_name, do: @owner_field_name
      def timestamp_field_name, do: @timestamp_field_name

      def get_display_name(name) do
        Map.get(@display_names, name)
      end

      def field(name, type \\ :value) do
        %{name: get_display_name(name), lookup: name, type: type}
      end

      def fields(names), do: Enum.map(names, &field/1)

      def core_links(item) do
        case Map.get(item, core_alt_field_name()) do
          nil -> []
          value -> value
          |> String.split(",")
          |> Enum.map(&String.trim/1)
        end
        # TODO: insert appropriate name prefix
      end

      def as_resource_def(item) do
        field_data = Map.keys(@display_names)
        |> Enum.map(fn k -> {k, Map.get(item, k)} end)
        |> Enum.into(%{})

        links = case core_alt_field_name() do
                       nil -> nil
                       :depleted -> nil
                       name -> %{
                               lookup: name,
                               name: get_display_name(name),
                               links: (
                                 core_links(item)
                                 |> Enum.map(&AutoLinking.maybe_link_item/1)
                                 |> List.first
                               ) || [],
                           }
                     end
        %{
          type: Model.type_of(item),
          id: item.id,
          timestamp: Map.get(item, timestamp_field_name()),
          fieldData: field_data,
          resourcePath: "/#{Model.type_of(item)}/#{item.id}",
          name: Model.named_number_string(item),
          shortDesc: %{
            lookup: info_field_name(),
            inlineValue: (
              (Map.get(item, info_field_name()) || "")
              |> AutoLinking.auto_link
            ),
            name: "Alias",
          },
          coreLinks: links,
          coreInfoSections: unquote(mod).core_info(item),
          sequenceInfo: unquote(mod).sequence_info(),
          supplementalFields: unquote(mod).supplemental_info(),
          numberFieldName: to_string(number_field_name()),
        }
      end
    end
  end

  def get(type, id) do
    module_for_type(type)
    |> Labdb.Repo.get(id)
  end

  def delete(type, id) do
    get(type, id)
    |> Labdb.Repo.delete!
  end

  def new(type, current_user) do
    mod = module_for_type(type)
    next_num = Labdb.Numbering.assign_number(mod)
    num_field = mod.number_field_name
    owner_field = mod.owner_field_name
    date_field = mod.timestamp_field_name

    curr_time_utc = Ecto.DateTime.utc

    result = struct(mod, %{
          num_field => next_num,
          owner_field => current_user.name,
          date_field => Ecto.DateTime.to_date(curr_time_utc),
          created_at: curr_time_utc,
          updated_at: curr_time_utc,
                    })
    |> Labdb.Repo.insert

    Labdb.Numbering.release(mod, next_num)

    {:ok, obj} = result
    obj
  end

  def get_list(type, direction: :desc) do
    module_for_type(type)
    |> Ecto.Query.order_by(desc: :id)
    |> Ecto.Query.limit(100)
    |> Labdb.Repo.all
  end

  def apply_updates(type, id, fields) do
    item = get(type, id)
    fields = Map.put(fields, :updated_at, Ecto.DateTime.utc)
    if item do
      ch = Ecto.Changeset.change(item, fields)

      {:ok, item} = ch |> Labdb.Repo.update
    end
  end

  def type_of(item) do
    item.__struct__
    |> to_string
    |> String.downcase
    |> String.replace_leading("elixir.", "")
  end

  defp plurals do
    %{
      "plasmids" => "plasmid",
      "oligos" => "oligo",
      "bacteria" => "bacterialstrain",
      "lines" => "line",
      "yeaststrains" => "yeaststrain",
      "samples" => "sample",
      "users" => "user",
      "antibodies" => "antibody",
    }
  end

  def depluralize(type) do
    Map.get(plurals, type)
  end

  def pluralize(type) do
    Enum.find(plurals, fn {k, v} -> v == type end)
    |> elem(0)
  end

  def module_for_type(type) do
    type
    |> String.capitalize
    |> (fn s -> "Elixir." <> s end).()
    |> String.to_atom
  end

  def named_number_string(item) do
    Names.name_for_model(type_of(item), item.id)
  end

end


defmodule Plasmid do
  @behaviour Model

  @number_field_name :plasmidnumber
  @info_field_name :plasmidalias
  @description_field_name :description
  @core_alt_field_name :strainnumbers
  @owner_field_name :enteredby
  @timestamp_field_name :date_entered

  @display_names %{
    antibiotic: "Antibiotic resistances",
    concentration: "Concentration (μg/mL)",
    date_entered: "Date",
    description: "Description",
    enteredby: "Entered by",
    notebook: "Notebook",
    plasmidalias: "Alias",
    plasmidnumber: "Plasmid Number",
    sequence: "Sequence",
    strainnumbers: "Strain Numbers",
    vector: "Vector",
    verified: "Sequence verified?",
  }

  use Model

  schema "plasmids" do
    field :date_entered, Ecto.Date
    field :enteredby, :string
    field :notebook, :string
    field :verified, :boolean
    field :plasmidalias, :string
    field :antibiotic, :string
    field :concentration, :float
    field :strainnumbers, :string
    field :description, :string
    field :sequence, :string
    field :vector, :string
    field :created_at, Ecto.DateTime
    field :updated_at, Ecto.DateTime
    field :plasmidnumber, :integer
  end

  def core_info(p) do
    [
      %{
        name: "Vector information",
        fields: fields([:vector, :antibiotic]),
      },
      %{
        name: "Description",
        preformatted: true,
        lookup: :description,
        single: true,
        # TODO: markdown
        inlineValue: (
          (p.description || "")
          |> Earmark.to_html
          |> AutoLinking.auto_link),
      },
    ]
  end

  def sequence_info do
    %{
      sequence: %{ lookup: :sequence },
      verified: %{ lookup: :verified },
    }
  end

  def supplemental_info, do: fields([:enteredby, :date_entered, :notebook, :concentration])
end

defmodule Antibody do
  @behaviour Model

  @number_field_name :ab_number
  @info_field_name :alias
  @description_field_name :comments
  @core_alt_field_name nil
  @owner_field_name :entered_by
  @timestamp_field_name :date_entered

  @display_names %{
    ab_number: "Antibody Number",
    date_entered: "Date entered",
    label: "Label",
    entered_by: "Entered by",
    alias: "Alias",
    comments: "Description",
    host: "Host",
    vendor: "Vendor",
    good_for_if: "Good for IF",
    good_for_western: "Good for westerns",
    fluorophore: "Fluorophores",
    box: "Box"
  }

  use Model
  schema "antibodies" do
    field :ab_number, :integer
    field :host, :string
    field :label, :string
    field :box, :string
    field :alias, :string
    field :fluorophore, :string
    field :entered_by, :string
    field :good_for_if, :boolean
    field :good_for_western, :boolean
    field :comments, :string
    field :vendor, :string
    field :created_at, Ecto.DateTime
    field :updated_at, Ecto.DateTime
    field :date_entered, Ecto.Date
  end

  def core_info(ab) do
    [
      %{
        name: "Antibody information",
        fields: fields([:host, :fluorophore]),
      },
      %{
        name: "Location information",
        fields: fields([:box, :label]),
      },
      %{
        name: "Uses",
        fields: [
          field(:good_for_if, :boolean),
          field(:good_for_western, :boolean)
        ],
      },
      %{
        name: "Description",
        preformatted: true,
        lookup: :comments,
        single: true,
        inlineValue: (
          (ab.comments || "")
          |> Earmark.to_html
          |> AutoLinking.auto_link
        ),
      }
    ]
  end

  def supplemental_info, do: fields([:entered_by, :date_entered, :vendor])

  def sequence_info, do: nil
end

defmodule Bacterialstrain do
  @behaviour Model

  @number_field_name :strain_number
  @info_field_name :strainalias
  @description_field_name :comments
  @core_alt_field_name :plasmid_number
  @owner_field_name :entered_by
  @timestamp_field_name :date_entered

  @display_names %{
    strain_number: "Strain Number",
    date_entered: "Date",
    entered_by: "Entered by",
    notebook: "Notebook",
    comments: "Description",
    plasmid_number: "Plasmid Number",
    species_bkg: "Species and background",
    genotype: "Genotype",
    sequence: "Sequence",
    strainalias: "Alias",
  }

  use Model
  schema "bacteria" do
    field :strain_number, :integer
    field :species_bkg, :string
    field :date_entered, Ecto.Date
    field :entered_by, :string
    field :notebook, :string
    field :genotype, :string
    field :comments, :string
    field :plasmid_number, :string
    field :created_at, Ecto.DateTime
    field :updated_at, Ecto.DateTime
    field :sequence, :string
    field :strainalias, :string
  end

  def core_info(bs) do
    [
      %{
        name: "Strain information",
        fields: fields([:species_bkg, :genotype])
      },
      %{
        name: "Description",
        preformatted: true,
        lookup: :comments,
        single: true,
        inlineValue: (bs.comments || "")
        |> Earmark.to_html
        |> AutoLinking.auto_link
      }
    ]
  end

  def supplemental_info, do: fields([:entered_by, :date_entered, :notebook])

  # TODO: show plasmid sequence if available
  def sequence_info, do: %{sequence: %{lookup: :sequence}, verified: nil}
end

defmodule Line do
  @behaviour Model

  @number_field_name :line_number
  @info_field_name :line_alias
  @description_field_name :description
  @core_alt_field_name :plasmid_numbers
  @owner_field_name :entered_by
  @timestamp_field_name :date_entered

  @display_names %{
    current_stock_counts: "Stock counts",
    date_entered: "Date entered",
    description: "Description",
    entered_by: "Entered by",
    line_alias: "Alias",
    # TODO(colin): what we call "lines" may change between databases
    line_number: "Number",
    locations: "Locations",
    notebook: "Notebook",
    parent_line: "Parent line",
    plasmid_numbers: "Plasmid numbers",
    selectable_markers: "Selectable markers",
    sequence: "Associated sequence",
    species: "Species",
    genotype: "Genotype",
    stock_person: "Person",
    stock_date: "Date",
    stock_clone: "Clone",
  }

  use Model
  schema "lines" do
    field :line_number, :integer
    field :line_alias, :string
    field :date_entered, Ecto.Date
    field :entered_by, :string
    field :notebook, :string
    field :species, :string
    field :parent_line, :string
    field :sequence, :string
    field :description, :string
    field :plasmid_numbers, :string
    field :selectable_markers, :string
    field :locations, :string
    field :current_stock_counts, :string
    field :created_at, Ecto.DateTime
    field :updated_at, Ecto.DateTime
    field :genotype, :string
    field :stock_person, :string
    field :stock_date, :string
  end

  def core_info(line) do
    [
      %{
        name: "Line information",
        fields: fields([:species, :genotype, :selectable_markers, :parent_line]),
      },
      %{
        name: "Description",
        preformatted: true,
        single: true,
        lookup: :description,
        inlineValue: (line.description || "")
        |> Earmark.to_html
        |> AutoLinking.auto_link,
      }
    ]
  end

  # TODO(colin): include linked plasmid sequence if available
  def sequence_info, do: %{sequence: %{lookup: :sequence}, verified: nil}

  def supplemental_info, do: fields([:entered_by, :date_entered, :notebook])

  # TODO(colin): inventory
end

defmodule Oligo do
  @behaviour Model

  @number_field_name :oligo_number
  @info_field_name :oligoalias
  @description_field_name :purpose
  @core_alt_field_name nil
  @owner_field_name :entered_by
  @timestamp_field_name :date_entered

  @display_names %{
    oligo_number: "Oligo Number",
    date_entered: "Date",
    entered_by: "Entered by",
    notebook: "Notebook",
    oligoalias: "Alias",
    purpose: "Description",
    sequence: "Sequence",
    organism: "Organism",
    vendor: "Vendor",
  }

  use Model

  schema "oligos" do
    field :oligo_number, :integer
    field :oligoalias, :string
    field :date_entered, Ecto.Date
    field :entered_by, :string
    field :notebook, :string
    field :vendor, :string
    field :organism, :string
    field :sequence, :string
    field :purpose, :string
    field :created_at, Ecto.DateTime
    field :updated_at, Ecto.DateTime
  end

  def core_info(oligo) do
    [%{
        name: "Description",
        preformatted: true,
        lookup: :purpose,
        single: true,
        inlineValue: (oligo.purpose || "")
        |> Earmark.to_html
        |> AutoLinking.auto_link,
     }]
  end

  def sequence_info, do: %{sequence: %{lookup: :sequence}, verified: nil}

  def supplemental_info, do: fields([:entered_by, :date_entered, :notebook, :organism, :vendor])
end

defmodule Sample do
  @behaviour Model

  @number_field_name :sample_number
  @info_field_name :sample_alias
  @description_field_name :description
  @core_alt_field_name :depleted
  @owner_field_name :entered_by
  @timestamp_field_name :date_entered

  @display_names %{
    date_entered: "Date",
    depleted: "Sample depleted?",
    description: "Description",
    entered_by: "Entered by",
    linked_items: "Linked to",
    notebook: "Notebook",
    sample_alias: "Alias",
    sample_number: "Sample number",
    sample_type: "Sample type",
    storage_type: "Storage location",
  }

  use Model
  schema "samples" do
    field :sample_number, :integer
    field :sample_alias, :string
    field :storage_type, :string
    field :date_entered, Ecto.Date
    field :entered_by, :string
    field :notebook, :string
    field :sample_type, :string
    field :depleted, :boolean
    field :description, :string
    field :linked_items, :string
    field :created_at, Ecto.DateTime
    field :updated_at, Ecto.DateTime
  end

  def core_info(sample) do
    [
      %{
        name: "Sample storage",
        fields: fields([:sample_type, :storage_type]),
      },
      %{
        name: "Description",
        preformatted: true,
        lookup: :description,
        single: true,
        inlineValue: (sample.description || "")
        |> Earmark.to_html
        |> AutoLinking.auto_link,
      },
      %{
        name: "Linked items",
        preformatted: true,
        lookup: :linked_items,
        inlineValue: (sample.linked_items || "")
        # TODO(colin): inline the descriptions of the linked items
        |> Earmark.to_html
        |> AutoLinking.auto_link
      },
    ]
  end

  def sequence_info, do: nil
  def supplemental_info, do: fields([:entered_by, :date_entered, :notebook])
end

defmodule User do
  @behaviour Model
  # TODO(colin): Make the id appear as the person's name

  @number_field_name nil
  @info_field_name :name
  @description_field_name :email
  @core_alt_field_name nil
  @owner_field_name :name
  @timestamp_field_name :created_at

  @display_names %{
    email: "E-mail",
    auth_read: "Read access",
    auth_write: "Write access",
    auth_admin: "Admin access",
    name: "Name",
    notes: "Notes",
    created_at: "Date",
  }

  use Model
  require Ecto.Query
  schema "users" do
    field :name, :string
    field :created_at, Ecto.DateTime
    field :updated_at, Ecto.DateTime
    field :email, :string
    field :auth_read, :boolean
    field :auth_write, :boolean
    field :auth_admin, :boolean
    field :notes, :string
  end

  def core_info(_user) do
    [%{
        name: "Permissions",
        fields: [
          field(:auth_read, :boolean),
          field(:auth_write, :boolean),
          field(:auth_admin, :boolean)
        ],
     }]
  end

  def sequence_info, do: nil

  def supplemental_info, do: fields([:name, :email, :notes, :created_at])

  def get_by_email(email) do
    Labdb.Repo.get_by(User, email: email)
  end

  def max_auth(user) do
    case user do
      %User{auth_admin: true} -> "admin"
      %User{auth_write: true} -> "write"
      %User{auth_read: true} -> "read"
      _ -> nil
    end
  end
end

defmodule Yeaststrain do
  @behaviour Model

  @number_field_name :strain_number
  @info_field_name :strainalias
  @description_field_name :comments
  @core_alt_field_name :plasmidnumber
  @owner_field_name :entered_by
  @timestamp_field_name :date_entered

  @display_names %{
    strain_number: "Strain Number",
    date_entered: "Date entered",
    entered_by: "Entered by",
    notebook: "Notebook",
    comments: "Description",
    plasmidnumber: "Plasmid Number",
    strain_bkg: "Strain background",
    genotype: "Genotype",
    antibiotic: "Antibiotics",
    location: "Location in freezer",
    sequence: "Sequence",
    species: "Species",
    strainalias: "Alias",
  }

  use Model
  schema "yeaststrains" do
    field :strainalias, :string
    field :antibiotic, :string
    field :plasmidnumber, :string
    field :strain_number, :integer
    field :strain_bkg, :string
    field :date_entered, Ecto.Date
    field :sequence, :string
    field :entered_by, :string
    field :comments, :string
    field :genotype, :string
    field :location, :string
    field :species, :string
    field :created_at, Ecto.DateTime
    field :updated_at, Ecto.DateTime
    field :notebook, :string
  end

  def core_info(ys) do
    [
      %{
        name: "Strain information",
        fields: fields([:species, :strain_bkg, :genotype, :antibiotic]),
      },
      %{
        name: "Description",
        preformatted: true,
        lookup: :comments,
        single: true,
        inlineValue: (ys.comments || "")
        |> Earmark.to_html
        |> AutoLinking.auto_link,
      }
    ]
  end

  # TODO(colin): embed plasmid sequence if available
  def sequence_info, do: %{sequence: %{lookup: :sequence}, verified: nil}

  def supplemental_info, do: fields([:entered_by, :date_entered, :notebook, :location])
end
