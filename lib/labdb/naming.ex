defmodule Names do

  def name_for_model(model, id) do
    obj = Model.get(model, id)
    if String.downcase(model) == "user" do
      obj.email
    else
      name = Application.fetch_env!(:labdb, :names)
      |> Keyword.get(model |> String.capitalize |> String.to_atom)

      name <> to_string(Map.get(obj, Model.module_for_type(model).number_field_name))
    end
  end

  def names do
  end

  def names_lookup do
  end

  def database_full do
    Application.fetch_env!(:labdb, :names)
    |> Keyword.get(:database_full)
  end
end
