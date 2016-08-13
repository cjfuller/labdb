defmodule Names do

  def name_for_model(model, id) do
    if String.downcase(model) == "user" do
      Model.get(model, id).email
    else
      name = Application.fetch_env!(:labdb, :names)
      |> Keyword.get(model |> String.capitalize |> String.to_atom)

      name <> to_string(id)
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
