defmodule Labdb.APIController do
  use Labdb.Web, :controller

  def current_user do
    # TODO: implement, move somewhere else
    Model.get(User, 1)
  end

  def model_get(conn, params) do
    %{"type" => type, "id" => id} = params
    resource = Model.get(type, id)
    |> Model.module_for_type(type).as_resource_def
    json conn, resource
  end

  def model_put(conn, params) do
    %{"type" => type, "id" => id} = params
    rest = Map.drop(params, ["type", "id"])
    # TODO: is there a way to just use the strings?  Generally bad to convert
    # user-supplied strings to atoms.
    |> Enum.map(fn {k, v} -> {String.to_atom(k), v} end)
    |> Enum.into(%{})
    Model.apply_updates(type, id, rest)
    conn
    |> put_status(:no_content)
    |> send_resp(204, "")
  end

  def model_new(conn, params) do
    # TODO(colin): implement
  end

  def model_delete(conn, params) do
    # TODO(colin): implement
  end

  def model_copy(conn, params) do
    # TODO(colin): implement
  end

  def plasmid_map(conn, params) do
    json conn, nil
  end
end
