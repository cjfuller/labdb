defmodule Labdb.APIController do
  use Labdb.Web, :controller

  def model_get(conn, params) do
    %{"type" => type, "id" => id} = params
    resource = Model.get(type, id)
    |> Model.module_for_type(type).as_resource_def
    json conn, resource
  end

  def model_list(conn, params) do
    %{"type" => type} = params
    # TODO: support sort order, paging
    resources = Model.get_list(type, direction: :desc)
    |> Enum.map(&Model.module_for_type(type).as_resource_def/1)
    content = %{
      type: "collection",
      resourcePath: "/" <> (Model.pluralize(type) |> String.downcase),
      items: resources,
      numberFieldName: Model.module_for_type(type).number_field_name,
    }
    json conn, content
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
    |> send_resp(:no_content, "")
  end

  def model_new(conn, params) do
    # TODO(colin): implement
  end

  def model_delete(conn, params) do
    %{"type" => type, "id" => id} = params
    Model.delete(type, id)
    conn
    |> send_resp(:no_content, "")
  end

  def model_copy(conn, params) do
    # TODO(colin): implement
  end

  def plasmid_map(conn, params) do
    json conn, nil
  end

  @app_id "146923434465-alq7iagpanjvoag20smuirj0ivdtfldk.apps.googleusercontent.com"

  def verify(conn, params) do
    %{"token" => token} = params
    verifier = "https://www.googleapis.com/oauth2/v3/tokeninfo?id_token=#{token}"
    resp = HTTPoison.get! verifier
    retval = nil
    with 200 <- resp.status_code,
         %{"aud" => @app_id, "email_verified" => "true", "email" => email} <- Poison.decode!(resp.body),
         %User{email: uid} <- User.get_by_email(email) do
      put_session(conn, :user_id, uid)
      |> send_resp(:no_content, "")
    else
      _ ->
        put_session(conn, :user_id, nil)
        |> send_resp(:forbidden, "")
    end
  end
end
