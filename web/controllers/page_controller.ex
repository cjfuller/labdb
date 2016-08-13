defmodule Labdb.PageController do
  use Labdb.Web, :controller

  def raw_embed!(content) do
    content
    |> Poison.encode!
    |> Phoenix.HTML.raw
  end

  def home(conn, _params) do
    render conn, "page.html", %{
      content_json: raw_embed!(nil),
      user_name: raw_embed!(nil),
      user_auth: raw_embed!(nil),
      search_results: raw_embed!(nil),
      login_page: true,
      labdb_name: raw_embed!(Names.database_full),
    }
  end

  def get_item(conn, params) do
    user = Labdb.Auth.current_user(conn)
    %{"type" => type, "id" => id} = params
    resource = Model.get(type, String.to_integer(id))
    |> Model.module_for_type(type).as_resource_def
    render conn, "page.html", %{
      content_json: raw_embed!(resource),
      user_name: raw_embed!(user.name <> " (#{user.email}) "),
      user_auth: raw_embed!(User.max_auth(user)),
      search_results: raw_embed!([]),
      login_page: false,
      labdb_name: raw_embed!(Names.database_full),
    }
  end

  def get_index(conn, params) do
    user = Labdb.Auth.current_user(conn)
    type = Dict.get(params, "type") |> Model.depluralize
    # TODO(colin): support sort order
    # TODO(colin): support pages
    resources = Model.get_list(type, direction: :desc)
    # TODO(colin): reduce the representation for the list so we don't send all the data
    |> Enum.map(&Model.module_for_type(type).as_resource_def/1)
    collection = %{
      type: "collection",
      items: resources,
      objectType: type,
    }
    render conn, "page.html", %{
      content_json: raw_embed!(collection),
      user_name: raw_embed!(user.name <> " (#{user.email}) "),
      user_auth: raw_embed!(User.max_auth(user)),
      search_results: raw_embed!([]),
      login_page: false,
      labdb_name: raw_embed!(Names.database_full),
    }
  end
end
