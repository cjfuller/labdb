defmodule Labdb.Auth do
  @behaviour Plug
  import Plug.Conn, only: [get_session: 2, send_resp: 3, halt: 1]

  def init([]), do: false
  def call(conn, _opts) do
    case conn.request_path do
      "/" -> conn
      "/api/verify" -> conn
      _ ->
        if String.contains?(conn.request_path, "user") do
          require_auth_admin(conn)
        else
            case conn.method do
            "GET" ->
                require_auth_read(conn)
            "HEAD" ->
                require_auth_read(conn)
            _ ->
                require_auth_write(conn)
            end
        end
    end
  end

  def current_user(conn) do
    get_session(conn, :user_id)
    |> case do
         nil -> nil
         u -> User.get_by_email(u)
       end
  end

  def forbidden(conn) do
    conn
    |> send_resp(:forbidden, "")
    |> halt
  end

  def require_auth_read(conn) do
    case current_user(conn) do
      %User{auth_read: true} -> conn
      _ -> forbidden(conn)
    end
  end

  def require_auth_write(conn) do
    case current_user(conn) do
      %User{auth_write: true} -> conn
      _ -> forbidden(conn)
    end
  end

  def require_auth_admin(conn) do
    case current_user(conn) do
      %User{auth_admin: true} -> conn
      _ -> forbidden(conn)
    end
  end
end
