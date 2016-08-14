defmodule Labdb.Numbering do
  @moduledoc """
  This module handles number reservation so that we don't accidentally get two
  items with the same user-facing id.  In order to do this, we use ETS to lock
  specific numbers by using insert_new combined with a uuid, and then verify
  that after making that call the current process's uuid is in ETS.

  Note that this will currently only work across a single server instance.
  TODO: back this with a shared database.
  """

  @server_name :labdb_numbering
  @ets_name :labdb_numbering_ets

  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, [name: @server_name])
  end

  defp server do
    @server_name
  end

  defp claim(model, number) do
    GenServer.call(server, {:claim, {model, number}})
  end

  def assign_number(model) do
    import Ecto.Query
    fields = [model.number_field_name]
    q = from m in model, select: map(m, ^fields), order_by: [desc: m.id], limit: 1
    case Labdb.Repo.one(q) do
      nil -> assign_number(model, 1)
      obj -> assign_number(model, Map.get(obj, model.number_field_name) + 1)
    end
  end

  defp number_in_db?(model, num) do
    !is_nil(Labdb.Repo.get_by(model, %{model.number_field_name => num}))
  end

  def assign_number(model, trynum) do
    if claim(model, trynum) do
      if !number_in_db?(model, trynum) do
        trynum
      else
        release(model, trynum)
        assign_number(model, trynum + 1)
      end
    else
      assign_number(model, trynum + 1)
    end
  end

  def release(model, number) do
    GenServer.call(server, {:release, {model, number}})
  end

  # Server callbacks

  def init(:ok) do
    e = :ets.new(@ets_name, [])
    {:ok, e}
  end

  def handle_call({:claim, id}, _from, e) do
    success? = :ets.insert_new(e, {id, true})
    {:reply, success?, e}
  end

  def handle_call({:release, id}, _from, e) do
    :ets.delete(e, id)
    {:reply, :ok, e}
  end
end
