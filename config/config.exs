use Mix.Config

config :labdb, Labdb.Endpoint,
  url: [host: "localhost"],
  root: Path.dirname(__DIR__),
  secret_key_base: "nJTDMBXpLIDLAeJH2/ikGkM1FLglYqdx8ej9ORA99+fGY/YE7ZaTr7hmtrT2w2Mr",
  render_errors: [accepts: ~w(html json)],
  pubsub: [name: Labdb.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

import_config "#{Mix.env}.exs"

config :phoenix, :generators,
  migration: true,
  binary_id: false

config :labdb, Labdb.Router,
  static: [from: "/public"]
