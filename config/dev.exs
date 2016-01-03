use Mix.Config

config :labdb, Labdb.Endpoint,
  http: [port: 4000],
  debug_errors: true,
  code_reloader: true,
  cache_static_lookup: false,
  check_origin: false,
  watchers: [node: ["node_modules/brunch/bin/brunch", "watch", "--stdin"]]

config :labdb, Labdb.Endpoint,
  live_reload: [
    patterns: [
      ~r{public/.*(js|css|png|svg)$},
      ~r{web/views/.*(ex)$},
      ~r{web/templates/.*(eex)$}
    ]
  ]

config :phoenix, :stacktrace_depth, 20

config :labdb, Labdb.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "labdb",
  password: "CCSHUSET",
  database: "labdb",
  hostname: "localhost",
  pool_size: 10
