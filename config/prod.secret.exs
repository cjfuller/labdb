use Mix.Config

# In this file, we keep production configuration that
# you likely want to automate and keep it away from
# your version control system.
config :labdb, Labdb.Endpoint,
  secret_key_base: "Pk7PNvG+FRWV48U6MvE1Qkfw5fYaVmULn1HhVW3Vzid3uZZc63fujJPgz0yHrLga"

# Configure your database
config :labdb, Labdb.Repo,
  adapter: Ecto.Adapters.Postgres,
  database: "labdb",
  pool_size: 20
