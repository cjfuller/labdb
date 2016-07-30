use Mix.Config

config :labdb, :names,
# TODO(colin): make this configurable based on lab name
  Plasmid: "ASP",
  Oligo: "ASO",
  Yeaststrain: "ASYS",
  Bacterialstrain: "ASBS",
  Antibody: "ASAB",
  database_short: "ASDB",
  database_full: "StraightLabDB",
  Line: "ASTC",
  Sample: "SLS",
  User: "UID"
