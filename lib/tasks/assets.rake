Rake::Task["assets:precompile"].clear
namespace :assets do
  task "precompile" do
    puts "Skipping assets pipeline; already precompiled with webpack."
  end
end
