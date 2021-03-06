require 'standalone_migrations'
StandaloneMigrations::Tasks.load_tasks

task :watch => [:build_backend, :elm_api_code_generator, :serve_webpack_hot_reload]

task :build => [:build_backend, :elm_api_code_generator, :build_frontend]

task :serve_app => [:build, :serve]
task :build_prod => [:build, :install, :docker_build ]

multitask :serve_webpack_hot_reload => [:serve, :webpack_hot_reload]

task :build_backend do
  sh("cd server && stack build")
end

task :serve do
  sh("cd server && stack exec app")
end

task :webpack_hot_reload do
  sh("cd client && npm run watch")
end



task :build_frontend do
  sh("cd client && npm run build")
end

task :elm_api_code_generator do
  mkdir_p "client/elm/Generated/"
  sh("cd server && stack exec code-generator")
end

task :npm do
  sh("cd client && npm install")
end

task :elm do
  sh("cd client && elm package install --yes")
end

task :installers do
  sh("stack setup")
  sh("stack install hpack")
  sh("stack install intero")
  sh("npm install -g typescript")
  sh("cd client && npm install")
  sh("gem install pg")
  sh("gem install standalone_migrations")
end

task :install => :build do
  sh("cd server && stack install --local-bin-path bin")
end

task :copy_prod do
  # mkdir_p "server/bin/client/dist"
  sh("robocopy client/dist server/bin/client/dist /S")
end

task :docker_build do
  sh("Get-Content Dockerfile | docker build -")
end