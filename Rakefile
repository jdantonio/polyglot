require 'rubygems'
require 'rspec'

$:.unshift 'tasks'
Dir.glob('tasks/**/*.rake').each do|rakefile|
  load rakefile
end

namespace :ruby do
  
  desc 'Run the RSpec tests against the Ruby implementations'
  task :spec do
    sh 'bundle exec rspec -fd --color ruby/word_sec_spec.rb'
  end
end
