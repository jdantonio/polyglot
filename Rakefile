require 'rubygems'
require 'rspec'

$:.unshift 'tasks'
Dir.glob('tasks/**/*.rake').each do|rakefile|
  load rakefile
end

namespace :clojure do

  desc 'Run the Clojure program against a test file'
  task :run, [:file] do |t, args|
    args.with_defaults(file: 'data/TestdataC.txt')
    sh "clj clojure/WordSec.clj #{args[:file]}"
  end

  desc 'Run and time the Clojure program against a test file'
  task :time, [:file] do |t, args|
    args.with_defaults(file: 'data/TestdataC.txt')
    sh "clj clojure/WordSec-Timed.clj #{args[:file]}"
  end
end
namespace :ruby do
  
  desc 'Run the RSpec tests against the Ruby implementations'
  task :spec do
    sh 'bundle exec rspec -fd --color ruby/word_sec_spec.rb'
  end

  desc 'Run the Ruby program against a test file'
  task :run, [:file] do |t, args|
    args.with_defaults(file: 'data/TestdataC.txt')
    sh "ruby ruby/word_sec.rb #{args[:file]}"
  end
end
