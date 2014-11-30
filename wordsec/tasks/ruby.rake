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
