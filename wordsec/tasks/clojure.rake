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
