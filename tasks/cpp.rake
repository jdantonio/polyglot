namespace :cpp do

  CPP_DIR = 'cpp'

  FNAME_MAIN = 'WordSec'
  FNAME_OPT = 'WordSec-Optimized'
  FNAME_REC = 'WordSec-Recursive'

  task :clean do
    FileUtils.rm_rf Dir.glob("#{CPP_DIR}/{*.o,*.exe,*.dSYM}")
  end

  namespace :make do

    CXX = 'g++'
    CXXFLAGS = '-g -Wall -W -Winline -ansi'
    IFLAGS = '-Isrc -Itest -Iinclude'

    desc "Build the main C++ program"
    task :main do
      sh "#{CXX} #{CXXFLAGS} #{IFLAGS} #{CPP_DIR}/#{FNAME_MAIN}.cpp -o #{CPP_DIR}/#{FNAME_MAIN}.exe"
      FileUtils.chmod('+x', "#{CPP_DIR}/#{FNAME_MAIN}.exe")
    end

    desc "Build the optimized C++ program"
    task :optimized do
      sh "#{CXX} #{CXXFLAGS} #{IFLAGS} #{CPP_DIR}/#{FNAME_OPT}.cpp -o #{CPP_DIR}/#{FNAME_OPT}.exe"
      FileUtils.chmod('+x', "#{CPP_DIR}/#{FNAME_OPT}.exe")
    end

    desc "Build the recursive C++ program"
    task :recursive do
      sh "#{CXX} #{CXXFLAGS} #{IFLAGS} #{CPP_DIR}/#{FNAME_REC}.cpp -o #{CPP_DIR}/#{FNAME_REC}.exe"
      FileUtils.chmod('+x', "#{CPP_DIR}/#{FNAME_REC}.exe")
    end

    desc "Build all C++ programs"
    task :all => ['cpp:clean', :main, :optimized, :recursive]
  end

  namespace :run do

    desc "Run the main C++ program against a test file"
    task :main, [:file] do |t, args|
      args.with_defaults(file: 'data/TestdataC.txt')
      sh "#{CPP_DIR}/#{FNAME_MAIN}.exe #{args[:file]}"
    end

    desc "Run the optimized C++ program against a test file"
    task :optimized, [:file] do |t, args|
      args.with_defaults(file: 'data/TestdataC.txt')
      sh "#{CPP_DIR}/#{FNAME_OPT}.exe #{args[:file]}"
    end

    desc "Run the recursive C++ program against a test file"
    task :recursive, [:file] do |t, args|
      args.with_defaults(file: 'data/TestdataC.txt')
      sh "#{CPP_DIR}/#{FNAME_REC}.exe #{args[:file]}"
    end
  end

  desc "Run the main C++ program against a test file"
  task :run, [:file] do |t, args|
    Rake::Task['cpp:run:main'].invoke(*args)
  end
end
