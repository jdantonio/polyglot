# NOTE: Replace these tests with cucumber/aruba

require 'rubygems'
require 'rspec'

EXECUTABLE = 'WordSec.exe'

#####################################################################
# helper functions

def create_list(words)
  current = nil
  words.each do |word|
    node = Node.new(word, nil, current)
    current.next = node unless current.nil?
    current = node
  end
  current = current.previous until current.previous.nil?
  return current
end

def create_data_file(filename, data)
  File.open(filename, 'w') { |file| file.write(data) }
end

def execute_program(filename)
  return %x[ #{File.dirname(__FILE__)}/#{EXECUTABLE} #{filename} ]
end

def subshell_program(filename)
  return system("#{File.dirname(__FILE__)}/#{EXECUTABLE} #{filename}")
end

#####################################################################
# specification

describe "WordSec.cpp" do

  define_method(:argv) { @argv ||= ['program_1.rb'] }
  define_method(:argc) { argv.size }

  before(:all) do
    %x[ make ]
  end

  context "command-line" do

    context "when the data file name is not given" do

      it "returns a non-zero error code" do
        status = subshell_program('')
        $?.exitstatus.should_not == 0
      end

      it "displays an error message" do
        output = execute_program('')
        output.should =~ /usage/i
      end

    end

    context "when the data file does not exist" do

      let(:test_file) { 'garbage_output_file.txt' }

      it "returns a non-zero error code" do
        status = subshell_program(test_file)
        $?.exitstatus.should_not == 0
      end

      it "displays an error message" do
        output = execute_program(test_file)
        output.should =~ /file error/i
      end

    end

    context "when the data file cannot be opened" do

      let(:test_file) { 'test_file.txt' }

      before(:each) do
        FileUtils.touch(test_file)
        FileUtils.chmod(0000, test_file)
      end

      after(:each) do
        FileUtils.rm(test_file)
      end

      it "returns a non-zero error code" do
        status = subshell_program(test_file)
        $?.exitstatus.should_not == 0
      end

      it "displays an error message" do
        output = execute_program(test_file)
        output.should =~ /file error/i
      end

    end

  end

  context "output" do

    let(:test_file) { 'test_file.txt' }

    let(:test_data) {
<<-DATA
A, ALSO, AND, ALWAYS
BE, BOTH 
be, both 
FULL-TIME, FOR
WHEN, WHERE, WHAT'S, WHOSE
youth
DATA
    }

    before(:each) do
      create_data_file(test_file, test_data)
    end

    after(:each) do
      begin
        FileUtils.rm(test_file)
      rescue Errno::ENOENT => e
        # ignore 'file not found' errors
      end
    end

    it "displays the input file name" do
      output = execute_program(test_file)
      output.should =~ /#{test_file}/i
    end

    it "displays the total word count" do
      output = execute_program(test_file)
      output.should =~ /15 total words/i
    end

    it "tallies the highest per-letter word count" do
      output = execute_program(test_file)
      output.should =~ /highest word count was 4/i
    end

    it "determines the letter with the most unique words" do
      output = execute_program(test_file)
      output.should =~ /that began words 4 times were\s+'a'\/'A'\s+'w'\/'W'/i
    end

    context "for each letter" do

      it "does not count duplicate words" do
        output = execute_program(test_file)
        output.should =~ /2 words beginning with 'b'/i
      end

      it "correctly tallies the words in each list" do
        output = execute_program(test_file)
        output.should =~ /4 words beginning with 'a'/i
        output.should =~ /2 words beginning with 'b'/i
        output.should =~ /2 words beginning with 'f'/i
        output.should =~ /4 words beginning with 'w'/i
        output.should =~ /1 word beginning with 'y'/i
      end

      it "does not tally for letters where no words exist" do
        excluded = (?a..?z).to_a.delete_if { |c| %{ a b f w y }.include? c }
        output = execute_program(test_file)
        excluded.each do |letter|
          output.should_not =~ /word beginning with '#{letter}'/i
        end
      end

      it "tallies the aggregate word count" do
        output = execute_program(test_file)
        output.should =~ /13 unique words/i
      end

      it "displays the word list in order found for each letter" do
        output = execute_program(test_file)
        output.should =~ /A, ALSO, AND, ALWAYS/i
        output.should =~ /BE, BOTH/i
        output.should =~ /FULL-TIME, FOR/i
        output.should =~ /WHEN, WHERE, WHAT'S, WHOSE/i
        output.should =~ /YOUTH/i
      end

    end
  end
end
