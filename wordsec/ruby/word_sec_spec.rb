require 'rubygems'
require 'rspec'

require_relative 'word_sec.rb'

require 'stringio'

#####################################################################
# helper functions

# http://thinkingdigitally.com/archive/capturing-output-from-puts-in-ruby/
module Kernel
  def capture_stdout
    out = StringIO.new
    $stdout = out
    yield
    return out.string
  ensure
    $stdout = STDOUT
  end
end

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

#####################################################################
# specification

describe "word_sec" do

  define_method(:argv) { @argv ||= [] }
  define_method(:argc) { argv.size }

  context "command-line" do

    context "when the data file name is not given" do

      it "returns a non-zero error code" do
        main(argc, argv).should_not == 0
      end

      it "displays an error message" do
        output = capture_stdout do
          main(argc, argv)
        end
        output.should =~ /input file name not given/i
      end

    end

    context "when the data file does not exist" do

      before(:each) do
        argv << 'garbage_output_file.txt'
      end

      it "returns a non-zero error code" do
        main(argc, argv).should_not == 0
      end

      it "displays an error message" do
        output = capture_stdout do
          main(argc, argv)
        end
        output.should =~ /No such file or directory/i
      end

    end

    context "when the data file cannot be opened" do

      let(:test_file) { 'test_file.txt' }

      before(:each) do
        FileUtils.touch(test_file)
        FileUtils.chmod(0000, test_file)
        argv << test_file
      end

      after(:each) do
        FileUtils.rm(test_file)
      end

      it "returns a non-zero error code" do
        main(argc, argv).should_not == 0
      end

      it "displays an error message" do
        output = capture_stdout do
          main(argc, argv)
        end
        output.should =~ /Permission denied/i
      end

    end

  end

  context "#parse_word_from_token" do

    it "rejects words that contain digits" do
      parse_word_from_token('abc1xyz').should be_nil
      parse_word_from_token('1abcxyz').should be_nil
      parse_word_from_token('abcxyz1').should be_nil
    end

    it "strips punctuation from the beginning of the word" do
      parse_word_from_token('?abcd').should == 'abcd'
    end

    it "strips punctuation from the end of the word" do
      parse_word_from_token('abcd!').should == 'abcd'
    end

    it "strips whitespace from the beginning of the word" do
      parse_word_from_token('  abcd').should == 'abcd'
      parse_word_from_token("\tabcd").should == 'abcd'
    end

    it "strips whitespace from the end of the word" do
      parse_word_from_token('abcd  ').should == 'abcd'
      parse_word_from_token("abcd\t").should == 'abcd'
    end

    it "retains dashes within words" do
      parse_word_from_token('abc-xyz').should == 'abc-xyz'
    end

    it "retains apostrophies within words" do
      parse_word_from_token("abc'xyz").should == "abc'xyz"
    end

    it "converts accepted words to lowercase" do
      parse_word_from_token('ABCD').should == 'abcd'
      parse_word_from_token('AbCd').should == 'abcd'
    end

  end

  context "#is_word_in_list?" do

    it "returns false when list is nil" do
      is_word_in_list?(nil, 'word').should be_false
    end

    it "returns false when the word is not in the list" do
      list = create_list( %w{ one two three four five six seven eight nine ten } )
      is_word_in_list?(list, 'zero').should be_false
    end

    it "returns true when the word is in the list" do
      list = create_list( %w{ one two three four five six seven eight nine ten } )
      is_word_in_list?(list, 'five').should be_true
    end

  end

  context "#insert_word_into_list" do

    it "creates a new list when the list is nil" do
      list = insert_word_into_list(nil, 'one')
      list.should_not be_nil
      list.word.should == 'one'
      list.previous.should be_nil
      list.next.should be_nil
    end

    it "inserts the new word at the head of the list" do
      list = create_list( %w{ one two three four five six seven eight nine ten } )
      list = insert_word_into_list(list, 'zero')
      list.word.should == 'zero'
      list.previous.should be_nil
      list.next.should_not be_nil
    end

    it "sets the list head #next pointer to the previous head" do
      list = create_list( %w{ one two three four five six seven eight nine ten } )
      list = insert_word_into_list(list, 'zero')
      list.next.word.should == 'one'
    end

    it "properly sets the #previous pointer of the old head node" do
      list = create_list( %w{ one two three four five six seven eight nine ten } )
      list = insert_word_into_list(list, 'zero')
      list.next.previous.should == list
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
      argv << test_file
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
      output = capture_stdout do
        main(argc, argv)
      end
      output.should =~ /#{test_file}/i
    end

    it "displays the total word count" do
      output = capture_stdout do
        main(argc, argv)
      end
      output.should =~ /15 total words/i
    end

    it "tallies the highest per-letter word count" do
      output = capture_stdout do
        main(argc, argv)
      end
      output.should =~ /highest word count was 4/i
    end

    it "determines the letter with the most unique words" do
      output = capture_stdout do
        main(argc, argv)
      end
      output.should =~ /that began words 4 times were\s+'a'\/'A'\s+'w'\/'W'/i
    end

    context "for each letter" do

      it "does not count duplicate words" do
        output = capture_stdout do
          main(argc, argv)
        end
        output.should =~ /2 words beginning with 'b'/i
      end

      it "correctly tallies the words in each list" do
        output = capture_stdout do
          main(argc, argv)
        end
        output.should =~ /4 words beginning with 'a'/i
        output.should =~ /2 words beginning with 'b'/i
        output.should =~ /2 words beginning with 'f'/i
        output.should =~ /4 words beginning with 'w'/i
        output.should =~ /1 word beginning with 'y'/i
      end

      it "does not tally for letters where no words exist" do
        excluded = (?a..?z).to_a.delete_if { |c| %{ a b f w y }.include? c }
        output = capture_stdout do
          main(argc, argv)
        end
        excluded.each do |letter|
          output.should_not =~ /word beginning with '#{letter}'/i
        end
      end

      it "tallies the aggregate word count" do
        output = capture_stdout do
          main(argc, argv)
        end
        output.should =~ /13 unique words/i
      end

      it "displays the word list in order found for each letter" do
        output = capture_stdout do
          main(argc, argv)
        end
        output.should =~ /A, ALSO, AND, ALWAYS/i
        output.should =~ /BE, BOTH/i
        output.should =~ /FULL-TIME, FOR/i
        output.should =~ /WHEN, WHERE, WHAT'S, WHOSE/i
        output.should =~ /YOUTH/i
      end

    end
  end
end
