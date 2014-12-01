require 'rspec'

require_relative 'greed'

describe Greed do

  it 'properly scores [1,1,1,5,1]' do
    Greed.score(1,1,1,5,1).should eq 1150
  end

  it 'properly scores [2,3,4,6,2]' do
    Greed.score(2,3,4,6,2).should eq 0
  end

  it 'properly scores [3,4,5,3,3]' do
    Greed.score(3,4,5,3,3).should eq 350
  end

  it 'properly scores [1,5,1,2,4]' do
    Greed.score(1,5,1,2,4).should eq 250
  end

  it 'properly scores [5,5,5,5,5]' do
    Greed.score(5,5,5,5,5).should eq 600
  end
end
