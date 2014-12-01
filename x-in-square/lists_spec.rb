require 'rspec'
require_relative 'lists'

describe 'x in a square' do

  let!(:empty) do
    Array.new
  end

  let!(:four_by_four) do
    [
      [0, 1, 2, 3],
      [4, 5, 6, 7],
      [8, 9, 10, 11],
      [12, 13, 14, 15]
    ]
  end

  specify 'an empty array returns an empty array' do
    expect(x_in_square(empty)).to be_empty
  end

  specify 'a 4x4 grid from 0-15 returns [0, 3, 5, 6, 9, 10, 12, 15]' do
    expected = [0, 3, 5, 6, 9, 10, 12, 15]
    expect(x_in_square(four_by_four)).to eq expected
  end

  it 'does something if the input is invalid ???' do
    pending('decide what to do with invalid input')
  end
end
