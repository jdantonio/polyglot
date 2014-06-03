require 'algorithms/catalog'
require 'algorithms/collection'
require 'algorithms/inflect'
require 'algorithms/search'
require 'algorithms/sort'
require 'algorithms/utilities'
require 'algorithms/version'

Infinity = 1/0.0 unless defined?(Infinity)
NaN = 0/0.0 unless defined?(NaN)

module Algorithms

  class << self
    include Collection
    include Inflect
    include Search
    include Sort
  end
end
