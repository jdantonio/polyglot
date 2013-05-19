require 'open-uri'
require 'xmlsimple'

module Globals

  API_URI = 'http://www.census.gov/developers/data/sf1.xml'
  SF1 = open(API_URI) do |f|
    XmlSimple.xml_in(f)
  end['concept'].freeze

  API_KEY = ENV['API_KEY']
end
