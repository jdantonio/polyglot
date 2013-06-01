require 'open-uri'
require 'xmlsimple'

module Globals

  API_URI = 'http://www.census.gov/developers/data/sf1.xml'
  SF1 = open(API_URI) do |f|
    XmlSimple.xml_in(f)
  end['concept'].freeze

  API_KEY = ENV['API_KEY']

  # http://www.fcc.gov/developers/census-block-conversions-api
  FCC_API = "http://data.fcc.gov/api/block/find?format=json&latitude=!!!LAT!!!&longitude=!!!LONG!!!&showall=true"
end
