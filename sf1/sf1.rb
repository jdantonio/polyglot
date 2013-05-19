require 'rubygems'
require 'xmlsimple'
require 'open-uri'
require 'census_api'
require 'pp'

#uri = 'http://www.census.gov/developers/data/sf1.xml'
#sf1 = open(uri) do |f|
  #XmlSimple.xml_in(f)
#end

API_KEY = ENV['API_KEY']

def format_number(number)
  number.to_s.reverse.gsub(/([0-9]{3}(?=([0-9])))/, "\\1#{','}").reverse
end

sf1 = XmlSimple.xml_in('sf1_1.xml');nil
concepts = sf1['concept'].freeze;nil

client = CensusApi::Client.new(API_KEY)

concepts.each do |concept|
  puts concept['name']

  fields = concept['variable'].collect{|var| var['name']}.join(',')
  data = client.sf1(fields, 'STATE:39').first
  concept['variable'].each do |var|
    puts "\t#{var['content']} for #{data['name']} is #{format_number(data[var['name']])}"
  end
end;nil

[{"name"=>"Geographies",
  "variable"=>
   [{"name"=>"NAME",
     "concept"=>"Geographies",
     "content"=>"Geographic Area Name"}]},
 {"name"=>"P1. Total Population [1]",
  "variable"=>
   [{"name"=>"P0010001",
     "concept"=>"P1. Total Population [1]",
     "content"=>"Total Population"}]},
 {"name"=>"P3. RACE [8]",
  "variable"=>
   [{"name"=>"P0030001",
     "concept"=>"P3. RACE [8]",
     "content"=>"Total population"},
    {"name"=>"P0030002", "concept"=>"P3. RACE [8]", "content"=>"White alone"},
    {"name"=>"P0030003",
     "concept"=>"P3. RACE [8]",
     "content"=>"Black or African American alone"},
    {"name"=>"P0030004",
     "concept"=>"P3. RACE [8]",
     "content"=>"American Indian and Alaska Native alone"},
    {"name"=>"P0030005", "concept"=>"P3. RACE [8]", "content"=>"Asian alone"},
    {"name"=>"P0030006",
     "concept"=>"P3. RACE [8]",
     "content"=>"Native Hawaiian and Other Pacific Islander alone"},
    {"name"=>"P0030007",
     "concept"=>"P3. RACE [8]",
     "content"=>"Some Other Race alone"},
    {"name"=>"P0030008",
     "concept"=>"P3. RACE [8]",
     "content"=>"Two or More Races"}]}]
