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
  p fields
  data = client.sf1(fields, 'STATE:39').first
  pp data

  #concept['variable'].each do |var|
    #data = client.sf1(var['name'], 'STATE:39').first
    #puts "\t#{var['content']} for #{data['name']} is #{format_number(data[var['name']])}"
  #end
end
