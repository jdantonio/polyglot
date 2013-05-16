require 'rubygems'
require 'xmlsimple'
#require 'nokogiri'
require 'open-uri'
require 'pp'

sf1 = XmlSimple.xml_in('sf1_1.xml')

#sf1 = Nokogiri.XML(File.open('sf1_1.xml', 'rb'))

# or

#uri = 'http://www.census.gov/developers/data/sf1.xml'
#sf1 = open(uri) do |f|
  #XmlSimple.xml_in(f)
#end
