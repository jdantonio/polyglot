require 'rubygems'
require 'xmlsimple'
require 'open-uri'

sf1 = XmlSimple.xml_in('sf1.xml')

# or

uri = 'http://www.census.gov/developers/data/sf1.xml'
sf1 = open(uri) do |f|
  XmlSimple.xml_in(f)
end
