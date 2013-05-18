require 'sinatra'
require 'census_api'
require 'xmlsimple'
require 'open-uri'
require 'rinruby'

$URI = 'http://www.census.gov/developers/data/sf1.xml'
$SF1 = open($URI) do |f|
  XmlSimple.xml_in(f)
end['concept'].freeze

API_KEY = ENV['API_KEY']

def format_number(number)
  number.to_s.reverse.gsub(/([0-9]{3}(?=([0-9])))/, "\\1#{','}").reverse
end

get '/counts/:concept_id' do

  html = ''

  concept_id = URI.decode(params[:concept_id])
  index = $SF1.index{|concept| concept['name'] =~ /^#{concept_id}\./}

  status(404);break if index.nil?

  client = CensusApi::Client.new(API_KEY)

  concept = $SF1[index]
  #pp concept
  #html += "<h2 class=\"featurette-heading\">Census Data Set '" + concept['name'] + "' for Ohio</h2>"

  fields = concept['variable'].collect{|var| var['name']}.join(',')
  #pp fields
  data = client.sf1(fields, 'STATE:39').first
  #pp data
  #html += '<p><ul>'
  #concept['variable'].each do |var|
    ##html+= "<li>#{var['content']} for #{data['name']} is #{format_number(data[var['name']])}</li>"
    #html+= "<li>#{var['content']} is #{format_number(data[var['name']])}</li>"
  #end
  #html += '</ul></p>'

  erb(:counts, layout: true, locals: {name: concept['name'], variables: concept['variable'], data: data})
end

get '/' do

  sample_size = 10
  html = ''

  #html += "<p>Running R code...</p>"

  begin

    #R.eval <<-REVAL
      #x <- rnorm(#{sample_size})
      #avg <- mean(x)
      #stddev <- sd(x)
    #REVAL

    #html += "<p>Suceeded running R code...</p>"
    #html += "<p>The sample is #{R.x}</p>"
    #html += "<p>The mean is #{R.avg}</p>"
    #html += "<p>The standard deviation is #{R.stddev}</p>"

    #html += "<p>Retrieving Census XML data...</p>"
    html += "<p><ul>"
    $SF1.each do |concept|
      name = concept['name']
      if name =~ /([A-Z]\d+)\./
        html += "<li><a href='" + url("/counts/#{$1}") + "'>" + name + '</a></li>'
        #html += "<li><a href='" + url("/counts/#{URI.encode(name)}") + "'>" + name + '</a></li>'
      end
    end
    html += "</ul></p>"
    #html += "<p>Succeeded in retrieving Census XML data...</p>"

    #html += "<p>Accessing the Census API...</p>"

    #client = CensusApi::Client.new(API_KEY)
    #ohio = client.sf1('P0010001', 'STATE:39')
    #population = ohio.first['P0010001'].reverse.gsub(/([0-9]{3}(?=([0-9])))/, "\\1#{','}").reverse
    #html += "<p>In 2010 there were #{population} people in Ohio.</p>"

    #html += "<p>Succeeded in accessing the Census API...</p>"

  #rescue Exception => e
    #html += "<p>Danger, Will Robinson!</p>"
    #html += "<p>#{e.class} - #{e.message}</p>"
  #ensure
    ##R.quit
  end

  erb(html, layout: true)
end
