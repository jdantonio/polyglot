require 'sinatra'
require 'census_api'
require 'xmlsimple'
require 'open-uri'
require 'rinruby'

get '/' do

  api_key = ENV['API_KEY']

  sample_size = 10

  html = "<p>Running R code...</p>"

  begin

    R.eval <<-REVAL
      x <- rnorm(#{sample_size})
      avg <- mean(x)
      stddev <- sd(x)
    REVAL

    html += "<p>Suceeded running R code...</p>"
    html += "<p>The sample is #{R.x}</p>"
    html += "<p>The mean is #{R.avg}</p>"
    html += "<p>The standard deviation is #{R.stddev}</p>"

    html += "<p>Retrieving Census XML data...</p>"
    uri = 'http://www.census.gov/developers/data/sf1.xml'
    sf1 = open(uri) do |f|
      XmlSimple.xml_in(f)
    end
    html += "<p>Succeeded in retrieving Census XML data...</p>"

    html += "<p>Accessing the Census API...</p>"

    client = CensusApi::Client.new(api_key)
    ohio = client.sf1('P0010001', 'STATE:39')
    population = ohio.first['P0010001'].reverse.gsub(/([0-9]{3}(?=([0-9])))/, "\\1#{','}").reverse
    html += "<p>In 2010 there were #{population} people in Ohio.</p>"

    html += "<p>Succeeded in accessing the Census API...</p>"

  rescue Exception => e
    html += "<p>Danger, Will Robinson!</p>"
    html += "<p>#{e.class} - #{e.message}</p>"
  ensure
    #R.quit
  end

  erb(html, layout: true)
end
