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

helpers do
  def format_number(number)
    number.to_s.reverse.gsub(/([0-9]{3}(?=([0-9])))/, "\\1#{','}").reverse
  end
end

get '/counts/:concept_id' do

  concept_id = URI.decode(params[:concept_id])
  index = $SF1.index{|concept| concept['name'] =~ /^#{concept_id}\./}

  halt(404) if index.nil?

  client = CensusApi::Client.new(API_KEY)

  concept = $SF1[index]
  fields = concept['variable'].collect{|var| var['name']}.join(',')
  data = client.sf1(fields, 'STATE:39')

  if data.is_a?(Hash)
    erb(:error, layout: true, locals: {api_request: concept['name'], api_response: data, request_fields: concept['variable']})
  else
    erb(:counts, layout: true, locals: {name: concept['name'], variables: concept['variable'], data: data.first})
  end
end

get '/' do

  #sample_size = 10
  #R.eval <<-REVAL
  #x <- rnorm(#{sample_size})
  #avg <- mean(x)
  #stddev <- sd(x)
  #REVAL

  erb(:index, layout: true, locals: {concepts: $SF1})
end
