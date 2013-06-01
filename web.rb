require 'sinatra'
require 'sinatra/partial'

require 'census_api'
require 'json'
require 'active_support/inflector'

require_relative 'lib/globals'
require_relative 'lib/geography'
require_relative 'lib/helpers'

require 'rinruby'
R.quit # kill global instance of R

helpers Helpers

set :partial_template_engine, :erb
enable :partial_underscores

get '/' do
  erb(:index, layout: true, locals: {concepts: Globals::SF1})
end

get '/counts/:concept_id' do

  concept = get_concept(params[:concept_id])

  halt(404) if concept.nil?

  client = CensusApi::Client.new(Globals::API_KEY)

  fields = concept['variable'].collect{|var| var['name']}.join(',')
  data = client.sf1(fields, 'STATE:39')

  if data.is_a?(Hash)
    erb(:error, layout: true, locals: {api_request: concept['name'], api_response: data, request_fields: concept['variable']})
  else
    erb(:counts, layout: true, locals: {name: concept['name'], variables: concept['variable'], data: data.first})
  end
end

post '/totals-for-state' do

  state = params[:state]
  concept = get_concept(params[:concept])

  halt(404) if concept.nil?

  client = CensusApi::Client.new(Globals::API_KEY)

  fields = concept['variable'].collect{|var| var['name']}.join(',')
  data = client.sf1(fields, "STATE:#{state}")

  if data.is_a?(Hash)
    erb(:error, layout: true, locals: {api_request: concept['name'], api_response: data, request_fields: concept['variable']})
  else
    erb(:counts, layout: true, locals: {name: concept['name'], state: Geography::STATE[state], variables: concept['variable'], data: data.first})
  end
end

post '/stats-by-county' do

  concept = get_concept(params[:concept])
  state = params[:state]

  halt(404) if concept.nil?

  client = CensusApi::Client.new(Globals::API_KEY)

  variable = concept['variable'].first['name']
  data = client.sf1(variable, 'COUNTY', "STATE:#{state}")

  if data.is_a?(Hash)
    erb(:error, layout: true, locals: {api_request: concept['name'], api_response: data, request_fields: concept['variable']})
  else
    populations = data.collect{|county| county[variable].to_i}

    r = RinRuby.new(false, false)
    r.assign('pop', populations)
    r.eval <<-REVAL
      library(pastecs)
      pop_summary <- as.numeric(summary(pop))
      pop_fivenum <- as.numeric(fivenum(pop))
      pop_mean <- as.numeric(mean(pop))
      pop_sd <- as.numeric(sd(pop))
      pop_var <- as.numeric(var(pop))
      pop_min <- as.numeric(min(pop))
      pop_max <- as.numeric(max(pop))
      pop_median <- as.numeric(median(pop))
      pop_range <- as.numeric(range(pop))
      pop_quantile <- as.numeric(quantile(pop))
    REVAL

    stats = {
      summary: r.pop_summary,
      fivenum: r.pop_fivenum,
      mean: r.pop_mean,
      sd: r.pop_sd,
      var: r.pop_var,
      min: r.pop_min,
      max: r.pop_max,
      median: r.pop_median,
      range: r.pop_range,
      quantile: r.pop_quantile
    }
    r.quit
    r = nil

    locals = {
      name: concept['name'],
      variable: variable,
      state: Geography::STATE[state],
      data: data,
      stats: stats
    }
    erb(:counties, layout: true, locals: locals)
  end
end

# http://localhost:9292/data-for-geography/P3/41.0814/-81.51928
get '/data-for-geography/:concept/:latitude/:longitude', :provides => :json do

  concept = get_concept(params[:concept])
  halt(404) if concept.nil?

  fcc = Globals::FCC_API.gsub(/!!!LAT!!!/, params[:latitude]).gsub(/!!!LONG!!!/, params[:longitude])
  blocks = JSON.parse(RestClient.get(fcc))

  pattern = /^(?<state>\d{2})(?<county>\d{3})(?<tract>\d{6})(?<block>(?<block_group>\d{1})\d{3})$/
  match = blocks['Block']['FIPS'].match(pattern)
  
  fields = concept['variable'].collect{|var| var['name']}.join(',')
  client = CensusApi::Client.new(Globals::API_KEY)
  data = client.sf1(fields, 'TABBLOCK', "STATE:#{match[:state]}+COUNTY:#{match[:county]}+TRACT:#{match[:tract]}")

  if data.is_a?(Hash)
    puts data
    halt(500)
  else
    {
      fips: blocks['Block']['FIPS'],
      geography: blocks,
      block: data.detect{|b| b['block'] == match[:block] },
      tract: data
    }.to_json
  end
end

# Sample return from '/data-for-geography'
# {
#   "fips": "391535083011017",
#   "geography": {
#     "Block": {
#       "FIPS": "391535083011017"
#     },
#     "County": {
#       "FIPS": "39153",
#       "name": "Summit"
#     },
#     "State": {
#       "FIPS": "39",
#       "code": "OH",
#       "name": "Ohio"
#     },
#     "status": "OK",
#     "executionTime": "8"
#   },
#   "block": {
#     "P0030001": "0",
#     "P0030002": "0",
#     "P0030003": "0",
#     "P0030004": "0",
#     "P0030005": "0",
#     "P0030006": "0",
#     "P0030007": "0",
#     "P0030008": "0",
#     "name": "Block 1017",
#     "state": "39",
#     "county": "153",
#     "tract": "508301",
#     "block": "1017"
#   },
#   "tract": [
#     {
#       "P0030001": "0",
#       "P0030002": "0",
#       "P0030003": "0",
#       "P0030004": "0",
#       "P0030005": "0",
#       "P0030006": "0",
#       "P0030007": "0",
#       "P0030008": "0",
#       "name": "Block 1000",
#       "state": "39",
#       "county": "153",
#       "tract": "508301",
#       "block": "1000"
#     },
#     {
#       "P0030001": "0",
#       "P0030002": "0",
#       "P0030003": "0",
#       "P0030004": "0",
#       "P0030005": "0",
#       "P0030006": "0",
#       "P0030007": "0",
#       "P0030008": "0",
#       "name": "Block 1001",
#       "state": "39",
#       "county": "153",
#       "tract": "508301",
#       "block": "1001"
#     }
#     # all the other tracks...
#   ]
# }
