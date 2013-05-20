require 'rubygems'
require 'xmlsimple'
require 'open-uri'
require 'census_api'
require 'pp'


require 'rinruby'
R.quit # get in the habit of creating per-use instances of R

#uri = 'http://www.census.gov/developers/data/sf1.xml'
#sf1 = open(uri) do |f|
  #XmlSimple.xml_in(f)
#end

API_KEY = ENV['API_KEY']

def format_number(number)
  number.to_s.reverse.gsub(/([0-9]{3}(?=([0-9])))/, "\\1#{','}").reverse
end

sf1 = XmlSimple.xml_in('sf1/sf1.xml');nil
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

counties = client.sf1('P0010001', 'COUNTY', 'STATE:39');nil
populations = counties.collect{|county| county['P0010001'].to_i}

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

#> summary(pop)
   #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #13440   37460   58190  131100  124700 1280000 
#> fivenum(pop)
#[1]   13435.0   37271.5   58185.5  124981.5 1280122.0
#> mean(pop)
#[1] 131096.6
#> sd(pop)
#[1] 212297.8
#> var(pop)
#[1] 45070347042
#> min(pop)
#[1] 13435
#> max(pop)
#[1] 1280122
#> median(pop)
#[1] 58185.5
#> range(pop)
#[1]   13435 1280122
#> quantile(pop)
        #0%        25%        50%        75%       100% 
  #13435.00   37456.75   58185.50  124728.25 1280122.00 

{:summary=>[13440.0, 37460.0, 58190.0, 131100.0, 124700.0, 1280000.0],
 :fivenum=>[13435.0, 37271.5, 58185.5, 124981.5, 1280122.0],
 :mean=>131096.63636363635,
 :sd=>212297.77917432156,
 :var=>45070347042.34901,
 :min=>13435.0,
 :max=>1280122.0,
 :median=>58185.5,
 :range=>[13435.0, 1280122.0],
 :quantile=>[13435.0, 37456.75, 58185.5, 124728.25, 1280122.0]}

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

[{"P0010001"=>"28550", "name"=>"Adams County", "state"=>"39", "county"=>"001"},
 {"P0010001"=>"106331",
  "name"=>"Allen County",
  "state"=>"39",
  "county"=>"003"},
 {"P0010001"=>"53139",
  "name"=>"Ashland County",
  "state"=>"39",
  "county"=>"005"},
 {"P0010001"=>"101497",
  "name"=>"Ashtabula County",
  "state"=>"39",
  "county"=>"007"},
 {"P0010001"=>"64757",
  "name"=>"Athens County",
  "state"=>"39",
  "county"=>"009"},
 {"P0010001"=>"45949",
  "name"=>"Auglaize County",
  "state"=>"39",
  "county"=>"011"},
 {"P0010001"=>"70400",
  "name"=>"Belmont County",
  "state"=>"39",
  "county"=>"013"},
 {"P0010001"=>"44846", "name"=>"Brown County", "state"=>"39", "county"=>"015"},
 {"P0010001"=>"368130",
  "name"=>"Butler County",
  "state"=>"39",
  "county"=>"017"},
 {"P0010001"=>"28836",
  "name"=>"Carroll County",
  "state"=>"39",
  "county"=>"019"},
 {"P0010001"=>"40097",
  "name"=>"Champaign County",
  "state"=>"39",
  "county"=>"021"},
 {"P0010001"=>"138333",
  "name"=>"Clark County",
  "state"=>"39",
  "county"=>"023"},
 {"P0010001"=>"197363",
  "name"=>"Clermont County",
  "state"=>"39",
  "county"=>"025"},
 {"P0010001"=>"42040",
  "name"=>"Clinton County",
  "state"=>"39",
  "county"=>"027"},
 {"P0010001"=>"107841",
  "name"=>"Columbiana County",
  "state"=>"39",
  "county"=>"029"},
 {"P0010001"=>"36901",
  "name"=>"Coshocton County",
  "state"=>"39",
  "county"=>"031"},
 {"P0010001"=>"43784",
  "name"=>"Crawford County",
  "state"=>"39",
  "county"=>"033"},
 {"P0010001"=>"1280122",
  "name"=>"Cuyahoga County",
  "state"=>"39",
  "county"=>"035"},
 {"P0010001"=>"52959", "name"=>"Darke County", "state"=>"39", "county"=>"037"},
 {"P0010001"=>"39037",
  "name"=>"Defiance County",
  "state"=>"39",
  "county"=>"039"},
 {"P0010001"=>"174214",
  "name"=>"Delaware County",
  "state"=>"39",
  "county"=>"041"},
 {"P0010001"=>"77079", "name"=>"Erie County", "state"=>"39", "county"=>"043"},
 {"P0010001"=>"146156",
  "name"=>"Fairfield County",
  "state"=>"39",
  "county"=>"045"},
 {"P0010001"=>"29030",
  "name"=>"Fayette County",
  "state"=>"39",
  "county"=>"047"},
 {"P0010001"=>"1163414",
  "name"=>"Franklin County",
  "state"=>"39",
  "county"=>"049"},
 {"P0010001"=>"42698",
  "name"=>"Fulton County",
  "state"=>"39",
  "county"=>"051"},
 {"P0010001"=>"30934",
  "name"=>"Gallia County",
  "state"=>"39",
  "county"=>"053"},
 {"P0010001"=>"93389",
  "name"=>"Geauga County",
  "state"=>"39",
  "county"=>"055"},
 {"P0010001"=>"161573",
  "name"=>"Greene County",
  "state"=>"39",
  "county"=>"057"},
 {"P0010001"=>"40087",
  "name"=>"Guernsey County",
  "state"=>"39",
  "county"=>"059"},
 {"P0010001"=>"802374",
  "name"=>"Hamilton County",
  "state"=>"39",
  "county"=>"061"},
 {"P0010001"=>"74782",
  "name"=>"Hancock County",
  "state"=>"39",
  "county"=>"063"},
 {"P0010001"=>"32058",
  "name"=>"Hardin County",
  "state"=>"39",
  "county"=>"065"},
 {"P0010001"=>"15864",
  "name"=>"Harrison County",
  "state"=>"39",
  "county"=>"067"},
 {"P0010001"=>"28215", "name"=>"Henry County", "state"=>"39", "county"=>"069"},
 {"P0010001"=>"43589",
  "name"=>"Highland County",
  "state"=>"39",
  "county"=>"071"},
 {"P0010001"=>"29380",
  "name"=>"Hocking County",
  "state"=>"39",
  "county"=>"073"},
 {"P0010001"=>"42366",
  "name"=>"Holmes County",
  "state"=>"39",
  "county"=>"075"},
 {"P0010001"=>"59626", "name"=>"Huron County", "state"=>"39", "county"=>"077"},
 {"P0010001"=>"33225",
  "name"=>"Jackson County",
  "state"=>"39",
  "county"=>"079"},
 {"P0010001"=>"69709",
  "name"=>"Jefferson County",
  "state"=>"39",
  "county"=>"081"},
 {"P0010001"=>"60921", "name"=>"Knox County", "state"=>"39", "county"=>"083"},
 {"P0010001"=>"230041", "name"=>"Lake County", "state"=>"39", "county"=>"085"},
 {"P0010001"=>"62450",
  "name"=>"Lawrence County",
  "state"=>"39",
  "county"=>"087"},
 {"P0010001"=>"166492",
  "name"=>"Licking County",
  "state"=>"39",
  "county"=>"089"},
 {"P0010001"=>"45858", "name"=>"Logan County", "state"=>"39", "county"=>"091"},
 {"P0010001"=>"301356",
  "name"=>"Lorain County",
  "state"=>"39",
  "county"=>"093"},
 {"P0010001"=>"441815",
  "name"=>"Lucas County",
  "state"=>"39",
  "county"=>"095"},
 {"P0010001"=>"43435",
  "name"=>"Madison County",
  "state"=>"39",
  "county"=>"097"},
 {"P0010001"=>"238823",
  "name"=>"Mahoning County",
  "state"=>"39",
  "county"=>"099"},
 {"P0010001"=>"66501",
  "name"=>"Marion County",
  "state"=>"39",
  "county"=>"101"},
 {"P0010001"=>"172332",
  "name"=>"Medina County",
  "state"=>"39",
  "county"=>"103"},
 {"P0010001"=>"23770", "name"=>"Meigs County", "state"=>"39", "county"=>"105"},
 {"P0010001"=>"40814",
  "name"=>"Mercer County",
  "state"=>"39",
  "county"=>"107"},
 {"P0010001"=>"102506",
  "name"=>"Miami County",
  "state"=>"39",
  "county"=>"109"},
 {"P0010001"=>"14642",
  "name"=>"Monroe County",
  "state"=>"39",
  "county"=>"111"},
 {"P0010001"=>"535153",
  "name"=>"Montgomery County",
  "state"=>"39",
  "county"=>"113"},
 {"P0010001"=>"15054",
  "name"=>"Morgan County",
  "state"=>"39",
  "county"=>"115"},
 {"P0010001"=>"34827",
  "name"=>"Morrow County",
  "state"=>"39",
  "county"=>"117"},
 {"P0010001"=>"86074",
  "name"=>"Muskingum County",
  "state"=>"39",
  "county"=>"119"},
 {"P0010001"=>"14645", "name"=>"Noble County", "state"=>"39", "county"=>"121"},
 {"P0010001"=>"41428",
  "name"=>"Ottawa County",
  "state"=>"39",
  "county"=>"123"},
 {"P0010001"=>"19614",
  "name"=>"Paulding County",
  "state"=>"39",
  "county"=>"125"},
 {"P0010001"=>"36058", "name"=>"Perry County", "state"=>"39", "county"=>"127"},
 {"P0010001"=>"55698",
  "name"=>"Pickaway County",
  "state"=>"39",
  "county"=>"129"},
 {"P0010001"=>"28709", "name"=>"Pike County", "state"=>"39", "county"=>"131"},
 {"P0010001"=>"161419",
  "name"=>"Portage County",
  "state"=>"39",
  "county"=>"133"},
 {"P0010001"=>"42270",
  "name"=>"Preble County",
  "state"=>"39",
  "county"=>"135"},
 {"P0010001"=>"34499",
  "name"=>"Putnam County",
  "state"=>"39",
  "county"=>"137"},
 {"P0010001"=>"124475",
  "name"=>"Richland County",
  "state"=>"39",
  "county"=>"139"},
 {"P0010001"=>"78064", "name"=>"Ross County", "state"=>"39", "county"=>"141"},
 {"P0010001"=>"60944",
  "name"=>"Sandusky County",
  "state"=>"39",
  "county"=>"143"},
 {"P0010001"=>"79499",
  "name"=>"Scioto County",
  "state"=>"39",
  "county"=>"145"},
 {"P0010001"=>"56745",
  "name"=>"Seneca County",
  "state"=>"39",
  "county"=>"147"},
 {"P0010001"=>"49423",
  "name"=>"Shelby County",
  "state"=>"39",
  "county"=>"149"},
 {"P0010001"=>"375586",
  "name"=>"Stark County",
  "state"=>"39",
  "county"=>"151"},
 {"P0010001"=>"541781",
  "name"=>"Summit County",
  "state"=>"39",
  "county"=>"153"},
 {"P0010001"=>"210312",
  "name"=>"Trumbull County",
  "state"=>"39",
  "county"=>"155"},
 {"P0010001"=>"92582",
  "name"=>"Tuscarawas County",
  "state"=>"39",
  "county"=>"157"},
 {"P0010001"=>"52300", "name"=>"Union County", "state"=>"39", "county"=>"159"},
 {"P0010001"=>"28744",
  "name"=>"Van Wert County",
  "state"=>"39",
  "county"=>"161"},
 {"P0010001"=>"13435",
  "name"=>"Vinton County",
  "state"=>"39",
  "county"=>"163"},
 {"P0010001"=>"212693",
  "name"=>"Warren County",
  "state"=>"39",
  "county"=>"165"},
 {"P0010001"=>"61778",
  "name"=>"Washington County",
  "state"=>"39",
  "county"=>"167"},
 {"P0010001"=>"114520",
  "name"=>"Wayne County",
  "state"=>"39",
  "county"=>"169"},
 {"P0010001"=>"37642",
  "name"=>"Williams County",
  "state"=>"39",
  "county"=>"171"},
 {"P0010001"=>"125488", "name"=>"Wood County", "state"=>"39", "county"=>"173"},
 {"P0010001"=>"22615",
  "name"=>"Wyandot County",
  "state"=>"39",
  "county"=>"175"}]

