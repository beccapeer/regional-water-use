#------------------------------------------------------------------------------|
#                   DATA PROCESSING CODE FOR EIA FORMS 860 and 923
# CREATOR: R.A.M. Peer
# DESCRIPTION: code to process and analyze reported data from the EIA to 
#              calculate regional water use for primary energy and electricity
#              generation.
#------------------------------------------------------------------------------|
#******************************************************************************|
#------------------------------------------------------------------------------|
# USER INPUT: set input parameters
#------------------------------------------------------------------------------|
# wd            = '~/Documents/Graduate/Journal Papers/in-progress/EST_regional-water-rates/data/'
# eia860.folder = '~/Documents/Graduate/Journal Papers/in-progress/EST_regional-water-rates/data/2014 EIA 860/'
# eia923.folder = '~/Documents/Graduate/Journal Papers/in-progress/EST_regional-water-rates/data/2014 EIA 923/'
wd            = '~/Desktop/Becca\ Files/ERL-paper/data/'
eia860.folder = '~/Desktop/Becca\ Files/ERL-paper/data/2014 EIA 860/'
eia923.folder = '~/Desktop/Becca\ Files/ERL-paper/data/2014 EIA 923/'


eia923.file.s2t5 = 'EIA923_Schedules_2_3_4_5_M_12_2014_Final_Revision.xlsx' #923 schedules 2 thru 5
eia923.file.s8d = 'EIA923_Schedule_8_Annual_Environmental_Information_2014_Final_Revision.xlsx' #923 schedule 8
eia860.file.s2 = '2___Plant_Y2014.xlsx' #860 schedule 2
eia860.file.s3a = '3_1_Generator_Y2014.xlsx' #860 schedule 3-1 Generator information
eia860.file.s6a = '6_1_EnviroAssoc_Y2014.xlsx' #860 schedule 6 Associations
eia860.file.envequip = '6_2_EnviroEquip_Y2014.xlsx' #860 schedule 6_2
wateruse.file = 'PE-water-use-rates.xlsx'
coolinguse.file = 'Electricity-water-use-rates.xlsx'
#output.dir    = '~/Documents/Graduate/Journal Papers/in-progress/EST_regional-water-rates'
#setwd(wd)

#------------------------------------------------------------------------------|
# MAIN SCRIPT
#------------------------------------------------------------------------------|

#load packages
library(data.table)
library(openxlsx)
library(plyr)
library(dplyr)
library(reshape2)


# read in spreadsheets ----------------------------------------------------

#EIA 923 spreadsheets
  #setwd('~')
  setwd(eia923.folder)
  page1.gensandfuel = as.data.table(read.xlsx(eia923.file.s2t5, sheet = 1, startRow = 6))   #fuel use data
  page3.gendata = as.data.table(read.xlsx(eia923.file.s2t5, sheet = 4, startRow = 6)) #generation data by generator ID
  page4.boilerfuel = as.data.table(read.xlsx(eia923.file.s2t5, sheet = 3, startRow = 6)) #boiler and fuel data 
  page5.purchasedfuel = as.data.table(read.xlsx(eia923.file.s2t5, sheet = 5, startRow = 5)) #purchased fuel data, including coal mine state & amount purchased
  schedule.8d = as.data.table(read.xlsx(eia923.file.s8d, sheet = 5, startRow = 5))
  

#EIA 860 spreadsheets
  #setwd('~')
  setwd(eia860.folder)
  plantinfo = as.data.table(read.xlsx(eia860.file.s2, sheet = 1, startRow = 2)) #information on location (lat, long)
  coolingwater.info = as.data.table(read.xlsx(eia860.file.envequip, sheet = 3, startRow = 2)) #information on cooling water source type and quality
  assn.boiler.gen = as.data.table(read.xlsx(eia860.file.s6a, sheet = 1, startRow = 2)) #associations between boiler ID and generator ID
  assn.boiler.cs = as.data.table(read.xlsx(eia860.file.s6a, sheet = 2, startRow = 2)) #associations between boiler ID and cooling system ID
  geninfo = as.data.table(read.xlsx(eia860.file.s3a, sheet =1, startRow = 2)) #information by generator - use this to associate combined cycle units
  
#Water use rates
  #setwd('~')
  #primary energy water use rates
  setwd(wd)
  pe.wateruse = as.data.table(read.xlsx(wateruse.file, sheet =1, startRow = 1)) #fuels in rows, water types in columns - units of GJ/m^3

  #cooling water use rates 
  setwd(wd)
  cooling.rates = as.data.table(read.xlsx(coolinguse.file, sheet = 2, startRow = 1))
  cooling.rates.cc = as.data.table(read.xlsx(coolinguse.file, sheet = 3, startRow = 1))
  
# define constants --------------------------------------------------------

#conversion factors
  ##GJ/GWh
  conversion.gj.gwh = 3600
  ##btu/kWh
  conversion.btu.kwh = 3412
  ##GJ/MMBTU
  conversion.gj.mmbtu = 1.05587
  ##gal/m^3
  conversion.gal.m3 = 264.172

#water consumption rates for primary fuels (excluding power plant cooling and combustion)
#taken from EG/KTS file "SupplementaryData-Grubert_Sanders_2017-WaterUSEnergySystem2014.xlsx"
#in units of m^3/GJ of energy delivered
  ##total water consumption
  # oil.tot.wc = as.numeric(all.wc[1, 7]) * conversion.gj.mmbtu
  # sub.ngp.tot.wc = as.numeric(all.wc[2,7]) * conversion.gj.mmbtu
  # bit.app.tot.wc = as.numeric(all.wc[3,7]) * conversion.gj.mmbtu
  # bit.int.tot.wc = as.numeric(all.wc[4,7]) * conversion.gj.mmbtu
  # bit.rmr.tot.wc = as.numeric(all.wc[5,7]) * conversion.gj.mmbtu
  # lig.gfc.tot.wc = as.numeric(all.wc[6,7]) * conversion.gj.mmbtu
  # lig.ngp.tot.wc = as.numeric(all.wc[7,7]) * conversion.gj.mmbtu
  # ng.tot.wc = as.numeric(all.wc[8,7]) * conversion.gj.mmbtu
  # ur.tot.wc = as.numeric(all.wc[9,7]) * conversion.gj.mmbtu
  # wat.tot.wc = as.numeric(all.wc[10,7]) * conversion.gj.mmbtu
  # wnd.tot.wc = as.numeric(all.wc[11,7]) * conversion.gj.mmbtu
  # brdf.tot.wc = as.numeric(all.wc[12,7]) * conversion.gj.mmbtu
  # bg.tot.wc = as.numeric(all.wc[13,7]) * conversion.gj.mmbtu
  # geo.tot.wc = as.numeric(all.wc[14,7]) * conversion.gj.mmbtu
  # spv.tot.wc = as.numeric(all.wc[15,7]) * conversion.gj.mmbtu
  # sth.tot.wc = as.numeric(all.wc[16,7]) * conversion.gj.mmbtu
  # sub.rmr.tot.wc = as.numeric(all.wc[17,7]) * conversion.gj.mmbtu
  # bit.avg.tot.wc = as.numeric(all.wc[18,7]) * conversion.gj.mmbtu
  # sub.avg.tot.wc = as.numeric(all.wc[19,7]) * conversion.gj.mmbtu
  
  
# make pivot tables -------------------------------------------------------

#pivoting reported fuel consumption (MMBTU) for EIA fuel codes 
  reported.fuels = dcast(page1.gensandfuel, Plant.Id~Reported.Fuel.Type.Code, value.var = 'Total.Fuel.Consumption.MMBtu', fun.aggregate = sum)
  
#pivoting for generation (by power plant)
  plant.generation = dcast(page1.gensandfuel, Plant.Id~Reported.Fuel.Type.Code, value.var = 'Net.Generation.(Megawatthours)', fun.aggregate = sum)
  plant.generation$total.gen = rowSums(plant.generation[,c(-1)])
  
#pivotting reported fuel use for coal by coal type and coal mine state
  #keep the records for coal purchases only (too many pipelines to track NG and Oil)
  purchased.coal = page5.purchasedfuel[which(page5.purchasedfuel$FUEL_GROUP=="Coal"),]
  #calculate total MMBTU used for each entry (multiplying average heat content of coal by physical units purchased. (MMBTU/ton *tons purchased)
  purchased.coal$MMBTU = purchased.coal$QUANTITY*purchased.coal$Average.Heat.Content 
  purchased.coal.states = dcast(purchased.coal, Plant.Id+Coalmine.State~ENERGY_SOURCE, value.var = 'MMBTU', fun.aggregate = sum)
  
  #isolate kentucky coal to assign East and West coal (all Bituminous coal)
  kentucky.coal = purchased.coal[which(purchased.coal$Coalmine.State == "KY"),]
  #use the county FIPS ID to determine if East or West
  kentucky.coal.cast = dcast(kentucky.coal, Plant.Id~Coalmine.County, value.var = 'MMBTU', fun.aggregate = sum)
  kentucky.coal.melted = melt(kentucky.coal.cast, id="Plant.Id")
  kentucky.coal.melted = kentucky.coal.melted[which(kentucky.coal.melted$value>0),]
  #classify Eastern Kentucky coal based on county FIPS ID
    #Eastern coal
  kentucky.coal.melted$east.west[kentucky.coal.melted$variable %in% c('13','19','25','51','65','71','95','115','119','121','127','131','133','153','159','193','195','203')] = 'KYE'
    #Western coal (note1: county 217, Taylor County, was determined to be West based on geographic location.)
    #(note2: Coal mine reporting 'NA' for FIPS ID was determined to be in Daviess County, FIPS ID=59)
  kentucky.coal.melted$east.west[kentucky.coal.melted$variable %in% c('59','107','139','149','177','183','217','225','233','NA')] = 'KYW'
  #get into same format as other purchased coal data 
  kentucky.eastwest = kentucky.coal.melted[,c(1,3,4)]
  #define coal region for Kentucky coal 
  kentucky.eastwest$coal.region[kentucky.eastwest$east.west %in% c('KYE')] = 'APPK' #Appalachia/Eastern - Kentucky
  kentucky.eastwest$coal.region[kentucky.eastwest$east.west %in% c('KYW')] = 'INTK' #Interior -  Kentucky
  kentucky.eastwest$energy.source = 'BIT'
  kentucky.eastwest.cast = dcast(kentucky.eastwest, Plant.Id~coal.region+energy.source, value.var = 'value', fun.aggregate = sum) 
  
  #Assign coal regions based on EG's definition of coal provinces
  purchased.coal = purchased.coal[which(purchased.coal$Coalmine.State != "KY")]
  purchased.coal$coal.region[purchased.coal$Coalmine.State %in% c('MT','ND','WY')] = 'NGP' #Northern Great Plains
  purchased.coal$coal.region[purchased.coal$Coalmine.State %in% c('AL','MD','OH','PA','TN','VA','WV')] = 'APPA' #Appalachia/Eastern (without Kentucky)
  purchased.coal$coal.region[purchased.coal$Coalmine.State %in% c('AR','IL','IN','KS','MO','OK')] = 'INTE' #Interior (without Kentucky)
  purchased.coal$coal.region[purchased.coal$Coalmine.State %in% c('LA','MS','TX')] = 'GFC' #Gulf Coast
  purchased.coal$coal.region[purchased.coal$Coalmine.State %in% c('AZ','CO','NM','UT')] = 'RMR' #Rocky Mountain Region
  purchased.coal$coal.region[purchased.coal$Coalmine.State %in% c('AU','CL','CN','IS','PL','RS','UK','VZ','OC','WA')] = 'OTH' #Other, other Countries & Washington state
  #redefine waste coal as bituminous coal (from EG's classifications)
  purchased.coal$ENERGY_SOURCE[purchased.coal$ENERGY_SOURCE %in% c('WC')] = 'BIT'
  #create pivot of coal.type_coal.region for each power plant
  purchased.coal.cast = dcast(purchased.coal, Plant.Id~coal.region+ENERGY_SOURCE, value.var = 'MMBTU', fun.aggregate = sum)
  
  #merge with Kentucky coal region allocation
  purchased.coal.m = merge(x=purchased.coal.cast, y=kentucky.eastwest.cast, all.x = TRUE, by.x=c('Plant.Id'), by.y=c('Plant.Id'), allow.cartesian = TRUE)
  purchased.coal.m$INT_BIT = purchased.coal.m$INTE_BIT + purchased.coal.m$INTK_BIT #add Kentucky coal for Interior region
  purchased.coal.m$APP_BIT = purchased.coal.m$APPA_BIT + purchased.coal.m$APPA_BIT #add Kentucky coal for Appalachia region
  purchased.coal.m[is.na(purchased.coal.m)] = 0
  #remove old Iterior and Appalachia columns
  purchased.coal.m$APPA_BIT = NULL
  purchased.coal.m$APPK_BIT = NULL
  purchased.coal.m$INTE_BIT = NULL
  purchased.coal.m$INTK_BIT = NULL
  
#pivoting for geothermal PP location
  geothermal.gen = page1.gensandfuel[which(page1.gensandfuel$Reported.Fuel.Type.Code=="GEO"),]
  geothermal.gen$geo.region[geothermal.gen$Plant.State %in% "CA"] = "GEO_CA"
  geothermal.gen$geo.region[geothermal.gen$Plant.State %in% "NV"] = "GEO_NV"
  geothermal.gen$geo.region[!(geothermal.gen$Plant.State %in% c("CA","NV"))] = "GEO"
  geo.mmbtu = dcast(geothermal.gen, Plant.Id~geo.region, value.var = 'Total.Fuel.Consumption.MMBtu', fun.aggregate = sum)
  geo.mmbtu$GEO_ALL = rowSums(geo.mmbtu[,c("GEO","GEO_CA","GEO_NV")])
    
# primary energy water use ------------------------------------------------

#calculate total fuel consumption (MMBTU) for each fuel category defined by EG
  plant.locations = plantinfo[,c(3,4,7,10,11)]
  fuel.categories$Plant.Id = reported.fuels[,c(1)]
  fuel.categories = as.data.frame(fuel.categories)

  ##individually deal with power plants that report "other" fuel classifications 
  other.fuels = reported.fuels[,c("Plant.Id","OTH","PUR","WH")] #gather all plants for "other" fuel categories
  other.fuels = other.fuels[which(rowSums(other.fuels[,2:4]) > 0),] #remove rows (plants) with all zeros 
  other.fuels.melted = melt(other.fuels, id = c("Plant.Id")) #restructure dataframe
  other.fuels.melted = other.fuels.melted[which(other.fuels.melted$value > 0),] #remove zeroes

  ##reclassify power plants based on EG's reclassification (searching for plant info online)
  other.fuels.melted$new.fuel[other.fuels.melted$Plant.Id %in% c(57842,57072,50305,54291)] = "OBIT" #reclassified as bituminous
  other.fuels.melted$new.fuel[other.fuels.melted$Plant.Id %in% c(10601,50304,50404,50633,52006,55066,55557,57938,52064,54224,50043,52065,52063,58197,58823,50487,50489,50488,50153,50205,5176857,1844732)] = "OOIL" #reclassified as oil
  other.fuels.melted$new.fuel[other.fuels.melted$Plant.Id %in% c(1745,50371,50388,50481,50624,55419,56163,58251,59383,57684,58216,10204,10434,10198,10004,50509,50274,54472,50473,50474,976021,2677147,1251259,1267121,370862,42680)] = "ONG" #reclassified as natural gas
  other.fuels.melted$new.fuel[other.fuels.melted$Plant.Id %in% c(10426,10208,57759,50424,58146,10466)] = "OBRDF" #reclassified as biomass and RDF
  other.fuels.melted$new.fuel[other.fuels.melted$Plant.Id %in% c(57172)] = "OSUB" #reclassified as subbituminous
  #other.fuels.melted$new.fuel[other.fuels.melted$Plant.Id %in% c(56294,50626,56258,56139,56833,56880,57281,57134)] = "UNK" #unknown fuels (weren't in EG's classifications)

  ##put back into same format as reported.fuels dataframe
  other.fuels.cast = dcast(other.fuels.melted, Plant.Id~new.fuel, value.var='value')
  other.fuels.cast[is.na(other.fuels.cast)] = 0

  ##merge reclassified data with reported data 
  reported.fuels.m = merge(x=reported.fuels,y=other.fuels.cast, all.x=TRUE, by.x = "Plant.Id", by.y = "Plant.Id")
  reported.fuels.m[is.na(reported.fuels.m)] = 0
  
  ##do calculations for primary energy for solar and wind based on generation (MWh)
  sun.wnd.gen = plant.generation[,c(1,34,41)]
  
  solar.gen = sun.wnd.gen[,c("Plant.Id","SUN")]
  solar.gen = solar.gen[which(solar.gen$SUN > 0),]
  solar.gen.melted = melt(solar.gen, id="Plant.Id")
  solar.gen.melted$new.fuel[solar.gen.melted$Plant.Id %in% c(6043,10437,10438,10439,10440,10441,10442,10443,10446,56405,56812,56943,57073,57074,57075,57323,57394,59060)] = "SUNT"
  solar.gen.melted[is.na(solar.gen.melted)] = "SUN"
  solar.gen.cast = dcast(solar.gen.melted, Plant.Id~new.fuel, value.var='value')
  solar.gen.cast[is.na(solar.gen.cast)] = 0
  
  sun.wnd.gen$SUN = NULL #remove old solar fuel column
  sun.wnd.gen = merge(x=sun.wnd.gen, y=solar.gen.cast, all.x=TRUE, by.x="Plant.Id", by.y="Plant.Id")
  sun.wnd.gen[is.na(sun.wnd.gen)] = 0
  
  #remove sun and wind MMBTU from reported fuels and merge back MWh's into reported fuels dataframe 
  reported.fuels.m$SUN = NULL
  reported.fuels.m$WND = NULL
  reported.fuels.m = merge(x=reported.fuels.m, y=sun.wnd.gen, all.x=TRUE, by.x="Plant.Id", by.y="Plant.Id")
  reported.fuels.m[is.na(reported.fuels.m)] = 0
  
  #Merge regional coal data back into reported fuels dataframe 
  reported.fuels.m$BIT = NULL
  reported.fuels.m$SUB = NULL
  reported.fuels.m$LIG = NULL
  
  reported.fuels.m = merge(x=reported.fuels.m, y=purchased.coal.m, all.x=TRUE, by.x="Plant.Id", by.y="Plant.Id")
  reported.fuels.m[is.na(reported.fuels.m)] = 0
  
  #merge geothermal regions back into reported fuels database
  reported.fuels.m$GEO = NULL
  reported.fuels.m = merge(x=reported.fuels.m,y=geo.mmbtu, all.x=T, by.x="Plant.Id", by.y="Plant.Id")
  reported.fuels.m[is.na(reported.fuels.m)] = 0
  
  ##reassign feul categories (EG's fuel categories)
  fuel.categories$Oil = rowSums(reported.fuels.m[,c("DFO","JF","KER","PC","PG","RFO","SGP","WO","OOIL")])
  fuel.categories$Subbituminous.Coal = rowSums(reported.fuels.m[,c("NGP_SUB", "RMR_SUB","OTH_SUB","OSUB")])
  fuel.categories$Bituminous.Coal.app = rowSums(reported.fuels.m[,c("APP_BIT","OTH_BIT","BFG","RC","SC","SGC","WC","OBIT")])
  fuel.categories$Bituminous.Coal.int = reported.fuels.m[,c("INT_BIT")]
  fuel.categories$Bituminous.Coal.rmr = reported.fuels.m[,c("RMR_BIT")]
  fuel.categories$Lignite.Coal.gfc = reported.fuels.m[,c("GFC_LIG")]
  fuel.categories$Lignite.Coal.ngp = reported.fuels.m[,c("NGP_LIG")]
  fuel.categories$Natural.Gas = rowSums(reported.fuels.m[,c("NG", "OG","ONG")])
  fuel.categories$Uranium = reported.fuels.m[,c("NUC")]
  fuel.categories$Hydropower = reported.fuels.m[,c("WAT")]
  fuel.categories$Wind = reported.fuels.m[,c("WND")]
  fuel.categories$Solid.Biomass.RDF = rowSums(reported.fuels.m[,c("AB","BLQ","MSN","MSB","OBL","OBS","SLW","TDF","WDL","WDS","OBRDF")])
  fuel.categories$Biogas= rowSums(reported.fuels.m[,c("LFG","OBG")])
  fuel.categories$Geothermal = reported.fuels.m[,c("GEO_ALL")]
  fuel.categories$Geothermal.CA = reported.fuels.m[,c("GEO_CA")]
  fuel.categories$Geothermal.NV = reported.fuels.m[,c("GEO_NV")]
  fuel.categories$Solar = reported.fuels.m[,c("SUN")]
  fuel.categories$Solar.Thermal = reported.fuels.m[,c("SUNT")]
    
  ##merge plant.locations with fuel.categories for primary energy MMBTU dataframe
  primary.energy.mmbtu = merge(x=plant.locations, y=fuel.categories, all.x=TRUE, by.x="Plant.Code", by.y="Plant.Id")
  primary.energy.mmbtu[,6:23][is.na(primary.energy.mmbtu[,6:23])] = 0
  byfuel.mmbtu = melt(colSums(primary.energy.mmbtu[,6:23]))
  
  for(i in c(2:7,9:18)){
    pe.wateruse[i,c(2:14)] = pe.wateruse[i,c(2:14)] / as.numeric(byfuel.mmbtu[i,1])
  }
 
  #convert oil and natural gas rates to mmbtu
  pe.wateruse[c(1,8),c(2:14)] = pe.wateruse[c(1,8),c(2:14)]*conversion.gj.mmbtu
  
#calculate water consumption for each primary energy by power plant
  pe.total.wc = as.data.frame(primary.energy.mmbtu)
  pe.gwbr.wc = as.data.frame(primary.energy.mmbtu)
  pe.gwfr.wc = as.data.frame(primary.energy.mmbtu)
  pe.gwnro.wc = as.data.frame(primary.energy.mmbtu)
  pe.gwsa.wc = as.data.frame(primary.energy.mmbtu)
  pe.pdbr.wc = as.data.frame(primary.energy.mmbtu)
  pe.pdfr.wc = as.data.frame(primary.energy.mmbtu)
  pe.pdnro.wc = as.data.frame(primary.energy.mmbtu)
  pe.pdsa.wc = as.data.frame(primary.energy.mmbtu)
  pe.swbr.wc = as.data.frame(primary.energy.mmbtu)
  pe.swfr.wc = as.data.frame(primary.energy.mmbtu)
  pe.swnro.wc = as.data.frame(primary.energy.mmbtu)
  pe.swsa.wc = as.data.frame(primary.energy.mmbtu)
  
  for(i in 1:18){
    pe.total.wc[,5+i] = pe.total.wc[,5+i]*as.numeric(pe.wateruse[i,2])
    pe.gwbr.wc[,5+i] = pe.gwbr.wc[,5+i]*as.numeric(pe.wateruse[i,3])
    pe.gwfr.wc[,5+i] =  pe.gwfr.wc[,5+i]*as.numeric(pe.wateruse[i,4])
    pe.gwnro.wc[,5+i] = pe.gwnro.wc[,5+i]*as.numeric(pe.wateruse[i,5])
    pe.gwsa.wc[,5+i] =  pe.gwsa.wc[,5+i]*as.numeric(pe.wateruse[i,6])
    pe.pdbr.wc[,5+i] =  pe.pdbr.wc[,5+i]*as.numeric(pe.wateruse[i,7])
    pe.pdfr.wc[,5+i] =  pe.pdfr.wc[,5+i]*as.numeric(pe.wateruse[i,8])
    pe.pdnro.wc[,5+i] = pe.pdnro.wc[,5+i]*as.numeric(pe.wateruse[i,9])
    pe.pdsa.wc[,5+i] =  pe.pdsa.wc[,5+i]*as.numeric(pe.wateruse[i,10])
    pe.swbr.wc[,5+i] =  pe.swbr.wc[,5+i]*as.numeric(pe.wateruse[i,11])
    pe.swfr.wc[,5+i] =  pe.swfr.wc[,5+i]*as.numeric(pe.wateruse[i,12])
    pe.swnro.wc[,5+i] = pe.swnro.wc[,5+i]*as.numeric(pe.wateruse[i,13])
    pe.swsa.wc[,5+i] =  pe.swsa.wc[,5+i]*as.numeric(pe.wateruse[i,14])
  }
  
  #create total water consumption columns
  pe.total.wc$total.consumption = rowSums(pe.total.wc[,c(6:23)]) 
  pe.gwbr.wc$total.consumption= rowSums(pe.gwbr.wc[,c(6:23)]) 
  pe.gwfr.wc$total.consumption= rowSums(pe.gwfr.wc[,c(6:23)]) 
  pe.gwnro.wc$total.consumption= rowSums(pe.gwnro.wc[,c(6:23)]) 
  pe.gwsa.wc$total.consumption= rowSums(pe.gwsa.wc[,c(6:23)]) 
  pe.pdbr.wc$total.consumption= rowSums(pe.pdbr.wc[,c(6:23)]) 
  pe.pdfr.wc$total.consumption= rowSums(pe.pdfr.wc[,c(6:23)]) 
  pe.pdnro.wc$total.consumption= rowSums(pe.pdnro.wc[,c(6:23)]) 
  pe.pdsa.wc$total.consumption= rowSums(pe.pdsa.wc[,c(6:23)]) 
  pe.swbr.wc$total.consumption= rowSums(pe.swbr.wc[,c(6:23)]) 
  pe.swfr.wc$total.consumption= rowSums(pe.swfr.wc[,c(6:23)]) 
  pe.swnro.wc$total.consumption= rowSums(pe.swnro.wc[,c(6:23)]) 
  pe.swsa.wc$total.consumption= rowSums(pe.swsa.wc[,c(6:23)]) 
  
  pe.water.consumption = pe.total.wc[,c(1:5,24)]
  pe.water.consumption$gwbr = pe.gwbr.wc[,24]
  pe.water.consumption$gwfr = pe.gwfr.wc[,24]
  pe.water.consumption$gwnro = pe.gwnro.wc[,24]
  pe.water.consumption$gwsa = pe.gwsa.wc[,24]
  pe.water.consumption$pdbr = pe.pdbr.wc[,24]
  pe.water.consumption$pdfr = pe.pdfr.wc[,24]
  pe.water.consumption$pdnro = pe.pdnro.wc[,24]
  pe.water.consumption$pdsa = pe.pdsa.wc[,24]
  pe.water.consumption$swbr = pe.swbr.wc[,24]
  pe.water.consumption$swfr = pe.swfr.wc[,24]
  pe.water.consumption$swnro = pe.swnro.wc[,24]
  pe.water.consumption$swsa = pe.swsa.wc[,24]
  pe.water.consumption = merge(pe.water.consumption, plant.generation[,c(1,43)], all.x=TRUE, by.x = 'Plant.Code', by.y='Plant.Id')
  
  #total volume by fuel type
  wc.pe.m3.byfuel = pe.total.wc
  wc.pe.m3.byfuel = merge(wc.pe.m3.byfuel, plant.generation[,c(1,43)], all.x=TRUE, by.x = 'Plant.Code', by.y='Plant.Id')
  wc.pe.m3MWh.byfuel = wc.pe.m3.byfuel
  wc.pe.m3MWh.byfuel[,c(6:24)] = wc.pe.m3MWh.byfuel[,c(6:24)]/wc.pe.m3MWh.byfuel[,25]
  
  wc.pe.gal.byfuel = wc.pe.m3.byfuel
  wc.pe.gal.byfuel[,c(6:24)] = wc.pe.gal.byfuel[,c(6:24)] * conversion.gal.m3
  wc.pe.galMWh.byfuel = wc.pe.gal.byfuel
  wc.pe.galMWh.byfuel[,c(6:24)] = wc.pe.galMWh.byfuel[,c(6:24)]/wc.pe.galMWh.byfuel[,25]
  
  #total volume by water type 
  wc.pe.m3 = pe.water.consumption
  wc.pe.gal = wc.pe.m3
  wc.pe.gal[,c(6:18)] = wc.pe.gal[,c(6:18)]*conversion.gal.m3
  #intensity
  wc.pe.m3MWh = wc.pe.m3
  wc.pe.m3MWh[,c(6:18)] = wc.pe.m3MWh[,c(6:18)]/wc.pe.m3MWh[,19]
  wc.pe.galMWh = wc.pe.gal
  wc.pe.galMWh[,c(6:18)] = wc.pe.galMWh[,c(6:18)]/wc.pe.galMWh[,19]
  

  #make a generation by fuel table
  ##individually deal with power plants that report "other" fuel classifications 
  other.gen = plant.generation[,c("Plant.Id","OTH","PUR","WH")] #gather all plants for "other" fuel categories
  other.gen = other.gen[which(rowSums(other.gen[,2:4]) > 0),] #remove rows (plants) with all zeros 
  other.gen.melted = melt(other.gen, id = c("Plant.Id")) #restructure dataframe
  other.gen.melted = other.gen.melted[which(other.gen.melted$value > 0),] #remove zeroes
  
  ##reclassify power plants based on EG's reclassification (searching for plant info online)
  other.gen.melted$new.fuel[other.gen.melted$Plant.Id %in% c(57842,57072,50305,54291)] = "OBIT" #reclassified as bituminous
  other.gen.melted$new.fuel[other.gen.melted$Plant.Id %in% c(10601,50304,50404,50633,52006,55066,55557,57938,52064,54224,50043,52065,52063,58197,58823,50487,50489,50488,50153,50205,5176857,1844732)] = "OOIL" #reclassified as oil
  other.gen.melted$new.fuel[other.gen.melted$Plant.Id %in% c(1745,50371,50388,50481,50624,55419,56163,58251,59383,57684,58216,10204,10434,10198,10004,50509,50274,54472,50473,50474,976021,2677147,1251259,1267121,370862,42680)] = "ONG" #reclassified as natural gas
  other.gen.melted$new.fuel[other.gen.melted$Plant.Id %in% c(10426,10208,57759,50424,58146,10466)] = "OBRDF" #reclassified as biomass and RDF
  other.gen.melted$new.fuel[other.gen.melted$Plant.Id %in% c(57172)] = "OSUB" #reclassified as subbituminous
  #other.fuels.melted$new.fuel[other.fuels.melted$Plant.Id %in% c(56294,50626,56258,56139,56833,56880,57281,57134)] = "UNK" #unknown fuels (weren't in EG's classifications)
  
  ##put back into same format as reported.fuels dataframe
  other.gen.cast = dcast(other.gen.melted, Plant.Id~new.fuel, value.var='value')
  other.gen.cast[is.na(other.gen.cast)] = 0
  
  ##merge reclassified data with reported data 
  plant.generation.m = merge(x=plant.generation,y=other.gen.cast, all.x=TRUE, by.x = "Plant.Id", by.y = "Plant.Id")
  plant.generation.m[is.na(plant.generation.m)] = 0
  
  ##add solar thermal distinction
  plant.generation.m$SUN = NULL
  plant.generation.m$WND = NULL
  plant.generation.m = merge(x=plant.generation.m, y=sun.wnd.gen, all.x=TRUE, by.x="Plant.Id", by.y="Plant.Id")
  plant.generation.m[is.na(plant.generation.m)] = 0
  
  gen.byfuel$Plant.Id = plant.generation[,1] 
  gen.byfuel = as.data.frame(gen.byfuel)
  gen.byfuel$Oil = rowSums(plant.generation.m[,c("DFO","JF","KER","PC","PG","RFO","SGP","WO","OOIL")])
  gen.byfuel$Subbituminous.Coal = rowSums(plant.generation.m[,c("SUB","OSUB")])
  gen.byfuel$Bituminous.Coal = rowSums(plant.generation.m[,c("BIT","BFG","RC","SC","SGC","WC","OBIT")])
  gen.byfuel$Lignite.Coal = plant.generation.m[,c("LIG")]
  gen.byfuel$Natural.Gas = rowSums(plant.generation.m[,c("NG", "OG","ONG")])
  gen.byfuel$Uranium = plant.generation.m[,c("NUC")]
  gen.byfuel$Hydropower = plant.generation.m[,c("WAT")]
  gen.byfuel$Wind = plant.generation.m[,c("WND")]
  gen.byfuel$Solid.Biomass.RDF = rowSums(plant.generation.m[,c("AB","BLQ","MSN","MSB","OBL","OBS","SLW","TDF","WDL","WDS","OBRDF")])
  gen.byfuel$Biogas= rowSums(plant.generation.m[,c("LFG","OBG")])
  gen.byfuel$Geothermal = plant.generation.m[,c("GEO")]
  gen.byfuel$Solar = plant.generation.m[,c("SUN")]
  gen.byfuel$Solar.Thermal = plant.generation.m[,c("SUNT")]
  
  
  #export files to csv
  write.csv(gen.byfuel, 'generation-byfuel.csv', row.names = F)
  write.csv(wc.pe.m3, 'water-consumption-pe-m3.csv', row.names = FALSE)
  write.csv(wc.pe.m3MWh, 'water-consumption-pe-m3MWh.csv', row.names = FALSE)
  write.csv(wc.pe.gal, 'water-consumption-pe-gal.csv', row.names = FALSE)
  write.csv(wc.pe.galMWh, 'water-consumption-pe-galMWh.csv', row.names = FALSE)
  
  write.csv(wc.pe.m3.byfuel, 'pe-wc-by-fuel-m3.csv', row.names = FALSE)
  write.csv(wc.pe.m3MWh.byfuel, 'pe-wc-by-fuel-m3MWh.csv', row.names = FALSE)
  write.csv(wc.pe.gal.byfuel, 'pe-wc-by-fuel-gal.csv', row.names = FALSE)
  write.csv(wc.pe.galMWh.byfuel, 'pe-wc-by-fuel-galMWh.csv', row.names = FALSE)
  
# Electricity water use ---------------------------------------------------

  #merge generator, boiler, and cooling system info 
  info.bygenerator = geninfo[,c(3,7,9,10,34)]
  boiler.fuels = page4.boilerfuel[,c(1,12,13,14)]
  info.bygenerator = merge(info.bygenerator, assn.boiler.gen[,c(3,5,6)], all.x=TRUE, by.x = c('Plant.Code','Generator.ID'), by.y=c('Plant.Code','Generator.ID'))
  info.bygenerator = merge(info.bygenerator, assn.boiler.cs[,c(3,5,6)], all.x = TRUE, by.x= c('Plant.Code', 'Boiler.ID'), by.y = c('Plant.Code', 'Boiler.ID'))
  
  #grab cooling system type from schedule 8D
  cooling.system = schedule.8d[,c('Plant.ID','Cooling.System.ID','Code')]
  cooling.system = unique(cooling.system)
  
  #grab cooling water type and quality from 860 data (merge with 923 data)
  cooling.system.m = merge(cooling.system, coolingwater.info, all.x=TRUE, by.x=c('Plant.ID','Cooling.System.ID'), by.y=c('Plant.Code','Cooling.ID'))
  cooling.system.m = cooling.system.m[,c(1:3,18:19)]
  coolingwater.type = dcast(cooling.system.m, Plant.ID+Cooling.System.ID~Water.Source.Code+Water.Type.Code, fun.aggregate = length)
  
  #merge cooling info and gen info
  info.bygenerator = merge(info.bygenerator, cooling.system, all.x = TRUE, allow.cartesian= TRUE, by.x = c('Plant.Code','Cooling.ID'), by.y = c('Plant.ID','Cooling.System.ID'))
  info.bygen.water = info.bygenerator
  info.bygen.water = merge(info.bygen.water, page1.gensandfuel[,c(1,14,15)], all.y=T, by.x=c('Plant.Code','Prime.Mover','Energy.Source.1'), by.y=c('Plant.Id','Reported.Prime.Mover','Reported.Fuel.Type.Code'))
  info.bygen.water =  merge(info.bygen.water, cooling.system.m[,c(1,2,4,5)], all.x=TRUE, by.x= c('Plant.Code', 'Cooling.ID'), by.y=c('Plant.ID', 'Cooling.System.ID'))
  
  ##reclassify power plants based on EG's reclassification (searching for plant info online)
  info.bygen.water$Energy.Source.1[info.bygen.water$Plant.Code %in% c(57842,57072,50305,54291)] = "OBIT" #reclassified as bituminous
  info.bygen.water$Energy.Source.1[info.bygen.water$Plant.Code %in% c(10601,50304,50404,50633,52006,55066,55557,57938,52064,54224,50043,52065,52063,58197,58823,50487,50489,50488,50153,50205,5176857,1844732)] = "OOIL" #reclassified as oil
  info.bygen.water$Energy.Source.1[info.bygen.water$Plant.Code %in% c(1745,50371,50388,50481,50624,55419,56163,58251,59383,57684,58216,10204,10434,10198,10004,50509,50274,54472,50473,50474,976021,2677147,1251259,1267121,370862,42680)] = "ONG" #reclassified as natural gas
  info.bygen.water$Energy.Source.1[info.bygen.water$Plant.Code %in% c(10426,10208,57759,50424,58146,10466)] = "OBRDF" #reclassified as biomass and RDF
  info.bygen.water$Energy.Source.1[info.bygen.water$Plant.Code %in% c(57172)] = "OSUB" #reclassified as subbituminous
  
  #assign new prime mover types (splitting combined cycle into gas turbines and steam turbines)
  info.bygen.water$new.pm[info.bygen.water$Prime.Mover %in% c('CS')] = 'CS'
  info.bygen.water$new.pm[info.bygen.water$Prime.Mover %in% c('BA','CE','CP','ES','FW','PS', 'FC')] = 'ES'
  info.bygen.water$new.pm[info.bygen.water$Prime.Mover %in% c('GT','IC')] = 'GT'
  info.bygen.water$new.pm[info.bygen.water$Prime.Mover %in% c('HA','HB','HK')] = 'HK'
  info.bygen.water$new.pm[info.bygen.water$Prime.Mover %in% c('HY')] = 'HY'
  info.bygen.water$new.pm[info.bygen.water$Prime.Mover %in% c('PV')] = 'PV'
  info.bygen.water$new.pm[info.bygen.water$Prime.Mover %in% c('ST')] = 'ST'
  info.bygen.water$new.pm[info.bygen.water$Prime.Mover %in% c('CA','CT')] = 'CC'
  info.bygen.water$new.pm[info.bygen.water$Prime.Mover %in% c('BT')] = 'BT'
  info.bygen.water$new.pm[info.bygen.water$Prime.Mover %in% c('WT','WS')] = 'WT'
  info.bygen.water$new.pm[info.bygen.water$Prime.Mover %in% c('OT')] = 'OT'
  
  #assign new fuel types
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('LFG','OBG')] = 'BG'
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('AB','BLQ','MSN','MSB','OBL','OBS','SLW','TDF','WDL','WDS','OBRDF')] = 'BM'
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('BIT','LIG','PC','RC','SC','SUB','WC','OBIT','OSUB','ANT')] = 'CL'
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('DFO','JF','KER','RFO','WO','OOIL')] = 'OIL'
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('GEO')] = 'GEO'
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('BFG','NG','PG','SGC','SGP','OG','ONG')] = 'NG'
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('NUC')] = 'NUC'
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('SUN')] = 'SUN'
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('WAT')] = 'WAT'
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('WND')] = 'WND'
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('MWH')] = 'ES'
  info.bygen.water$new.fuel[info.bygen.water$Energy.Source.1 %in% c('OTH','PUR','WH')] = 'OTH'
  
 #deal with combined cycle power plants -- assign cooling systems to CT part of CC unit
  cc.subset = info.bygen.water[complete.cases(info.bygen.water[,c('Unit.Code')])]
  nocc.subset = info.bygen.water[is.na(info.bygen.water$Unit.Code)]
  testing = cc.subset[,c("Plant.Code", "Unit.Code","Code","Water.Source.Code","Water.Type.Code")]
  testing2 = nocc.subset[,c("Plant.Code","Prime.Mover","Code","Water.Source.Code","Water.Type.Code")]
  testing = unique(testing)
  testing = testing[complete.cases(testing[,3])]
  
  testing2 = unique(testing2)
  testing2 = testing2[complete.cases(testing2[,3])]
  
  cc.subset = merge(cc.subset, testing, all.x = T, allow.cartesian = T, by.x = c('Plant.Code','Unit.Code'), by.y =c('Plant.Code','Unit.Code'))
  cc.subset[,c('Code.x','Water.Source.Code.x','Water.Type.Code.x')] = NULL
  colnames(cc.subset)[10:12] = c('Code','Water.Source.Code','Water.Type.Code')
  
  nocc.subset = merge(nocc.subset, testing2, all.x=T, allow.cartesian = T, by.x = c('Plant.Code', 'Prime.Mover'), by.y=c('Plant.Code', 'Prime.Mover'))
  nocc.subset[,c('Code.x','Water.Source.Code.x','Water.Type.Code.x')] = NULL
  colnames(nocc.subset)[10:12] = c('Code','Water.Source.Code','Water.Type.Code')
  
  #merge fixed CC data back with other data
  newinfo.bygen.water = rbind(nocc.subset, cc.subset)
  newinfo.bygen.water.cast = dcast(newinfo.bygen.water, Plant.Code+Prime.Mover+Energy.Source.1+new.pm+new.fuel+Water.Source.Code+Water.Type.Code~Code)
  newinfo.bygen.water.cast[,c(8:14)] = newinfo.bygen.water.cast[,c(8:14)]/rowSums(newinfo.bygen.water.cast[,c(8:14)]) #fractions of cs's instesd of counts
  
  #grab generation from generation spreadsheet and deal with NA values
  new.geninfo.water = merge(newinfo.bygen.water.cast, page1.gensandfuel[,c(1,14,15,96)], all.y = T, by.x=c('Plant.Code','Prime.Mover','Energy.Source.1'), by.y=c('Plant.Id','Reported.Prime.Mover','Reported.Fuel.Type.Code'))
  new.geninfo.water[,14][is.na(new.geninfo.water[,14])] = 1
  new.geninfo.water[,8:13][is.na(new.geninfo.water[,8:13])] = 0
  
  
  #calculate generation by cooling system type
  gen.bycool = new.geninfo.water
  gen.bycool[,8:14] = gen.bycool[,8:14]*gen.bycool[,15]
  gen.bycool[,c(2:7,15)] = NULL
  gen.bycool = melt(gen.bycool, id ='Plant.Code')

  gen.bycool = dcast(gen.bycool, Plant.Code~variable, value.var = 'value', fun.aggregate = sum)
  gen.bycool = merge(plant.locations, gen.bycool, all.y = TRUE, by.x = 'Plant.Code', by.y= 'Plant.Code')
  
  #caluclate water consumption by water type
  generation.bycode.total = new.geninfo.water
  generation.bycode.total$id <- seq_len(nrow(generation.bycode.total))
  generation.bycode.duplicated = generation.bycode.total[duplicated(generation.bycode.total[,c("Plant.Code","Net.Generation.(Megawatthours)")]) | duplicated(generation.bycode.total[,c("Plant.Code","Net.Generation.(Megawatthours)")], fromLast = TRUE),]
  generation.bycode.unique = generation.bycode.total[!(generation.bycode.total$id %in% generation.bycode.duplicated$id),]
  generation.bycode.duplicated = generation.bycode.duplicated[which(generation.bycode.duplicated$`Net.Generation.(Megawatthours)` != 0),]
  
  generation.bycode.dup2 = ddply(generation.bycode.duplicated,.(Plant.Code,`Net.Generation.(Megawatthours)`),summarize,sum=sum(`Net.Generation.(Megawatthours)`),number=length(Plant.Code))
  generation.bycode.dup2$generation = generation.bycode.dup2$`Net.Generation.(Megawatthours)`/generation.bycode.dup2$number
  generation.bycode.duplicated = merge(generation.bycode.duplicated, generation.bycode.dup2[,c(1,2,5)], all.x=T, by.x=c("Plant.Code","Net.Generation.(Megawatthours)"), by.y=c("Plant.Code","Net.Generation.(Megawatthours)"))
  generation.bycode.duplicated$`Net.Generation.(Megawatthours)` = NULL
  colnames(generation.bycode.duplicated)[16] = "Net.Generation.(Megawatthours)"
  
  generation.bycode.new = rbind(generation.bycode.unique, generation.bycode.duplicated)
  
 
  generation.bycode.new = as.data.frame(generation.bycode.new)
  generation.bycode.new[,8:14] = generation.bycode.new[,8:14]*generation.bycode.new[,15]
  generation.bycode.new[,c(2:3)] = NULL
  generation.bycs = generation.bycode.new
  generation.bycode.new[,c(13,14)] = NULL
  generation.bycode.new = melt(generation.bycode.new, id = c('Plant.Code','new.pm','new.fuel','Water.Source.Code','Water.Type.Code'))
  colnames(generation.bycode.new)[6] = 'cooling.system'
  generation.bycode.new = generation.bycode.new[which(generation.bycode.new$value != 0),]
  generation.bycode.new[generation.bycode.new$cooling.system == 'DC','Water.Source.Code'] = 'DC'
  generation.bycode.new[generation.bycode.new$cooling.system == 'DC','Water.Type.Code'] = 'DC'
  generation.bycode.wt = dcast(generation.bycode.new, Plant.Code+new.pm+new.fuel+cooling.system~Water.Source.Code+Water.Type.Code, value.var = 'value', fun.aggregate = sum)
  generation.bycode.wt$totalgen = rowSums(generation.bycode.wt[,c(5:13)])
  
  
  generation.bycode.dcdc = dcast(generation.bycode.wt, Plant.Code~new.fuel+new.pm+cooling.system, value.var = 'DC_DC', fun.aggregate = sum)
  generation.bycode.gwbr = dcast(generation.bycode.wt, Plant.Code~new.fuel+new.pm+cooling.system, value.var = 'GW_BR', fun.aggregate = sum)
  generation.bycode.gwfr = dcast(generation.bycode.wt, Plant.Code~new.fuel+new.pm+cooling.system, value.var ='GW_FR', fun.aggregate = sum)
  generation.bycode.gwsa = dcast(generation.bycode.wt, Plant.Code~new.fuel+new.pm+cooling.system, value.var ='GW_SA', fun.aggregate = sum)
  generation.bycode.pdfr = dcast(generation.bycode.wt, Plant.Code~new.fuel+new.pm+cooling.system, value.var = 'PD_FR', fun.aggregate = sum)
  generation.bycode.swbr = dcast(generation.bycode.wt, Plant.Code~new.fuel+new.pm+cooling.system, value.var = 'SW_BR', fun.aggregate = sum)
  generation.bycode.swfr = dcast(generation.bycode.wt, Plant.Code~new.fuel+new.pm+cooling.system, value.var = 'SW_FR', fun.aggregate = sum)
  generation.bycode.swsa = dcast(generation.bycode.wt, Plant.Code~new.fuel+new.pm+cooling.system, value.var ='SW_SA', fun.aggregate = sum)
  generation.bycode.nana = dcast(generation.bycode.wt, Plant.Code~new.fuel+new.pm+cooling.system, value.var ='NA_NA', fun.aggregate = sum)
  generation.bycode.total = dcast(generation.bycode.wt, Plant.Code~new.fuel+new.pm+cooling.system, value.var ='totalgen', fun.aggregate = sum)
  
  #apply water use rates to all categories
  coolingwater.total = generation.bycode.total
  coolingwater.dcdc = generation.bycode.dcdc
  coolingwater.gwbr = generation.bycode.gwbr
  coolingwater.gwfr = generation.bycode.gwfr
  coolingwater.gwsa = generation.bycode.gwsa
  coolingwater.pdfr = generation.bycode.pdfr
  coolingwater.swbr = generation.bycode.swbr
  coolingwater.swfr = generation.bycode.swfr
  coolingwater.swsa = generation.bycode.swsa
  coolingwater.nana = generation.bycode.nana
  #############################################
   
  for(i in 2:67) {
    #multiply each category by it's water consumption rate ***(make sure everything is in the right order! - colnames(coolingwater.total))
    coolingwater.total[,i] = coolingwater.total[,i]*as.numeric(cooling.rates.cc[i-1,2]) #water consumption = 2nd column. in Gal/MWh
    coolingwater.dcdc[,i] = generation.bycode.dcdc[,i]*as.numeric(cooling.rates.cc[i-1,2])
    coolingwater.gwbr[,i] = generation.bycode.gwbr[,i]*as.numeric(cooling.rates.cc[i-1,2])
    coolingwater.gwfr[,i] = generation.bycode.gwfr[,i]*as.numeric(cooling.rates.cc[i-1,2])
    coolingwater.gwsa[,i] = generation.bycode.gwsa[,i]*as.numeric(cooling.rates.cc[i-1,2])
    coolingwater.pdfr[,i] = generation.bycode.pdfr[,i]*as.numeric(cooling.rates.cc[i-1,2])
    coolingwater.swbr[,i] = generation.bycode.swbr[,i]*as.numeric(cooling.rates.cc[i-1,2])
    coolingwater.swfr[,i] = generation.bycode.swfr[,i]*as.numeric(cooling.rates.cc[i-1,2])
    coolingwater.swsa[,i] = generation.bycode.swsa[,i]*as.numeric(cooling.rates.cc[i-1,2])
    }
  
  #make a total water use column
  coolingwater.total$total.consumption = rowSums(coolingwater.total[,c(2:67)])
  coolingwater.dcdc$dcdc.consumption = rowSums(coolingwater.dcdc[,c(2:67)]) 
  coolingwater.gwbr$gwbr.consumption = rowSums(coolingwater.gwbr[,c(2:67)]) 
  coolingwater.gwfr$gwfr.consumption = rowSums(coolingwater.gwfr[,c(2:67)])
  coolingwater.gwsa$gwsa.consumption = rowSums(coolingwater.gwsa[,c(2:67)])
  coolingwater.pdfr$pdfr.consumption = rowSums(coolingwater.pdfr[,c(2:67)])
  coolingwater.swbr$swbr.consumption = rowSums(coolingwater.swbr[,c(2:67)])
  coolingwater.swfr$swfr.consumption = rowSums(coolingwater.swfr[,c(2:67)])
  coolingwater.swsa$swsa.consumption = rowSums(coolingwater.swsa[,c(2:67)])
  
  #make spredsheets for export to csv -- add PP locations for spatial join
  electric.wc.gal = coolingwater.total[,c(1,68)]
  electric.wc.gal$dcdc.consumption = rowSums(coolingwater.dcdc[,c(2:67)]) 
  electric.wc.gal$gwbr.consumption = rowSums(coolingwater.gwbr[,c(2:67)]) 
  electric.wc.gal$gwfr.consumption = rowSums(coolingwater.gwfr[,c(2:67)])
  electric.wc.gal$gwsa.consumption = rowSums(coolingwater.gwsa[,c(2:67)])
  electric.wc.gal$pdfr.consumption = rowSums(coolingwater.pdfr[,c(2:67)])
  electric.wc.gal$swbr.consumption = rowSums(coolingwater.swbr[,c(2:67)])
  electric.wc.gal$swfr.consumption = rowSums(coolingwater.swfr[,c(2:67)])
  electric.wc.gal$swsa.consumption = rowSums(coolingwater.swsa[,c(2:67)])
  
  electric.wc.gal = merge(plant.locations, electric.wc.gal, all.y = TRUE, by.x = 'Plant.Code', by.y= 'Plant.Code')
  
  electric.wc.m3 = electric.wc.gal
  electric.wc.m3[,c(6:14)] = electric.wc.m3[,c(6:14)] / conversion.gal.m3
  
  # electric.wc.galMWh = as.data.frame(electric.wc.gal)
  # electric.wc.galMWh[,c(6:14)] = electric.wc.galMWh[,c(6:14)]/electric.wc.galMWh[,15]
  # electric.wc.m3MWh = as.data.frame(electric.wc.m3)
  # electric.wc.m3MWh[,c(6:14)] = electric.wc.m3MWh[,c(6:14)]/electric.wc.m3MWh[,15]
  
  write.csv(electric.wc.gal,'electric-wc-gal.csv',row.names = FALSE)
  # write.csv(electric.wc.galMWh, 'electric-wc-galMWh.csv', row.names = FALSE)
  write.csv(electric.wc.m3, 'electric-wc-m3.csv',row.names = FALSE)
  # write.csv(electric.wc.m3MWh, 'electric-wc-m3MWh.csv', row.names = FALSE)
  
  #make tables separated by fuel and cooling system (for analysis) using coolingwater.total data frame
  elec.wc.gal.byfuel = coolingwater.total
  #add columns for fuels
  elec.wc.gal.byfuel$Biogas = rowSums(elec.wc.gal.byfuel[,c(2:7)])
  elec.wc.gal.byfuel$Biomass = rowSums(elec.wc.gal.byfuel[,c(8:13)])
  elec.wc.gal.byfuel$Oil = rowSums(elec.wc.gal.byfuel[,c(48:55)])
  elec.wc.gal.byfuel$Coal = rowSums(elec.wc.gal.byfuel[,c(14:21)])
  elec.wc.gal.byfuel$Natural.Gas = rowSums(elec.wc.gal.byfuel[,c(25:42)])
  elec.wc.gal.byfuel$Geothermal = rowSums(elec.wc.gal.byfuel[,c(23:24)])
  elec.wc.gal.byfuel$Uranium = rowSums(elec.wc.gal.byfuel[,c(43:47)])
  elec.wc.gal.byfuel$Other = rowSums(elec.wc.gal.byfuel[,c(56:57)])
  elec.wc.gal.byfuel$Solar = rowSums(elec.wc.gal.byfuel[,c(58:63)])
  #not including hydro
  elec.wc.gal.byfuel$Wind = elec.wc.gal.byfuel[,c(66)]
  elec.wc.gal.byfuel$Not.Reported = elec.wc.gal.byfuel[,c(67)]
  #remove old values & classifications
  elec.wc.gal.byfuel[,c(2:67)] = NULL
  #merge with pp locations and generation
  elec.wc.gal.byfuel = merge(plant.locations, elec.wc.gal.byfuel, all.y = TRUE, by.x = 'Plant.Code', by.y= 'Plant.Code')
  # elec.wc.gal.byfuel = merge(elec.wc.gal.byfuel, pp.generation[,1:2], all.x=TRUE, by.x='Plant.Code',by.y='Plant.Id')
  
  elec.wc.m3.byfuel = elec.wc.gal.byfuel
  elec.wc.m3.byfuel[,c(6:17)] = elec.wc.m3.byfuel[,c(6:17)] / conversion.gal.m3
  
  # elec.wc.galMWh.byfuel = as.data.frame(elec.wc.gal.byfuel)
  # elec.wc.galMWh.byfuel[,c(6:16)] = elec.wc.galMWh.byfuel[,c(6:16)]/elec.wc.galMWh.byfuel[,17]
  # elec.wc.m3MWh.byfuel = as.data.frame(elec.wc.m3.byfuel)
  # elec.wc.m3MWh.byfuel[,c(6:16)] = elec.wc.m3MWh.byfuel[,c(6:16)]/elec.wc.m3MWh.byfuel[,17]
  # 
  write.csv(elec.wc.gal.byfuel,'elec-wc-byfuel-gal.csv',row.names = FALSE)
  # write.csv(elec.wc.galMWh.byfuel, 'elec-wc-byfuel-galMWh.csv', row.names = FALSE)
  write.csv(elec.wc.m3.byfuel, 'elec-wc-byfuel-m3.csv',row.names = FALSE)
  # write.csv(elec.wc.m3MWh.byfuel, 'elec-wc-byfuel-m3MWh.csv', row.names = FALSE)
  write.csv(gen.bycool, 'generation-bycool.csv', row.names = FALSE)
  
  
  elec.wc.gal.bycool = coolingwater.total
  #add columns for fuels
  elec.wc.gal.bycool$None = rowSums(elec.wc.gal.bycool[,c(2,9:10,17,33,35:36,48,52:53,59:60,62:66)])
  elec.wc.gal.bycool$Dry.Cooling = rowSums(elec.wc.gal.bycool[,c(3,19,24,30,37,61)])
  elec.wc.gal.bycool$Once.Through = rowSums(elec.wc.gal.bycool[,c(4,11,20,26,34,38,44,49,54,18,23,41:43,56)])
  elec.wc.gal.bycool$Ponds = rowSums(elec.wc.gal.bycool[,c(5,14,21,27,39,45)])
  elec.wc.gal.bycool$Recirculating = rowSums(elec.wc.gal.bycool[,c(6,12,15,22,28,31,40,46,47,50,55,58,7:8,13,16,29,32,51,57)])
  elec.wc.gal.bycool$Hybrid = elec.wc.gal.bycool[,c(25)]
  #remove old values & classifications
  elec.wc.gal.bycool[,c(2:66)] = NULL
  
  #merge with pp locations and generation
  elec.wc.gal.bycool = merge(plant.locations, elec.wc.gal.bycool, all.y = TRUE, by.x = 'Plant.Code', by.y= 'Plant.Code')
  # elec.wc.gal.bycool = merge(elec.wc.gal.bycool, pp.generation[,1:2], all.x=TRUE, by.x='Plant.Code',by.y='Plant.Id')
  
  elec.wc.m3.bycool = elec.wc.gal.bycool
  elec.wc.m3.bycool[,c(6:12)] = elec.wc.m3.bycool[,c(6:12)] / conversion.gal.m3
  
  # elec.wc.galMWh.bycool = as.data.frame(elec.wc.gal.bycool)
  # elec.wc.galMWh.bycool[,c(6:12)] = elec.wc.galMWh.bycool[,c(6:12)]/elec.wc.galMWh.bycool[,13]
  # elec.wc.m3MWh.bycool = as.data.frame(elec.wc.m3.bycool)
  # elec.wc.m3MWh.bycool[,c(6:12)] = elec.wc.m3MWh.bycool[,c(6:12)]/elec.wc.m3MWh.bycool[,13]
  # 
  write.csv(elec.wc.gal.bycool,'elec-wc-bycool-gal.csv',row.names = FALSE)
  # write.csv(elec.wc.galMWh.bycool, 'elec-wc-bycool-galMWh.csv', row.names = FALSE)
  write.csv(elec.wc.m3.bycool, 'elec-wc-bycool-m3.csv',row.names = FALSE)
  # write.csv(elec.wc.m3MWh.bycool, 'elec-wc-bycool-m3MWh.csv', row.names = FALSE)
  
  
  
  
# Plant generation --------------------------------------------------------
#create a spreadsheet with total generation by power plant to calculate 
#water consumption intensities after spatial joins
  
total.generation = plant.generation[,c("Plant.Id","total.gen")]
total.generation = merge(total.generation, plant.locations, all.x=T, by.x="Plant.Id",by.y="Plant.Code")

write.csv(total.generation, 'power-plant-generation.csv',row.names = FALSE)


##old code and random calcs
# ## averages and medians
# mean(wc.pe.m3$total.consumption)
# # [1] 134450.1
# is.na(wc.pe.m3MWh$total.consumption) = sapply(wc.pe.m3MWh$total.consumption, is.infinite)
# wc.pe.m3MWh$total.consumption[is.na(wc.pe.m3MWh$total.consumption)] = 0
# mean(wc.pe.m3MWh$total.consumption)
# # [1] 1.518495
# 
# mean(electric.wc.m3$total.consumption)
# # [1] 1377075
# is.na(electric.wc.m3MWh$total.consumption) = sapply(electric.wc.m3MWh$total.consumption, is.infinite)
# electric.wc.m3MWh$total.consumption[is.na(electric.wc.m3MWh$total.consumption)] = 0
# mean(electric.wc.m3MWh$total.consumption)
# # [1] 0.3061531
# 
# total.wc.df$Plant.Id = wc.pe.m3$Plant.Code
# total.wc.df$generation = wc.pe.m3$total.gen
# total.wc.df$pe.wc = wc.pe.m3$total.consumption
# total.wc.df$elec.wc = electric.wc.m3$total.consumption
# total.wc.df$total.wc = rowSums()
# total.wc.df$total.wci = total.wc.df$total.wc/total.wc.df$generation
# 
# 
# 
# #### cooling system generation
# plant.cs.gen = generation.cs.total
# plant.cs.gen = unique(plant.cs.gen)
# plant.cs.gen[,c(2,3)] = NULL
# plant.cs.count = plant.cs.gen
# plant.cs.count = plant.cs.count[which(plant.cs.count$value != 0),] 
# plant.cs.count = dcast(plant.cs.count, Plant.Code~cooling.system, value.var = 'value', fun.aggregate = length)
# plant.cs.count[,c(8)] = NULL #remove no cooling systems
# plant.cs.count[plant.cs.count$DC > 0, 'DC'] = 1
# plant.cs.count[plant.cs.count$HB > 0, 'HB'] = 1
# plant.cs.count[plant.cs.count$ON > 0, 'ON'] = 1
# plant.cs.count[plant.cs.count$PN > 0, 'PN'] = 1
# plant.cs.count[plant.cs.count$RC > 0, 'RC'] = 1
# plant.cs.count[plant.cs.count$RT > 0, 'RT'] = 1
# plant.cs.count$count = rowSums(plant.cs.count[,c(2:7)])
# 
# plant.cs.gen = dcast(plant.cs.gen, Plant.Code~cooling.system, value.var = 'value', fun.aggregate = sum)
# plant.cs.gen$generation = rowSums(plant.cs.gen[,c(2:8)])
# plant.cs.gen = merge(plant.cs.gen, plant.cs.count[,c(1,8)], all.x=T, by.x='Plant.Code',by.y='Plant.Code')
# #sum of generation
# allgeneration = sum(plant.cs.gen$generation)
# #sum of generation that has multiple cooling
# sum(DF[which(DF[,1]>30 & DF[,4]>90),2])