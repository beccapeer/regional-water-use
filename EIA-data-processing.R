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
wd            = '~/Documents/Graduate/Journal Papers/in-progress/EST_regional-water-rates/data/'
eia860.folder = '~/Documents/Graduate/Journal Papers/in-progress/EST_regional-water-rates/data/2014 EIA 860/'
eia923.folder = '~/Documents/Graduate/Journal Papers/in-progress/EST_regional-water-rates/data/2014 EIA 923/'
eia923.file.s2t5 = 'EIA923_Schedules_2_3_4_5_M_12_2014_Final_Revision.xlsx' #923 schedules 2 thru 5
eia923.file.s8d = 'EIA923_Schedule_8_Annual_Environmental_Information_2014_Final_Revision.xlsx' #923 schedule 8
eia860.file.s2 = '2___Plant_Y2014.xlsx' #860 schedule 2
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
  
#Water use rates
  #setwd('~')
  #primary energy water use rates
  setwd(wd)
  pe.wateruse = as.data.table(read.xlsx(wateruse.file, sheet =1, startRow = 1)) #fuels in rows, water types in columns - units of GJ/m^3
  #old file set up
  # all.wc = as.data.table(read.xlsx(wateruse.file, sheet = 1, startRow = 2)) #total water consumption
  # ground.wc = as.data.table(read.xlsx(wateruse.file, sheet = 3, startRow = 2)) #groundwater consumption
  # surface.wc = as.data.table(read.xlsx(wateruse.file, sheet = 5, startRow = 2)) #surface water consumption
  # reuse.wc = as.data.table(read.xlsx(wateruse.file, sheet = 7, startRow = 2)) #reuse water consumption
  # fresh.wc = as.data.table(read.xlsx(wateruse.file, sheet = 9, startRow = 2)) #freshwater consumption
  # brackish.wc = as.data.table(read.xlsx(wateruse.file, sheet = 11, startRow = 2)) #brackish water consumption
  # saline.wc = as.data.table(read.xlsx(wateruse.file, sheet = 13, startRow = 2)) #saline water consumption
  # notRO.wc = as.data.table(read.xlsx(wateruse.file, sheet = 15, startRow = 2)) #not RO-treatable water consumption

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
  oil.tot.wc = as.numeric(all.wc[1, 7]) * conversion.gj.mmbtu
  sub.ngp.tot.wc = as.numeric(all.wc[2,7]) * conversion.gj.mmbtu
  bit.app.tot.wc = as.numeric(all.wc[3,7]) * conversion.gj.mmbtu
  bit.int.tot.wc = as.numeric(all.wc[4,7]) * conversion.gj.mmbtu
  bit.rmr.tot.wc = as.numeric(all.wc[5,7]) * conversion.gj.mmbtu
  lig.gfc.tot.wc = as.numeric(all.wc[6,7]) * conversion.gj.mmbtu
  lig.ngp.tot.wc = as.numeric(all.wc[7,7]) * conversion.gj.mmbtu
  ng.tot.wc = as.numeric(all.wc[8,7]) * conversion.gj.mmbtu
  ur.tot.wc = as.numeric(all.wc[9,7]) * conversion.gj.mmbtu
  wat.tot.wc = as.numeric(all.wc[10,7]) * conversion.gj.mmbtu
  wnd.tot.wc = as.numeric(all.wc[11,7]) * conversion.gj.mmbtu
  brdf.tot.wc = as.numeric(all.wc[12,7]) * conversion.gj.mmbtu
  bg.tot.wc = as.numeric(all.wc[13,7]) * conversion.gj.mmbtu
  geo.tot.wc = as.numeric(all.wc[14,7]) * conversion.gj.mmbtu
  spv.tot.wc = as.numeric(all.wc[15,7]) * conversion.gj.mmbtu
  sth.tot.wc = as.numeric(all.wc[16,7]) * conversion.gj.mmbtu
  sub.rmr.tot.wc = as.numeric(all.wc[17,7]) * conversion.gj.mmbtu
  bit.avg.tot.wc = as.numeric(all.wc[18,7]) * conversion.gj.mmbtu
  sub.avg.tot.wc = as.numeric(all.wc[19,7]) * conversion.gj.mmbtu
  
  
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
  # purchased.coal.melted = melt(purchased.coal.cast, id=c('Plant.Id','Coalmine.State'))
  # purchased.coal.melted = purchased.coal.melted[which(purchased.coal.melted$value  > 0),]
  
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
  
  ##assign solar thermal fuel (separate from solar PV) for water calcs
  # solar.fuels = reported.fuels[c("Plant.Id","SUN")]
  # solar.fuels = solar.fuels[which(solar.fuels$SUN > 0),]
  # solar.fuels.melted = melt(solar.fuels, id="Plant.Id")
  # solar.fuels.melted$new.fuel[solar.fuels.melted$Plant.Id %in% c(6043,10437,10438,10439,10440,10441,10442,10443,10446,56405,56812,56943,57073,57074,57075,57323,57394,59060)] = "SUNT"
  # solar.fuels.melted[is.na(solar.fuels.melted)] = "SUN"
  # solar.fuels.cast = dcast(solar.fuels.melted, Plant.Id~new.fuel, value.var='value')
  # solar.fuels.cast[is.na(solar.fuels.cast)] = 0
  
  ##merge solar data with reported data 
  # reported.fuels.m$SUN = NULL #remove old solar fuel column
  # reported.fuels.m = merge(x=reported.fuels.m, y=solar.fuels.cast, all.x=TRUE, by.x="Plant.Id", by.y="Plant.Id")
  # reported.fuels.m[is.na(reported.fuels.m)] = 0
  
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
  
  ##reassign feul categories (EG's fuel categories)
  fuel.categories$Oil = rowSums(reported.fuels.m[,c("DFO","JF","KER","PC","PG","RFO","SGP","WO","OOIL")])
  fuel.categories$Subbituminous.Coal.ngp = reported.fuels.m[,c("NGP_SUB")]
  fuel.categories$Bituminous.Coal.app = reported.fuels.m[,c("APP_BIT")]
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
  fuel.categories$Geothermal = reported.fuels.m[,c("GEO")]
  fuel.categories$Solar = reported.fuels.m[,c("SUN")]
  fuel.categories$Solar.Thermal = reported.fuels.m[,c("SUNT")]
  fuel.categories$Subbituminous.Coal.rmr = reported.fuels.m[,c("RMR_SUB")]
  fuel.categories$Bituminous.Coal.avg = rowSums(reported.fuels.m[,c("OTH_BIT","BFG","RC","SC","SGC","WC","OBIT")])
  fuel.categories$Subbituminous.Coal.avg = rowSums(reported.fuels.m[,c("OTH_SUB", "OSUB")])
    
  ##merge plant.locations with fuel.categories for primary energy MMBTU dataframe
  primary.energy.mmbtu = merge(x=plant.locations, y=fuel.categories, all.x=TRUE, by.x="Plant.Code", by.y="Plant.Id")
  primary.energy.mmbtu[,6:24][is.na(primary.energy.mmbtu[,6:24])] = 0
  
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
  
  for(i in 1:19){
    pe.total.wc[,5+i] = pe.total.wc[,5+i]*as.numeric(pe.wateruse[i,2]*conversion.gj.mmbtu)
    pe.gwbr.wc[,5+i] = pe.gwbr.wc[,5+i]*as.numeric(pe.wateruse[i,3]*conversion.gj.mmbtu)
    pe.gwfr.wc[,5+i] =  pe.gwfr.wc[,5+i]*as.numeric(pe.wateruse[i,4]*conversion.gj.mmbtu)
    pe.gwnro.wc[,5+i] = pe.gwnro.wc[,5+i]*as.numeric(pe.wateruse[i,5]*conversion.gj.mmbtu)
    pe.gwsa.wc[,5+i] =  pe.gwsa.wc[,5+i]*as.numeric(pe.wateruse[i,6]*conversion.gj.mmbtu)
    pe.pdbr.wc[,5+i] =  pe.pdbr.wc[,5+i]*as.numeric(pe.wateruse[i,7]*conversion.gj.mmbtu)
    pe.pdfr.wc[,5+i] =  pe.pdfr.wc[,5+i]*as.numeric(pe.wateruse[i,8]*conversion.gj.mmbtu)
    pe.pdnro.wc[,5+i] = pe.pdnro.wc[,5+i]*as.numeric(pe.wateruse[i,9]*conversion.gj.mmbtu)
    pe.pdsa.wc[,5+i] =  pe.pdsa.wc[,5+i]*as.numeric(pe.wateruse[i,10]*conversion.gj.mmbtu)
    pe.swbr.wc[,5+i] =  pe.swbr.wc[,5+i]*as.numeric(pe.wateruse[i,11]*conversion.gj.mmbtu)
    pe.swfr.wc[,5+i] =  pe.swfr.wc[,5+i]*as.numeric(pe.wateruse[i,12]*conversion.gj.mmbtu)
    pe.swnro.wc[,5+i] = pe.swnro.wc[,5+i]*as.numeric(pe.wateruse[i,13]*conversion.gj.mmbtu)
    pe.swsa.wc[,5+i] =  pe.swsa.wc[,5+i]*as.numeric(pe.wateruse[i,14]*conversion.gj.mmbtu)
  }
  
  #create total water consumption columns
  pe.total.wc$total.consumption = rowSums(pe.total.wc[,c(6:24)]) 
  pe.gwbr.wc$total.consumption= rowSums(pe.gwbr.wc[,c(6:24)]) 
  pe.gwfr.wc$total.consumption= rowSums(pe.gwfr.wc[,c(6:24)]) 
  pe.gwnro.wc$total.consumption= rowSums(pe.gwnro.wc[,c(6:24)]) 
  pe.gwsa.wc$total.consumption= rowSums(pe.gwsa.wc[,c(6:24)]) 
  pe.pdbr.wc$total.consumption= rowSums(pe.pdbr.wc[,c(6:24)]) 
  pe.pdfr.wc$total.consumption= rowSums(pe.pdfr.wc[,c(6:24)]) 
  pe.pdnro.wc$total.consumption= rowSums(pe.pdnro.wc[,c(6:24)]) 
  pe.pdsa.wc$total.consumption= rowSums(pe.pdsa.wc[,c(6:24)]) 
  pe.swbr.wc$total.consumption= rowSums(pe.swbr.wc[,c(6:24)]) 
  pe.swfr.wc$total.consumption= rowSums(pe.swfr.wc[,c(6:24)]) 
  pe.swnro.wc$total.consumption= rowSums(pe.swnro.wc[,c(6:24)]) 
  pe.swsa.wc$total.consumption= rowSums(pe.swsa.wc[,c(6:24)]) 
  
  pe.water.consumption = pe.total.wc[,c(1:5,25)]
  pe.water.consumption$gwbr = pe.gwbr.wc[,25]
  pe.water.consumption$gwfr = pe.gwfr.wc[,25]
  pe.water.consumption$gwnro = pe.gwnro.wc[,25]
  pe.water.consumption$gwsa = pe.gwsa.wc[,25]
  pe.water.consumption$pdbr = pe.pdbr.wc[,25]
  pe.water.consumption$pdfr = pe.pdfr.wc[,25]
  pe.water.consumption$pdnro = pe.pdnro.wc[,25]
  pe.water.consumption$pdsa = pe.pdsa.wc[,25]
  pe.water.consumption$swbr = pe.swbr.wc[,25]
  pe.water.consumption$swfr = pe.swfr.wc[,25]
  pe.water.consumption$swnro = pe.swnro.wc[,25]
  pe.water.consumption$swsa = pe.swsa.wc[,25]
  pe.water.consumption = merge(pe.water.consumption, plant.generation[,c(1,43)], all.x=TRUE, by.x = 'Plant.Code', by.y='Plant.Id')
  
  
  # total.wc.m3 = primary.energy.mmbtu[,c(1:5)] #power plant names, id, and location
  # total.wc.m3$Hydropower = primary.energy.mmbtu[,c("Hydropower")]*wat.tot.wc
  # total.wc.m3$Solar = primary.energy.mmbtu[,c("Solar")]*spv.tot.wc
  # total.wc.m3$Solar.Thermal = primary.energy.mmbtu[,c("Solar.Thermal")]*sth.tot.wc
  # total.wc.m3$Wind = primary.energy.mmbtu[,c("Wind")]*wnd.tot.wc
  # total.wc.m3$Geothermal = primary.energy.mmbtu[,c("Geothermal")]*geo.tot.wc
  # total.wc.m3$Solid.Biomass.RDF = primary.energy.mmbtu[,c("Solid.Biomass.RDF")]*brdf.tot.wc
  # total.wc.m3$Biogas = primary.energy.mmbtu[,c("Biogas")]*bg.tot.wc
  # total.wc.m3$Oil = primary.energy.mmbtu[,c("Oil")]*oil.tot.wc
  # total.wc.m3$Natural.Gas = primary.energy.mmbtu[,c("Natural.Gas")]*ng.tot.wc
  # total.wc.m3$Uranium = primary.energy.mmbtu[,c("Uranium")]*ur.tot.wc
  # total.wc.m3$Lignite.Coal.gfc = primary.energy.mmbtu[,c("Lignite.Coal.gfc")]*lig.gfc.tot.wc
  # total.wc.m3$Ligite.Coal.ngp = primary.energy.mmbtu[,c("Lignite.Coal.ngp")]*lig.ngp.tot.wc
  # total.wc.m3$Bituminous.Coal.avg = primary.energy.mmbtu[,c("Bituminous.Coal.avg")]*bit.avg.tot.wc
  # total.wc.m3$Bituminous.Coal.app = primary.energy.mmbtu[,c("Bituminous.Coal.app")]*bit.app.tot.wc
  # total.wc.m3$Bituminous.Coal.int = primary.energy.mmbtu[,c("Bituminous.Coal.int")]*bit.int.tot.wc
  # total.wc.m3$Bituminous.Coal.rmr = primary.energy.mmbtu[,c("Bituminous.Coal.rmr")]*bit.rmr.tot.wc
  # total.wc.m3$Subbituminous.Coal.avg = primary.energy.mmbtu[,c("Subbituminous.Coal.avg")]*sub.avg.tot.wc
  # total.wc.m3$Subbituminous.Coal.ngp = primary.energy.mmbtu[,c("Subbituminous.Coal.ngp")]*sub.ngp.tot.wc
  # total.wc.m3$Subbituminous.Coal.rmr = primary.energy.mmbtu[,c("Subbituminous.Coal.rmr")]*sub.rmr.tot.wc
  # 
  # #calculate total water embedded in primary energy by power plant
  # total.wc.m3$total.wc = rowSums(total.wc.m3[,c(6:24)])
  # total.wc.m3[is.na(total.wc.m3)] = 0
  # total.wc.m3 = total.wc.m3[which(total.wc.m3$total.wc > 0),]
  # total.wc.gal = total.wc.m3
  # total.wc.gal[,c(6:25)] =total.wc.gal[,c(6:25)] * conversion.gal.m3
  
  #set up dataframe for exporting water use to csv files (for GIS!)
  # wc.pe.m3 = total.wc.m3[,c(1:5,25)]
  # wc.pe.gal = total.wc.gal[,c(1:5,25)]
  #total volume
  wc.pe.m3 = pe.water.consumption
  wc.pe.gal = wc.pe.m3
  wc.pe.gal[,c(6:18)] = wc.pe.gal[,c(6:18)]*conversion.gal.m3
  #intensity
  wc.pe.m3MWh = wc.pe.m3
  wc.pe.m3MWh[,c(6:18)] = wc.pe.m3MWh[,c(6:18)]/wc.pe.m3MWh[,19]
  wc.pe.galMWh = wc.pe.gal
  wc.pe.galMWh[,c(6:18)] = wc.pe.galMWh[,c(6:18)]/wc.pe.galMWh[,19]
  
  #export files to csv
  # write.csv(total.wc.m3, 'total-wc-by-fuel-m3.csv', row.names = FALSE)
  write.csv(wc.pe.m3, 'water-consumption-pe-m3.csv', row.names = FALSE)
  write.csv(wc.pe.m3MWh, 'water-consumption-pe-m3MWh.csv', row.names = FALSE)
  # write.csv(total.wc.gal, 'total-wc-by-fuel-gal.csv', row.names = FALSE)
  write.csv(wc.pe.gal, 'water-consumption-pe-gal.csv', row.names = FALSE)
  write.csv(wc.pe.galMWh, 'water-consumption-pe-galMWh.csv', row.names = FALSE)

  
# Electricity water use ---------------------------------------------------

  #merge generator, boiler, and cooling system info 
  info.bygenerator = page1.gensandfuel[,c(1:2,4,14,15,96)]
  # info.bygenerator = merge(info.bygenerator, assn.boiler.gen, all.x = TRUE, by.x= c('Plant.Id', 'Generator.Id'), by.y=c('Plant.Code','Generator.ID'))
  # info.bygenerator[,c('Utility.ID','Utility.Name','Plant.Name.y')] = NULL
  #get fuel codes from 923 boiler data 
  boiler.fuels = page4.boilerfuel[,c(1,12,13,14)]
  # boiler.fuels = unique(boiler.fuels)
  info.bygenerator = merge(info.bygenerator, boiler.fuels, all.x = TRUE, by.x = c('Plant.Id','Reported.Prime.Mover','Reported.Fuel.Type.Code'), by.y = c('Plant.Id','Reported.Prime.Mover','Reported.Fuel.Type.Code'))
  info.bygenerator = merge(info.bygenerator, assn.boiler.cs, all.x = TRUE, by.x= c('Plant.Id', 'Boiler.Id'), by.y = c('Plant.Code', 'Boiler.ID'))
  info.bygenerator[,c('Utility.ID','Utility.Name','Plant.Name.y', 'Steam.Plant.Type')] = NULL
  #info.bygenerator[,c(8:10,12)] = NULL #remove superfluous info
  
  #grab cooling system type from schedule 8D
  cooling.system = schedule.8d[,c('Plant.ID','Cooling.System.ID','Code')]
  cooling.system = unique(cooling.system)
  
  #grab cooling water type and quality from 860 data (merge with 923 data)
  cooling.system.m = merge(cooling.system, coolingwater.info, all.x=TRUE, by.x=c('Plant.ID','Cooling.System.ID'), by.y=c('Plant.Code','Cooling.ID'))
  cooling.system.m = cooling.system.m[,c(1:3,18:19)]
  coolingwater.type = dcast(cooling.system.m, Plant.ID+Cooling.System.ID~Water.Source.Code+Water.Type.Code, fun.aggregate = length)
  
  #merge cooling info and gen info
  info.bygenerator = merge(info.bygenerator, cooling.system, all.x = TRUE, allow.cartesian= TRUE, by.x = c('Plant.Id','Cooling.ID'), by.y = c('Plant.ID','Cooling.System.ID'))
  info.bygen.water = merge(info.bygenerator, coolingwater.type, all.x=TRUE, by.x= c('Plant.Id', 'Cooling.ID'), by.y=c('Plant.ID', 'Cooling.System.ID'))
  info.bygen.water[,10:17][is.na(info.bygen.water[,10:17])] = 0
  
  #generation for each water type 
  info.bygen.water$DC_DC.gen = info.bygen.water$DC_DC*info.bygen.water$`Net.Generation.(Megawatthours)`
  info.bygen.water$GW_BR.gen = info.bygen.water$GW_BR*info.bygen.water$`Net.Generation.(Megawatthours)`
  info.bygen.water$GW_FR.gen = info.bygen.water$GW_FR*info.bygen.water$`Net.Generation.(Megawatthours)`
  info.bygen.water$GW_SA.gen = info.bygen.water$GW_SA*info.bygen.water$`Net.Generation.(Megawatthours)`
  info.bygen.water$PD_FR.gen = info.bygen.water$PD_FR*info.bygen.water$`Net.Generation.(Megawatthours)`
  info.bygen.water$SW_BR.gen = info.bygen.water$SW_BR*info.bygen.water$`Net.Generation.(Megawatthours)`
  info.bygen.water$SW_FR.gen = info.bygen.water$SW_FR*info.bygen.water$`Net.Generation.(Megawatthours)`
  info.bygen.water$SW_SA.gen = info.bygen.water$SW_SA*info.bygen.water$`Net.Generation.(Megawatthours)`
  
  #redefine fuel and prime mover codes (cooling system codes assigned in 860 spreadsheet)
  # boiler.fuels$new.pm[boiler.fuels$Reported.Prime.Mover %in% c('CA','CS','CT')] = 'CC'
  # boiler.fuels$new.pm[boiler.fuels$Reported.Prime.Mover %in% c('BA','CE','CP','ES','FW','PS', 'FC')] = 'ES'
  # boiler.fuels$new.pm[boiler.fuels$Reported.Prime.Mover %in% c('GT','IC')] = 'GT'
  # boiler.fuels$new.pm[boiler.fuels$Reported.Prime.Mover %in% c('HA','HB','HK')] = 'HK'
  # boiler.fuels$new.pm[boiler.fuels$Reported.Prime.Mover %in% c('HY')] = 'HY'
  # boiler.fuels$new.pm[boiler.fuels$Reported.Prime.Mover %in% c('PV')] = 'PV'
  # boiler.fuels$new.pm[boiler.fuels$Reported.Prime.Mover %in% c('ST','BT')] = 'ST'
  # boiler.fuels$new.pm[boiler.fuels$Reported.Prime.Mover %in% c('WT','WS')] = 'WT'
  # boiler.fuels$new.pm[boiler.fuels$Reported.Prime.Mover %in% c('OT')] = 'OT'
  
  # boiler.fuels$new.fuel[boiler.fuels$Reported.Fuel.Type.Code %in% c('LFG','OBG')] = 'BMG'
  # boiler.fuels$new.fuel[boiler.fuels$Reported.Fuel.Type.Code %in% c('AB','BLQ','MSN','MSB','OBL','OBS','SLW','TDF','WDL','WDS')] = 'BM'
  # boiler.fuels$new.fuel[boiler.fuels$Reported.Fuel.Type.Code %in% c('BIT','DFO','JF','KER','LIG','PC','RC','RFO','SC','SUB','WC','WO')] = 'FSL'
  # boiler.fuels$new.fuel[boiler.fuels$Reported.Fuel.Type.Code %in% c('GEO')] = 'GEO'
  # boiler.fuels$new.fuel[boiler.fuels$Reported.Fuel.Type.Code %in% c('BFG','NG','PG','SGC','SGP','OG')] = 'FSLG'
  # boiler.fuels$new.fuel[boiler.fuels$Reported.Fuel.Type.Code %in% c('NUC')] = 'NUC'
  # boiler.fuels$new.fuel[boiler.fuels$Reported.Fuel.Type.Code %in% c('SUN')] = 'SUN'
  # boiler.fuels$new.fuel[boiler.fuels$Reported.Fuel.Type.Code %in% c('WAT')] = 'WAT'
  # boiler.fuels$new.fuel[boiler.fuels$Reported.Fuel.Type.Code %in% c('WND')] = 'WND'
  # boiler.fuels$new.fuel[boiler.fuels$Reported.Fuel.Type.Code %in% c('OTH','PUR','WH')] = 'OTH'
  
  #remove old classifications and duplicate rows
  # boiler.fuels[,c(3)] = NULL
  # boiler.fuels = unique(boiler.fuels)  
  
  #merge with generator infos 
  # info.bygenerator[,c(7:14,17)] = NULL
  # info.bygenerator = unique(info.bygenerator)
  # info.bygenerator = merge(info.bygenerator, boiler.fuels, all.x = TRUE, by.x = c('Plant.Id','Boiler.ID'), by.y=c('Plant.Id','Boiler.Id'))

  #assign new prime mover types (splitting combined cycle into gas turbines and steam turbines)
  info.bygen.water$new.pm[info.bygen.water$Reported.Prime.Mover %in% c('CS')] = 'CS'
  info.bygen.water$new.pm[info.bygen.water$Reported.Prime.Mover %in% c('BA','CE','CP','ES','FW','PS', 'FC')] = 'ES'
  info.bygen.water$new.pm[info.bygen.water$Reported.Prime.Mover %in% c('GT','IC')] = 'GT'
  info.bygen.water$new.pm[info.bygen.water$Reported.Prime.Mover %in% c('HA','HB','HK')] = 'HK'
  info.bygen.water$new.pm[info.bygen.water$Reported.Prime.Mover %in% c('HY')] = 'HY'
  info.bygen.water$new.pm[info.bygen.water$Reported.Prime.Mover %in% c('PV')] = 'PV'
  info.bygen.water$new.pm[info.bygen.water$Reported.Prime.Mover %in% c('ST')] = 'ST'
  info.bygen.water$new.pm[info.bygen.water$Reported.Prime.Mover %in% c('CA','CT')] = 'CC'
  info.bygen.water$new.pm[info.bygen.water$Reported.Prime.Mover %in% c('BT')] = 'BT'
  info.bygen.water$new.pm[info.bygen.water$Reported.Prime.Mover %in% c('WT','WS')] = 'WT'
  info.bygen.water$new.pm[info.bygen.water$Reported.Prime.Mover %in% c('OT')] = 'OT'
  
  #assign new fuel types
  info.bygen.water$new.fuel[info.bygen.water$Reported.Fuel.Type.Code %in% c('LFG','OBG')] = 'BMG'
  info.bygen.water$new.fuel[info.bygen.water$Reported.Fuel.Type.Code %in% c('AB','BLQ','MSN','MSB','OBL','OBS','SLW','TDF','WDL','WDS')] = 'BM'
  info.bygen.water$new.fuel[info.bygen.water$Reported.Fuel.Type.Code %in% c('BIT','DFO','JF','KER','LIG','PC','RC','RFO','SC','SUB','WC','WO')] = 'FSL'
  info.bygen.water$new.fuel[info.bygen.water$Reported.Fuel.Type.Code %in% c('GEO')] = 'GEO'
  info.bygen.water$new.fuel[info.bygen.water$Reported.Fuel.Type.Code %in% c('BFG','NG','PG','SGC','SGP','OG')] = 'FSLG'
  info.bygen.water$new.fuel[info.bygen.water$Reported.Fuel.Type.Code %in% c('NUC')] = 'NUC'
  info.bygen.water$new.fuel[info.bygen.water$Reported.Fuel.Type.Code %in% c('SUN')] = 'SUN'
  info.bygen.water$new.fuel[info.bygen.water$Reported.Fuel.Type.Code %in% c('WAT')] = 'WAT'
  info.bygen.water$new.fuel[info.bygen.water$Reported.Fuel.Type.Code %in% c('WND')] = 'WND'
  info.bygen.water$new.fuel[info.bygen.water$Reported.Fuel.Type.Code %in% c('OTH','PUR','WH')] = 'OTH'
  
  generation.bycode.total = dcast(info.bygen.water, Plant.Id~new.fuel+new.pm+Code+Combined.Heat.And.Power.Plant, value.var = 'Net.Generation.(Megawatthours)', fun.aggregate = sum)
  generation.bycode.summed = as.data.frame(colSums(generation.bycode.total[,-1])) #checking total generation for each code category (verify assumptions)
  pp.generation = as.data.frame(rowSums(generation.bycode.total[,-1])) #annual generation for each power plant by PP ID
  pp.generation$Plant.Id = generation.bycode.total$Plant.Id
  
  generation.bycode.dcdc = dcast(info.bygen.water, Plant.Id~new.fuel+new.pm+Code+Combined.Heat.And.Power.Plant, value.var = 'DC_DC.gen', fun.aggregate = sum)
  generation.bycode.gwbr = dcast(info.bygen.water, Plant.Id~new.fuel+new.pm+Code+Combined.Heat.And.Power.Plant, value.var = 'GW_BR.gen', fun.aggregate = sum)
  generation.bycode.gwfr = dcast(info.bygen.water, Plant.Id~new.fuel+new.pm+Code+Combined.Heat.And.Power.Plant, value.var = 'GW_FR.gen', fun.aggregate = sum)
  generation.bycode.gwsa = dcast(info.bygen.water, Plant.Id~new.fuel+new.pm+Code+Combined.Heat.And.Power.Plant, value.var = 'GW_SA.gen', fun.aggregate = sum)
  generation.bycode.pdfr = dcast(info.bygen.water, Plant.Id~new.fuel+new.pm+Code+Combined.Heat.And.Power.Plant, value.var = 'PD_FR.gen', fun.aggregate = sum)
  generation.bycode.swbr = dcast(info.bygen.water, Plant.Id~new.fuel+new.pm+Code+Combined.Heat.And.Power.Plant, value.var = 'SW_BR.gen', fun.aggregate = sum)
  generation.bycode.swfr = dcast(info.bygen.water, Plant.Id~new.fuel+new.pm+Code+Combined.Heat.And.Power.Plant, value.var = 'SW_FR.gen', fun.aggregate = sum)
  generation.bycode.swsa = dcast(info.bygen.water, Plant.Id~new.fuel+new.pm+Code+Combined.Heat.And.Power.Plant, value.var = 'SW_SA.gen', fun.aggregate = sum)
  
  #double check generation separation by water type (hydro not included in water type gen)
  pp.generation$dcdc.gen = rowSums(generation.bycode.dcdc[,-1])
  pp.generation$gwbr.gen = rowSums(generation.bycode.gwbr[,-1])
  pp.generation$gwfr.gen = rowSums(generation.bycode.gwfr[,-1])
  pp.generation$gwsa.gen = rowSums(generation.bycode.gwsa[,-1])
  pp.generation$pdfr.gen = rowSums(generation.bycode.pdfr[,-1])
  pp.generation$swbr.gen = rowSums(generation.bycode.swbr[,-1])
  pp.generation$swfr.gen = rowSums(generation.bycode.swfr[,-1])
  pp.generation$swsa.gen = rowSums(generation.bycode.swsa[,-1])

  
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
  
  for(i in 2:118) {
    #multiply each category by it's water consumption rate ***(make sure everything is in the right order!)
    coolingwater.total[,i] = coolingwater.total[,i]*as.numeric(cooling.rates.cc[i,7]) #water consumption = 7th column. in Gal/MWh
    coolingwater.dcdc[,i] = generation.bycode.dcdc[,i]*as.numeric(cooling.rates.cc[i,7])
    coolingwater.gwbr[,i] = generation.bycode.gwbr[,i]*as.numeric(cooling.rates.cc[i,7])
    coolingwater.gwfr[,i] = generation.bycode.gwfr[,i]*as.numeric(cooling.rates.cc[i,7])
    coolingwater.gwsa[,i] = generation.bycode.gwsa[,i]*as.numeric(cooling.rates.cc[i,7])
    coolingwater.pdfr[,i] = generation.bycode.pdfr[,i]*as.numeric(cooling.rates.cc[i,7])
    coolingwater.swbr[,i] = generation.bycode.swbr[,i]*as.numeric(cooling.rates.cc[i,7])
    coolingwater.swfr[,i] = generation.bycode.swfr[,i]*as.numeric(cooling.rates.cc[i,7])
    coolingwater.swsa[,i] = generation.bycode.swsa[,i]*as.numeric(cooling.rates.cc[i,7])
    }
  
  #make a total water use column
  coolingwater.total$total.consumption = rowSums(coolingwater.total[,c(2:119)])
  coolingwater.dcdc$dcdc.consumption = rowSums(coolingwater.dcdc[,c(2:119)]) 
  coolingwater.gwbr$gwbr.consumption = rowSums(coolingwater.gwbr[,c(2:119)]) 
  coolingwater.gwfr$gwfr.consumption = rowSums(coolingwater.gwfr[,c(2:119)])
  coolingwater.gwsa$gwsa.consumption = rowSums(coolingwater.gwsa[,c(2:119)])
  coolingwater.pdfr$pdfr.consumption = rowSums(coolingwater.pdfr[,c(2:119)])
  coolingwater.swbr$swbr.consumption = rowSums(coolingwater.swbr[,c(2:119)])
  coolingwater.swfr$swfr.consumption = rowSums(coolingwater.swfr[,c(2:119)])
  coolingwater.swsa$swsa.consumption = rowSums(coolingwater.swsa[,c(2:119)])
  
  #make spredsheets for export to csv -- add PP locations for spatial join
  electric.wc.gal = coolingwater.total[,c(1,120)]
  electric.wc.gal$dcdc.consumption = rowSums(coolingwater.dcdc[,c(2:119)]) 
  electric.wc.gal$gwbr.consumption = rowSums(coolingwater.gwbr[,c(2:119)]) 
  electric.wc.gal$gwfr.consumption = rowSums(coolingwater.gwfr[,c(2:119)])
  electric.wc.gal$gwsa.consumption = rowSums(coolingwater.gwsa[,c(2:119)])
  electric.wc.gal$pdfr.consumption = rowSums(coolingwater.pdfr[,c(2:119)])
  electric.wc.gal$swbr.consumption = rowSums(coolingwater.swbr[,c(2:119)])
  electric.wc.gal$swfr.consumption = rowSums(coolingwater.swfr[,c(2:119)])
  electric.wc.gal$swsa.consumption = rowSums(coolingwater.swsa[,c(2:119)])
  #electric.totalwc.gal = electric.totalwc.gal[which(electric.totalwc.gal$total.consumption > 0),] #keep only the plants consuming water
  electric.wc.gal = merge(plant.locations, electric.wc.gal, all.y = TRUE, by.x = 'Plant.Code', by.y= 'Plant.Id')
  electric.wc.gal = merge(electric.wc.gal, pp.generation[,1:2], all.x=TRUE, by.x='Plant.Code',by.y='Plant.Id')
  
  electric.wc.m3 = electric.wc.gal
  electric.wc.m3[,c(6:14)] = electric.wc.m3[,c(6:14)] / conversion.gal.m3
  
  electric.wc.galMWh = as.data.frame(electric.wc.gal)
  electric.wc.galMWh[,c(6:14)] = electric.wc.galMWh[,c(6:14)]/electric.wc.galMWh[,15]
  electric.wc.m3MWh = as.data.frame(electric.wc.m3)
  electric.wc.m3MWh[,c(6:14)] = electric.wc.m3MWh[,c(6:14)]/electric.wc.m3MWh[,15]
  
  write.csv(electric.wc.gal,'electric-wc-gal.csv',row.names = FALSE)
  write.csv(electric.wc.galMWh, 'electric-wc-galMWh.csv', row.names = FALSE)
  write.csv(electric.wc.m3, 'electric-wc-m3.csv',row.names = FALSE)
  write.csv(electric.wc.m3MWh, 'electric-wc-m3MWh.csv', row.names = FALSE)
  