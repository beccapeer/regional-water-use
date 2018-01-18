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
eia860.file.s2 = '2___Plant_Y2014.xlsx' #860 schedule 2
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
page5.purchasedfuel = as.data.table(read.xlsx(eia923.file.s2t5, sheet = 5, startRow = 6)) #purchased fuel data, including coal mine state & amount purchased


#EIA 860 spreadsheets
#setwd('~')
setwd(eia860.folder)
plantinfo = as.data.table(read.xlsx(eia860.file.s2, sheet = 1, startRow = 2)) #information on location (lat, long)


# define constants --------------------------------------------------------

#conversion factors


#average water use rates for primary fuels 



# make pivot tables -------------------------------------------------------

#pivoting reported fuel consumption (MMBTU) for EIA fuel codes 
reported.fuels = dcast(page1.gensandfuel, Plant.Id~Reported.Fuel.Type.Code, value.var = 'Total.Fuel.Consumption.MMBtu', fun.aggregate = sum)


#pivotting reported fuel use for coal by coal type and coal mine state
  #calculate total MMBTU used for each entry (multiplying average heat content of coal by physical units purchased. (MMBTU/ton *tons purchased)
purchased.coal 


# primary energy water use ------------------------------------------------

#calculate total fuel consumption (MMBTU) for each fuel category defined by EG
plant.locations = plantinfo[,c(3,4,7,10,11)]
fuel.categories$Plant.Id = reported.fuels[,c(1)]

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

##reassign feul categories (EG's fuel categories)
fuel.categories$Hydropower = reported.fuels.m[,c("WAT")]
fuel.categories$Solar = reported.fuels.m[,c("SUN")]
fuel.categories$Wind = reported.fuels.m[,c("WND")]
fuel.categories$Geothermal = reported.fuels.m[,c("GEO")]
fuel.categories$Solid.Biomass.RDF = rowSums(reported.fuels.m[,c("AB","BLQ","MSN","MSB","OBL","OBS","SLW","TDF","WDL","WDS","OBRDF")])
fuel.categories$Biogas= rowSums(reported.fuels.m[,c("LFG","OBG")])
fuel.categories$Oil = rowSums(reported.fuels.m[,c("DFO","JF","KER","PC","PG","RFO","SGP","WO","OOIL")])
fuel.categories$Natural.Gas = rowSums(reported.fuels.m[,c("NG", "OG","ONG")])
fuel.categories$Uranium = reported.fuels.m[,c("NUC")]
fuel.categories$Lignite.Coal = reported.fuels.m[,c("LIG")]
fuel.categories$Bituminous.Coal = rowSums(reported.fuels.m[,c("BIT","BFG","RC","SC","SGC","WC","OBIT")])
fuel.categories$Subbituminous.Coal = rowSums(reported.fuels.m[,c("SUB", "OSUB")])
#fuel.categories$Unknown = reported.fuels.m[,c("UNK")]

##merge plant.locations with fuel.categories for primary energy MMBTU dataframe
primary.energy.mmbtu = merge(x=plant.locations, y=fuel.categories, all.x=TRUE, by.x="Plant.Code", by.y="Plant.Id")

##remove plants outside the continental US (Alaska=AK, Hawaii=HI)
primary.energy.mmbtu = primary.energy.mmbtu[which(State != "AK")]
primary.energy.mmbtu = primary.energy.mmbtu[which(State != "HI")]


