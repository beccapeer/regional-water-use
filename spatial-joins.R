#------------------------------------------------------------------------------|
#                 SPATIAL JOINING EGRID REGIONS WITH WATER USE DATA
# CREATOR: R.A.M. Peer
# DESCRIPTION: code to perform spatial joins of eGRID regions with water use 
#              data from power plants (water embedded in primary energy and 
#              electricity generation).
#------------------------------------------------------------------------------|
#******************************************************************************|
#------------------------------------------------------------------------------|
# USER INPUT: set input parameters
#------------------------------------------------------------------------------|
# wd = '~/Documents/Graduate/Journal Papers/in-progress/EST_regional-water-rates/data/'
wd = '~/Desktop/Becca\ Files/ERL-paper/data/'

#------------------------------------------------------------------------------|
# MAIN SCRIPT
#------------------------------------------------------------------------------|

#load packages
library(data.table)
library(openxlsx)
library(xlsx)
library(dplyr)
library(reshape2)
library(rgdal)
library(sp)
library(raster)

setwd(wd)

# read in csv's with data for total generation & consumptive water embedded in primary energy and electricity generation
##GENERATION
points.generation = read.csv('power-plant-generation.csv')
points.generation$Longitude[is.na(points.generation$Longitude)] = 0
points.generation = points.generation[which(points.generation$Longitude < 0 ),]
coordinates(points.generation) = ~Longitude+Latitude
proj4string(points.generation) = CRS("+init=epsg:4326")

points.genbyfuel = read.csv('generation-byfuel.csv')
points.genbyfuel$Longitude[is.na(points.genbyfuel$Longitude)] = 0
points.genbyfuel = points.genbyfuel[which(points.genbyfuel$Longitude < 0 ),]
coordinates(points.genbyfuel) = ~Longitude+Latitude
proj4string(points.genbyfuel) = CRS("+init=epsg:4326")

points.genbycool = read.csv('generation-bycool.csv')
points.genbycool$Longitude[is.na(points.genbycool$Longitude)] = 0
points.genbycool = points.genbycool[which(points.genbycool$Longitude < 0 ),]
coordinates(points.genbycool) = ~Longitude+Latitude
proj4string(points.genbycool) = CRS("+init=epsg:4326")

##BY WATER TYPE
points.electricity = read.csv('electric-wc-m3.csv')
#points.electricity = read.csv('electric-wc-gal.csv')
points.electricity$Longitude[is.na(points.electricity$Longitude)] = 0
points.electricity = points.electricity[which(points.electricity$Longitude < 0 ),]
coordinates(points.electricity) = ~Longitude+Latitude
proj4string(points.electricity) = CRS("+init=epsg:4326")


points.penergy = read.csv('water-consumption-pe-m3.csv')
#points.penergy = read.csv('water-consumption-pe-gal.csv')
points.penergy$Longitude[is.na(points.penergy$Longitude)] = 0
points.penergy = points.penergy[which(points.penergy$Longitude < 0),]
coordinates(points.penergy) = ~Longitude+Latitude
proj4string(points.penergy) = CRS("+init=epsg:4326")


##BY FUEL
points.elec.byfuel = read.csv('elec-wc-byfuel-m3.csv')
#points.elec.byfuel = read.csv('elec-wc-byfuel-gal.csv')
points.elec.byfuel$Longitude[is.na(points.elec.byfuel$Longitude)] = 0
points.elec.byfuel[,6:17][is.na(points.elec.byfuel[,6:17])] = 0
points.elec.byfuel = points.elec.byfuel[which(points.elec.byfuel$Longitude < 0),]
coordinates(points.elec.byfuel) = ~Longitude+Latitude
proj4string(points.elec.byfuel) = CRS("+init=epsg:4326")

points.pe.byfuel = read.csv('pe-wc-by-fuel-m3.csv')
#points.pe.byfuel = read.csv('pe-wc-by-fuel-gal.csv')
points.pe.byfuel$Longitude[is.na(points.pe.byfuel$Longitude)] = 0
# is.na(points.pe.byfuel[,6:19]) = sapply(points.pe.byfuel[,6:19], is.infinite)
points.pe.byfuel[,6:25][is.na(points.pe.byfuel[,6:25])] = 0
points.pe.byfuel = points.pe.byfuel[which(points.pe.byfuel$Longitude < 0),]
coordinates(points.pe.byfuel) = ~Longitude+Latitude
proj4string(points.pe.byfuel) = CRS("+init=epsg:4326")



##BY COOLING SYSTEM (electricity only)
points.elec.bycool = read.csv('elec-wc-bycool-m3.csv')
#points.elec.bycool = read.csv('elec-wc-bycool-gal.csv')
points.elec.bycool$Longitude[is.na(points.elec.bycool$Longitude)] = 0
points.elec.bycool[,6:12][is.na(points.elec.bycool[,6:12])] = 0
points.elec.bycool = points.elec.bycool[which(points.elec.bycool$Longitude < 0),]
coordinates(points.elec.bycool) = ~Longitude+Latitude
proj4string(points.elec.bycool) = CRS("+init=epsg:4326")

egrid.hydro.wc = read.csv('hydro-egrid-wc.csv')

# load shapefile for eGRID regions
egrid.regions = shapefile('~/Documents/Graduate/Journal Papers/in-progress/EST_regional-water-rates/data/shapefiles/egrid_subregions/eGRID2014_subregions.shp')
proj4string(egrid.regions) = CRS("+init=epsg:4326") #throws a warning, but ignore bc already in WGS84

# perform spatial joins
#generation
generation.join = over(points.generation, egrid.regions)
egrid.generation = points.generation@data
egrid.generation$zips_for_G = generation.join$zips_for_G
egrid.generation = as.data.table(egrid.generation)
egrid.generation = egrid.generation[,.(Generation = sum(total.gen)),by=zips_for_G]
egrid.generation = egrid.generation[-25,] #remove NA row


genbyfuel.join = over(points.genbyfuel, egrid.regions)
egrid.genbyfuel = points.genbyfuel@data
egrid.genbyfuel$zips_for_G = genbyfuel.join$zips_for_G
egrid.genbyfuel = as.data.table(egrid.genbyfuel)
egrid.genbyfuel = egrid.genbyfuel[,.(Biomass = sum(Solid.Biomass.RDF),
                                     Biogas = sum(Biogas),
                                     Coal = sum(Bituminous.Coal, Lignite.Coal, Subbituminous.Coal),
                                     Oil = sum(Oil),
                                     Natural.Gas = sum(Natural.Gas),
                                     Geothermal = sum(Geothermal),
                                     Uranium = sum(Uranium),
                                     Solar = sum(Solar,Solar.Thermal),
                                     Wind = sum(Wind)), by=zips_for_G]
egrid.genbyfuel = egrid.genbyfuel[-25,]
egrid.genbyfuel = merge(egrid.genbyfuel, egrid.hydro.wc[,c(1,2)], all.x=T, by.x='zips_for_G', by.y='eGrid.region')


genbycool.join = over(points.genbycool, egrid.regions)
egrid.genbycool = points.genbycool@data
egrid.genbycool$zips_for_G = genbycool.join$zips_for_G
egrid.genbycool = as.data.table(egrid.genbycool)
egrid.genbycool = egrid.genbycool[,.(None = sum(NA.),
                                     Dry.Cooling = sum(DC),
                                     Once.Through = sum(ON),
                                     Ponds = sum(PN),
                                     Recirculating = sum(RC),
                                     Hybrid = sum(HB)),by=zips_for_G]
egrid.genbycool = egrid.genbycool[-25,] #remove NA row
egrid.genbycool = merge(egrid.genbycool, egrid.hydro.wc[,c(1,2)], all.x=T, by.x='zips_for_G', by.y='eGrid.region')
egrid.genbycool$no.cooling = egrid.genbycool$None - egrid.genbycool$annual.hydro.generation.MWh
egrid.genbycool = egrid.genbycool[,-2]

#electricity water use
pp.egrid.region = over(points.electricity, egrid.regions)
# pp.egrid.region = as.data.frame(pp.egrid.region)
pp.egrid.region$Plant.Id = points.electricity@data$Plant.Code
pp.egrid.region$total.consumption = points.electricity@data$total.consumption
pp.egrid.region$dcdc.consumption = points.electricity@data$dcdc.consumption
pp.egrid.region$gwbr.consumption = points.electricity@data$gwbr.consumption
pp.egrid.region$gwfr.consumption = points.electricity@data$gwfr.consumption
pp.egrid.region$gwsa.consumption = points.electricity@data$gwsa.consumption
pp.egrid.region$pdfr.consumption = points.electricity@data$pdfr.consumption
pp.egrid.region$swbr.consumption = points.electricity@data$swbr.consumption
pp.egrid.region$swfr.consumption = points.electricity@data$swfr.consumption
pp.egrid.region$swsa.consumption = points.electricity@data$swsa.consumption
#pp.egrid.region$generation  = points.electricity@data$rowSums.generation.bycode.total....1..

pp.egrid.region = as.data.table(pp.egrid.region)
total.elec.wc.egrid = pp.egrid.region[,.(total.wc = sum(total.consumption),
                                         gwbr.consumption = sum(gwbr.consumption),
                                         gwfr.consumption = sum(gwfr.consumption),
                                         gwsa.consumption = sum(gwsa.consumption),
                                         pdfr.consumption = sum(pdfr.consumption),
                                         swbr.consumption = sum(swbr.consumption),
                                         swfr.consumption = sum(swfr.consumption),
                                         swsa.consumption = sum(swsa.consumption)), by=zips_for_G]
total.elec.wc.egrid = total.elec.wc.egrid[-24,] #remove NA row
#make alphabetical order
total.elec.wc.egrid = total.elec.wc.egrid[order(zips_for_G)]


elec.wc.byfuel.join = over(points.elec.byfuel, egrid.regions)
egrid.elec.wc.byfuel = points.elec.byfuel@data
egrid.elec.wc.byfuel$zips_for_G = elec.wc.byfuel.join$zips_for_G
egrid.elec.wc.byfuel = as.data.table(egrid.elec.wc.byfuel)
egrid.elec.wc.byfuel = egrid.elec.wc.byfuel[,.(total.consumption = sum(total.consumption),
                                               Biomass = sum(Biomass),
                                               Biogas = sum(Biogas),
                                               Coal = sum(Coal),
                                               Oil = sum(Oil),
                                               Natural.Gas = sum(Natural.Gas),
                                               Geothermal = sum(Geothermal),
                                               Uranium = sum(Uranium),
                                               Other = sum(Other),
                                               Solar = sum(Solar),
                                               Wind = sum(Wind),
                                               Not.Reported = sum(Not.Reported)),by=zips_for_G]
egrid.elec.wc.byfuel = egrid.elec.wc.byfuel[-24,] #remove NA row

elec.wc.bycool.join = over(points.elec.bycool, egrid.regions)
egrid.elec.wc.bycool = points.elec.bycool@data
egrid.elec.wc.bycool$zips_for_G = elec.wc.bycool.join$zips_for_G
egrid.elec.wc.bycool = as.data.table(egrid.elec.wc.bycool)
egrid.elec.wc.bycool = egrid.elec.wc.bycool[,.(total.consumption = sum(total.consumption),
                                               None = sum(None),
                                               Dry.Cooling = sum(Dry.Cooling),
                                               Once.Through = sum(Once.Through),
                                               Ponds = sum(Ponds),
                                               Recirculating = sum(Recirculating),
                                               Hybrid = sum(Hybrid)),by=zips_for_G]
egrid.elec.wc.bycool = egrid.elec.wc.bycool[-24,] #remove NA row

#primary energy water use
pe.egrid.region = over(points.penergy, egrid.regions)
pe.egrid.region$Plant.Id = points.penergy@data$Plant.Code
pe.egrid.region$total.consumption = points.penergy@data$total.consumption
pe.egrid.region$gwbr.consumption = points.penergy@data$gwbr
pe.egrid.region$gwfr.consumption = points.penergy@data$gwfr
pe.egrid.region$gwnro.consumption = points.penergy@data$gwnro
pe.egrid.region$gwsa.consumption = points.penergy@data$gwsa
pe.egrid.region$pdbr.consumption = points.penergy@data$pdbr
pe.egrid.region$pdfr.consumption = points.penergy@data$pdfr
pe.egrid.region$pdnro.consumption = points.penergy@data$pdnro
pe.egrid.region$pdsa.consumption = points.penergy@data$pdsa
pe.egrid.region$swbr.consumption = points.penergy@data$swbr
pe.egrid.region$swfr.consumption = points.penergy@data$swfr
pe.egrid.region$swnro.consumption = points.penergy@data$swnro
pe.egrid.region$swsa.consumption = points.penergy@data$swsa
pe.egrid.region$generation = points.penergy@data$total.gen
pe.egrid.region$generation[is.na(pe.egrid.region$generation)] = 0

pe.egrid.region = as.data.table(pe.egrid.region)
total.pe.wc.egrid = pe.egrid.region[,.(water.consumption = sum(total.consumption), 
                                       gwbr.consumption = sum(gwbr.consumption),
                                       gwfr.consumption = sum(gwfr.consumption),
                                       gwnro.consumption = sum(gwnro.consumption),
                                       gwsa.consumption = sum(gwsa.consumption),
                                       pdbr.consumption = sum(pdbr.consumption),
                                       pdfr.consumption = sum(pdfr.consumption),
                                       pdnro.consumption = sum(pdnro.consumption),
                                       pdsa.consumption = sum(pdsa.consumption),
                                       swbr.consumption = sum(swbr.consumption),
                                       swfr.consumption = sum(swfr.consumption),
                                       swnro.consumption = sum(swnro.consumption),
                                       swsa.consumption = sum(swsa.consumption), 
                                       generation = sum(generation)), by=zips_for_G]
total.pe.wc.egrid = total.pe.wc.egrid[-26,] #remove NA row
#make alphabetical order
total.pe.wc.egrid = total.pe.wc.egrid[order(zips_for_G)]

pe.wc.byfuel.join = over(points.pe.byfuel,egrid.regions)
egrid.pe.wc.byfuel = points.pe.byfuel@data
egrid.pe.wc.byfuel$zips_for_G = pe.wc.byfuel.join$zips_for_G
egrid.pe.wc.byfuel = as.data.table(egrid.pe.wc.byfuel)
egrid.pe.wc.byfuel = egrid.pe.wc.byfuel[,.(total.consumption = sum(total.consumption),
                                               Biomass = sum(Solid.Biomass.RDF),
                                               Biogas = sum(Biogas),
                                               Oil = sum(Oil),
                                               Coal = sum(Bituminous.Coal.app,Bituminous.Coal.int,Bituminous.Coal.rmr, Lignite.Coal.gfc, Lignite.Coal.ngp, Subbituminous.Coal),
                                               Natural.Gas = sum(Natural.Gas),
                                               Geothermal = sum(Geothermal,Geothermal.CA, Geothermal.NV),
                                               Uranium = sum(Uranium),
                                               Solar = sum(Solar, Solar.Thermal),
                                               Wind = sum(Wind),
                                               Generation = sum(total.gen)),by=zips_for_G]
egrid.pe.wc.byfuel = egrid.pe.wc.byfuel[-26,]

#merge hydro water use (in m3) to primary energy water use
total.pe.wc.egrid = as.data.frame(total.pe.wc.egrid)
total.pe.wc.egrid = merge(total.pe.wc.egrid, egrid.hydro.wc[,c(1,4)], all.x=TRUE, by.x='zips_for_G', by.y='eGrid.region')

egrid.pe.wc.byfuel = as.data.frame(egrid.pe.wc.byfuel)
egrid.pe.wc.byfuel = merge(egrid.pe.wc.byfuel, egrid.hydro.wc[,c(1,4)], all.x=T, by.x='zips_for_G', by.y='eGrid.region')

egrid.elec.wc.bycool = as.data.frame(egrid.elec.wc.bycool)
egrid.elec.wc.bycool = merge(egrid.elec.wc.bycool, egrid.hydro.wc[,c(1,4)], all.x=T, by.x='zips_for_G', by.y='eGrid.region')

#sum for total primary energy water consumption and surface freshwater 
total.pe.wc.egrid$total.consumption = total.pe.wc.egrid$water.consumption + total.pe.wc.egrid$EG.water.consumption.m3
total.pe.wc.egrid$swfr.withhydro = total.pe.wc.egrid$swfr.consumption + total.pe.wc.egrid$EG.water.consumption.m3

#remove old columns (without hydro added) for shapefile
total.pe.wc.egrid[,c(2,12,16)] = NULL
total.pe.wc.egrid[13] = NULL

#merge generation (by egrid region) with data for shapefiles
total.elec.wc.egrid = merge(total.elec.wc.egrid, egrid.generation, by.x="zips_for_G", by.y="zips_for_G")
# total.elec.wc.egrid = total.elec.wc.egrid[,-8]
total.pe.wc.egrid = merge(total.pe.wc.egrid,egrid.generation, by.x="zips_for_G", by.y="zips_for_G")
# total.pe.wc.egrid = total.pe.wc.egrid[,-15]
egrid.elec.wc.bycool = merge(egrid.elec.wc.bycool, egrid.generation, by.x="zips_for_G", by.y="zips_for_G")
egrid.elec.wc.bycool = egrid.elec.wc.bycool[,-9]
egrid.elec.wc.byfuel = merge(egrid.elec.wc.byfuel, egrid.generation, by.x="zips_for_G", by.y="zips_for_G")
#egrid.elec.wc.byfuel = egrid.elec.wc.byfuel[,-13]
egrid.pe.wc.byfuel = merge(egrid.pe.wc.byfuel, egrid.generation, by.x="zips_for_G", by.y="zips_for_G")
egrid.pe.wc.byfuel = egrid.pe.wc.byfuel[,-12]

#calculate water consumption intensities for primary energies and electricity
total.elec.wc.egrid$total.wci = total.elec.wc.egrid$total.wc/total.elec.wc.egrid$Generation
total.elec.wc.egrid$gwbr.wci = total.elec.wc.egrid$gwbr.consumption /total.elec.wc.egrid$Generation
total.elec.wc.egrid$gwfr.wci = total.elec.wc.egrid$gwfr.consumption/total.elec.wc.egrid$Generation
total.elec.wc.egrid$gwsa.wci = total.elec.wc.egrid$gwsa.consumption/total.elec.wc.egrid$Generation
total.elec.wc.egrid$pdfr.wci = total.elec.wc.egrid$pdfr.consumption/total.elec.wc.egrid$Generation
total.elec.wc.egrid$swbr.wci = total.elec.wc.egrid$swbr.consumption/total.elec.wc.egrid$Generation
total.elec.wc.egrid$swfr.wci = total.elec.wc.egrid$swfr.consumption /total.elec.wc.egrid$Generation
total.elec.wc.egrid$swsa.wci = total.elec.wc.egrid$swsa.consumption/total.elec.wc.egrid$Generation

total.pe.wc.egrid = as.data.frame(total.pe.wc.egrid)
total.pe.wc.egrid$total.wci = total.pe.wc.egrid$total.consumption /total.pe.wc.egrid$Generation
total.pe.wc.egrid$gwbr.wci = total.pe.wc.egrid$gwbr.consumption /total.pe.wc.egrid$Generation
total.pe.wc.egrid$gwfr.wci = total.pe.wc.egrid$gwfr.consumption /total.pe.wc.egrid$Generation
total.pe.wc.egrid$gwnro.wci = total.pe.wc.egrid$gwnro.consumption /total.pe.wc.egrid$Generation
total.pe.wc.egrid$gwsa.wci = total.pe.wc.egrid$gwsa.consumption /total.pe.wc.egrid$Generation
total.pe.wc.egrid$pdbr.wci = total.pe.wc.egrid$pdbr.consumption /total.pe.wc.egrid$Generation
total.pe.wc.egrid$pdfr.wci = total.pe.wc.egrid$pdfr.consumption /total.pe.wc.egrid$Generation
total.pe.wc.egrid$pdnro.wci = total.pe.wc.egrid$pdnro.consumption /total.pe.wc.egrid$Generation
total.pe.wc.egrid$pdsa.wci = total.pe.wc.egrid$pdsa.consumption /total.pe.wc.egrid$Generation
total.pe.wc.egrid$swbr.wci = total.pe.wc.egrid$swbr.consumption /total.pe.wc.egrid$Generation
total.pe.wc.egrid$swfr.wci = total.pe.wc.egrid$swfr.withhydro /total.pe.wc.egrid$Generation
total.pe.wc.egrid$swnro.wci = total.pe.wc.egrid$swnro.consumption /total.pe.wc.egrid$Generation
total.pe.wc.egrid$swsa.wci = total.pe.wc.egrid$swsa.consumption /total.pe.wc.egrid$Generation

#create total water consumption and total water consumption intensity df's to make "total" layers
total.wc.egrid = merge(total.elec.wc.egrid, total.pe.wc.egrid, all.x = TRUE, by.x = 'zips_for_G', by.y='zips_for_G')
total.wc.egrid$total.wc = total.wc.egrid$total.wc + total.wc.egrid$total.consumption
total.wc.egrid$gwbr.wc = total.wc.egrid$gwbr.consumption.x + total.wc.egrid$gwbr.consumption.y
total.wc.egrid$gwfr.wc = total.wc.egrid$gwfr.consumption.x + total.wc.egrid$gwfr.consumption.y
total.wc.egrid$gwnro.wc = total.wc.egrid$gwnro.consumption
total.wc.egrid$gwsa.wc = total.wc.egrid$gwsa.consumption.x +total.wc.egrid$gwsa.consumption.y
total.wc.egrid$pdbr.wc = total.wc.egrid$pdbr.consumption
total.wc.egrid$pdfr.wc = total.wc.egrid$pdfr.consumption.x + total.wc.egrid$pdfr.consumption.y
total.wc.egrid$pdnro.wc = total.wc.egrid$pdnro.consumption
total.wc.egrid$pdsa.wc = total.wc.egrid$pdsa.consumption
total.wc.egrid$swbr.wc = total.wc.egrid$swbr.consumption.x + total.wc.egrid$swbr.consumption.y
total.wc.egrid$swfr.wc = total.wc.egrid$swfr.consumption +total.wc.egrid$swfr.withhydro
total.wc.egrid$swnro.wc = total.wc.egrid$swnro.consumption
total.wc.egrid$swsa.wc = total.wc.egrid$swsa.consumption.x + total.wc.egrid$swsa.consumption.y

total.wc.egrid$total.wci = total.wc.egrid$total.wci.x + total.wc.egrid$total.wci.y
total.wc.egrid$gwbr.wci = total.wc.egrid$gwbr.wci.x + total.wc.egrid$gwbr.wci.y
total.wc.egrid$gwfr.wci = total.wc.egrid$gwfr.wci.x + total.wc.egrid$gwfr.wci.y
total.wc.egrid$gwnro.wci = total.wc.egrid$gwnro.wci
total.wc.egrid$gwsa.wci = total.wc.egrid$gwsa.wci.x +total.wc.egrid$gwsa.wci.y
total.wc.egrid$pdbr.wci = total.wc.egrid$pdbr.wci
total.wc.egrid$pdfr.wci = total.wc.egrid$pdfr.wci.x + total.wc.egrid$pdfr.wci.y
total.wc.egrid$pdnro.wci = total.wc.egrid$pdnro.wci
total.wc.egrid$pdsa.wci = total.wc.egrid$pdsa.wci
total.wc.egrid$swbr.wci = total.wc.egrid$swbr.wci.x + total.wc.egrid$swbr.wci.y
total.wc.egrid$swfr.wci = total.wc.egrid$swfr.wci.x +total.wc.egrid$swfr.wci.y
total.wc.egrid$swnro.wci = total.wc.egrid$swnro.wci
total.wc.egrid$swsa.wci = total.wc.egrid$swsa.wci.x + total.wc.egrid$swsa.wci.y

total.wc.egrid[,c(2:45)] = NULL


#save as spreadsheets (for reference)
write.csv(egrid.genbyfuel, 'egrid-generation-byfuel.csv', row.names = F)
write.csv(egrid.genbycool, 'egrid-generation-bycool.csv', row.names = F)

#in cubic meters
write.csv(total.elec.wc.egrid, 'egrid-elec-water-consumption.csv',row.names=FALSE)
write.csv(total.pe.wc.egrid, 'egrid-pe-water-consumption.csv',row.names=FALSE)
write.csv(total.wc.egrid, 'egrid-total-water-consumption.csv', row.names = FALSE)
write.csv(egrid.elec.wc.byfuel, 'egrid-elec-wc-byfuel.csv',row.names = F)
write.csv(egrid.elec.wc.bycool, 'egrid-elec-wc-bycool.csv', row.names = F)
write.csv(egrid.pe.wc.byfuel, 'egrid-pe-wc-byfuel.csv',row.names = F)

#in gallons
# write.csv(total.elec.wc.egrid, 'egrid-elec-wc-gal.csv', row.names=FALSE)
# write.csv(total.pe.wc.egrid, 'egrid-pe-wc-gal.csv', row.names=FALSE)
# write.csv(total.wc.egrid, 'egrid-total-wc-gal.csv', row.names=FALSE)
# write.csv(egrid.elec.wc.byfuel, 'egrid-elec-wc-byfuel-gal.csv',row.names = F)
# write.csv(egrid.elec.wc.bycool, 'egrid-elec-wc-bycool-gal.csv', row.names = F)
# write.csv(egrid.pe.wc.byfuel, 'egrid-pe-wc-byfuel-gal.csv',row.names = F)

#save as shapefiles
total.elec.wc.egrid = SpatialPolygonsDataFrame(egrid.regions,total.elec.wc.egrid, match.ID = FALSE) 
total.pe.wc.egrid = SpatialPolygonsDataFrame(egrid.regions,total.pe.wc.egrid, match.ID=FALSE)
total.wc.egrid = SpatialPolygonsDataFrame(egrid.regions, total.wc.egrid, match.ID = FALSE)

#in cubic meters
writeOGR(obj=total.pe.wc.egrid, dsn='pe-wc-V4', layer = "pe-water-consumption", driver = "ESRI Shapefile")
writeOGR(obj=total.elec.wc.egrid, dsn='electric-wc-V3', layer = "electric-water-consumption", driver = "ESRI Shapefile")
writeOGR(obj=total.wc.egrid, dsn= 'total-wc-V5', layer="total-wc", driver = "ESRI Shapefile")

#in gallons
# writeOGR(obj=total.pe.wc.egrid, dsn="pe-wc-gal", layer = 'pe-wc-gal', driver = 'ESRI Shapefile')
# writeOGR(obj=total.elec.wc.egrid, dsn = 'electric-wc-gal', layer = 'electric-wc-gal', driver = "ESRI Shapefile")
# writeOGR(obj=total.wc.egrid, dsn='total-wc-gal', layer = "total-wc-gal", driver = "ESRI Shapefile")
 
