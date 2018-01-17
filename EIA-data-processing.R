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
eia860.file.s2 = '2___Plant_Y2014' #860 schedule 2
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
plantinfo = as.data.table(read.xlsx(eia860.file.s2, sheet = 1, startRow = 3)) #information on location (lat, long)


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


