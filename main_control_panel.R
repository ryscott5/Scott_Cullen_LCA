###MAIN CONTROL FOR MODEL

require(ggthemes)
require(RCurl)
require(scales)
getwd()
###Enter location of CZTS_model folder
setwd("/Users/Ryan/Git/Scott_Cullen_LCA")
#What is the anticipated device efficiency

deveff<-.105

#How big is each device?
m2size<-1


#Desired End Capacity in Kilowatts Installed
desired_capacity_kw<-1000000

#Size of each installation
System_Size<-4

#Runs Code for Base Locations, returns actual kwh/year for year 1 that will be produced given the capacity and the geocodes of the locations.

source("Model_files/area.R",echo=TRUE)

##Annual Produced KWH
kw_actual/desired_capacity_kw
kwh_produced_year

#What is the manufacture location? Enter in form c("PHOENIX+AZ","ATLANTA+GA")

manufacture_locations<-c("Phoenix+AZ","Atlanta+GA","Dallas+TX")

source("geo/shippingmaps.R",echo=TRUE)

###Reports Number of Kilometers that need to be shipped from manufacture to bases based on current estimates###

kilometers_shipping
source("Model_files/normalization.R",echo=TRUE)
source("transport/shiptobase.R",echo=TRUE)
print(shippingscores)

#Number of "installations" that are needed at set size to achieve desired capacity
print(reps)

###LCA Results
source("Graphite Paint/graphite_paint_model.R",echo=TRUE)
source("Graphene/graphene.R",echo=TRUE)
source("Molybdenum_glass_ito/Molybdenum_glass_ito.R",echo=TRUE)

##Load Normalization Data
source("Model_files/normalization.R",echo=TRUE)

##Conduct full LCA and analyze results
source("Model_files/LCA_ProcessResults.R",echo=TRUE)
print(resultgraph1)

resultgraph1

