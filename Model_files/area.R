require(rjson)

###4 KW System Size
device_rating<-(deveff*1000)*m2size
panels<-(System_Size*1000)/device_rating
area_m2<-m2size*panels
reps=desired_capacity_kw/System_Size

# 
# locs<-read.csv("geo/locates.csv")
# head(locs)
# require(ggmap)
# locs$Base<-as.character(locs$Base)
# 
# geos<-geocode(locs$Base,output=c("latlon"))
# require(sp)
# geopulled<-data.frame(geos[1],geos[2])
# head(geopulled)
# geopulled$KWH<-locs$KWH
# geopulled$percent<-geopulled$KWH/sum(geopulled$KWH)
# geopulled$Base<-locs$Base
# geopulled<-na.omit(geopulled)
# head(geopulled)
# rownames(geopulled)<-seq(1,nrow(geopulled),1)
# coordinates(geopulled)<-cbind(geopulled$lon,geopulled$lat)
# 
# 
# NRELapikey<-"CvvpGjrazEu00pgknfaVkLqfaj7Xla29LlDEcR2H"
# 
# geopulled$lattilt<-NA
# weathdat<-list()
# for(i in 1:nrow(geopulled)){
#   litx<-coordinates(geopulled)[,1][i]
#   lity<-coordinates(geopulled)[,2][i]
#   parta<-"http://developer.nrel.gov/api/solar/data_query/v1.json?api_key="
#   partb<-"&lat="
#   latstr<-lity
#   partc<-"&lon="
#   longstr<-litx
#   json_file<-paste(parta,NRELapikey,partb,latstr,partc,longstr, sep="")
#   json_data<-fromJSON(file=json_file)
#   weathdat[[i]]<-json_data$output
#   Sys.sleep(.3)
# }
# 
# soldat<-list()
# for(i in 1:nrow(geopulled)){
#   litx<-coordinates(geopulled)[,1][i]
#   lity<-coordinates(geopulled)[,2][i]
#   parta<-"http://developer.nrel.gov/api/solar/solar_resource/v1.json?api_key="
#   partb<-"&lat="
#   latstr<-lity
#   partc<-"&lon="
#   longstr<-litx
#   json_file<-paste(parta,NRELapikey,partb,latstr,partc,longstr, sep="")
#   json_data<-fromJSON(file=json_file)
#   soldat[[i]]<-json_data$outputs
#   print(i)
# }
# soldat[[1]]
# for(i in 1:nrow(geopulled)){
#   if(soldat[[i]]$avg_lat_tilt=="no data") {geopulled$lattilt[i]<-5.29}
#   else{geopulled$lattilt[i]<-soldat[[i]]$avg_lat_tilt$annual[1]}
#   print(i)
# }
# 
# geopulled$id<-1:nrow(geopulled)
#write.csv(geopulled, "Model_files/geopulled.csv")

geopulled<-read.csv("Model_files/geopulled.csv")

site_ids<-sample(geopulled$id,reps,replace=TRUE,prob=geopulled$percent)

site_lattilts<-geopulled[site_ids,]$lattilt
site_lon<-geopulled[site_ids,]$lon
site_lat<-geopulled[site_ids,]$lat
derate<-.77
efficiency<-deveff
kwh<-site_lattilts*area_m2*derate*efficiency*365
outs<-data.frame(site_lon,site_lat,kwh)


totalkwhperlon<-tapply(outs$kwh,factor(outs$site_lon),sum)
new_lon<-row.names(data.frame(tapply(outs$kwh,factor(outs$site_lon),sum)))
results<-data.frame(new_lon,totalkwhperlon)
results$new_lon<-as.numeric(as.character(new_lon))
colnames(results)<-c("site_lon","totkwh")
fullset<-merge(outs,results, by=c("site_lon"))
require(sp)
coordinates(fullset)<-c("site_lon","site_lat")
fullsetb<-as(fullset,"data.frame")
partset<-unique(fullsetb)
kw_actual<-sum(partset$totkwh, na.rm=TRUE)/(365*24)
kwh_produced_year<-sum(partset$totkwh)


#dat<-merge(fullsetb, distance, by="site_lon")
#dat$kgkm=mass_one_install_graphene*(dat$min_km)
#transport_kgkm_graphene<-sum(dat$kgkm)




#site_ids_rep<-rep(sample(geopulled$id,reps,replace=TRUE,prob=geopulled$percent),10000)
#site_lattilts<-geopulled[site_ids_rep,]$lattilt
#site_lon<-geopulled[site_ids_rep,]$lon
#kwh<-site_lattilts*area_m2*derate*efficiency*365
#fullsetbc<-data.frame(site_lattilts,site_lon,kwh)
#dat<-merge(fullsetbc, distance, by="site_lon")
#distance1000<-append(distance10000,sum(dat(distance)))

#mean(dat$)
