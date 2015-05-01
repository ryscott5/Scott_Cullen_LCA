1 gigawatt
require(sp)
ratedgoal<-1000000
swm2<-1

number_of_devices<-ratedgoal/(swm2*efficiency)



x
ggplot()+geom_point(aes(x=geopulled$lon,y=geopulled$lat,size=geopulled$KWH))
usa<-get_map(location=as.vector(c(-125.0011, 24.9493, -66.9326, 49.5904)), source="google", zoom=4, color ="bw")

geobasic<-as(geopulled,"data.frame")
geobasic$MWh<-geobasic$KWH*.001
geobasic$GWh<-geobasic$MWh*.001

ggmap(usa, extent = 'panel', maprange = TRUE)+geom_point(data=geobasic,aes(x=lon,y=lat,colour=GWh, size=percent))+scale_colour_gradient(low="blue",high="red")+ggtitle("Locations of US Army Installations and Solar Energy Potential")+scale_size(guide=FALSE)
ggsave("Google Drive/CZTS_model/geo/mapofinstallations.png")
help(ggmap)





ggmap(usa, extent = 'panel', maprange = TRUE)+geom_point(data=unique(fullsetb),aes(x=site_lon,y=site_lat,colour=totkwh, size=totkwh))+scale_colour_gradient(low="blue",high="red")+ggtitle("Locations of US Army Installations and Solar Energy Potential")+scale_size(guide=FALSE)

#add thing for each site that as installations increase the likelihood of installing more decreases



####This is the code for the transportation part of the model
###What is the manufacturing location?#######


require(RCurl)
dest_string<-paste(partset$site_lat[1:50],partset$site_lon[1:50], sep=",",collapse="|")
help(paste)
origins<-paste(manufacture_locations,collapse="|")
part1<-"https://maps.googleapis.com/maps/api/distancematrix/json?"
part2<-"origins="
part2a<-origins
part3<-"&destinations="
part3a<-dest_string
part4<-"&mode=driving&key="
google_API_KEY<-PUTKEYHERE
dir_address<-paste(part1,part2,part2a,part3,part3a,part4,google_API_KEY,sep="")
file=getURL(dir_address)
file
goog_data<-fromJSON(file)
for(i in 1:50){
    rs<-c()
    for(k in 1:length(manufacture_locations)){
        if(length(goog_data$rows[[k]]$elements[[i]]$distance$value)>=1){
          rs<-append(rs,goog_data$rows[[k]]$elements[[i]]$distance$value)
        }
    }
    if(length(rs)>=1)
    {partset$distance[i]<-min(rs)}
}

goog_data$rows[[2]]$elements[[2]]$distance$value
head(partset)

dest_string2<-paste(partset$site_lat[51:nrow(partset)],partset$site_lon[51:nrow(partset)], sep=",",collapse="|")
dir_address2<-paste(part1,part2,part2a,part3,dest_string2,part4,google_API_KEY,sep="")
file2=getURL(dir_address2)
goog_data2<-fromJSON(file2)

for(i in 1:(nrow(partset)-50)){
  rs<-c()
  for(k in 1:length(manufacture_locations)){
    if(length(goog_data2$rows[[k]]$elements[[i]]$distance$value)>=1){
      rs<-append(rs,goog_data2$rows[[k]]$elements[[i]]$distance$value)
    }
  }
  if(length(rs)>=1)
  {partset$distance[i+50]<-min(rs)}
}


summary(partset)

####

##distance####
distance<-data.frame(partset$site_lon)
distance$km<-partset$distance/1000

for(i in 1:nrow(distance)){
  if(is.na(distance$km[i])==TRUE){distance$km[i]<-median(na.omit(distance$km))
  }
}
colnames(distance)<-c("site_lon","min_km")
dat<-merge(fullsetb, distance, by="site_lon")
dat$kgkm=mass_one_install_graphene*(dat$min_km)
transport_kgkm_graphene<-sum(dat$kgkm)
transport_kgkm_graphene

kilometers_shipping<-sum(dat$min_km)

#Phoenix
#Los Angeles
#Denver
#Dallas
#San Antonio
#Atlanta
#Chicago


