#1 gigawatt
require(sp)
ratedgoal<-1000000
swm2<-1
require(ggmap)
efficiency<-deveff
number_of_devices<-ratedgoal/(swm2*efficiency)




usa<-get_map(location=as.vector(c(-125.0011, 24.9493, -66.9326, 49.5904)), source="google", zoom=4, color ="bw")

geobasic<-as(geopulled,"data.frame")
geobasic$MWh<-geobasic$KWH*.001
geobasic$GWh<-geobasic$MWh*.001

locationsandpotential<-ggmap(usa, extent = 'panel', maprange = TRUE)+geom_point(data=geobasic,aes(x=lon,y=lat,colour=GWh, size=percent))+scale_colour_gradient(low="blue",high="red")+ggtitle("Locations of US Army Installations and Solar Energy Potential")+scale_size(guide=FALSE)
locationsandpotential
ggsave("geo/mapofinstallations.png")

#ggmap(usa, extent = 'panel', maprange = TRUE)+geom_point(data=unique(fullsetb),aes(x=site_lon,y=site_lat,colour=totkwh, size=totkwh))+scale_colour_gradient(low="blue",high="red")+ggtitle("Locations of US Army Installations and Solar Energy Potential")+scale_size(guide=FALSE)

#add thing for each site that as installations increase the likelihood of installing more decreases



####This is the code for the transportation part of the model
###What is the manufacturing location?#######


require(RCurl)


origins<-paste(manufacture_locations,collapse="|")
part1<-"https://maps.googleapis.com/maps/api/distancematrix/json?"
part2<-"origins="
part2a<-origins
part3<-"&destinations="
part4<-"&mode=driving&key="
google_API_KEY<-"AIzaSyAslkcbAgUfvwcaHFhwD_hCnak9Dg82yNo"


beg<-seq(1,nrow(partset),10)
end<-c(seq(10,nrow(partset),10),nrow(partset))
partset$city<-NA

#for(k in 1:ceiling(nrow(partset)/10)){
#dest_string<-paste(partset$site_lat[beg[k]:end[k]],partset$site_lon[beg[k]:end[k]], sep=",",collapse="|")
#dir_address<-paste(part1,part2,part2a,part3,dest_string,part4,google_API_KEY,sep="")
#file=getURL(dir_address)
#goog_data<-fromJSON(file)
#for(i in beg[k]:end[k]){
 #   p=i-beg[k]+1
  #  rs<-c()
   # for(w in 1:length(manufacture_locations)){
    #    if(length(goog_data$rows[[w]]$elements[[p]]$distance$value)>=1){
     #     rs<-append(rs,goog_data$rows[[w]]$elements[[p]]$distance$value)
     #   }
#    }
 #   if(length(rs)>=1)
  #  {partset$distance[i]<-min(rs); partset$city[i]<-manufacture_locations[which(rs==min(rs))]}
   # Sys.sleep(5)
  #  }
#}

#write.csv(partset,"Model_files/partset.csv"

partset<-read.csv("Model_files/partset.csv")
summary(partset)

####

##distance####
distance<-data.frame(partset$site_lon)
head(distance)
distance$km<-partset$distance/1000     
summary(distance)
for(i in 1:nrow(distance)){
  if(is.na(distance$km[i])==TRUE){distance$km[i]<-median(na.omit(distance$km))
  }
}
head(distance)
colnames(distance)<-c("site_lon","min_km")
summary(distance)
dat<-merge(fullsetb, distance, by="site_lon")

summary(fullsetb)
sum(fullsetb$kwh)
sum(dat$kwh)
kilometers_shipping<-sum(dat$min_km)
kilometers_shipping
geo_origin<-geocode(manufacture_locations,output=c("latlon"))
geo_origin$city<-manufacture_locations


dum_dat<-merge(partset,geo_origin, by="city")
dum_dat$id<-1:nrow(dum_dat)
head(dum_dat)
top<-dum_dat[,c(11,3,4,6)]
head(bottom)
bottom<-dum_dat[,c(11,9,10,6)]
colnames(top)<-c("id","lon","lat","totkwh")
colnames(bottom)<-c("id","lon","lat","totkwh")
paths<-rbind(top,bottom)
#Phoenix
#Los Angeles
#Denver
#Dallas
#San Antonio
#Atlanta
#Chicago

mapsusa<-ggmap(usa, extent = 'panel', maprange = TRUE)

head(paths)
ggplot()+geom_path(data=paths,aes(x=lon,y=lat,group=id, colour=totkwh/sum(totkwh,na.rm=TRUE),size=totkwh/sum(totkwh,na.rm=TRUE)), lineend="round")+ggtitle("Map of Origins and Destinations for PV Panels\n and % of Produced KWH")+scale_colour_continuous("% Total Production",low="yellow",high="red")+scale_size(guide="none")

mapship<-mapsusa+geom_path(data=paths,aes(x=lon,y=lat,group=id, colour=totkwh/sum(totkwh,na.rm=TRUE),size=totkwh/sum(totkwh,na.rm=TRUE)), lineend="round")+ggtitle("Map of Origins and Destinations for PV Panels\n and % of Produced KWH")+scale_colour_continuous("% Total Production",low="yellow",high="red")+scale_size(guide="none")
mapship
ggsave("Figures/mapship.png",height=10,width=7)
