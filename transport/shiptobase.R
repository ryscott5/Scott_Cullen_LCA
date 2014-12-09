##shipping_code

source("impact-methods/rOpenLCA.R")
Thickness_of_graphene<-(7*10^-7)
volumeper<-(m2size*100)^2*Thickness_of_graphene
volumeper
gramsperrun<-volumeper*2.26
panel_mass_graphene<-gramsperrun/1000
tkm_graphene<-sum(dat$min_km*((panels*panel_mass_graphene)/1000))

am<-read.csv("transport/technology_matrix.csv", header=FALSE, stringsAsFactors=FALSE)

bmat<-read.csv("transport/intervention_matrix.csv", header=TRUE,stringsAsFactors=FALSE)

fv<-c(tkm_graphene)
lcres<-lci(am,bmat,fv)
lcres[[1]]
tport<-lciaformat(lcres[[1]])

tportf<-lciaTRACI(tport)
graphene_shiptobase<-TRACIsum(tportf)


panelmass_graphitep<-.015228
tkm_graphitepaint<-sum(dat$min_km*((panels*panelmass_graphitep)/1000))
fv<-c(tkm_graphitepaint)
tport<-lciaformat(lci(am,bmat,fv)[[1]])
tportf<-lciaTRACI(tport)
graphitepaint_shiptobase<-TRACIsum(tportf)

panelmass_molybdenum<-0.0102
tkm_molybdenum<-sum(dat$min_km*(panels*panelmass_molybdenum)/1000)
fv<-c(tkm_molybdenum)
tport<-lciaformat(lci(am,bmat,fv)[[1]])
tportf<-lciaTRACI(tport)
molybdenum_shiptobase<-TRACIsum(tportf)
help(data.frame)
data.frame(molybdenum_shiptobase)
jt<-data.frame("acidification","ecotoxicity","eutrophication","globalwarming","ozonedepletion","photochemicaloxidation","carcinogenics","non.carcinogentics","respiratory.effects")
colnames(jt)<-impfacts
jt[,1:9]<-as.numeric(jt[,1:9])
jt[1,]<-t(molybdenum_shiptobase[,2]/normfacts)
jt[2,]<-t(graphene_shiptobase[,2]/normfacts)
jt[3,]<-t(graphitepaint_shiptobase[,2]/normfacts)
jt$labels<-c("Moly","Graphene","Graphite")
head(jt)
require(reshape2)
jnew<-melt(jt,id="labels")
require(ggplot2)
shippingscores<-ggplot(jnew)+geom_point(aes(x=variable,y=log(value),colour=labels),size=5)+theme_bw()+theme(axis.text.x = element_text(angle = 30, hjust = 1))+scale_colour_discrete("Material")+ylab("this needs to be weighted")+ggtitle("Impact of Transportation on Device Impact")+xlab("Impact Area")
print(shippingscores)

