source("impact-methods/rOpenLCA.R")

moly_mass_device<-.01022*100*100*.0001
#Specify number of MC trials

am<-read.csv("Molybdenum_glass_ito/technology_matrix.csv", header=FALSE,stringsAsFactors=FALSE)

bm<-read.csv("Molybdenum_glass_ito/intervention_matrix.csv", header=TRUE,stringsAsFactors=FALSE)

fv<-c(panels*reps*moly_mass_device,rep(0,nrow(am)-1))
fv


impfacts<-c("Acidification","Ecotoxicity","Eutrophication","Global Warming","Ozone Depletion","Photochemicaloxidation","Carcinogenic","Non Carcinogenic","Respiratory Effects")
runouts<-data.frame(impfacts)


#runs model all of the uncertain paramters must be assigned to values in the a or b matrix

#Matrix Inverse and TRACI impacts out

minv<-lci(am,bm,fv)
runouts[,2]<-TRACIsum(lciaTRACI(lciaformat(minv[[1]])))[,2]
runouts
test2<-contribute(minv)
test2
rems1<-melt(test2$percents)
rems1$actual<-melt(test2$values)$value
require(ggplot2)
rems1sub<-subset(rems1, rems1$value!=0)
rems1sub<-subset(rems1sub, is.na(rems1sub$value)==FALSE)
levels(rems1sub$Var2)<-gsub(",","\n",levels(rems1sub$Var2))
levels(rems1sub$Var1)<-c("Acidification","Ecotoxicity","Eutrophication","Global Warming","Ozone Depletion","Smog Potential","Human Carcinogens","Human Non Carcinogens","Repiratory Effects")
head(rems1sub)



molybdenum_total<-runouts[,2]+molybdenum_shiptobase[,2]
molybdenum_total
process_moly<-(runouts[,2]/molybdenum_total)
transport_moly<-(molybdenum_shiptobase[,2]/molybdenum_total)
runouts
compares<-runouts
compares[,2]<-process_moly
compares[,3]<-transport_moly


###for combining with the transport data
molyships<-molybdenum_shiptobase
molyships$Var2<-"Transport"
molyships$value<-NA
molyships
levels(molyships$impfacts)<-c("Acidification","Human Carcinogens","Ecotoxicity","Eutrophication","Global Warming","Human Non Carcinogens","Ozone Depletion","Smog Potential","Repiratory Effects")
molyships
head(rems1sub)
colnames(molyships)<-c("Var1","actual","Var2","value")
molycontwtransp<-rbind(molyships,rems1sub)
formtest<-data.frame(Var1=c("Acidification","Ecotoxicity","Eutrophication","Global Warming","Ozone Depletion","Smog Potential","Human Carcinogens","Human Non Carcinogens","Repiratory Effects"),totalvalue=molybdenum_total)
formtest
molycontwtransp<-merge(molycontwtransp,formtest, by="Var1")
molycontwtransp
molycontwtransp$value<-molycontwtransp$actual/molycontwtransp$totalvalue

#rems1sub for no trasnport
molycont<-ggplot(molycontwtransp, aes(Var1, value*100,fill=Var2))+geom_bar(stat="identity",position="stack")+geom_hline(yintercept=0)+ggtitle("Contribution of Production Processes to Overall Impact for Molybdenum")+theme_bw()+xlab("Impact Categories")+ylab("Percent of Total Impact")+theme(text = element_text(family="Times New Roman",size =12))+theme(axis.text.x=element_text(angle=90))+coord_flip()+scale_fill_tableau("Process Name",palette="tableau20")

molycont

#theme_bw()+xlab("")+ylab("Percent of Total Impact")+scale_x_discrete(breaks=NULL)+theme(axis.text.x = element_blank())+scale_fill_discrete("Unit Process")+theme(text=element_text(family="Times New Roman"))+theme(plot.title=element_text(hjust=-.5))
help(ggsave)
ggsave("Figures/moly_production_contribution.png",molycont,width=8,height=5,units=c("in"))

colnames(compares)<-c("Category","Process","Transport")
compmelt_moly<-melt(compares)
levels(compmelt_moly$Category)<-c("Acidification","Carcinogenic","Ecotoxicity","Eutrophication","Global Warming","Non Carcinogenic","Ozone Depletion","Photochemicaloxidation","Respiratory Effects")
ggplot(compmelt_moly)+geom_bar(aes(x=Category,y=(value*100),fill=variable),position="dodge",stat="identity")+theme_bw()+facet_grid(~Category, scales="free_x")+ggtitle("Contribution of Transportation to Site Versus Production for Molybdenum")+scale_fill_manual("", values=c("#3bb87b", "#0070fb"))+scale_x_discrete(breaks=NULL)+ylab("% Contribution")+theme(legend.position = "bottom")+xlab("")
nform<-lciaformat(minv[[1]])
depletionmolyscore<-sum(lciaDEPLETE(nform)[,2])

data.frame(minv[[6]],minv[[2]])

molybdenumcost<-cost_pw_calculator(minv,TRUE,"Molybdenum",naicsname=FALSE, uselocal=TRUE)

cost_pw_calculator
