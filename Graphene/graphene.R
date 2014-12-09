source("impact-methods/rOpenLCA.R")
am<-read.csv("Graphene/technology_matrix.csv", header=FALSE,stringsAsFactors=FALSE)

bm<-read.csv("Graphene/intervention_matrix.csv", header=TRUE,stringsAsFactors=FALSE)


Thickness_of_graphene<-(7*10^-7)
volumeper<-(m2size*100)^2*Thickness_of_graphene
volumeper
gramsperrun<-volumeper*2.26
panel_mass_graphene<-gramsperrun/1000



fv<-c(panels*reps*panel_mass_graphene,rep(0,nrow(am)-1))


impfacts<-c("acidification","ecotoxicity","eutrophication","globalwarming","ozonedepletion","photochemicaloxidation","carcinogenics","non.carcinogenics","respiratory.effects")

runouts<-data.frame(impfacts)

#Specify uncertainty distribution

#runs model all of the uncertain paramters must be assigned to values in the a or b matrix


minverse<-lci(am,bm,fv)
format<-lciaformat(minverse[[1]])
b<-lciaTRACI(format)
b
runouts[,2]<-TRACIsum(b)[,2]
runouts[,2]<-round(runouts[,2],digits=7)
runouts
graphene_total<-runouts[,2]+graphene_shiptobase[,2]
graphene_total
test3<-contribute(minverse)

rems2<-melt(test3$percents)
rems2$actual<-melt(test3$values)$value
help(gsub)
require(ggplot2)
rems2<-subset(rems2, rems2$value!=0)
rems2<-subset(rems2, is.na(rems2$value)==FALSE)
levels(rems2$Var2)<-gsub(",","\n",levels(rems2$Var2))
library(ggthemes)
levels(rems2$Var1)<-c("Acidification","Ecotoxicity","Eutrophication","Global Warming","Ozone Depletion","Smog Potential","Human Carcinogens","Human Non Carcinogens","Repiratory Effects")
summary(rems2)

test3
graphene_total

###for combining with the transport data
greneships<-graphene_shiptobase
greneships$Var2<-"Transport"
greneships$value<-NA
greneships
levels(greneships$impfacts)<-c("Acidification","Human Carcinogens","Ecotoxicity","Eutrophication","Global Warming","Human Non Carcinogens","Ozone Depletion","Smog Potential","Repiratory Effects")
greneships
colnames(greneships)<-c("Var1","actual","Var2","value")
colnames(rems2)
colnames(greneships)
grenecontwtransp<-rbind(greneships,rems2)

formtestgrene<-data.frame(Var1=c("Acidification","Ecotoxicity","Eutrophication","Global Warming","Ozone Depletion","Smog Potential","Human Carcinogens","Human Non Carcinogens","Repiratory Effects"),totalvalue=graphene_total)
formtestgrene
grenecontwtransp<-merge(grenecontwtransp,formtestgrene, by="Var1")
grenecontwtransp
grenecontwtransp$value<-grenecontwtransp$actual/grenecontwtransp$totalvalue





##for no transport rems2
grenecont<-ggplot(grenecontwtransp, aes(Var1,value*100,fill=Var2))+geom_bar(stat="identity",position="stack")+geom_hline(yintercept=0)+ggtitle("Contribution of Production Processes to Overall Impact for Graphene")+theme_bw()+xlab("Impact Categories")+ylab("Percent of Total Impact")+theme(text = element_text(family="Times New Roman",size =12))+theme(axis.text.x=element_text(angle=90))+scale_fill_tableau("Process Name",palette="tableau20")+coord_flip()
grenecont
#plot.title=element_text(hjust=-.75),+facet_wrap(~Var1, scales="free_x")+scale_x_discrete(breaks=NULL)+theme(axis.text.x = element_blank())

ggsave("Figures/graph_prod_contrib.png",grenecont, width=9,height=5.5,units="in")

process_graph<-(runouts[,2]/graphene_total)
transport_graph<-(graphene_shiptobase[,2]/graphene_total)

compares_gren<-runouts
compares_gren[,2]<-process_graph
compares_gren[,3]<-transport_graph
colnames(compares_gren)<-c("Category","Process","Transport")
compmelt_gren<-melt(compares_gren)
head(compmelt_gren)
levels(compmelt_gren$Category)<-c("Acidification","Carcinogenic","Ecotoxicity","Eutrophication","Global Warming","Non Carcinogenic","Ozone Depletion","Photochemicaloxidation","Respiratory Effects")

ggplot(compmelt_gren)+geom_bar(aes(x=Category,y=value*100,fill=variable),stat="identity",position="stack",alpha=.7)+theme_bw()+facet_grid(~Category, scales="free_x")+ggtitle("Contribution of Transportation to Site Versus Production for Graphene")+scale_fill_manual("", values=c("#3bb87b", "#0070fb"))+scale_x_discrete(breaks=NULL)+ylab("% Contribution")+theme(legend.position = "bottom")+xlab("")


graphenedepletionscore<-sum(lciaDEPLETE(lciaformat(minverse[[1]]))[,2])
graphenecost<-cost_pw_calculator(minverse,TRUE,"Graphene",naicsname=TRUE)

