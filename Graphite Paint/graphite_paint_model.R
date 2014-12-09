source("impact-methods/rOpenLCA.R")

mcn<-1
#Rmemeber to Specify number of MC trials

am<-read.csv("Graphite Paint/technology_matrix.csv", header=FALSE, stringsAsFactors=FALSE)

bm<-read.csv("Graphite Paint/intervention_matrix.csv", header=TRUE,stringsAsFactors=FALSE)

#native thickness is 25um. Some evidence 5um might be achievable.
thickness=5
q=25/thickness
fv<-c((panels*reps)/q, rep(0,nrow(am)-1))
impfacts<-c("acidification","ecotoxicity","eutrophication","globalwarming","ozonedepletion","phytochemicaloxidation","carcinogenics","non.carcinogenics","respiratory.effects")
runouts<-data.frame(impfacts)


#Specify uncertainty distribution

#runs model all of the uncertain paramters must be assigned to values in the a or b matrix

	#target uncertainty to matrix
#Matrix Inverse and TRACI impacts out
minversegp<-lci(am,bm,fv)


runouts[,2]<-TRACIsum(lciaTRACI(lciaformat(minversegp[[1]])))[,2]

graphite_total<-runouts[,2]+graphitepaint_shiptobase[,2]

test<-contribute(minversegp)

rems_gp<-melt(test$percents)
rems_gp$actual<-melt(test$values)$value
rems_gp<-subset(rems_gp, rems_gp$value!=0)
rems_gp<-subset(rems_gp, is.na(rems_gp$value)==FALSE)
rems_gp
levels(rems_gp$Var2)<-gsub(",","\n",levels(rems_gp$Var2))
levels(rems_gp$Var1)<-c("Acidification","Ecotoxicity","Eutrophication","Global Warming","Ozone Depletion","Smog Potential","Human Carcinogens","Human Non Carcinogens","Repiratory Effects")
require(ggplot2)

griteships<-graphitepaint_shiptobase
griteships$Var2<-"Transport"
griteships$value<-NA
griteships
levels(griteships$impfacts)<-c("Acidification","Human Carcinogens","Ecotoxicity","Eutrophication","Global Warming","Human Non Carcinogens","Ozone Depletion","Smog Potential","Repiratory Effects")

colnames(griteships)<-c("Var1","actual","Var2","value")
gritecontwtransp<-rbind(griteships,rems_gp)
formtestgrite<-data.frame(Var1=c("Acidification","Ecotoxicity","Eutrophication","Global Warming","Ozone Depletion","Smog Potential","Human Carcinogens","Human Non Carcinogens","Repiratory Effects"),totalvalue=graphite_total)
formtestgrite
gritecontwtransp<-merge(gritecontwtransp,formtestgrite, by="Var1")
gritecontwtransp$value<-gritecontwtransp$actual/gritecontwtransp$totalvalue

gritecontwtransp
##for non transport rems_gp
ggplot(gritecontwtransp, aes(Var1,value*100,fill=Var2))+geom_bar(stat="identity",position="stack")+geom_hline(yintercept=0)+ggtitle("Contribution of Production Processes to Overall Impact, Graphite Paint")+theme_bw()+xlab("Impact Categories")+ylab("Percent of Total Impact")+theme(text = element_text(family="Times New Roman",size =12))+theme(axis.text.x=element_text(angle=90))+coord_flip()+scale_fill_tableau("Process Name",palette="tableau10")


ggsave('Figures/graphite_paint_contrib.png',height=5,width=8,units="in")



lciaformat(minversegp[[1]])
part<-(minversegp[[5]]*diag(minversegp[[2]]))
sum(part[150,])

format<-lciaformat(minversegp[[1]])
b<-lciaTRACI(format)
runouts[,2]<-TRACIsum(b)[,2]
runouts


graphite_total<-runouts[,2]+graphitepaint_shiptobase[,2]


graphite_total

depletionscoregraphite<-sum(lciaDEPLETE(format)[,2])



process_gp<-(runouts[,2]/graphite_total)
transport_gp<-(graphitepaint_shiptobase[,2]/graphite_total)

compares_gp<-runouts
compares_gp[,2]<-process_gp
compares_gp[,3]<-transport_gp
colnames(compares_gp)<-c("Category","Process","Transport")
compmelt_gp<-melt(compares_gp)
head(compmelt_gp)

levels(compmelt_gp$Category)<-c("Acidification","Carcinogenic","Ecotoxicity","Eutrophication","Global Warming","Non Carcinogenic","Ozone Depletion","Photochemicaloxidation","Respiratory Effects")

ggplot(compmelt_gp)+geom_bar(aes(x=Category,y=value*100,fill=variable),position="stack",stat="identity")+theme_bw()+facet_grid(~Category, scales="free_x")+ggtitle("Contribution of Transportation to Site Versus Production for Graphite Paint")+scale_fill_manual("", values=c("#3bb87b", "#0070fb"))+scale_x_discrete(breaks=NULL)+ylab("% Contribution")+theme(legend.position = "bottom")+xlab("")

graphitecost<-cost_pw_calculator(minversegp,TRUE,"Graphite",naicsname=TRUE)






