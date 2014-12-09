
#LCA Process Results

gritecontwtransp$mat<-"Graphite"
molycontwtransp$mat<-"Molybdenum"
grenecontwtransp$mat<-"Graphene"

levels(grenecontwtransp$Var1)
subset(grenecontwtransp, grenecontwtransp$Var1=="Human Non Carcinogens")
colnames(grenecontwtransp)
colnames(molycontwtransp)
colnames(gritecontwtransp)
fullset<-rbind(gritecontwtransp,molycontwtransp,grenecontwtransp)

levels(fullset$Var1)
#normmm<-data.frame(Var1=c("Acidification","Ecotoxicity","Eutrophication","Global Warming","Ozone Depletion","Smog Potential","Hunan Carcinogens","Human Non Carcinogens","Respiratory Effects"),nf=normfacts)
#normmm
#fullset<-merge(fullset,normmm, by.x="Var1", all.x=TRUE)
levels(fullset$Var1)<-c("Acidification (kg SO2 eq)","Carcinogens (kg Benz eq)", "Ecotoxicity (kg 2-4D eq)","Eutrophication (kg N eq)","Global Warming (kg CO2 eq)","Non Carcinogens (kg Tol eq)","Ozone Depletion (kg CFC-11 eq)","Smog (kg O3 eq)","Respiratory (kg PM2.5 eq)")

fullset
fullset2<-fullset
fullset2<-na.omit(fullset2)
fullset2$Var2<-as.character(fullset2$Var2)

for(k in 1:nrow(fullset2)){
  if(fullset2$value[k]<.1) {fullset2$Var2[k]="Other"} 
}

head(fullset2)
r1<-subset(fullset2,fullset2$Var1=="Non Carcinogens (kg Tol eq)")
r2<-subset(r1,r1$mat=="Graphene")

head(fullset2)
fullset2$Var2<-as.factor(fullset2$Var2)
levels(fullset2$Var2)<-c("Copper","Curing Graphite Paint","De-Ionized Water","Gasoline","Hydogen gas","MoO3","Natural Gas Related","Natural Gas Related","Natural Gas Related","Other","Transport")
fullplot<-subset(fullset2, fullset2$actual>0)


ggplot(fullset2[with(fullset2, order(-value)),], aes(mat,(value*totalvalue),fill=Var2))+geom_bar(stat="identity",position="stack")+geom_hline(yintercept=0)+facet_wrap(~Var1, scales="free_y")+ggtitle("Contribution of Processes to Back Contact Impacts")+xlab("Impact Categories")+ylab("Total Impact")+scale_fill_tableau("Process Name",palette="tableau20")+scale_x_discrete(breaks=c("Graphene","Graphite","Molybdenum"))+scale_y_continuous(labels=scientific_format(digits=2))+theme_bw()+theme(strip.background=element_rect(colour="NA", fill="NA"), panel.border=element_rect(colour="grey"), panel.grid.major=element_line(colour="NA"))+theme(axis.text.x=element_text(angle=90))+theme(text = element_text(family="Times New Roman",size=26))


ggsave("Figures/contributeappliedtototals.png",width=14,height=10, unit="in")


partlca<-data.frame("acidification","ecotoxicity","eutrophication","globalwarming","ozonedepletion","photochemicaloxidation","carcinogenics","non.carcinogentics","respiratory.effects")
colnames(partlca)<-impfacts
partlca
partlca[,1:ncol(partlca)]<-as.numeric(partlca[,1:ncol(partlca)])
partlca

partlca[1,]<-graphite_total/(normfacts)
partlca[2,]<-molybdenum_total/(normfacts)
partlca[3,]<-graphene_total/(normfacts)
partlca$material<-c("graphite","molybdenum","graphene")
pmlca<-melt(partlca,id="material")
require(xtable)
rownames(partlca)<-partlca$material
partlca<-partlca[,1:9]
levels(pmlca$variable)<-c("Acidification (kg SO2 eq)","Ecotoxicity (kg 2-4D eq","Eutrophication (kg N eq)","Global Warming (kg CO2 eq)","Ozone Depletion (kg CFC-11 eq)","Smog (kg O3 eq)","Carcinogens (kg Benz eq)","Non Carcinogens (kg Tol eq)","Respiratory (kg PM2.5 eq)")


resultgraph1<-ggplot()+geom_bar(data=pmlca, aes(x=variable,y=value*100,fill=material),size=5, position="dodge", stat="identity")+facet_wrap(~variable, scales="free_x")+theme_bw()+scale_fill_manual("Material", values=c("#307ced","#f99726","#84879e"), labels=c("Graphene","Graphite","Molybdenum"))+ylab("% of US Impact")+ggtitle("Normalized, Based on US Emissions (Kim et al 2012)")+theme(text=element_text(family="Times New Roman",size=25))+theme(legend.position="bottom")+scale_x_discrete(breaks=NULL)+xlab(NULL)+theme(strip.background=element_rect(colour="NA", fill="NA"), panel.border=element_rect(colour="grey"), panel.grid.major=element_line(colour="NA"))
resultgraph1


ggsave("Figures/normvalue.png",resultgraph1,width=15,height=10,units=c("in"))
nonelca<-partlca
nonelca[1,]<-nonelca[1,]*normfacts
nonelca[2,]<-nonelca[2,]*normfacts
nonelca[3,]<-nonelca[3,]*normfacts
nonelca$material<-c("graphite","molybdenum","graphene")
nonelca
unnormalized<-melt(nonelca,id="material")
levels(unnormalized$variable)<-c("Acidification (kg SO2 eq)","Ecotoxicity (kg 2-4D eq)","Eutrophication (kg N eq)","Global Warming (kg CO2 eq)","Ozone Depletion (kg CFC-11 eq)","Smog (kg O3 eq)","Carcinogens (kg Benz eq)","Non Carcinogens (kg Tol eq)","Respiratory (kg PM2.5 eq)")

require(scales)
summary(unnormalized$material)
unnormalized$material<-as.character(unnormalized$material)
ggplot()+geom_bar(data=unnormalized, aes(x=material,y=value,fill=material),size=5, position="dodge", stat="identity")+facet_wrap(~variable, scales="free")+theme_bw()+ggtitle("Comparing Results Across TRACI Categories")+theme(text=element_text(family="Times New Roman",size=26))+scale_x_discrete(breaks=NULL)+scale_fill_manual("Material", values=c("#307ced","#f99726","#84879e"), labels=c("Graphene","Graphite","Molybdenum"))+xlab("")+ylab("Values, see label for units")+scale_y_continuous(labels=scientific_format(digits=2))+theme(strip.background=element_rect(colour="NA", fill="NA"), panel.border=element_rect(colour="grey"), panel.grid.major=element_line(colour="NA"))


help(labels)
ggsave("Figures/unstandardizedcomparison.png",width=15,height=10, unit="in")

resultgraph1
help(ggsave)
write.csv(partlca,"LCIA_Result_Normalized.csv")
write.csv(nonelca,"LCIA_Result.csv")


