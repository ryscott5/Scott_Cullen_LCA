


thick<-c(1*10^-7,3*10^-7)
density_graphite<-2.267
cm3min*density_graphite
cm3max*density_graphite
TENP=240956821.7


volume_produced<-60.96*45.72*thick
watts_used_furnace=8000
watts_used_pump=550
heat<-60
growth<-30
cool<-60
sccm_h2<-2
sccm_ch4<-35

#Three Stages, heat, growth, and cooling. two gas flows. Pump and furnace.

kwh_electricity<-((heat+growth+cool)/60)*(watts_used_pump/1000)+((heat+growth+cool)/60)*(watts_used_furnace/1000)
sccm_h2used<-(heat+growth+cool)*sccm_h2
sccm_ch4used<-(growth+cool)*sccm_ch4
graphene_produced<-volume_produced*density_graphite


((1/graphene_produced)*wh_electricity)/1000


iron_nitrate_solution<-

  
  16.04 g/mol


ch4+h2+cu2

#RUN PLACE THROUGH GOOGLE
#PULL OUT INSOLATION FOR LOCATION LATITUDE TILT FROM NREL
#APPLY STANDARD PV-WATTS DERATE AND DEVICE EFFICIENCY
#SHOULD GET M^2 FOR AREA W/ PV POTENTIAL.

