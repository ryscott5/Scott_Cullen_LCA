
#lci takes a data frame and returns a vector of inputs and outputs. It assumes base matrix csv export from Open LCA. For flexibility, the program figures you will specify your own functional group.

impfacts<-c("acidification","ecotoxicity","eutrophication","globalwarming","ozonedepletion","photochemicaloxidation","carcinogenics","non.carcinogenics","respiratory.effects")


lci<-function(a,b,f) {
	require(MASS)
  A<-data.frame(a[1:nrow(a),3:ncol(a)])
  B<-data.frame(b[2:nrow(b),3:ncol(b)])
	for(i in 1:ncol(B)) {
	B[,i]<-as.numeric(B[,i])
	}
  B<-as.matrix(B)
	for(i in 1:ncol(A)) {
	A[,i]<-as.numeric(A[,i])
}
  A<-as.matrix(A)
  methodofinverse<-function(){}
  if(det(A)==0){methodofinverse==ginv}else{methodofinverse=solve}
  s<-methodofinverse(A) %*% matrix(f)
	impacts<-B%*%s
	rownames(impacts)<-b[2:nrow(b),1]
	output<-data.frame(b[2:nrow(b),1])
	type<-b[2:nrow(b),2]
	colnames(output)<-c("Environmental Flows")
	output$type<-type
	output$impacts<-impacts
	demand<-data.frame(a[,1],matrix(f))
	outputs<-list("result"<-output,"s"<-s,"g"<-impacts,"A"<-A,"B"<-B,"processes"<-a[,1])
  outputs
	}	


lciaformat<-function(lciout){
	r<-lciout
	require(reshape2)
	new<-colsplit(r[,1],"[[]",names=c("name","unit"))
	for(i in 1:nrow(new)){
	  if(r[i,2]==""){r[i,2]<-" / "}
	}
	new[,3:4]<-colsplit(r[,2]," /", names=c("type","class"))
	new[,5]<-r[,3]
	new$name<-gsub(" $", "",new$name,perl=T)
	new
	}

lciaTRACI<-function(lciaformatted)  {
  for(i in 1:nrow(lciaformatted)){
    if(lciaformatted[i,"type"]==" "){lciaformatted[i,"type"]=NA}
    if(lciaformatted[i,"class"]==" "){lciaformatted[i,"class"]=NA}
  }
  new<-na.omit(lciaformatted)
  traci<-read.csv("impact-methods/traci_factors.csv",stringsAsFactors=FALSE)
  head(traci)
  new[,6:14]<-0
  for(i in 1:nrow(new)){
    name<-new[i,1]
    type<-new[i,3]
    n<-subset(traci, traci[,4]==name)
    if(nrow(n)==0) {n[1,1:14]=0} else {n[1,1:14]=n[1,1:14]}
    if(is.na(type)==F) {k<-subset(n, n$Category==type)} else {k=n}
    new[i,6:14]<-k[1,6:14]*new[i,5]
  }  
  colnames(new)=c("name","unit","type","class","MatrixValue",colnames(traci[,6:14]))
  traci<-0
  new
}

lciaDEPLETE<-function(lciaformatted)  {
  for(i in 1:nrow(lciaformatted)){
    if(lciaformatted[i,"type"]==" "){lciaformatted[i,"type"]=NA}
    if(lciaformatted[i,"class"]==" "){lciaformatted[i,"class"]=NA}
  }
  new<-na.omit(lciaformatted)
  recdep<-read.csv("impact-methods/impact99_resourcedep.csv",stringsAsFactors=FALSE)
  recdep[,6:7]<-recdep[,6:7]*-1
  new[,6:7]<-0
  for(i in 1:nrow(new)){
    name<-new[i,1]
    type<-new[i,3]
    n<-subset(recdep, recdep[,4]==name)
    if(nrow(n)==0) {n[1,1:7]=0} else {n[1,1:7]=n[1,1:7]}
    if(is.na(type)==F) { k<-subset(n, n$Category==type)} else {k=n}
    new[i,6:7]<-k[1,6:7]*new[i,5]
  }  
  colnames(new)=c("name","unit","type","class","MatrixValue",colnames(recdep[,6:7]))
  new
  impfacts<-colnames(recdep[,6:7])
  char_Result<-data.frame(impfacts)
  char_Result$total<-0
  for(i in 6:7){
    char_Result[i-5,2]<-sum(new[,i],na.rm=TRUE)
  }
  
  char_Result
}

TRACIsum<-function(lciaTRACIob) {
	traci<-read.csv("impact-methods/traci_factors.csv")
	impfacts<-colnames(traci[,6:14])
	char_Result<-data.frame(impfacts)
	char_Result$total<-0
	for(i in 6:14){
	char_Result[i-5,2]<-sum(lciaTRACIob[,i],na.rm=TRUE)
}

char_Result
}


contribute<-function(minverse){
  #uses the output of lci
  As<-minverse[[4]]%*%diag(c(minverse[[2]]),nrow=nrow(minverse[[2]]))
  full<-minverse[[1]]
  fullresult<-TRACIsum(lciaTRACI(lciaformat(minverse[[1]])))
  individual_processes<-matrix(nrow=nrow(fullresult),ncol=ncol(As))
  colnames(individual_processes)<-minverse[[6]]
  rownames(individual_processes)<-fullresult[,1]
  nonscaled<-individual_processes
  for(i in 1:ncol(As)){
    full[,3]<-(minverse[[5]]%*%diag(c(minverse[[2]]), nrow=nrow(minverse[[2]])))[,i]
    individual_processes[,i]<-TRACIsum(lciaTRACI(lciaformat(full)))[,2]/fullresult[,2]
    nonscaled[,i]<-TRACIsum(lciaTRACI(lciaformat(full)))[,2]
    
  }
  list(percents=round(individual_processes, digits=3),values=nonscaled,totals=fullresult)
}


pdat<-"https://docs.google.com/spreadsheets/d/10gpa3a8ezhE3yYuzRJlz-r5JVkjiLu4f9ZxY9HgZQcA/export?format=csv&id=10gpa3a8ezhE3yYuzRJlz-r5JVkjiLu4f9ZxY9HgZQcA"
localstore<-read.csv(textConnection(getURL(pdat)), stringsAsFactors=FALSE)

require(RCurl)
cost_pw_calculator<-function(rinvert,returnfigure,materialname,naicsname=TRUE,uselocal=TRUE){
  if(uselocal==TRUE){
    pdat<-localstore
  }
    else{
  pdat<-"https://docs.google.com/spreadsheets/d/10gpa3a8ezhE3yYuzRJlz-r5JVkjiLu4f9ZxY9HgZQcA/export?format=csv&id=10gpa3a8ezhE3yYuzRJlz-r5JVkjiLu4f9ZxY9HgZQcA"
  pdat<-read.csv(textConnection(getURL(pdat)), stringsAsFactors=FALSE)
    }
  set1<-data.frame("Materials"=rinvert[[6]],"Quant"=rinvert[[2]])
  colnames(set1)<-c("Materials","Quant")
  set1<-merge(set1,pdat,by.x="Materials")
  cost_dol<-sum(set1$Quant*set1$Cost)/(desired_capacity_kw*1000)
  if(returnfigure==TRUE){
    require(ggplot2)
    set1$cont<-(set1$Quant*set1$Cost)/(desired_capacity_kw*1000)/(sum(set1$Quant*set1$Cost)/(desired_capacity_kw*1000))
    
    #makes a ggplot
    chart<-ggplot(set1)+geom_bar(aes(x=Materials,y=cont))+coord_flip()+theme_bw()+ylab("% of Cost per Watt")+xlab("Process")+ggtitle(paste("Contribution of Process to Cost per Watt of",materialname," "))
    
    #attaches names
if(naicsname==TRUE){
    for(i in 1:nrow(set1)){
      j<-set1[i,"NAICS.Code"]
      is.na<-"NA"
      if(is.na(j)==F){
        locurl<-paste("http://naics.us/v0/q?year=2012&code=",as.character(j),"&above=1",sep="")
        dat<-fromJSON(paste(readLines(locurl), collapse=""))
        set1$naicsname[i]<-dat[[2]]$title} else {set1$naicsname[i]<-NA}
    }
}

    rm(pdat)
    list(cost_dol,chart,set1)
  }
  else{
    rm(pdat)
    cost_dol
  }
}


zero_to_scale<-function(dataframein,max_num=100,reverse=FALSE){
  dataframeout<-data.frame(dataframein)
  dataframein<-data.frame(dataframein)
  if(reverse==FALSE){
  for(k in 1:ncol(dataframein)){
    constant<-max_num/(max(dataframein[,k])-min(dataframein[,k]))
    for(i in 1:nrow(dataframein)){
      dataframeout[i,k]<-(dataframein[i,k]-min(dataframein[,k]))*constant
    }
  }
  dataframeout
  }
  else{ 
  
    for(k in 1:ncol(dataframein)){
      constant<-max_num/(min(dataframein[,k])-max(dataframein[,k]))
      for(i in 1:nrow(dataframein)){
        dataframeout[i,k]<-(dataframein[i,k]-max(dataframein[,k]))*constant
      }
    }
    dataframeout
  
  
  }
}


zero_to_scale(c(0,1,2))

