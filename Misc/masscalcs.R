


45.3592

Copper volume
5.08*5.08*.002*8.96
grams of copper
pi*.5^2*7.81

Graphite.s/gram
(5.08^2*.0000105*2.23)
nickel.s<-0.0025*5.08^2*7.81*(174.3/200)
a<-am
b<-bm
f<-fv

(399-nickel.s)/(5.08^2*.0000105*2.23)


require(MASS)
for(i in 3:ncol(b)) {
  b[,i]<-as.numeric(b[,i])
}
for(i in 2:ncol(a)) {
  a[,i]<-as.numeric(a[,i])
}
s<-ginv(as.matrix(a[,3:ncol(a)])) %*% matrix(f)
u<-as.matrix(b[2:nrow(b),3:ncol(b)])
impacts<-u%*%s
rownames(impacts)<-b[2:nrow(b),1]
output<-data.frame(b[2:nrow(b),1])
type<-b[2:nrow(b),2]
colnames(output)<-c("Environmental Flows")
output$type<-type
output$impacts<-impacts
demand<-data.frame(a[,1],matrix(f))
output

r<-output
require(reshape2)
new<-colsplit(r[,1],"[[]",names=c("name","unit"))
new[,3:4]<-colsplit(r[,2]," /", names=c("type","class"))
new[,5]<-r[,3]
new$name<-gsub(" $", "",new$name,perl=T)
new


traci<-read.csv("impact-methods/traci_factors.csv")
new[,6:14]<-0
for(i in 1:nrow(new)){
  i=125
  name<-new[i,1]
  type<-new[i,3]
  n<-subset(traci, traci[,4]==name)
  if(nrow(n)==0) {n[1,1:14]=0} else {n[1,1:14]=n[1,1:14]}
  if(is.na(type)==F) { k<-subset(n, n$Category==type)} else {k=n}
  new[i,6:14]<-k[1,6:14]*new[i,5]
new[125,]

summary(traci)
subset(traci, traci$Unit=="m3")
which(traci$carcinogenics==max(traci$carcinogenics))
traci[661,]

for(i in 3:ncol(bm)){
print(sum(bm[2:length(bm[,i]),c(i)], na.omit=TRUE))
}
bm[,c(16]
