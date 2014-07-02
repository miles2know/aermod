

###################################################### Reading and writing from/to clipboard ##########################################################


read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,as.is=TRUE,...)
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
 

a<-1

######################################################## Plotting AirMod Concentrations (to GIT hub) ############################################################


ScaleTable<-read.excel()
BreakTable<-read.excel()
ColorTable<-read.excel()

flist<-paste("C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Input files/",list.files("C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Input files/",pattern="*.GRF"),sep="")

cnames<-c("X","Y","ACON","ZELEV","ZHILL","ZFLAG","AVE","GRP","RANK")

rm(list=c("ppp1","ppp2"))


library(sp)
library(gstat)
library(spatstat)
source("C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Rwork/plot.im.R")

##################################################### Begin AIRMOD Plotting (All File Types) #############################################################

for(i in 1:length(flist)){

# Read files and output to master file

file<-read.table(flist[i],header=FALSE,comment.char="*",as.is=TRUE,fill=TRUE,col.names=1:max(count.fields(flist[i])))
file<-file[,1:9]
names(file)<-cnames
fname<-unlist(strsplit(flist[i],"/"))[8]
file$fname<-fname 
file$size<-unlist(strsplit(flist[i],"_"))[2]
 
 
#if(i==1){
#fopen<-file("C:/AirModOut/MasterFile-RES_NBAT_KSLK_MNTN_BLD.csv",open="wt")
#write.table(file,fopen,sep=",",row.names=FALSE)
#close(fopen)} 
#else{
#fopen<-file("C:/AirModOut/MasterFile-RES_NBAT_KSLK_MNTN_BLD.csv",open="at")
#write.table(file,fopen,sep=",",row.names=FALSE,col.names=FALSE)
#close(fopen) 
#}


# Extract file suffix, re-code rank and lookup scaling
 
Fsuffix<-unlist(strsplit(unlist(strsplit(flist[i],"_"))[10],"[.]"))[1]
file$RANK1<-ifelse(file$AVE=="ANNUAL","ANNUAL",file$RANK)
file$lkup<-paste(file$AVE,file$RANK1,sep=",")
sector<-unlist(strsplit(fname,"_"))[1]

current.scale<-paste(unlist(strsplit(fname,"_"))[1],unlist(strsplit(fname,"_"))[2],unlist(strsplit(fname,"_"))[3],unlist(strsplit(fname,"_"))[4],unlist(strsplit(fname,"_"))[5],unlist(strsplit(unlist(strsplit(fname,"_"))[10],"[.]"))[1],sep="-")
current.scale1<-paste(unlist(strsplit(fname,"_"))[1],unlist(strsplit(fname,"_"))[2],unlist(strsplit(fname,"_"))[3],unlist(strsplit(fname,"_"))[4],unlist(strsplit(fname,"_"))[5],unlist(strsplit(unlist(strsplit(fname,"_"))[10],"[.]"))[1],sep="-")

scale.val1<-ScaleTable[ScaleTable$Lkup==current.scale,2]
scale.val2<-scale.val1
 
# Select correct rows 
 
if(Fsuffix %in% c("SO2","OTHER")){
 
 fstring<-switch(Fsuffix,SO2="1-HR,4TH",OTHER="4-HR,8TH")
 ppp1<-file[file$lkup==fstring,]
 ch.vec1<-paste(sector,Fsuffix,fstring,sep=",")
 ppp.br1<-BreakTable[BreakTable$Lkup==ch.vec1,]
 ppp.co1<-ColorTable[ColorTable$Lkup==ch.vec1,]
 
} else if(Fsuffix=="NO2"){
 
 ppp1<-file[file$lkup=="1-HR,8TH",];fstring1<-"1-HR,8TH"
 ch.vec1<-paste(sector,Fsuffix,fstring1,sep=",")
 ppp.br1<-BreakTable[BreakTable$Lkup==ch.vec1,]
 ppp.co1<-ColorTable[ColorTable$Lkup==ch.vec1,]
 
 ppp2<-file[file$lkup=="ANNUAL,ANNUAL",];fstring2<-"ANNUAL,ANNUAL"
 ch.vec2<-paste(sector,Fsuffix,fstring2,sep=",")
 ppp.br2<-BreakTable[BreakTable$Lkup==ch.vec2,]
 ppp.co2<-ColorTable[ColorTable$Lkup==ch.vec2,]
 
 substr(current.scale1,nchar(current.scale1)-2,nchar(current.scale1))<-"VOC"
 scale.val2<-ScaleTable[ScaleTable$Lkup==current.scale1,2]
 
} else if(Fsuffix=="PM25"){
 
 ppp1<-file[file$lkup=="24-HR,8TH",];fstring1<-"24-HR,8TH"
 ch.vec1<-paste(sector,Fsuffix,fstring1,sep=",")
 ppp.br1<-BreakTable[BreakTable$Lkup==ch.vec1,]
 ppp.co1<-ColorTable[ColorTable$Lkup==ch.vec1,]
  
 ppp2<-file[file$lkup=="ANNUAL,ANNUAL",];fstring2<-"ANNUAL,ANNUAL"
 ch.vec2<-paste(sector,Fsuffix,fstring2,sep=",")
 ppp.br2<-BreakTable[BreakTable$Lkup==ch.vec2,]
 ppp.co2<-ColorTable[ColorTable$Lkup==ch.vec2,]
 
} else{
 
 ppp1<-file[file$lkup=="1-HR,2ND",];fstring1<-"1-HR,2ND"
 ch.vec1<-paste(sector,Fsuffix,fstring1,sep=",")
 ppp.br1<-BreakTable[BreakTable$Lkup==ch.vec1,]
 ppp.co1<-ColorTable[ColorTable$Lkup==ch.vec1,]
 
 ppp2<-file[file$lkup=="8-HR,2ND",];fstring2<-"8-HR,2ND"
 ch.vec2<-paste(sector,Fsuffix,fstring2,sep=",")
 ppp.br2<-BreakTable[BreakTable$Lkup==ch.vec2,]
 ppp.co2<-ColorTable[ColorTable$Lkup==ch.vec2,]
  
}

if(exists("ppp2")==TRUE){

fname1<-unique(ppp1$lkup) 
fname2<-unique(ppp2$lkup) 

# Create point process 
 
run.ppf1<-ppp1[c("X","Y","ACON")]
run.ppf2<-ppp2[c("X","Y","ACON")] 
pp.Window1<-owin(xrange=c(range(run.ppf1$X)[1]+395,range(run.ppf1$X)[2]-395),yrange=c(range(run.ppf1$Y)[1]+395,range(run.ppf1$Y)[2]-395))
pp.Window2<-owin(xrange=c(range(run.ppf2$X)[1]+395,range(run.ppf2$X)[2]-395),yrange=c(range(run.ppf2$Y)[1]+395,range(run.ppf2$Y)[2]-395))
run.ppp1<-ppp(run.ppf1$X,run.ppf1$Y,marks=run.ppf1$ACON,window=pp.Window1)
run.ppp2<-ppp(run.ppf2$X,run.ppf2$Y,marks=run.ppf2$ACON,window=pp.Window2)

# Scale concentrations, add logic for scaling
 
pcon1<-marks(run.ppp1)
pcon2<-marks(run.ppp2)
pcon.s1<-pcon1*scale.val1
pcon.s2<-pcon2*scale.val2

if((range(pcon.s1)[2]-range(pcon.s1)[1])<.5){
 pcon.s1<-pcon1*1000000
 write.table(paste(fname,fname1,sep="-"),"C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Errors-RES_NBAT_KSLK_MNTN_BLD.csv",sep=",",row.names=FALSE,append=TRUE)
}
if((range(pcon.s2)[2]-range(pcon.s2)[1])<.5){
 pcon.s2<-pcon2*1000000
 write.table(paste(fname,fname2,sep="-"),"C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Errors-RES_NBAT_KSLK_MNTN_BLD.csv",sep=",",row.names=FALSE,append=TRUE)
} 
 
marks(run.ppp1)<-pcon.s1
marks(run.ppp2)<-pcon.s2 

max.p1<-run.ppp1[run.ppp1$marks==max(run.ppp1$marks),]
max.p2<-run.ppp2[run.ppp2$marks==max(run.ppp2$marks),]
 
run.vg1<-data.frame(x=run.ppp1$x,y=run.ppp1$y,conc=run.ppp1$marks)
run.vg1<-as.geodata(run.vg1)  
 
run.vg2<-data.frame(x=run.ppp2$x,y=run.ppp2$y,conc=run.ppp2$marks)
run.vg2<-as.geodata(run.vg2) 
 
# Create plot breaks and color levels
 
if(max.p1$marks<ppp.br1[5]){
br1<-unlist(c(0,ppp.br1[2],ppp.br1[3],ppp.br1[4],ppp.br1[5]))
co1<-unlist(c(ppp.co1[2],ppp.co1[3],ppp.co1[4],ppp.co1[5]))
}else{
br1<-unlist(c(0,ppp.br1[2],ppp.br1[3],ppp.br1[4],ppp.br1[5],round(max.p1$marks,0)))
co1<-unlist(c(ppp.co1[2],ppp.co1[3],ppp.co1[4],ppp.co1[5],ppp.co1[6]))
}  
 

if(max.p2$marks<ppp.br2[5]){
br2<-unlist(c(0,ppp.br2[2],ppp.br2[3],ppp.br2[4],ppp.br2[5]))
co2<-unlist(c(ppp.co2[2],ppp.co2[3],ppp.co2[4],ppp.co2[5]))
}else{
br2<-unlist(c(0,ppp.br2[2],ppp.br2[3],ppp.br2[4],ppp.br2[5],round(max.p2$marks,0)))
co2<-unlist(c(ppp.co2[2],ppp.co2[3],ppp.co2[4],ppp.co2[5],ppp.co2[6]))
}  
 
lb1<-length(br1)
lb2<-length(br2)

# Output file names 

substr(fname,nchar(fname)-3,nchar(fname))<-".png"
fpath1<-paste("C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Output Charts/",fname1,sep="")
fpath2<-paste("C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Output Charts/",fname2,sep="") 
png.out1<-paste(fpath1,fname,sep="-")
png.out2<-paste(fpath2,fname,sep="-")
 
# Write plots to png files
 
png(png.out1,height=480*1.25,width=480*1.25)
 
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
par(oma=c(0,0,0,0),mar=c(1,1,1,1))
plot.im(Smooth(run.ppp1,sigma=8),col=co1,breaks=br1,zlim=c(0,br1[lb1]),main=NA,ribargs=list(at=br1),cex.axis=1.5)
points(run.ppp1$x,run.ppp1$y,pch=16,cex=.25)
points(max.p1$x,max.p1$y,pch="+",col="red",cex=2.5)
text(max.p1$x,max.p1$y,labels=round(max.p1$marks,0),col="red",pos=3,cex=1.75)
par(mar=omar)
hist(marks(run.ppp1),main=NA,xlab="concentration",cex.lab=1.5,cex.axis=1.5)
abline(v=quantile(marks(run.ppp1),probs=c(.5,.75,.95,.99)),col="green",lwd=2.5)
plot(variog(run.vg1,max.dist=110),cex.lab=1.5,cex.axis=1.5,pch=16)
 
dev.off()  
 
png(png.out2,height=480*1.25,width=480*1.25)
 
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
par(oma=c(0,0,0,0),mar=c(1,1,1,1))
plot.im(Smooth(run.ppp2,sigma=8),col=co2,breaks=br2,zlim=c(0,br2[lb2]),main=NA,ribargs=list(at=br2),cex.axis=1.5)
points(run.ppp2$x,run.ppp2$y,pch=16,cex=.25)
points(max.p2$x,max.p2$y,pch="+",col="red",cex=2.5)
text(max.p2$x,max.p2$y,labels=round(max.p2$marks,0),col="red",pos=3,cex=1.5)
par(mar=omar)
hist(marks(run.ppp2),main=NA,xlab="concentration",cex.lab=1.5,cex.axis=1.5)
abline(v=quantile(marks(run.ppp2),probs=c(.5,.75,.95,.99)),col="green",lwd=2.5) 
plot(variog(run.vg2,max.dist=110),cex.lab=1.5,cex.axis=1.5,pch=16)

dev.off()
 
df1<-data.frame(run=paste(current.scale,fname1,sep="-"),max=max(marks(run.ppp1)),avg=mean(marks(run.ppp1)),sd=sd(marks(run.ppp1)))
df2<-data.frame(run=paste(current.scale,fname2,sep="-"),max=max(marks(run.ppp2)),avg=mean(marks(run.ppp2)),sd=sd(marks(run.ppp2))) 

 
} else{

fname1<-unique(ppp1$lkup) 

# Create point process 
 
run.ppf1<-ppp1[c("X","Y","ACON")]
pp.Window1<-owin(xrange=c(range(run.ppf1$X)[1]+395,range(run.ppf1$X)[2]-395),yrange=c(range(run.ppf1$Y)[1]+395,range(run.ppf1$Y)[2]-395))
run.ppp1<-ppp(run.ppf1$X,run.ppf1$Y,marks=run.ppf1$ACON,window=pp.Window1)

# Scale concentrations, add logic for scaling
 
pcon1<-marks(run.ppp1)
pcon.s1<-pcon1*scale.val1
 
if((range(pcon.s1)[2]-range(pcon.s1)[1])<.5){
 pcon.s1<-pcon1*1000000
 write.table(paste(fname,fname1,sep="-"),"C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Errors-RES_NBAT_KSLK_MNTN_BLD.csv",sep=",",row.names=FALSE,append=TRUE)
} 

marks(run.ppp1)<-pcon.s1
 
max.p1<-run.ppp1[run.ppp1$marks==max(run.ppp1$marks),]
run.vg1<-data.frame(x=run.ppp1$x,y=run.ppp1$y,conc=run.ppp1$marks)
run.vg1<-as.geodata(run.vg1) 
 
# Create plot breaks and color levels  
 
if(max.p1$marks<ppp.br1[5]){
br1<-unlist(c(0,ppp.br1[2],ppp.br1[3],ppp.br1[4],ppp.br1[5]))
co1<-unlist(c(ppp.co1[2],ppp.co1[3],ppp.co1[4],ppp.co1[5]))
}else{
br1<-unlist(c(0,ppp.br1[2],ppp.br1[3],ppp.br1[4],ppp.br1[5],round(max.p1$marks,0)))
co1<-unlist(c(ppp.co1[2],ppp.co1[3],ppp.co1[4],ppp.co1[5],ppp.co1[6]))
} 
 
lb1<-length(br1)
 
# Output file names 

substr(fname,nchar(fname)-3,nchar(fname))<-".png"
fpath1<-paste("C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Output Charts/",fname1,sep="")
png.out1<-paste(fpath1,fname,sep="-")
 
# Write plots to png files
 
png(png.out1,height=480*1.25,width=480*1.25)
 
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
par(oma=c(0,0,0,0),mar=c(1,1,1,1))
plot.im(Smooth(run.ppp1,sigma=8),col=co1,breaks=br1,zlim=c(0,br1[lb1]),main=NA,ribargs=list(at=br1,las=1),cex.axis=1.5)
points(run.ppp1$x,run.ppp1$y,pch=16,cex=.25)
points(max.p1$x,max.p1$y,pch="+",col="red",cex=2.5)
text(max.p1$x,max.p1$y,labels=round(max.p1$marks,0),col="red",pos=3,cex=1.75)
par(mar=omar)
hist(marks(run.ppp1),main=NA,xlab="concentration",cex.lab=1.5,cex.axis=1.5)
abline(v=quantile(marks(run.ppp1),probs=c(.5,.75,.95,.99)),col="green",lwd=2.5)
plot(variog(run.vg1,max.dist=100),cex.lab=1.5,cex.axis=1.5,pch=16)
 
dev.off()

df1<-data.frame(run=paste(current.scale,fname1,sep="-"),max=max(marks(run.ppp1)),avg=mean(marks(run.ppp1)),sd=sd(marks(run.ppp1)))
 
}
 

if(exists("ppp2")==TRUE){
  df<-rbind(df1,df2)
} else{
  df<-df1 
} 
 
#if(i==1){
#fopen1<-file("C:/AirModOut/SummaryStats-RES_BAU_KSLK_MNTN_BLD.csv",open="wt")
#write.table(df,fopen,sep=",",row.names=FALSE)
#close(fopen1)} 
#else{
#fopen1<-file("C:/AirModOut/SummaryStats-RES_BAU_KSLK_MNTN_BLD.csv",open="at")
#write.table(df,fopen,sep=",",row.names=FALSE,col.names=FALSE)
#close(fopen1) 
#} 
 
 
#if(exists("run.ppp2")){
# rm(list=c("run.ppp1","run.ppp2"))
#} else{
# rm("run.ppp1")
#}

 
if(dev.cur()!=1){dev.off()}
 
}




##################################################### Plotting just PM25 ##############################################################################


flist<-paste("C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Input files/",list.files("C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Input files/",pattern="*.GRF"),sep="")


#######################################################################################################################################################


for(i in 1:length(flist)){
 
Fsuffix<-unlist(strsplit(unlist(strsplit(flist[i],"_"))[10],"[.]"))[1]

if(Fsuffix=="PM25"){
 
# Read files and output to master file

file<-read.table(flist[i],header=FALSE,comment.char="*",as.is=TRUE,fill=TRUE,col.names=1:max(count.fields(flist[i])))
file<-file[,1:9]
names(file)<-cnames
fname<-unlist(strsplit(flist[i],"/"))[8]
file$fname<-fname 
 
# Extract file suffix, re-code rank and lookup scaling
 
file$RANK1<-ifelse(file$AVE=="ANNUAL","ANNUAL",file$RANK)
file$lkup<-paste(file$AVE,file$RANK1,sep=",")

current.scale<-paste(unlist(strsplit(fname,"_"))[1],unlist(strsplit(fname,"_"))[2],unlist(strsplit(fname,"_"))[3],unlist(strsplit(fname,"_"))[4],unlist(strsplit(fname,"_"))[5],unlist(strsplit(unlist(strsplit(fname,"_"))[10],"[.]"))[1],sep="-")
current.scale1<-paste(unlist(strsplit(fname,"_"))[1],unlist(strsplit(fname,"_"))[2],unlist(strsplit(fname,"_"))[3],unlist(strsplit(fname,"_"))[4],unlist(strsplit(fname,"_"))[5],unlist(strsplit(unlist(strsplit(fname,"_"))[10],"[.]"))[1],sep="-")

scale.val1<-ScaleTable[ScaleTable$Lkup==current.scale,2]
scale.val2<-scale.val1
 
# Select correct rows 
 
ppp1<-file[file$lkup=="24-HR,8TH",]
fname1<-unique(ppp1$lkup) 


# Create point process 
 
run.ppf1<-ppp1[c("X","Y","ACON")]
pp.Window1<-owin(xrange=c(range(run.ppf1$X)[1]+395,range(run.ppf1$X)[2]-395),yrange=c(range(run.ppf1$Y)[1]+395,range(run.ppf1$Y)[2]-395))
run.ppp1<-ppp(run.ppf1$X,run.ppf1$Y,marks=run.ppf1$ACON,window=pp.Window1)

# Scale concentrations, add logic for scaling
 
pcon1<-marks(run.ppp1)
pcon.s1<-pcon1*scale.val1

if((range(pcon.s1)[2]-range(pcon.s1)[1])<.5){
 pcon.s1<-pcon1*1000000
 write.table(paste(fname,fname1,sep="-"),"C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Errors-RES_BAU_KSLK_MNTN_BLD_PM25.csv",sep=",",row.names=FALSE,append=TRUE)
}
 
marks(run.ppp1)<-pcon.s1
 
# Output file names 

substr(fname,nchar(fname)-3,nchar(fname))<-".png"
fpath1<-paste("C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Output Charts/",fname1,sep="")
png.out1<-paste(fpath1,fname,sep="-")
 
# Write plots to png files
 
png(png.out1)
 
source("C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Rwork/plot.im.R")

plot.im(Smooth(run.ppp1,sigma=15),col=c("white","yellow","darkorange","red"),breaks=c(0,10,35,65,100),zlim=c(0,100),main=NA,ribargs=list(at=c(0,10,35,65,100)))
points(run.ppp1$x,run.ppp1$y,pch=16,cex=.25) 

dev.off()  
 
}

}

##################################################################################################################################################

