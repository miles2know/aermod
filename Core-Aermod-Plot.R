###### Core plot function ########################################################################################################################################

# This is a copy of the core skech out of the Aermod plotting function befor functionalizing

plots<-l_ply(ppp_PMAnn,function(df){
       if(max(df$marks)<br_PMAnn[4]){
          br1<-c(0,br_PMAnn[1],br_PMAnn[2],br_PMAnn[3],br_PMAnn[4])
          co1<-c(co_PMAnn[1],co_PMAnn[2],co_PMAnn[3],co_PMAnn[4])
        }else{
          br1<-c(0,br_PMAnn[1],br_PMAnn[2],br_PMAnn[3],br_PMAnn[4],round(max(df$marks),0))
          co1<-c(co_PMAnn[1],co_PMAnn[2],co_PMAnn[3],co_PMAnn[4],co_PMAnn[5])
        }
 
       lb1<-length(br1)
       c1.x<-centroid.owin(df)$x
       c1.y<-centroid.owin(df)$y
       cr<-range(df$x)
       seq.max<-(cr[1]-c1.x-5)*-1
       seq.s<-seq.max/5  # divide by 4 for houses
       seq<-seq(seq.s,seq.max,seq.s) 
       dat<-cbind(df$x,df$y,df$marks)
       maxc<-dat[dat[,3]==max(a=dat[,3]),]
       
       png.out1<-paste("C:/Users/jrudokas/Desktop/NA Projects/AirMod Plotting/Output Charts/",getNameAttr(df),sep="",".png")
       png(png.out1,height=480*1.25,width=480*1.25)
       
       par(mar=c(5.1,4.1,4.1,3.1),mgp=c(2,1,0),adj=.4)
       plot.im(Smooth(df,sigma=30),col=co1,breaks=br1,zlim=c(0,br1[lb1]),main=NA,ribargs=list(at=br1,las=1),cex.axis=1.5,xlab=paste("Max Concentration:",round(max(df$marks),0),sep=" "),cex.lab=1.5)
       axis(1,at=c((c1.x-seq),c1.x,(c1.x+seq)),labels=c(-100,-200,-300,-400,-500,"0m",100,200,300,400,500),lwd=0,lwd.ticks=1,las=2,line=-2.9)
       axis(2,at=c((c1.y-seq),c1.y,(c1.y+seq)),labels=c(-100,-200,-300,-400,-500,"0m",100,200,300,400,500),lwd=0,lwd.ticks=1,las=1,line=-1.28)  
       points(df$x,df$y,pch=16,cex=.25)
       points(maxc[1],maxc[2],pch="+",cex=2.5)
       dev.off()                       
        
                               })