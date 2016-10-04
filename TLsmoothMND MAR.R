get.excel2 <- function(filename)
  #function to convert a column based spreadsheet of measurements into a dataframe.
  #the first column should consist of grouping variable names, corresponding to the contents
  #of each row.  The first row of data should have a row name of "Data"
  {
     vars <- as.character(read.table(file=filename,sep=",",fill=TRUE,blank.lines.skip=FALSE)[,1])
     data.start <- match("Data", vars)
     data.table <- read.table(file=filename,sep=",",skip=data.start-1,fill=TRUE)[,-1]
     last <- ifelse(match("",vars)>data.start,data.start-1,match("",vars)-1) #where is the last grouping variable? 
     header <- read.table(file=filename, sep=",",fill=TRUE,as.is=TRUE)[1:last,-1]
     time.frame <- data.table[,header[1,]=="time"] #extract the time information
     colnames(time.frame) <- header[3,header[1,]=="time"]
     data.table <- data.table[,header[1,]!="time"] #remove time information from data table, leaving hyp info
     header <- header[,header[1,]!="time"] #remove time information from header
     data.na <- is.na(data.table)
     time.table <- time.frame[,match(header[3,],colnames(time.frame))] # expand time.frame so that there is one column per plant
     tmp.frame <- data.frame(line=col(data.na)[!data.na],
     						 time=time.table[!data.na],
     						 y=data.table[!data.na])

      for (i in (1:last))
       tmp.frame[,vars[i]] <- factor(unlist(header[i,])[tmp.frame$line])
     return(tmp.frame)
   }
   
plot.splines <- function(splines.avg,title=NA,oneScale=T) {
	op <- par()

	#quartz(w=22,h=15)

	par(mfrow=c(3,2),oma=c(0,0,2,0))
	
	ylim=range(splines.avg,na.rm=T)
	
	for (i in 1:ncol(splines.avg)) {
		if(oneScale) {plot(as.numeric(rownames(splines.avg)),splines.avg[,i],type="l",main=colnames(splines.avg)[i],
						xlab="",ylab="",ylim=ylim)
			}
		else {plot(as.numeric(rownames(splines.avg)),splines.avg[,i],type="l",main=colnames(splines.avg)[i],
						xlab="",ylab="")
			}
		lim <- par()$usr # get coordinate limits for the current graph
		dusk <- seq(480,lim[2],by=1440)
		rect(xleft=dusk,ybottom=lim[3],xright=dusk+960,ytop=lim[4],density=10,col="gray")
		abline(h=0)
			}#for i
			
		title(title,outer=T,cex.main=2)

	#par(op)
}#plot.splines

tl <- get.excel2("MND1MAR2009.csv")

summary(tl)

scale <- 1179/63 # 1179pixels/63mm between plate markings

tl$y <- tl$y/scale

tl <- tl[tl$time<4320,]

summary(tl)

#library(lattice)

#xyplot(y~time|gt,data=tl,type="l",groups=plant)


#####try smoothing inputs
###testing...
#
#test <- tl$y[tl$plant=="mnd1sep0908BLCol_2"]
#time <- tl$time[tl$plant=="mnd1sep0908BLCol_2"]
#
#plot(test~time,type="l")
#
#span <- seq(0.02,0.1,.02)
#
#col <- rainbow(length(span))
#
#for(i in (1:length(span))) {
#	lines(x=time,y=loess(test~time,span=span[i])$fitted,col=col[i],lwd=2)
#	}
#
#legend("bottomright",legend=span,lwd=2,col=col)
##0.1 looks good

	
tl$yOriginal<-tl$y

for(p in levels(tl$plant)) {
	tmp <- tl[tl$plant==p,]
	tl$y[tl$plant==p] <- loess(tmp$yOriginal~tmp$time,span=0.1)$fitted
	rm(tmp)
	}

#quartz()

#xyplot(y~time|gt,data=tl,type="l",groups=plant)

###next, compute running average

runavg <- function(x,window=5) {    #This will compute a running average rate
	x <- x[order(x$time),]          #for one plant at a time.  
	x$rate <- NA					#window is number of timepoints, not number of time intervals
	for (i in (round(window/2)+1):(nrow(x)-round(window/2))) {
		total <- 0
		for (j in (-trunc(window/2)+1):trunc(window/2)) {
			total <- total + ((x$y[j+i]-x$y[j+i-1]) / ((x$time[j+i]-x$time[j+i-1])*(window-1)))
			}               #for j
		x$rate[i+(round(window/2))] <- total
		}                       # for i
	x
	}
	
calcRate <- function(x) { #no running average
	x <- x[order(x$time),] 
	x$rate <- NA
	for (i in 2:nrow(x)) {
		x$rate[i] <- (x$y[i]-x$y[i-1]) / (x$time[i] -x$time[i-1])
		}
		x
		}
			
#rate.list <- by(tl,tl$plant,runavg,5)     #a list of dataframes, each contaiing the rates + other information on a
                                        #per plant basis
#rate.list <- by(tl,tl$plant,runavg,3)  

rate.list <- by(tl,tl$plant,calcRate) #if we are going to smooth, maybe no running average? 
                                        
tl.rates <- rate.list[[1]]              #need to recompile the list of dataframes into one dataframe.  this will hold it

for (i in 2:length(rate.list)) tl.rates <- rbind(tl.rates,rate.list[[i]]) #merge into one frame

tl.rates <- tl.rates[complete.cases(tl.rates),]	#remove rows with no data

#try plotting directly using ggplot2
library(ggplot2)


night <- seq(60*8,max(tl.rates$time),60*24)
day <- seq(60*24,max(tl.rates$time),60*24)
if (length(night)>length(day)) day <- c(day,max(tl.rates$time))
bottom <- rep(0,length(night))
top <- rep(0.004,length(night))

#data frame to hold coordinates for nighttime rectangles
rectangles <- data.frame(night,day,top,bottom,rate=0,time=0)

rm(night,day,top,bottom)
quartz()	
pl <- ggplot(data=tl.rates,aes(y=rate,x=time))  + facet_wrap(~gt) + theme_bw()
pl <- pl + geom_rect(data=rectangles,mapping=aes(xmin=night,xmax=day,ymin=bottom,ymax=top),fill="grey80")
pl <- pl + stat_smooth(data=tl.rates,fill="grey20",method="loess",span=.1,n=200)
pl <- pl + coord_cartesian(ylim=c(-0.001,0.005))
pl


fitspline <- function(x,interval=10) {  #use splines to estimate rate at
                                        #common time points
  #print(x[1,])
  fxn <- splinefun(x$time,x$rate,method="natural")
  start <- ((min(x$time) %/% interval) + 1 ) * interval
  end <- (max(x$time) %/% interval) * interval
  time <- seq(start,end,interval)
  tmp <- x[rep(1,length(time)),] #create a new data frame with the basic information from x
  tmp$time <- time
  tmp$interval <- interval
  tmp$rate <- fxn(time)
  tmp
}

fitsmooth <- function(x,interval=10,spar=.5) { # alternate, using smoothed splines
	tmp.smooth <- smooth.spline(x$time,x$rate,all.knots=T,spar=spar)
  start <- ((min(x$time) %/% interval) + 1 ) * interval
  end <- (max(x$time) %/% interval) * interval
  time <- seq(start,end,interval)
  tmp <- x[rep(1,length(time)),] #create a new data frame with the basic information from x
  tmp$time <- time
  tmp$interval <- interval
  tmp$rate <- predict(tmp.smooth,time)$y
  tmp
}

interval <- 10

#without smoothing
pdf("noSmooth.pdf",w=18,h=9)
spline.list <- by(tl.rates,tl.rates$plant,fitspline)
spline.rates <- spline.list[[1]]           #need to recompile the list of dataframes into one dataframe.  this will hold it
for (i in 2:length(spline.list)) spline.rates <- rbind(spline.rates,spline.list[[i]]) #merge into one frame
spline.matrix <-tapply(spline.rates$rate,list(spline.rates$time,spline.rates$plant),function(x) x)
splines.avg <-tapply(spline.rates$rate,list(spline.rates$time,spline.rates$gt),mean,na.rm=T)
plot.splines(splines.avg,title="loess smoothing only")
dev.off()


pdf("spar.pdf",w=18,h=9)

for (s in seq(0,.5,.1)) {

	spline.list <- by(tl.rates,tl.rates$plant,fitsmooth,spar=s) 

	spline.rates <- spline.list[[1]]              #need to recompile the list of dataframes into one dataframe.  this will hold it

	for (i in 2:length(spline.list)) spline.rates <- rbind(spline.rates,spline.list[[i]]) #merge into one frame

	spline.matrix <-tapply(spline.rates$rate,list(spline.rates$time,spline.rates$plant),function(x) x)
	
	splines.avg <-tapply(spline.rates$rate,list(spline.rates$time,spline.rates$gt),mean,na.rm=T)

	plot.splines(splines.avg,title=paste("spar = ",s,sep=""))
	
	print(paste("spar = ",s,sep=""))

}

dev.off()

#plot Mendel

s <- 0.25

spline.list <- by(tl.rates,tl.rates$plant,fitsmooth,spar=s) 

spline.rates <- spline.list[[1]]              #need to recompile the list of dataframes into one dataframe.  this will hold it

for (i in 2:length(spline.list)) spline.rates <- rbind(spline.rates,spline.list[[i]]) #merge into one frame

spline.matrix <-tapply(spline.rates$rate,list(spline.rates$time,spline.rates$plant),function(x) x)
	
splines.avg <-tapply(spline.rates$rate,list(spline.rates$time,spline.rates$gt),mean,na.rm=T)

splines.avg <- splines.avg*60 #convert rate to hours

pdf("MND1.pdf",w=11,h=8.5)

plot.splines(splines.avg,title="growth rates")

dev.off()


