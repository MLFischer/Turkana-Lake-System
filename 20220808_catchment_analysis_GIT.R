################
# Catchment Evaluation 
# M.L.Fischer 03/2022
################

# Sec 1 Preparation
remove(list=ls()) 
library(raster)
library(rgdal)

setwd("../data")
freqs <- list()


####################################################################
#CHALBI
catch.shp <- readOGR(".",layer = "Chalbi")
catch.dem <- raster('SRTM_cut_utm_final_aggf2.tif') #dem data of catchment
plot(catch.dem);plot(catch.shp,add=T)

# mask the DEM with the shapefile 
catch.dem<-crop(catch.dem,catch.shp)
catch.dem<-mask(catch.dem,catch.shp)
plot(catch.dem);plot(catch.shp,add=T)

# manipulate DEM
catch.dem[catch.dem > 505.5]<- NA
catch.dem[catch.dem < 365]  <- 365

505.5 - 365
plot(catch.dem);plot(catch.shp,add=T)

# do bathymetry analysis
freq<-as.data.frame(freq(catch.dem,digit=1, merge=F))
freq <- freq[order(freq[,1]),] 

#area below altitude
for (i in 1:(length(freq[,1])-1))
{freq[i,3]<-sum(freq[1:i,2])}  

# area by the resolution of the pixels, km²
freq<-freq[-(length(freq[,1])),] #delete the last control row
freq[,4]<-freq[,3]*(res(catch.dem)[1]^2)/(1000^2) 
freq[,5]<-freq[,1]-365
plot(freq[,4],freq[,5],type="l",xlab="area (sqkm)",ylab="lake level (m)",main="paleo-lake Chalbi")

freq[,c(6,7,8,9)]<-NA #bad hardcoded workaround cause I dont want to change the numbers later on, so thats it <3
freq[,10]<- 100*freq[,3]/freq[length(freq[,3]),3] #lake area ratio


freqs$chalbi<-freq


####################################################################
#SUGUTA
catch.shp <- readOGR(".",layer = "Suguta")
catch.dem <- raster('SRTM_cut_utm_final_aggf2.tif') #dem data of catchment
plot(catch.dem);plot(catch.shp,add=T)

# mask the DEM with the shapefile 
catch.dem<-crop(catch.dem,catch.shp)
catch.dem<-mask(catch.dem,catch.shp)
plot(catch.dem);plot(catch.shp,add=T)

# manipulate DEM
catch.dem[catch.dem > 583.7]<- NA
catch.dem[catch.dem < 275]  <- 275
583.7 - 275
plot(catch.dem);plot(catch.shp,add=T)

# do bathymetry analysis
freq<-as.data.frame(freq(catch.dem,digit=1, merge=F))
freq <- freq[order(freq[,1]),] 

#area below altitude
for (i in 1:(length(freq[,1])-1))
{freq[i,3]<-sum(freq[1:i,2])}  

# area by the resolution of the pixels, km²
freq<-freq[-(length(freq[,1])),] #delete the last control row
freq[,4]<-freq[,3]*(res(catch.dem)[1]^2)/(1000^2) 
freq[,5]<-freq[,1]-275
plot(freq[,4],freq[,5],type="l",xlab="area (sqkm)",ylab="lake level (m)",main="paleo-lake Suguta")
freq[,c(6,7,8,9)]<-NA #bad hardcoded workaround cause I dont want to change the numbers later on, so thats it <3
freq[,10]<- 100*freq[,3]/freq[length(freq[,3]),3] #lake area ratio
sug_a <- freq[length(freq[,3]),3]
freqs$suguta<-freq

####################################################################
#BAHIR
catch.shp <- readOGR(".",layer = "Bahir")
catch.dem <- raster('SRTM_cut_utm_final_aggf2.tif') #dem data of catchment
plot(catch.dem);plot(catch.shp,add=T)

# mask the DEM with the shapefile 
catch.dem<-crop(catch.dem,catch.shp)
catch.dem<-mask(catch.dem,catch.shp)
plot(catch.dem);plot(catch.shp,add=T)

# manipulate DEM
catch.dem[catch.dem > 542]<- NA
catch.dem[catch.dem < 500]  <- 500
plot(catch.dem);plot(catch.shp,add=T)

# do bathymetry analysis
freq<-as.data.frame(freq(catch.dem,digit=1, merge=F))
freq <- freq[order(freq[,1]),] 

#area below altitude
for (i in 1:(length(freq[,1])-1))
{freq[i,3]<-sum(freq[1:i,2])}  

# area by the resolution of the pixels, km²
freq<-freq[-(length(freq[,1])),] #delete the last control row
freq[,4]<-freq[,3]*(res(catch.dem)[1]^2)/(1000^2) 
freq[,5]<-freq[,1]-500
plot(freq[,4],freq[,5],type="l",xlab="area (sqkm)",ylab="lake level (m)",main="paleo-lake Bahir")
freq[,c(6,7,8,9)]<-NA #bad hardcoded workaround cause I dont want to change the numbers later on, so thats it <3
freq[,10]<- 100*freq[,3]/freq[length(freq[,3]),3] #lake area ratio
freqs$bahir<-freq

####################################################################
#TURKANA
catch.shp <- readOGR(".",layer = "Turkana")
catch.dem <- raster('SRTM_cut_utm_final_aggf2.tif') #dem data of catchment
plot(catch.dem);plot(catch.shp,add=T)

# mask the DEM with the shapefile 
catch.dem<-crop(catch.dem,catch.shp)
catch.dem<-mask(catch.dem,catch.shp)
plot(catch.dem);plot(catch.shp,add=T)

# manipulate DEM
catch.dem[catch.dem > 455.75]<- NA
catch.dem[catch.dem < 362]   <- 362
455.75-362
plot(catch.dem);plot(catch.shp,add=T)

# do bathymetry analysis
freq<-as.data.frame(freq(catch.dem,digit=1, merge=F))
freq <- freq[order(freq[,1]),] 

#area below altitude
for (i in 1:(length(freq[,1])-1))
{freq[i,3]<-sum(freq[1:i,2])}  

# area by the resolution of the pixels, km²
freq<-freq[-(length(freq[,1])),] #delete the last control row
freq[,4]<-freq[,3]*(res(catch.dem)[1]^2)/(1000^2) 
freq[,5]<-freq[,1]-361
plot(freq[,4],freq[,5],type="l",xlab="area (sqkm)",ylab="lake level (m)",main="paleo-lake Turkana")
freq$V5<-freq$V5+109 
freq[,c(6,7,8,9)]<-NA #bad hardcoded workaround cause I dont want to change the numbers later on, so thats it <3
freq[,10]<- 100*freq[,3]/freq[length(freq[,3]),3] #lake area ratio
turk_a <- freq[length(freq[,3]),3]
freqs$turkana<-freq

########################################################
par(mfrow=c(1,1))
plot(freqs[[4]][,4],freqs[[4]][,5],type="l",xlab="area (sqkm)",ylab="lake level (m)",ylim=c(0,320), xlim=c(0,22000))
for (i in 1:3){
lines(freqs[[i]][,4],freqs[[i]][,5])
}

for (i in 1:4){
freqs[[i]][,6]<-freqs[[i]][,5]/freqs[[i]][length(freqs[[i]][,5]),5]
}
plot(freqs[[4]][,4],freqs[[4]][,6],type="l",xlab="area (sqkm)",ylab="lake level (m)",ylim=c(0,1), xlim=c(0,22000))
for (i in 1:3){
  lines(freqs[[i]][,4],freqs[[i]][,6])
}
####################################################################
####################################################################
####################################################################
#####do some stuff again, cause I need the freqs for a few locations above the overflow sill
freqs2 <- list()
####################################################################
#SUGUTA
catch.shp <- readOGR(".",layer = "Suguta")
catch.dem <- raster('SRTM_cut_utm_final_aggf2.tif') #dem data of catchment
plot(catch.dem);plot(catch.shp,add=T)

# mask the DEM with the shapefile 
catch.dem<-crop(catch.dem,catch.shp)
catch.dem<-mask(catch.dem,catch.shp)
plot(catch.dem);plot(catch.shp,add=T)

# manipulate DEM
catch.dem[catch.dem > 700]<- NA
catch.dem[catch.dem < 275]  <- 275
583.7 - 275
plot(catch.dem);plot(catch.shp,add=T)

# do bathymetry analysis
freq<-as.data.frame(freq(catch.dem,digit=1, merge=F))
freq <- freq[order(freq[,1]),] 

#area below altitude
for (i in 1:(length(freq[,1])-1))
{freq[i,3]<-sum(freq[1:i,2])}  

# area by the resolution of the pixels, km²
freq<-freq[-(length(freq[,1])),] #delete the last control row
freq[,4]<-freq[,3]*(res(catch.dem)[1]^2)/(1000^2) 
freq[,5]<-freq[,1]-275
plot(freq[,4],freq[,5],type="l",xlab="area (sqkm)",ylab="lake level (m)",main="paleo-lake Suguta")
freq[,c(6,7,8,9)]<-NA #bad hardcoded workaround cause I dont want to change the numbers later on, so thats it <3
freq[,10]<- 100*freq[,3]/sug_a #lake area ratio
freqs2$suguta<-freq
####################################################################
#TURKANA
catch.shp <- readOGR(".",layer = "Turkana")
catch.dem <- raster('SRTM_cut_utm_final_aggf2.tif') #dem data of catchment
plot(catch.dem);plot(catch.shp,add=T)

# mask the DEM with the shapefile 
catch.dem<-crop(catch.dem,catch.shp)
catch.dem<-mask(catch.dem,catch.shp)
plot(catch.dem);plot(catch.shp,add=T)

# manipulate DEM
catch.dem[catch.dem > 700]<- NA
catch.dem[catch.dem < 362]   <- 362
455.75-362
plot(catch.dem);plot(catch.shp,add=T)

# do bathymetry analysis
freq<-as.data.frame(freq(catch.dem,digit=1, merge=F))
freq <- freq[order(freq[,1]),] 

#area below altitude
for (i in 1:(length(freq[,1])-1))
{freq[i,3]<-sum(freq[1:i,2])}  

# area by the resolution of the pixels, km²
freq<-freq[-(length(freq[,1])),] #delete the last control row
freq[,4]<-freq[,3]*(res(catch.dem)[1]^2)/(1000^2) 
freq[,5]<-freq[,1]-361
plot(freq[,4],freq[,5],type="l",xlab="area (sqkm)",ylab="lake level (m)",main="paleo-lake Turkana")
freq$V5<-freq$V5+109 
freq[,c(6,7,8,9)]<-NA #bad hardcoded workaround cause I dont want to change the numbers later on, so thats it <3
freq[,10]<- 100*freq[,3]/turk_a #lake area ratio
freqs2$turkana<-freq

###########################
get(load("ts_s.RData"))
ts<-ts_s #otherwise I will be annoyed by that "_s".. even tho it remindes me of ... ahh.. anyhow.
ts[,1]<-ts[,1]*1000
####################
ts_CB03<- read.csv2("20220326_ts_CB03_K.csv")
ts_CB01<- read.csv2("20220326_ts_CB01_K.csv")

ts_CB01<- ts_CB01[1:which.min(ts_CB01$Age.CB01<15000),]
ts_CB03<- ts_CB03[1:which.min(ts_CB03$ï..Age.CB03<15000),]

####################
#load the shore line data table
sla <- read.csv("20220419_shorelines_final.csv",sep=";",dec=",")
sla <- sla[,c(1,5,8,9,10,2)]
colnames(sla)<-c("ID","Elev","age","age min","age max","lake")
IDnr=6
#relative lake level (rll) per lake:
sla[which(sla[,IDnr]=="S"),7] <- sla[which(sla[,IDnr]=="S"),2]-275
sla[which(sla[,IDnr]=="B"),7] <- sla[which(sla[,IDnr]=="B"),2]-500
sla[which(sla[,IDnr]=="T"),7] <- sla[which(sla[,IDnr]=="T"),2]-362
sla[which(sla[,IDnr]=="TA"),7] <- sla[which(sla[,IDnr]=="TA"),2]-362
sla[which(sla[,IDnr]=="C"),7] <- sla[which(sla[,IDnr]=="C"),2]-365


#normalized relative lake level (nrll):
sla[which(sla[,IDnr]=="S"),8] <- sla[which(sla[,IDnr]=="S"),7]/308.7
sla[which(sla[,IDnr]=="B"),8] <- sla[which(sla[,IDnr]=="B"),7]/42
sla[which(sla[,IDnr]=="T"),8] <- sla[which(sla[,IDnr]=="T"),7]/93.75
sla[which(sla[,IDnr]=="TA"),8] <- sla[which(sla[,IDnr]=="TA"),7]/93.75
sla[which(sla[,IDnr]=="C"),8] <- sla[which(sla[,IDnr]=="C"),7]/140.5

#area per lake level per lake:
sla[which(sla[,IDnr]=="T"),9] <- approx(freqs$turkana[,5], freqs$turkana[,4], xout=sla[which(sla[,IDnr]=="T"),2]-362+109)$y
sla[which(sla[,IDnr]=="TA"),9] <- approx(freqs2$turkana[,5], freqs2$turkana[,4], xout=sla[which(sla[,IDnr]=="TA"),2]-362+109)$y
sla[which(sla[,IDnr]=="S"),9] <- approx(freqs2$suguta[,5], freqs2$suguta[,4], xout=sla[which(sla[,IDnr]=="S"),2]-275)$y
sla[which(sla[,IDnr]=="B"),9] <- approx(freqs$bahir[,5], freqs$bahir[,4], xout=sla[which(sla[,IDnr]=="B"),2]-500)$y
sla[which(sla[,IDnr]=="C"),9] <- approx(freqs$chalbi[,5], freqs$chalbi[,4], xout=sla[which(sla[,IDnr]=="C"),2]-365)$y


#lake area ratio LAR:
sla[,c(10,11,12)]<-NA
sla[which(sla[,IDnr]=="T"),13] <- approx(freqs$turkana[,5], freqs$turkana[,10], xout=sla[which(sla[,IDnr]=="T"),2]-362+109)$y
sla[which(sla[,IDnr]=="TA"),13] <- approx(freqs2$turkana[,5], freqs2$turkana[,10], xout=sla[which(sla[,IDnr]=="TA"),2]-362+109)$y
sla[which(sla[,IDnr]=="S"),13] <- approx(freqs2$suguta[,5], freqs2$suguta[,10], xout=sla[which(sla[,IDnr]=="S"),2]-275)$y
sla[which(sla[,IDnr]=="B"),13] <- approx(freqs$bahir[,5], freqs$bahir[,10], xout=sla[which(sla[,IDnr]=="B"),2]-500)$y
sla[which(sla[,IDnr]=="C"),13] <- approx(freqs$chalbi[,5], freqs$chalbi[,10], xout=sla[which(sla[,IDnr]=="C"),2]-365)$y

colnames(sla)<-c("ID","Elev","age","age min","age max","lake","rll","nrll","area","NA","NA","NA","LAR")

#############################
#setwd('..')
colS="#9DD2A5"
colB="#F26C54"
colT="#B1A553"
colC="#4C54E0"
# plot the Bathymetry and all Shorelines with specific layout rules
#pdf(file = "20220317_Bathyplot_v01.pdf",   # The directory you want to save the file in
#    width = 6, # The width of the plot in inches
#    height = 6) # The height of the plot in inches

par(mfrow=c(3,1))
plot(freqs[[4]][,4],freqs[[4]][,5],type="n",xlab="Area (sqkm)",ylab="Relative Lake Level (m)",ylim=c(0,320), xlim=c(0,22000))

lines(c(0,freqs[[4]][1,4]),c(0,110),cex=0.8,col="grey",lty=2)
lines(c(0,freqs[[3]][1,4]),c(0,0),cex=0.8,col="grey",lty=2)

for (i in 1:4){
  lines(freqs[[i]][,4],freqs[[i]][,5])
}
points(sla[which(sla[,IDnr]=="T"),9], sla[which(sla[,IDnr]=="T"),7]+109,pch=15,cex=0.8,col=colT)
points(sla[which(sla[,IDnr]=="TA"),9], sla[which(sla[,IDnr]=="TA"),7]+119,pch=25,cex=0.8,col=colT)
points(sla[which(sla[,IDnr]=="S"),9], sla[which(sla[,IDnr]=="S"),7],pch=17,cex=1,col=colS)
points(sla[which(sla[,IDnr]=="B"),9], sla[which(sla[,IDnr]=="B"),7],pch=16,cex=1,col=colB)
points(sla[which(sla[,IDnr]=="C"),9], sla[which(sla[,IDnr]=="C"),7],pch=18,cex=1,col=colC)


for (i in 1:4){points(max(freqs[[i]][,4]),max(freqs[[i]][,5]),pch=16)}
#dev.off()

###############################################################
# plot all Shorelines as time series

plot(sla[,8]~sla[,3],pch=sla[,6],type="n",xlim=c(0,15000),ylim=c(0,1),xlab="Age (cal BP)",ylab="Normalized Relative Lake Level")


#par(new=TRUE)
#plot(ts[,1],ts[,2],type="l",xlim=c(0,15000),ylim = rev(range(ts[,2])),yaxt="n",xlab="",ylab="",col="lightgrey")
#axis(2, pos=15500) 
#par(new=TRUE)
#plot(ts[,1],ts[,3],type="l",xlim=c(0,15000),ylim = rev(range(ts[,3])),yaxt="n",xlab="",col="darkgrey",ylab="",yaxt="n")
#par(new=TRUE)
#plot(ts_CB03$ï..Age.CB03,ts_CB03$K.CB3,type="l",xlim=c(0,15000),ylim = rev(range(ts_CB03$K.CB3,na.rm=T)),col="lightgrey",yaxt="n",xlab="",ylab="",cex=0.4)

par(new=TRUE)
plot(ts_CB01$Age.CB01,ts_CB01$K.CB01,type="l",xlim=c(0,15000),ylim = rev(range(ts_CB01$K.CB01,na.rm=T)),yaxt="n",xlab="",ylab="",cex=0.7)

par(new=TRUE)
plot(sla[,8]~sla[,3],pch=sla[,6],type="n",xlim=c(0,15000),ylim=c(0,1),xaxt="n",yaxt="n",ylab="",xlab="")

points(sla[which(sla[,IDnr]=="T"),8]~ sla[which(sla[,IDnr]=="T"),3],pch=15,cex=0.7,col=colT)
points(sla[which(sla[,IDnr]=="TA"),8]~ sla[which(sla[,IDnr]=="TA"),3],pch=25,cex=0.7,col=colT)
points(sla[which(sla[,IDnr]=="S"),8]~ sla[which(sla[,IDnr]=="S"),3],pch=17,cex=1,col=colS)
points(sla[which(sla[,IDnr]=="B"),8]~ sla[which(sla[,IDnr]=="B"),3],pch=16,cex=1.3,col=colB)
points(sla[which(sla[,IDnr]=="C"),8]~ sla[which(sla[,IDnr]=="C"),3],pch=18,cex=1.3,col=colC)


arrows(x0=sla[which(sla[,IDnr]=="T"),4],x1=sla[which(sla[,IDnr]=="T"),5],
       y0=sla[which(sla[,IDnr]=="T"),8],y1=sla[which(sla[,IDnr]=="T"),8],length=0,col=colT,lwd=1)

arrows(x0=sla[which(sla[,IDnr]=="TA"),4],x1=sla[which(sla[,IDnr]=="TA"),5],
       y0=sla[which(sla[,IDnr]=="TA"),8]+0.005,y1=sla[which(sla[,IDnr]=="TA"),8]+0.005,length=0,col=colT,lwd=1)

arrows(x0=sla[which(sla[,IDnr]=="S"),4],x1=sla[which(sla[,IDnr]=="S"),5],
       y0=sla[which(sla[,IDnr]=="S"),8],y1=sla[which(sla[,IDnr]=="S"),8],length=0,col=colS,lwd=1)

arrows(x0=sla[which(sla[,IDnr]=="B"),4],x1=sla[which(sla[,IDnr]=="B"),5],
       y0=sla[which(sla[,IDnr]=="B"),8],y1=sla[which(sla[,IDnr]=="B"),8],length=0,col=colB,lwd=1)

arrows(x0=sla[which(sla[,IDnr]=="C"),4],x1=sla[which(sla[,IDnr]=="C"),5],
       y0=sla[which(sla[,IDnr]=="C"),8],y1=sla[which(sla[,IDnr]=="C"),8],length=0,col=colC,lwd=1)

#######
par(new=TRUE)
plot(sla[,13]~sla[,3],pch=sla[,6],type="n",xlim=c(0,15000),ylim=c(0,100),xaxt="n",yaxt="n",ylab="",xlab="")

points(sla[which(sla[,IDnr]=="T"),13]~ sla[which(sla[,IDnr]=="T"),3],pch=15,cex=0.7,col=colT)
points(sla[which(sla[,IDnr]=="TA"),13]~ sla[which(sla[,IDnr]=="TA"),3],pch=25,cex=0.7,col=colT)
points(sla[which(sla[,IDnr]=="S"),13]~ sla[which(sla[,IDnr]=="S"),3],pch=17,cex=1,col=colS)
points(sla[which(sla[,IDnr]=="B"),13]~ sla[which(sla[,IDnr]=="B"),3],pch=16,cex=1.3,col=colB)
points(sla[which(sla[,IDnr]=="C"),13]~ sla[which(sla[,IDnr]=="C"),3],pch=18,cex=1.3,col=colC)
######################################################
#
# calculate the average K in the range of 2sigma shoreline elevation:
#load time series data


# CB03
sla$Kmean03 <- NULL
for (i in 1:length(sla$ID)){
age_min <- which.min(abs(ts_CB03$ï..Age.CB03 - sla$`age min`[i])) #proximate min age in TS
age_max <- which.min(abs(ts_CB03$ï..Age.CB03 - sla$`age max`[i]))
#print(ts_CB03$ï..Age.CB03[c(age_min:age_max)])
#print(ts_CB03$K.CB3[c(age_min:age_max)])
sla$Kmean03[i] <- mean(ts_CB03$K.CB3[c(age_min:age_max)],na.rm=T)
}
sla$Kmean03 <- sla$Kmean03*(-1)

#CB01
sla$Kmean01 <- NULL
for (i in 1:length(sla$ID)){
  age_min <- which.min(abs(ts_CB01$Age.CB01 - sla$`age min`[i])) #proximate min age in TS
  age_max <- which.min(abs(ts_CB01$Age.CB01 - sla$`age max`[i]))
  #print(ts_CB03$ï..Age.CB03[c(age_min:age_max)])
  #print(ts_CB03$K.CB3[c(age_min:age_max)])
  sla$Kmean01[i] <- mean(ts_CB01$K.CB01[c(age_min:age_max)],na.rm=T)
}
sla$Kmean01 <- sla$Kmean01*(-1)

#CB01 precise ages (no sd range)
sla$Kmean012 <- NULL
for (i in 1:length(sla$ID)){
  age_min <- which.min(abs(ts_CB01$Age.CB01 - sla$age[i])) #proximate min age in TS
  #print(ts_CB03$ï..Age.CB03[c(age_min:age_max)])
  #print(ts_CB03$K.CB3[c(age_min:age_max)])
  sla$Kmean012[i] <- ts_CB01$K.CB01[age_min]
}
sla$Kmean012 <- sla$Kmean012*(-1)

plot(sla$nrll~sla$Kmean01,type="n",xlab="K (*-1)",ylab="Normalized Relative Lake Level (m)",ylim=c(0,1))

points(sla$nrll[which(sla[,IDnr]=="T")]~ sla$Kmean01[which(sla[,IDnr]=="T")],pch=15,cex=0.8,col=colT)
points(sla$nrll[which(sla[,IDnr]=="TA")]~ sla$Kmean01[which(sla[,IDnr]=="TA")],pch=25,cex=0.8,col=colT)
points(sla$nrll[which(sla[,IDnr]=="S")]~ sla$Kmean01[which(sla[,IDnr]=="S")],pch=17,cex=1,col=colS)
points(sla$nrll[which(sla[,IDnr]=="B")]~ sla$Kmean01[which(sla[,IDnr]=="B")],pch=16,cex=0.8,col=colB)

cor(sla$nrll,sla$Kmean01,use="complete.obs",method="pearson")^2
KSm <- lm(sla$nrll~sla$Kmean01)
abline(KSm)
#####################################
par(mfrow=c(1,2))
plot(sla$nrll~sla$Kmean01,type="n",xlab="K (*-1)",ylab="Normalized Relative Lake Level (m)",ylim=c(0,1))

points(sla$nrll[which(sla[,IDnr]=="T")]~ sla$Kmean01[which(sla[,IDnr]=="T")],pch=15,cex=0.8,col=colT)
points(sla$nrll[which(sla[,IDnr]=="TA")]~ sla$Kmean01[which(sla[,IDnr]=="TA")],pch=25,cex=0.8,col=colT)
points(sla$nrll[which(sla[,IDnr]=="S")]~ sla$Kmean01[which(sla[,IDnr]=="S")],pch=17,cex=1,col=colS)
points(sla$nrll[which(sla[,IDnr]=="B")]~ sla$Kmean01[which(sla[,IDnr]=="B")],pch=16,cex=0.8,col=colB)
points(sla$nrll[which(sla[,IDnr]=="C")]~ sla$Kmean01[which(sla[,IDnr]=="C")],pch=18,cex=1,col=colC)

cor(sla$nrll,sla$Kmean01,use="complete.obs",method="pearson")^2
KSm <- lm(sla$nrll~sla$Kmean01)
abline(KSm)

###do the same for exact K values
plot(sla$nrll~sla$Kmean012,type="n",xlab="K (*-1)",ylab="Normalized Relative Lake Level (m)",ylim=c(0,1))

points(sla$nrll[which(sla[,IDnr]=="T")]~ sla$Kmean012[which(sla[,IDnr]=="T")],pch=15,cex=0.8,col=colT)
points(sla$nrll[which(sla[,IDnr]=="TA")]~ sla$Kmean012[which(sla[,IDnr]=="TA")],pch=25,cex=0.8,col=colT)
points(sla$nrll[which(sla[,IDnr]=="S")]~ sla$Kmean012[which(sla[,IDnr]=="S")],pch=17,cex=1,col=colS)
points(sla$nrll[which(sla[,IDnr]=="B")]~ sla$Kmean012[which(sla[,IDnr]=="B")],pch=16,cex=0.8,col=colB)
points(sla$nrll[which(sla[,IDnr]=="C")]~ sla$Kmean012[which(sla[,IDnr]=="C")],pch=18,cex=1,col=colC)

cor(sla$nrll,sla$Kmean012,use="complete.obs",method="pearson")^2
KSm <- lm(sla$nrll~sla$Kmean01)
abline(KSm)


