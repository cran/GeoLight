getElevation <- function(tFirst,tSecond,type,known.coord,plot=TRUE) {
	

	table <- data.frame(tFirst=as.POSIXct(as.character(tFirst),"UTC"),tSecond = as.POSIXct(as.character(tSecond),"UTC"),type=as.numeric(type))

coord <- known.coord
tab <- table
degElevation <- +12


	lat1   <- coord(tab[,1],tab[,2],tab[,3],degElevation,note=F)[,2]
	x0 <- i.loxodrom.dist(coord[1],coord[2],coord[1],median(lat1))
 	degElevation <- degElevation - 0.025
 	x1 <- i.loxodrom.dist(coord[1],coord[2],coord[1],median(coord(tab[,1],tab[,2],tab[,3],degElevation,note=F)[,2]))
	
	while(x0 > x1)
		{    
			degElevation <- degElevation - 0.025
			if(degElevation==10) break
			x0 <- x1
			x1 <- i.loxodrom.dist(coord[1],coord[2],coord[1],median(coord(tab[,1],tab[,2],tab[,3],degElevation,note=F)[,2]))
		}

if(plot==TRUE)
	{
	Tw <- as.POSIXct(subset(tab,type=1,select=c(tFirst))$tFirst,"UTC")
	
	SElev <- i.sunelevation(coord[1],coord[2],as.numeric(substring(Tw,1,4)),as.numeric(substring(Tw,6,7)),
        as.numeric(substring(Tw,9,10)),as.numeric(substring(Tw,12,13)),as.numeric(substring(Tw,15,16)),0)
	
	
	par(mfrow=c(1,2),oma=c(0.2,0.2,2,0.2))
	par(mar=c(6,4,6,1),bty="n",yaxt="n",xaxt="s")
	
	plot(SElev[as.numeric(substring(Tw,12,13)) %in% 0:12], 
        rep(1,length(SElev[as.numeric(substring(Tw,12,13)) %in% 0:12])),pch=20,
        cex=1,xlim=c(-10,max(SElev)+3),ylim=c(0.9,1.1),ylab="",xlab="",main="Sunrise",cex.main=1.1,font.main=3)
	mtext("Light intensity threshold",side=2,cex=1.1,font=6)
	arrows(-9.8,1,-8.64,1,length=0.1)
	abline(v=-6,lty=2,lwd=0.3)
	abline(v=degElevation,lty=2,lwd=2,col="orange")
	
	
	par(mar=c(6,1,6,3),bty="n",yaxt="n",xaxt="s")
	plot(SElev[as.numeric(substring(Tw,12,13)) %in% 13:23], 
      rep(1,length(SElev[as.numeric(substring(Tw,12,13)) %in% 13:23])),
      pch=1,cex=0.7,xlim=c(max(SElev)+3,-10),ylim=c(0.9,1.1),xlab="",main="Sunset",cex.main=1.1,font.main=3)
	abline(v=-6,lty=2,lwd=0.3)
	abline(v=degElevation,lty=2,lwd=2,col="orange")
	
	legend("topleft",lty=c(2,2,2),lwd=c(0.3,2,2),col=c("black","transparent","orange"),
      c("- 6 degrees","",paste("getElevation\n",round(degElevation-0.025,3)," degrees",sep="")),bg="white",box.col="white",cex=.9)
	
	mtext("Twilight times over sun elevation angles",line=0, adj=0.52, cex=1.5,col="black", outer=TRUE)
	mtext("Sun elevation angle (degrees)",SOUTH<-1,line=-3.2, adj=1,at=0.65, cex=1.1,col="black", outer=TRUE)
	
	}

return(degElevation - 0.025)
}

