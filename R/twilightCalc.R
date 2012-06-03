twilightCalc <-
function(datetime,light,LightThreshold=TRUE,ask=TRUE) {
		
# --------------------------------------------------------------------------------------------------------------
# file:					either a *.glf file (directory) or a table containing only date (wth time) and light intensities
# atom.LightThreshold:	if TRUE, the light intensity Threshold is calculated as 3 values above the basline
#						if a numerical value is given, the Threshold will be set likewise
# --------------------------------------------------------------------------------------------------------------

	bas <- data.frame(datetime=as.POSIXct(as.character(datetime),"UTC"),light)
	 
	   
if (is.logical(LightThreshold))
	{  
	   # Basic level
	   min <- min(bas$light)
	   LightThreshold <- median(bas$light[bas$light<min+10]) + 3
	 } else {
	 	LightThreshold <- as.numeric(LightThreshold)
	 	min <- min(bas$light)
	 	}
	 
	   

bas$after   <- c(bas$light[2:nrow(bas)],min)
bas$before  <- c(min,bas$light[1:(nrow(bas)-1)])

index1      <- bas$light == bas$before & bas$light == bas$after
index1[1]   <- FALSE
tab         <- bas[!index1,1:2]


out <- data.frame(e=as.POSIXct("1900-01-01 01:01:01","UTC"),type=0)

row <- 1
for (i in 2:(length(tab[,1])-1))
	{
		if (tab$light[i] ==  LightThreshold & (tab$light[i] %in% tab$light[i-1]:tab$light[i+1]))
			{
				out[row,1] <- tab$date[i]
				out[row,2] <- if (tab$light[i-1] < LightThreshold) 1 else 2
				
				row <- row+1
			}
		
		if (tab$light[i] < LightThreshold & tab$light[i+1] > LightThreshold)
			{
				x1 <- as.numeric(substring(tab$date[i],12,13))+(as.numeric(substring(tab$date[i],15,16)))/60
	 	 		y1 <- tab$light[i]
	 	 		x2 <- (as.numeric(substring(tab$date[i+1],12,13)))+(as.numeric(substring(tab$date[i+1],15,16)))/60
	 	 		y2 <- tab$light[i+1]
	 	 		m <- (y2-y1)/(x2-x1)
	 	 		b <- y2-(m*x2)
	
	 	 		xnew <- (LightThreshold -b)/m
	 	 		out[row,1] <- as.POSIXct(paste(as.Date(tab$date[i])," ",floor(xnew),":",floor((xnew-(floor(xnew)))*60),":",0,sep=""),"UTC")
				out[row,2] <- 1
				
				row <- row+1
			}
			
	   	if (tab$light[i] > LightThreshold & tab$light[i+1] < LightThreshold)
			{
				x1 <- as.numeric(substring(tab$date[i],12,13))+(as.numeric(substring(tab$date[i],15,16)))/60
	 	 		y1 <- tab$light[i]
	 	 		x2 <- (as.numeric(substring(tab$date[i+1],12,13)))+(as.numeric(substring(tab$date[i+1],15,16)))/60
	 	 		y2 <- tab$light[i+1]
	 	 		m <- (y2-y1)/(x2-x1)
	 	 		b <- y2-(m*x2)
	
	 	 		xnew <- (LightThreshold -b)/m
	 	 		out[row,1] <- as.POSIXct(paste(as.Date(tab$date[i])," ",floor(xnew),":",floor((xnew-(floor(xnew)))*60),":",0,sep=""),"UTC")
				out[row,2] <- 2
				
				row <- row+1
			}
	}

if(ask==TRUE) {

ask <- menu(title="Would you like to check the sunrise/sunset definition manually?",c("yes","no"))

out$mod <- 0

if(ask==1)	
	{	
		start <- as.Date(out$e[1])
		end <- as.Date(out$e[nrow(out)])
		row <- 1
		repeat
			{
	
				temp <- subset(tab,datetime %in% ((out$e[row]- 126000):(out$e[row]+ 126000)))
				if(row > (nrow(out)-5) & out$e[row] <= (out$e[row]+ 126000)) seq <- seq else
				{
				seq    <- seq((temp$datetime[1]+3600),temp$datetime[nrow(temp)],(5*60*60))
				}
				x      <- substring(seq,12,16)
				
				
				par(mar=c(3,4,3,0.5),oma=c(1,2,0,1))
				plot(temp$datetime,temp$light,type="o",cex=0.6,pch=20,xaxt="n",ylab="Light intensity",main=paste(as.Date(out$e[row])," ","(Data range: ",start," to ",end,")",sep=""))
				axis(1,at=seq,labels=x)
				abline(h=LightThreshold,lty=3,col="blue",lwd=1.3)
				abline(v=c(out$e[out$type==1 & out$mod==0]),lwd=0.6,col="darkgrey")
				abline(v=c(out$e[out$type==2 & out$mod==0]),lty=2,lwd=0.6,col="darkgrey")
				
				abline(v=out$e[row],lty=if(out$type[row]==1) 1 else 2,lwd=2,col="orange")
				legend("topright",lty=c(3,1,2,if(out$type[row]==1) 1 else 2),lwd=c(1.3,2,2,2),col=c("blue","darkgrey","darkgrey","orange"),c("Light\nThreshold","sunrise","sunset","focal event"),cex=1,bg="white")
				


ask2 <- (menu(title="Would you like to confirm this twilight event?",c("yes","no","go back","end")))
		if (ask2==0) break
		if (ask2==1) {out$mod[row] <- 0
					  row <- row+1
					  if (row==(nrow(out)+1)) cat("Thank you!\n\n")
					  }
		if (ask2==2) 
					{out$mod[row] <- 1
					 row <- row +1
					 if (row==(nrow(out)+1)) cat("Thank you!\n\n")
					 }
		if (ask2==3) 
					{out$mod[row] <- 0
					 row <- row-1
					if (row==(nrow(out)+1)) cat("Thank you!\n\n")
					}
		if (ask2==4) {cat("Thank you!\n\n")
					  break}
		if (row==(nrow(out)+1)) break
			}

	}

out <- subset(out,out$mod==0,select=-"mod")

	
}


opt <- data.frame(tFirst=as.POSIXct("1900-01-01 01:01","UTC"),tSecond=as.POSIXct("1900-01-01 01:01","UTC"),type=0)


row <- 1
for (k in 1:(length(out$e)-1))
	{
	  if (as.numeric(difftime(out$e[k],out$e[k+1]))< 24 & out$e[k] != out$e[k+1])
	  	{
	  		opt[row,1] <- out$e[k]
	  		opt[row,2] <- out$e[k+1]
			opt[row,3] <- out$type[k]
			
			row <- row+1
		}
	}
	
	
return (opt)

}

