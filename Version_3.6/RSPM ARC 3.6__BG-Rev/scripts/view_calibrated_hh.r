#view_calibrated_hh.r
#====================
#Regional Strategic Planning Model (RSPM) GreenSTEP Version
#Copyright 2009 - 2015, Oregon Department of Transportation
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
#http://www.apache.org/licenses/LICENSE-2.0
#Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
#Version: 3.5
#Date: 6/22/15

#------
#SET UP
#------

# Load library to read district shapefile
library( rgdal )

# Define function to create a color scale for coropleth maps
#-----------------------------------------------------------
#image.scale function code comes from http://menugget.blogspot.com/2011/08/adding-scale-to-image-plot.html#more
#This function creates a color scale for use with e.g. the image()
#function. Input parameters should be consistent with those
#used in the corresponding image plot. The "horiz" argument
#defines whether the scale is horizonal(=TRUE) or vertical(=FALSE).
#Depending on the orientation, x- or y-limits may be defined that
#are different from the z-limits and will reduce the range of
#colors displayed.
 
image.scale <- function(z, zlim, col = heat.colors(12),
breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
 if(!missing(breaks)){
  if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
 }
 if(missing(breaks) & !missing(zlim)){
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
 }
 if(missing(breaks) & missing(zlim)){
  zlim <- range(z, na.rm=TRUE)
  zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
  zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
 }
 poly <- vector(mode="list", length(col))
 for(i in seq(poly)){
  poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
 }
 xaxt <- ifelse(horiz, "s", "n")
 yaxt <- ifelse(horiz, "n", "s")
 if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
 if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
 if(missing(xlim)) xlim=XLIM
 if(missing(ylim)) ylim=YLIM
 plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
 for(i in seq(poly)){
  if(horiz){
   polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
  }
  if(!horiz){
   polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
  }
 }
}

#--------------------------------------
#READ IN AND FORMAT DATA TO BE COMPARED
#--------------------------------------

# Read in synthetic populations and calculate district summaries
#---------------------------------------------------------------
Smry_Md <- list()
for(md in Md) {
  SynPopFile <- paste0(ModelDir, "/SynHsld/Year", BaseYear, "/", md, ".RData")
  SynPop.. <- assignLoad(SynPopFile)
  Smry_Md[[md]]$TotHhInc.Dx <- tapply( SynPop..$Hhincttl, SynPop..$District, sum )
  Smry_Md[[md]]$TotPop.Dx <- tapply( SynPop..$Hhsize, SynPop..$District, sum )
}    
TotalHhInc.Di <- unlist(lapply(Smry_Md, function(x) x$TotHhInc.Dx))
names(TotalHhInc.Di) <- unlist(lapply(Smry_Md, function(x) names(x$TotHhInc.Dx)))
TotalHhInc.Di <- TotalHhInc.Di[Di]
TotPop.Di <- do.call(c, lapply(Smry_Md, function(x) x$TotPop.Dx))
names(TotPop.Di) <- unlist(lapply(Smry_Md, function(x) names(x$TotPop.Dx)))
TotPop.Di <- TotPop.Di[Di]
PerCapInc.Di <- TotalHhInc.Di / TotPop.Di 
rm(Smry_Md)
    
# Read in observed household and population data
#-----------------------------------------------
BaseHhPopIncFile <- paste0( ModelDir, "/base_hh_pop_inc.csv")
BaseHhPopInc.. <- read.csv( BaseHhPopIncFile, row.names=1 )[Di,]
ObsPerCapInc.Di <- BaseHhPopInc..$HhPerCapInc
names( ObsPerCapInc.Di ) <- Di
ObsTotPop.Di <- BaseHhPopInc..$TotHhPop
names( ObsTotPop.Di ) <- Di
rm( BaseHhPopIncFile )

#----------------------------------------------------------------------------------------
# PREPARE SCATTERPLOTS COMPARING OBSERVED AND MODELED POPULATIONS AND INCOMES BY DISTRICT
#----------------------------------------------------------------------------------------

# Save as pdf
pdf( file=paste( ModelDir, "/Documentation/compare_inc_pop.pdf", sep="" ), width=8.5, height=11 )
par( mfrow=c(2,1), oma=c(1,1,1,1) )
# Plot income comparison
Title <- paste( "Observed and Modeled District Per Capita Income\n(N = ", length(Di), ")", sep="" ) 
plot( ObsPerCapInc.Di, PerCapInc.Di, xlim=c(10000,50000), ylim=c(10000,50000),
  xlab="Observed", ylab="Modeled", main=Title )
abline( 0, 1, col="grey30" )
IncSeq. <- seq( 10000, 50000, 100 )
lines( IncSeq., 1.05 * IncSeq., lty=2, col="springgreen4" )
lines( IncSeq., 0.95 * IncSeq., lty=2, col="springgreen4" ) 
lines( IncSeq., 1.1 * IncSeq., lty=2, col="orange" )
lines( IncSeq., 0.9 * IncSeq., lty=2, col="orange" )
points( ObsPerCapInc.Di, PerCapInc.Di )
IncDiff. <- round( 100 * ( 1 - ObsPerCapInc.Di / PerCapInc.Di ), 0 ) # Calculate percentage difference
Dx <- Di[ which( abs( IncDiff. ) > 10 ) ] # Identify districts where difference is greater than 10%
#If difference of any are greater than 10%, highlight and identify them
if(length(Dx) != 0) {
  points( ObsPerCapInc.Di[Dx], PerCapInc.Di[Dx], pch=20, col="red" )
  text( ObsPerCapInc.Di[Dx] + 5000, PerCapInc.Di[Dx], labels=paste( Dx, " (", IncDiff.[Dx], "%)", sep="" ), pos=4 )
  segments( ObsPerCapInc.Di[Dx], PerCapInc.Di[Dx], ObsPerCapInc.Di[Dx] + 5000, PerCapInc.Di[Dx], col="red", lty=3 )
} 
legend( "topleft", col=c( "grey30", "springgreen4", "orange" ), lty=c(1,2,2),
        legend=c( "Ideal (modeled = observed)", 
                  paste( "Within 5% of ideal (n=", sum( abs( IncDiff. ) <= 5 ), ")", sep="" ), 
                  paste( "Within 10% of ideal (n=", sum( abs( IncDiff. ) <= 10 ), ")", sep="" ) ) )  
# Plot population comparixon
Title <- paste( "Observed and Modeled District Population\n(N = ", length(Di), ")", sep="" ) 
plot( ObsTotPop.Di, TotPop.Di, xlim=c(0,8200), ylim=c(0,8200),
  xlab="Observed", ylab="Modeled", main=Title )
abline( 0, 1, col="grey30" )
PopSeq. <- seq( 0, 8200, 10 )
lines( PopSeq., 1.05 * PopSeq., lty=2, col="springgreen4" )
lines( PopSeq., 0.95 * PopSeq., lty=2, col="springgreen4" ) 
lines( PopSeq., 1.1 * PopSeq., lty=2, col="orange" )
lines( PopSeq., 0.9 * PopSeq., lty=2, col="orange" )
points( ObsTotPop.Di, TotPop.Di )
PopDiff. <- round( 100 * ( 1 - ObsTotPop.Di / TotPop.Di ), 0 ) # Calculate percentage difference
Dx <- Di[ which( abs( PopDiff. ) > 10 ) ] # Identify districts where difference is greater than 10%
#If difference of any are greater than 10%, highlight and identify them
if(length(Dx) != 0) {
  points( ObsTotPop.Di[Dx], TotPop.Di[Dx], pch=20, col="red" )
  text( ObsTotPop.Di[Dx] + 1000, TotPop.Di[Dx], labels=paste( Dx, " (", PopDiff.[Dx], "%)", sep="" ), pos=4 )
  segments( ObsTotPop.Di[Dx], TotPop.Di[Dx], ObsTotPop.Di[Dx] + 1000, TotPop.Di[Dx], col="red", lty=3 ) 
}
legend( "topleft", col=c( "grey30", "springgreen4", "orange" ), lty=c(1,2,2),
        legend=c( "Ideal (modeled = observed)", 
                  paste( "Within 5% of ideal (n=", sum( abs( PopDiff. ) <= 5 ), ")", sep="" ), 
                  paste( "Within 10% of ideal (n=", sum( abs( PopDiff. ) <= 10 ), ")", sep="" ) ) )  
dev.off()

#----------------------------------------------------------------------------
# MAP THE RELATIVE DISTRICT WEIGHTS IF A DISTRICT SHAPEFILE HAS BEEN PROVIDED
#----------------------------------------------------------------------------

#Only do this if a shapefile is supplied
if( exists("Shapefile") ) { 
   if( all(!(is.na(Shapefile))) ) {

  #Initial checks
  #--------------
  #Check that shapefile specifications are proper
  if( !all(names(Shapefile) %in% c("Filename", "DistrictField", "DivisionField")) ) {
    stop( "Shapefile is improperly specified." )
  }
  #Check that the shapefile exists
  if( !file.exists(paste0(ModelDir, "/shapefiles/", Shapefile["Filename"], ".shp")) ) {
    stop( "Shapefile of the specified name does not exist. Remember leave off the .shp extension" )
  }   

  #Read in shapefile
  #------------------
  Districts_SP <- readOGR(paste0(ModelDir, "/shapefiles"), Shapefile["Filename"])
  DistrictsSP.. <- data.frame( Districts_SP )
  #Check that the division names correspond to what is in the model
  Mx <- DistrictsSP..[[Shapefile["DivisionField"]]]
  if( !all(Mx %in% Md) & !all(Md %in% Mx) ) {
    stop( "Division names in shapefile do not conform to the division names in the model" )
  } 
  Dx <- DistrictsSP..[[Shapefile["DistrictField"]]]
  if( !all(Dx %in% Di) & !all(Di %in% Dx) ) {
    stop( "District names in shapefile do not conform to the district names in the model" )
  } 

  # Read in saved income weights
  #-----------------------------
  IncWts.. <- read.csv( paste0(ModelDir, "/calibrated_income_weights.csv") )
  IncWts.Dx <- IncWts..$Weights[ IncWts..$DevType == "Metropolitan" ]
  names( IncWts.Dx ) <- IncWts..$District[ IncWts..$DevType == "Metropolitan" ]
  IncWts.Dx <- IncWts.Dx[Dx]
  #Split into list by division
  IncWts_Md.Dx <- split( IncWts.Dx, DistrictsSP..[[Shapefile["DivisionField"]]] )

  # Set up levels and colors for mapping
  #-------------------------------------
  # Identify income weight breaks using log of weights
  IncWtsBreaks_Md. <- lapply( IncWts_Md.Dx, function(x) {
    if( length(x) > 1 ) {
      LogWts. <- log(x)
      MaxAbs <- max( abs( LogWts. ) )
      Breaks. <- seq( -MaxAbs, MaxAbs, length=100 )
    } else {
      1
    } } )
  # Cut the values based on the breaks
  IncWtsCut_Md.Dx <- lapply( IncWts_Md.Dx, function(x) {
    if( length(x) > 1 ) {
      LogWts. <- log(x)
      MaxAbs <- max( abs( LogWts. ) )
      Breaks. <- seq( -MaxAbs, MaxAbs, length=100 )
      cut( LogWts., Breaks., include.lowest=TRUE, labels=FALSE )
    } else {
      1
    } } )
  # Identify colors corresponding to each set of breaks
  Colors_Md. <- lapply( IncWtsBreaks_Md., function(x) {
    if( length(x) == 1 ) {
      "white"
    } else {
      colorRampPalette( c("red", "yellow", "white", "turquoise", "blue"), space="rgb" )( length(x)-1 ) 
    } } )

  # Plot the maps
  #--------------
  #Each division is plotted separately
  pdf( file=paste( ModelDir, "/Documentation/inc_wts_by_district.pdf", sep="" ), width=11, height=8.5 )
  for( md in Md ) {
    par( mar=c(4,1,1,1), oma=c(1,1,3,1) )
    layout(matrix(c(1,2), nrow=2), heights=c(6,1))
    plot( Districts_SP )
    DistToPlot. <- DistrictsSP..[[Shapefile["DivisionField"]]] == md
    plot( Districts_SP[DistToPlot.,], col=Colors_Md.[[md]][ IncWtsCut_Md.Dx[[md]] ], add=TRUE )
    if( length(Colors_Md.[[md]]) > 1 ) {
      image.scale( range( IncWtsBreaks_Md.[[md]] ), col=Colors_Md.[[md]], 
                   breaks=IncWtsBreaks_Md.[[md]], ylab="",
                   horiz=TRUE, xlab="log(Weight)" )
    }
    mtext( paste(md, "Districts Calibrated Weights"), outer=TRUE, side=3, line=0, cex=1.2 )
  }
  dev.off()
#End if statement for map plotting
} }