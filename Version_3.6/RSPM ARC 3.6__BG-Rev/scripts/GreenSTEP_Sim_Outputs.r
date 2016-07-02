#GreenSTEP_Sim_Outputs.r
#=======================
#Regional Strategic Planning Model (RSPM) GreenSTEP Version
#Copyright 2009 - 2016, Oregon Department of Transportation
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
#http://www.apache.org/licenses/LICENSE-2.0
#Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
#Version: 3.6
#Date: 6/29/16
#Author: Brian Gregor, Oregon Systems Analytics LLC, gregor@or-analytics.com


#Revisions
#=========
#2/4/16: 
#Added tabulation of total parking cashout payments by district, income group, development type, and population type. The new array is named "HhCashout.DiIgDtPp" and is a component of the "Hh_" list.
#Added tabulation of the number of zero vehicle households. The new array is named NumZeroVehHh.DiIgDtPpHt and is a component of the "Hh_" list.
#6/29/16:
#Revised script to tabulate car services travel (e.g. car-sharing and shared autonomous vehicles), autonomous vehicles, and walk, bike and transit trips.

#Define functions for calculating water use and critical air pollutant emissions
#===============================================================================

#Function to calculate water use
calcWaterUse <- function( Den ) {
  # Data to estimate a regression model
  WaterUse.. <- data.frame( list( DuAc = c( 2, 3, 4, 5, 6, 7 ),
                                  DailyPersonUse = c( 215, 160, 130, 110, 105, 100 ) ) )
  WaterUse..$PopDen <- 2.5 * 640 * WaterUse..$DuAc
  # Double log plot of water use and density is approximately linear
  # Inflexion point at lot size of 5 units per acre
  WaterUse..$LogPopDen <- log( WaterUse..$PopDen )
  WaterUse..$LogUse <- log( WaterUse..$DailyPersonUse )
  WaterUse1_LM <- lm( LogUse[1:4] ~ LogPopDen[1:4], data=WaterUse.. )
  WaterUse2_LM <- lm( LogUse[4:6] ~ LogPopDen[4:6], data=WaterUse.. )
  Coeff1. <- coefficients( WaterUse1_LM )
  Coeff2. <- coefficients( WaterUse2_LM )
  # Compute the water useage
  if( Den == 0 ) Den <- NA
  if( is.na( Den ) ){
    result <- NA
  } else {
    if( Den < 8000 ) {
      result <- exp( Coeff1.[1] + Coeff1.[2] * log( Den ) )
    } else {
      result <- exp( Coeff2.[1] + Coeff2.[2] * log( Den ) )
    }
  }
  names( result ) <- NULL
  result
}

# Define function to calculate criteria air pollution
calcEmissions <- function( HcDvmt, Year ) {
  Po <- c( "Hydrocarbons", "CarbonMonoxide", "NitrogenOxides", "PM25" )
  PolluteRates.PoYr <- cbind( c( 1.1351, 13.1728, 1.6118, 0.0299 ), c( 0.3423, 4.9117, 0.3785, 0.0133 ),
                              c( 0.1989, 3.7269, 0.1843, 0.0107 ), c( 0.1755, 3.4720, 0.1715, 0.0103 ) )
  rownames( PolluteRates.PoYr ) <- Po
  colnames( PolluteRates.PoYr ) <- c( "2005", "2020", "2035", "2050" )
  Year <- as.numeric( Year )
  Years. <- as.numeric( colnames( PolluteRates.PoYr ) )
  if( Year %in% Years. ) {                                    
    PolluteRates.Po <- PolluteRates.PoYr[ , which( Year == Years. ) ]
    Pollution.Po <- HcDvmt * PolluteRates.Po
  } else {
    YearBracket. <- Years.[ which( rank( abs( Years. - Year ) ) <= 2 ) ]
    Weights. <- rev( abs( YearBracket. - Year ) ) / diff( YearBracket. )
    PolluteRates.Po <- rowSums( sweep( PolluteRates.PoYr[ , as.character( YearBracket. ) ], 
                                       2, Weights., "*" ) )
    Pollution.Po <- HcDvmt * PolluteRates.Po
  }
  Pollution.Po
}

#Define function to tabulate synthetic population data
tabulate <- function(TabField, ByFields_, Dims_, Function) {
    if(length(Dims_) > 5) {
        stop("Function does not support tabulating more than 5 dimensions.")
    }
    if(length(ByFields_) != length(Dims_)) {
        stop("Number of ByFields_ must equal number of Dims_")
    }
    if(!(Function %in% c("Sum", "Mean", "Count"))) {
        stop("Function must be Sum, Mean, or Count")
    }
    # Make array of all dimensions
    Result. <- array(0, dim=sapply(Dims_, length), dimnames=Dims_)
    # Tabulate
    if(Function == "Mean") {
        Tab. <- tapply(TabField, ByFields_, function(x) mean(x, na.rm=TRUE))
    }
    if(Function == "Sum") {
        Tab. <- tapply(TabField, ByFields_, function(x) sum(x, na.rm=TRUE))
    }
    if(Function == "Count") {
        Tab. <- tapply(TabField, ByFields_, length)
    }
    Tab.[is.na(Tab.)] <- 0
    # Put results of tabulation in array
    if(length(dim(Tab.)) == 2) {
        Result.[dimnames(Tab.)[[1]], dimnames(Tab.)[[2]]] <- Tab.
    }
    if(length(dim(Tab.)) == 3) {
        Result.[dimnames(Tab.)[[1]], dimnames(Tab.)[[2]],
                dimnames(Tab.)[[3]]] <- Tab.
    }
    if(length(dim(Tab.)) == 4) {
        Result.[dimnames(Tab.)[[1]], dimnames(Tab.)[[2]],
                dimnames(Tab.)[[3]], dimnames(Tab.)[[4]]] <- Tab.
    }
    if(length(dim(Tab.)) == 5) {
        Result.[dimnames(Tab.)[[1]], dimnames(Tab.)[[2]],
                dimnames(Tab.)[[3]], dimnames(Tab.)[[4]],
                dimnames(Tab.)[[5]]] <- Tab.
    }
    # Return the result
    Result.
}    

#Iterate through all years
#=========================

for( yr in RunYears ) {
  
  # Print year and start time
  print( paste(yr, " outputs running") )
  print( Sys.time() )
  # Identify the directory where outputs are stored
  OutYearDir <- paste( OutputDir, "/Year", yr, sep="" )     
  
  #Make objects to store results
  #=============================
  
  # Define urban mixed use designation ( Yes:"1", No: "0" )
  Mx <- c( "0", "1" )
  
  # Define light vehicle types
  Vt <- c( "Auto", "LtTruck" )
  
  # Define age group
  Ag <- as.factor( as.character(0:32) )
  
  # Define powertrain types
  Pt <- c( "Ice", "Hev", "Phev", "Ev" )
  
  # Define the population types (household, group quarters)
  Pp <- c( "Household", "GroupQtr" )
    
  # Define the pollution types
  Po <- c( "Hydrocarbons", "CarbonMonoxide", "NitrogenOxides", "PM25" )
    
  # Define housing types
  Ht <- c( "A24", "SFD", "MH", "A5P", "GQ", "SFA" )
		
  # Set up dimensions for arrays by district, income group, development type, and population type
  OutputDims. <- c( length( Di ), length( Ig ), length( Dt ), length( Pp ) )
  OutputDimnames_ <- list( Di, Ig, Dt, Pp ) 
  
  # Initialize arrays for storing household data cross-tabulations
  #---------------------------------------------------------------
  Hh_ <- list()
  Hh_$AveDensity.DiDt <- array( 0, dim=c( length(Di), length(Dt)), dimnames=list(Di,Dt) )
  Hh_$Acres.DiDt <- array( 0, dim=c( length(Di), length(Dt)), dimnames=list(Di,Dt) )
  Hh_$AveVehAge.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$DrvAgePop.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$Dvmt.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$ElecCo2e.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )          
  Hh_$ElecKwh.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$EvDvmt.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$EvDvmt.DiVtPt <- array( 0, dim=c( length(Di), length(Vt), length(Pt) ), 
                              dimnames=list( Di, Vt, Pt ) )
  Hh_$Fuel.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$Fuel.DiVtPt <- array( 0, dim=c( length(Di), length(Vt), length(Pt) ), dimnames=list( Di, Vt, Pt ) )
  Hh_$FuelCo2e.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HcDvmt.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HcDvmt.DiVtPt <- array( 0, dim=c( length(Di), length(Vt), length(Pt) ), dimnames=list( Di, Vt, Pt ) )
  Hh_$Hh.DiIgDtPpHt <- array( 0, dim=c(OutputDims.,length(Ht)), dimnames=c(OutputDimnames_, list(Ht)) )
  Hh_$NumZeroVehHh.DiIgDtPpHt <- array( 0, dim=c(OutputDims.,length(Ht)), dimnames=c(OutputDimnames_, list(Ht)) )
  Hh_$HhCo2e.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhInc.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhCashout.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhExtCost.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhOpCost.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhParkingCost.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhVehOwnCost.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$LtWtVehDvmt.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$LtWtVehDvmt.DiDtPpMx <- array( 0, dim=c( length(Di), length(Dt), length(Pp), length(Mx)),
                                     dimnames=list(Di,Dt,Pp,Mx) )          
  Hh_$NumAuto.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$NumLtTruck.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$NumPowertrain.DiDtPpPt <- array( 0, dim=c( length(Di), length(Dt), length(Pp), length(Pt) ),
                                       dimnames=list( Di, Dt, Pp, Pt ) ) 
  Hh_$NumPowertrain.DiVtPt <- array( 0, dim=c( length(Di), length(Vt), length(Pt) ), dimnames=list( Di, Vt, Pt ) )
  Hh_$Pop.DiDt <- array( 0, dim=c( length(Di), length(Dt)), dimnames=list(Di,Dt) )
  Hh_$Pop.DiIgDtPpHt <- array( 0, dim=c(OutputDims.,length(Ht)), dimnames=c(OutputDimnames_, list(Ht)) )
  Hh_$Pop.DiDtPpMx <- array( 0, dim=c( length(Di), length(Dt), length(Pp), length(Mx)), 
                             dimnames=list(Di,Dt,Pp,Mx) )          
  Hh_$VehAge.DiAg <- array( 0, dim=c( length(Di), length(Ag) ), dimnames=list( Di, Ag ) )
  Hh_$WalkTrips.DiDtPpMx <- array( 0, dim=c( length(Di), length(Dt), length(Pp), length(Mx)), 
                                   dimnames=list(Di,Dt,Pp,Mx) )
  Hh_$BikeTrips.DiDtPpMx <- array( 0, dim=c( length(Di), length(Dt), length(Pp), length(Mx)), 
                                   dimnames=list(Di,Dt,Pp,Mx) )
  Hh_$TransitTrips.DiDtPpMx <- array( 0, dim=c( length(Di), length(Dt), length(Pp), length(Mx)), 
                                      dimnames=list(Di,Dt,Pp,Mx) )
  Hh_$WaterUse.DiDt <- array( 0, dim=c( length(Di), length(Dt)), dimnames=list(Di,Dt) )
  Hh_$CritAirPol.DiDtPo <- array( 0, dim=c( length(Di), length(Dt), length(Po)), 
                                  dimnames=list(Di,Dt,Po) )
  Hh_$AuDvmt.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$NAuDvmt.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhVehDvmt.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ ) 
  Hh_$CarSvcDvmt.DiIgDtPp <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ ) 
  
  #Save metropolitan area summary data tables
  #==========================================
  
  Metropolitan_ <- list()
  # Data that was summarized for all years
  TableNames. <- 
    c( "ArtLnMiCap.Yr", "BusRevMi.Yr", "FwyLnMiCap.Yr", "Inc.DtYr", 
       "Pop.DiDtYr", "Pop.DtYr", "RailRevMi.Yr", "TranRevMiCap.Yr" )
  for( tn in TableNames. ) {
    TableFile <- paste( OutputDir, "/", tn, ".RData", sep="" )
    Metropolitan_[[tn]] <- assignLoad( TableFile )
    rm( TableFile )
  }
  rm( TableNames. ) 
  # Data that was summarized for the year
  TableNames. <- 
    c( "BusHcCo2e", "BusFuel.Ft", "CostSummary.MdVa", "Dvmt.MdDt", 
       "RailBusEvCo2e", "RailBusPower", "RoadCostSummary.", "TruckCo2e", 
       "TruckFuel.Ft", "VmtSurcharge.It", "AuDvmt.MdDt" )
  for( tn in TableNames. ) {
    TableFile <- paste( OutYearDir, "/", tn, ".RData", sep="" )
    Metropolitan_[[tn]] <- assignLoad( TableFile )
  }
  rm( TableNames. )
  # Congestion results
  load( paste0(OutYearDir, "/CongResults_.RData") )
  for( name in names(CongResults_) ) {
    Metropolitan_[[name]] <- CongResults_[[name]]
  }
  rm( CongResults_ )
  
  #Iterate through metropolitan divisions and make summary tables for household data
  #=================================================================================
  
  for( md in Md ) { local( {
    
    # Load county files
    Filename <- paste( OutYearDir, "/", md, ".RData", sep="" )
    SynPop.. <- assignLoad( Filename )
    SynPop..$DevType <- factor( SynPop..$DevType, levels=c("Metropolitan", "Town", "Rural") )
    SynPop..$Urban <- factor( as.character( SynPop..$Urban ), levels=c( "0", "1" ) )
    
    # Create vector of population types and identify if any group quarters population
    PopType. <- rep( "Household", nrow( SynPop.. ) )
    PopType.[ SynPop..$HouseType == "GQ" ] <- "GroupQtr"
    HasGQ <- any(PopType. == "GroupQtr")

    # Identify districts in the division
    Dx <- unique( SynPop..$District )
    
    # Identify households having vehicles
    HasVeh.Hh <- SynPop..$Hhvehcnt >= 1   

    # Calculate average densities
    #============================

    # Compute the average density by district and development type 
    Density.DxDt <- tabulate(SynPop..$Htppopdn, 
                             list( SynPop..$District, SynPop..$DevType ), 
                             list(Dx, Dt),
                             "Mean")
    Hh_$AveDensity.DiDt[ Dx, ] <<- Density.DxDt
    #Hh_$AveDensity.DiDt[ Dx, ] <- Density.DxDt
      
    # Compute the population by district and development type
    Pop.DxDt <- tabulate(SynPop..$Hhsize, 
                         list( SynPop..$District, SynPop..$DevType ), 
                         list(Dx, Dt),
                         "Sum" )
    Pop.DxDt[ is.na( Pop.DxDt ) ] <- 0
      
    # Compute the area in acres by district and development type
    Acres.DxDt <-  640 * Pop.DxDt / Density.DxDt
    Acres.DxDt[ is.na( Acres.DxDt ) ] <- 0
    Hh_$Acres.DiDt[ Dx, ] <<- Acres.DxDt
    #Hh_$Acres.DiDt[ Dx, ] <- Acres.DxDt
      
    # Calculate water consumption by district and development type
    WaterUse.DxDt <- apply( Density.DxDt, c(1,2), function(x) {
                            calcWaterUse( x ) } )
    Hh_$WaterUse.DiDt[Dx,] <<- WaterUse.DxDt
    #Hh_$WaterUse.DiDt[Dx,] <- WaterUse.DxDt
    rm( WaterUse.DxDt )

    # Tabulate household characteristics
    #===================================
    
    # Household population
    Pop.5d <- tabulate(SynPop..$Hhsize, 
                       list(SynPop..$District, SynPop..$IncGrp, 
                            SynPop..$DevType, PopType., SynPop..$HouseType),
                       list(Dx,Ig,Dt,Pp,Ht),
                       "Sum")
    Hh_$Pop.DiIgDtPpHt[ Dx, , , , ] <<- Pop.5d
    #Hh_$Pop.DiIgDtPpHt[ Dx, , , , ] <- Pop.5d

    # Population by district and development type
    Pop.2d <- tabulate(SynPop..$Hhsize, 
                       list( SynPop..$District, SynPop..$DevType ), 
                       list(Dx, Dt),
                       "Sum")
    Hh_$Pop.DiDt[ Dx, ] <<- Pop.2d
    #Hh_$Pop.DiDt[ Dx, ] <- Pop.2d
    rm( Pop.2d )
      
    # Household population by urban mixed use designation
    Pop.4d <- tabulate(SynPop..$Hhsize, 
                       list(SynPop..$District, SynPop..$DevType, 
                            PopType., SynPop..$Urban ),
                       list(Dx,Dt,Pp,Mx),
                       "Sum")
    Hh_$Pop.DiDtPpMx[Dx, , , ] <<- Pop.4d
    #Hh_$Pop.DiDtPpMx[Dx, , , ] <- Pop.4d
    rm( Pop.4d )

    # Number of households
    Tab.5d <- tabulate(SynPop..$Houseid,
                       list(SynPop..$District, SynPop..$IncGrp, 
                            SynPop..$DevType, PopType., SynPop..$HouseType ),
                       list(Dx,Ig,Dt,Pp,Ht),
                       "Count")
    Hh_$Hh.DiIgDtPpHt[Dx, , , , ] <<- Tab.5d
    #Hh_$Hh.DiIgDtPpHt[Dx, , , , ] <- Tab.5d
    rm( Tab.5d )

    # Number of zero-vehicle households
    IsZeroVeh <- SynPop..$Hhvehcnt == 0
    Tab.5d <- tabulate(SynPop..$Houseid[IsZeroVeh],
                       list(SynPop..$District[IsZeroVeh], 
                            SynPop..$IncGrp[IsZeroVeh],
                            SynPop..$DevType[IsZeroVeh],
                            PopType.[IsZeroVeh],
                            SynPop..$HouseType[IsZeroVeh]),
                       list(Dx,Ig,Dt,Pp,Ht),
                       "Count")
    Hh_$NumZeroVehHh.DiIgDtPpHt[Dx, , , , ] <<- Tab.5d
    #Hh_$NumZeroVehHh.DiIgDtPpHt[Dx, , , , ] <- Tab.5d
    rm( Tab.5d, IsZeroVeh )
    
    # Driver age population
    VehHhDrvPop.4d <- tabulate(SynPop..$DrvAgePop,
                               list(SynPop..$District, SynPop..$IncGrp, 
                                    SynPop..$DevType, PopType.),
                               list(Dx, Ig, Dt, Pp),
                               "Sum") 
    Hh_$DrvAgePop.DiIgDtPp[Dx, , , ] <<- VehHhDrvPop.4d
    #Hh_$DrvAgePop.DiIgDtPp[Dx, , , ] <- VehHhDrvPop.4d
    rm( VehHhDrvPop.4d )

    # All household and car service DVMT
    Dvmt.4d <- 
      tabulate(SynPop..$Dvmt + SynPop..$CarSvcDvmt * (1 + SynPop..$CarSvcRepoProp), 
               list(SynPop..$District, SynPop..$IncGrp, 
                    SynPop..$DevType, PopType.),
               list(Dx, Ig, Dt, Pp),
               "Sum") 
    Hh_$Dvmt.DiIgDtPp[Dx, , , ] <<- Dvmt.4d[Dx, Ig, Dt, Pp]
    #Hh_$Dvmt.DiIgDtPp[Dx, , , ] <- Dvmt.4d[Dx, Ig, Dt, Pp]
    rm( Dvmt.4d )

    # DVMT in household vehicles
    Dvmt.4d <- 
      tabulate(SynPop..$Dvmt, 
               list(SynPop..$District, SynPop..$IncGrp, 
                    SynPop..$DevType, PopType.),
               list(Dx, Ig, Dt, Pp),
               "Sum")
    Hh_$HhVehDvmt.DiIgDtPp[Dx, , , ] <<- Dvmt.4d[Dx, Ig, Dt, Pp]
    #Hh_$HhVehDvmt.DiIgDtPp[Dx, , , ] <- Dvmt.4d[Dx, Ig, Dt, Pp]
    rm( Dvmt.4d )

    # DVMT from use of car services (includes the repositioning DVMT)
    Dvmt.4d <- 
      tabulate(SynPop..$CarSvcDvmt * (1 + SynPop..$CarSvcRepoProp), 
               list(SynPop..$District, SynPop..$IncGrp, 
                    SynPop..$DevType, PopType.),
               list(Dx, Ig, Dt, Pp),
               "Sum")
    Hh_$CarSvcDvmt.DiIgDtPp[Dx, , , ] <<- Dvmt.4d[Dx, Ig, Dt, Pp]
    #Hh_$CarSvcDvmt.DiIgDtPp[Dx, , , ] <- Dvmt.4d[Dx, Ig, Dt, Pp]
    rm( Dvmt.4d )
    
    # DVMT in autonomous vehicles
    Dvmt.4d <- 
      tabulate(SynPop..$AuDvmt + SynPop..$CarSvcDvmt * (1 + SynPop..$CarSvcRepoProp) * !is.na(CarSvcCostParm..["AVTechCost",yr]), 
               list(SynPop..$District, SynPop..$IncGrp, 
                    SynPop..$DevType, PopType.),
               list(Dx, Ig, Dt, Pp),
               "Sum")
    Hh_$AuDvmt.DiIgDtPp[Dx, , , ] <<- Dvmt.4d[Dx, Ig, Dt, Pp]
    #Hh_$AuDvmt.DiIgDtPp[Dx, , , ] <- Dvmt.4d[Dx, Ig, Dt, Pp]
    rm( Dvmt.4d )

    # DVMT in non-autonomous vehicles
    Dvmt.4d <- 
      tabulate(SynPop..$NAuDvmt + SynPop..$CarSvcDvmt * (1 + SynPop..$CarSvcRepoProp) * is.na(CarSvcCostParm..["AVTechCost",yr]), 
               list(SynPop..$District, SynPop..$IncGrp, 
                    SynPop..$DevType, PopType.),
               list(Dx, Ig, Dt, Pp),
               "Sum")
    Hh_$NAuDvmt.DiIgDtPp[Dx, , , ] <<- Dvmt.4d[Dx, Ig, Dt, Pp]
    #Hh_$NAuDvmt.DiIgDtPp[Dx, , , ] <- Dvmt.4d[Dx, Ig, Dt, Pp]
    rm( Dvmt.4d )

    # Light Vehicle (e.g. bicycle) Dvmt 
    LtWtVehDvmt.4d <- tabulate(SynPop..$LtVehDvmt, 
                               list(SynPop..$District, SynPop..$IncGrp, 
                                    SynPop..$DevType, PopType.),
                               list(Dx, Ig, Dt, Pp),
                               "Sum") 
    Hh_$LtWtVehDvmt.DiIgDtPp[Dx, , , ] <<- LtWtVehDvmt.4d
    #Hh_$LtWtVehDvmt.DiIgDtPp[Dx, , , ] <- LtWtVehDvmt.4d
    rm(LtWtVehDvmt.4d) 

    # Household light weight vehicle (e.g. bicycle) DVMT
    LtWtVehDvmt.4d <- tabulate(SynPop..$LtVehDvmt, 
                               list(SynPop..$District, SynPop..$DevType, 
                                    PopType., SynPop..$Urban),
                               list(Dx, Dt, Pp, Mx),
                               "Sum")
    Hh_$LtWtVehDvmt.DiDtPpMx[Dx, , , ] <<- LtWtVehDvmt.4d
    #Hh_$LtWtVehDvmt.DiDtPpMx[Dx, , , ] <- LtWtVehDvmt.4d
    rm(LtWtVehDvmt.4d)

    # Household operating cost                                                                     
    TotCost.4d <- tabulate(SynPop..$HhTotCost,
                           list(SynPop..$District, SynPop..$IncGrp, 
                                SynPop..$DevType, PopType.),
                           list(Dx, Ig, Dt, Pp),
                           "Sum")
    TotCost.4d <- adjMoneyVal(TotCost.4d, BaseCostYear, BaseYear, Cpi.Yr)
    Hh_$HhOpCost.DiIgDtPp[Dx, , , ] <<- TotCost.4d
    #Hh_$HhOpCost.DiIgDtPp[Dx, , , ] <- TotCost.4d
    rm(TotCost.4d)

    # Household external cost                                                                     
    TotExtCost.4d <- tabulate(SynPop..$TotExtCost, 
                              list(SynPop..$District, SynPop..$IncGrp, 
                                   SynPop..$DevType, PopType.), 
                              list(Dx, Ig, Dt, Pp),
                              "Sum")
    TotExtCost.4d <- adjMoneyVal(TotExtCost.4d, BaseCostYear, BaseYear, Cpi.Yr)
    Hh_$HhExtCost.DiIgDtPp[Dx, , , ] <<- TotExtCost.4d
    #Hh_$HhExtCost.DiIgDtPp[Dx, , , ] <- TotExtCost.4d
    rm(TotExtCost.4d)

    # Household vehicle ownership cost                                                                     
    VehOwnExp.4d <- tabulate(SynPop..$VehOwnExp, 
                            list(SynPop..$District, SynPop..$IncGrp, 
                                 SynPop..$DevType, PopType.),
                            list(Dx, Ig, Dt, Pp),
                            "Sum")
    VehOwnExp.4d <- adjMoneyVal(VehOwnExp.4d, BaseCostYear, BaseYear, Cpi.Yr)
    Hh_$HhVehOwnCost.DiIgDtPp[Dx, , , ] <<- VehOwnExp.4d
    #Hh_$HhVehOwnCost.DiIgDtPp[Dx, , , ] <- VehOwnExp.4d
    rm( VehOwnExp.4d )

    # Household parking cost
    ParkingCost.4d <- tabulate(SynPop..$DailyPkgCost, 
                               list(SynPop..$District, SynPop..$IncGrp, 
                                    SynPop..$DevType, PopType.),
                               list(Dx, Ig, Dt, Pp),
                               "Sum")
    ParkingCost.4d <- adjMoneyVal(ParkingCost.4d, BaseCostYear, BaseYear, Cpi.Yr)
    Hh_$HhParkingCost.DiIgDtPp[Dx, , , ] <<- ParkingCost.4d
    #Hh_$HhParkingCost.DiIgDtPp[Dx, , , ] <- ParkingCost.4d
    rm( ParkingCost.4d )

    # Household income
    HhInc.4d <- tabulate(SynPop..$Hhincttl, 
                         list(SynPop..$District, SynPop..$IncGrp, 
                              SynPop..$DevType, PopType.),
                         list(Dx, Ig, Dt, Pp),
                         "Sum") 
    HhInc.4d <- adjMoneyVal(HhInc.4d, BaseCostYear, BaseYear, Cpi.Yr)
    Hh_$HhInc.DiIgDtPp[Dx, , , ] <<- HhInc.4d
    #Hh_$HhInc.DiIgDtPp[Dx, , , ] <- HhInc.4d
    rm(HhInc.4d)
                                                                                
    # Household parking cashout income adjustment
    HhCashout.4d <- tabulate(SynPop..$CashOutIncAdj, 
                             list(SynPop..$District, SynPop..$IncGrp, 
                                  SynPop..$DevType, PopType.),
                             list(Dx, Ig, Dt, Pp),
                             "Sum") 
    HhCashout.4d <- adjMoneyVal(HhCashout.4d, BaseCostYear, BaseYear, Cpi.Yr)
    Hh_$HhCashout.DiIgDtPp[Dx, , , ] <<- HhCashout.4d
    #Hh_$HhCashout.DiIgDtPp[Dx, , , ] <- HhCashout.4d
    rm(HhCashout.4d)
    
    # Household CO2e
    Co2e.Hh <- 
      with(SynPop.., FuelCo2e + ElecCo2e + CarSvcFuelCo2e + CarSvcElecCo2e)   
    Co2e.4d <- tabulate(Co2e.Hh, 
                        list(SynPop..$District, SynPop..$IncGrp, 
                             SynPop..$DevType, PopType.),
                        list(Dx, Ig, Dt, Pp),
                        "Sum") 
    Hh_$HhCo2e.DiIgDtPp[Dx, , , ] <<- Co2e.4d
    #Hh_$HhCo2e.DiIgDtPp[Dx, , , ] <- Co2e.4d
    rm(Co2e.Hh, Co2e.4d)

    # Household walk trips
    AveWalkTrips.4d <- tabulate(SynPop..$AveWalkTrips, 
                                list(SynPop..$District, SynPop..$DevType, 
                                     PopType., SynPop..$Urban),
                                list(Dx, Dt, Pp, Mx),
                                "Sum")
    Hh_$WalkTrips.DiDtPpMx[Dx, , , ] <<- AveWalkTrips.4d
    #Hh_$WalkTrips.DiDtPpMx[Dx, , , ] <- AveWalkTrips.4d
    rm(AveWalkTrips.4d)

    # Household bike trips
    AveBikeTrips.4d <- tabulate(SynPop..$AveBikeTrips, 
                                list(SynPop..$District, SynPop..$DevType, 
                                     PopType., SynPop..$Urban),
                                list(Dx, Dt, Pp, Mx),
                                "Sum")
    Hh_$BikeTrips.DiDtPpMx[Dx, , , ] <<- AveBikeTrips.4d
    #Hh_$BikeTrips.DiDtPpMx[Dx, , , ] <- AveBikeTrips.4d
    rm(AveBikeTrips.4d)
    
    # Household transit trips
    AveTransitTrips.4d <- tabulate(SynPop..$AveTransitTrips, 
                                list(SynPop..$District, SynPop..$DevType, 
                                     PopType., SynPop..$Urban),
                                list(Dx, Dt, Pp, Mx),
                                "Sum")
    Hh_$TransitTrips.DiDtPpMx[Dx, , , ] <<- AveTransitTrips.4d
    #Hh_$TransitTrips.DiDtPpMx[Dx, , , ] <- AveTransitTrips.4d
    rm(AveTransitTrips.4d)
    
    # Fuel consumed by household
    Fuel.4d <- 
      tabulate(SynPop..$FuelGallons + SynPop..$CarSvcFuelGal, 
               list(SynPop..$District, SynPop..$IncGrp, 
                    SynPop..$DevType, PopType.),
               list(Dx, Ig, Dt, Pp),
               "Sum") 
    Hh_$Fuel.DiIgDtPp[Dx, , , ] <<- Fuel.4d
    #Hh_$Fuel.DiIgDtPp[Dx, , , ] <- Fuel.4d
    rm( Fuel.4d )

    # Electricity consumed by household
    ElecKwh.4d <- 
      tabulate(SynPop..$ElecKwh + SynPop..$CarSvcElecKwh, 
               list(SynPop..$District, SynPop..$IncGrp, 
                    SynPop..$DevType, PopType.),
               list(Dx, Ig, Dt, Pp),
               "Sum") 
    Hh_$ElecKwh.DiIgDtPp[Dx, , , ] <<- ElecKwh.4d
    #Hh_$ElecKwh.DiIgDtPp[Dx, , , ] <- ElecKwh.4d
    rm( ElecKwh.4d )      

    # Greenhouse gas emissions from fuel consumption
    FuelCo2e.4d <- 
      tabulate(SynPop..$FuelCo2e + SynPop..$CarSvcFuelCo2e, 
               list(SynPop..$District, SynPop..$IncGrp, 
                    SynPop..$DevType, PopType.),
               list(Dx, Ig, Dt, Pp),
               "Sum") 
    Hh_$FuelCo2e.DiIgDtPp[Dx, , , ] <<- FuelCo2e.4d
    #Hh_$FuelCo2e.DiIgDtPp[Dx, , , ] <- FuelCo2e.4d
    rm(FuelCo2e.4d)

    # Greenhouse gas emissions from electricity consumption
    ElecCo2e.4d <- 
      tabulate(SynPop..$ElecCo2e + SynPop..$CarSvcElecCo2e, 
               list(SynPop..$District, SynPop..$IncGrp, 
                    SynPop..$DevType, PopType.),
               list(Dx, Ig, Dt, Pp),
               "Sum") 
    Hh_$ElecCo2e.DiIgDtPp[Dx, , , ] <<- ElecCo2e.4d
    #Hh_$ElecCo2e.DiIgDtPp[Dx, , , ] <- ElecCo2e.4d
    rm( ElecCo2e.4d )

    # Tabulate EV DVMT
    HhEvDvmtTab.4d <- 
      tabulate(unlist(SynPop..$EvVehDvmt[HasVeh.Hh]),
               list(rep(SynPop..$District, SynPop..$Hhvehcnt), 
                    rep(SynPop..$IncGrp, SynPop..$Hhvehcnt), 
                    rep(SynPop..$DevType, SynPop..$Hhvehcnt),
                    rep(PopType., SynPop..$Hhvehcnt)),
               list(Dx, Ig, Dt, Pp),
               "Sum")
    CarSvcEvDvmtTab.4d <-
      tabulate(SynPop..$CarSvcDvmt * CarSvcCostParm..["PropEV", yr],
               list(SynPop..$District, SynPop..$IncGrp,
                    SynPop..$DevType, PopType.),
               list(Dx, Ig, Dt, Pp),
               "Sum")
    Tab.4d <- HhEvDvmtTab.4d + CarSvcEvDvmtTab.4d
    Hh_$EvDvmt.DiIgDtPp[Dx, , , ] <<- Tab.4d
    #Hh_$EvDvmt.DiIgDtPp[Dx, , , ] <- Tab.4d
    rm( HhEvDvmtTab.4d, CarSvcEvDvmtTab.4d, Tab.4d )
    
    # Tabulate HC-vehicle DVMT
    HhHcDvmtTab.4d <- 
      tabulate(unlist(SynPop..$HcVehDvmt[HasVeh.Hh]),
               list(rep(SynPop..$District, SynPop..$Hhvehcnt), 
                    rep(SynPop..$IncGrp, SynPop..$Hhvehcnt), 
                    rep(SynPop..$DevType, SynPop..$Hhvehcnt),
                    rep(PopType., SynPop..$Hhvehcnt)),
               list(Dx, Ig, Dt, Pp),
               "Sum")
    CarSvcHcDvmtTab.4d <- 
      tabulate(SynPop..$CarSvcDvmt * (1 - CarSvcCostParm..["PropEV", yr]),
               list(SynPop..$District, SynPop..$IncGrp,
                    SynPop..$DevType, PopType.),
               list(Dx, Ig, Dt, Pp),
               "Sum")
    Tab.4d <- HhHcDvmtTab.4d + CarSvcHcDvmtTab.4d  
    Hh_$HcDvmt.DiIgDtPp[Dx, , , ] <<- Tab.4d
    #Hh_$HcDvmt.DiIgDtPp[Dx, , , ] <- Tab.4d
    rm( HhHcDvmtTab.4d, CarSvcHcDvmtTab.4d, Tab.4d )
    
    # Calculate criteria air pollution
    HhHcDvmt.DxDt <- 
      tabulate(unlist(SynPop..$HcVehDvmt[HasVeh.Hh]),
               list(rep(SynPop..$District, SynPop..$Hhvehcnt), 
                    rep(SynPop..$DevType, SynPop..$Hhvehcnt)),
               list(Dx, Dt),
               "Sum")
    CarSvcHcDvmt.DxDt <- 
      tabulate(SynPop..$CarSvcDvmt * (1 - CarSvcCostParm..["PropEV", yr]),
               list(SynPop..$District, SynPop..$DevType),
               list(Dx, Dt),
               "Sum")
    HcDvmt.DxDt <- HhHcDvmt.DxDt + CarSvcHcDvmt.DxDt
    CritAirPol.PoDxDt <- apply(HcDvmt.DxDt, c(1,2), function(x) {
      calcEmissions( x, yr )})
    CritAirPol.DxDtPo <- aperm(CritAirPol.PoDxDt, c(2,3,1))
    Hh_$CritAirPol.DiDtPo[Dx, , ] <<- CritAirPol.DxDtPo
    #Hh_$CritAirPol.DiDtPo[Dx, , ] <- CritAirPol.DxDtPo
    rm(HhHcDvmt.DxDt, CarSvcHcDvmt.DxDt, CritAirPol.PoDxDt, CritAirPol.DxDtPo, 
       HcDvmt.DxDt)
    
    
    # Tabulate household vehicle characteristics
    #===========================================
    
    # Tabulate average vehicle age
    Tab.4d <- tabulate(unlist(SynPop..$VehAge[ HasVeh.Hh ]),
                       list(rep(SynPop..$District, SynPop..$Hhvehcnt), 
                            rep(SynPop..$IncGrp, SynPop..$Hhvehcnt), 
                            rep(SynPop..$DevType, SynPop..$Hhvehcnt),
                            rep(PopType., SynPop..$Hhvehcnt)),
                       list(Dx, Ig, Dt, Pp),
                       "Mean")
    Hh_$AveVehAge.DiIgDtPp[Dx, , , ] <<- Tab.4d
    #Hh_$AveVehAge.DiIgDtPp[Dx, , , ] <- Tab.4d
    rm(Tab.4d)

    # Tabulate number of vehicles by type
    Tab.5d <- tabulate(rep(SynPop..$Houseid, SynPop..$Hhvehcnt),
                       list(rep(SynPop..$District, SynPop..$Hhvehcnt),
                            rep(SynPop..$IncGrp, SynPop..$Hhvehcnt),
                            rep(SynPop..$DevType, SynPop..$Hhvehcnt),
                            rep(PopType., SynPop..$Hhvehcnt),
                            unlist(SynPop..$VehType[ HasVeh.Hh])),
                       list(Dx, Ig, Dt, Pp, Vt),
                       "Count")
    Hh_$NumAuto.DiIgDtPp[Dx, , , ] <<- Tab.5d[, , , , "Auto"]
    Hh_$NumLtTruck.DiIgDtPp[Dx, , , ] <<- Tab.5d[, , , , "LtTruck"]
    #Hh_$NumAuto.DiIgDtPp[Dx, , , ] <- Tab.5d[, , , , "Auto"]
    #Hh_$NumLtTruck.DiIgDtPp[Dx, , , ] <- Tab.5d[, , , , "LtTruck"]
    rm(Tab.5d)
 
    # Tabulate vehicle age distributions
    Tab.2d <- tabulate(rep(SynPop..$Houseid, SynPop..$Hhvehcnt),
                       list(rep(SynPop..$District, SynPop..$Hhvehcnt), 
                            unlist(SynPop..$VehAge[HasVeh.Hh])),
                       list(Dx, Ag),
                       "Count")
    Hh_$VehAge.DiAg[Dx, ] <<- Tab.2d          
    #Hh_$VehAge.DiAg[Dx, ] <- Tab.2d          
    rm( Tab.2d )               

    # Tabulate number of vehicles by powertrain
    Tab.4d <- tabulate(rep(SynPop..$Houseid, SynPop..$Hhvehcnt),
                       list(rep(SynPop..$District, SynPop..$Hhvehcnt),
                            rep(SynPop..$DevType, SynPop..$Hhvehcnt), 
                            rep(PopType., SynPop..$Hhvehcnt),
                            unlist(SynPop..$Powertrain[ HasVeh.Hh])),
                       list(Dx, Dt, Pp, Pt),
                       "Count")
    Hh_$NumPowertrain.DiDtPpPt[Dx, , , ] <<- Tab.4d
    #Hh_$NumPowertrain.DiDtPpPt[Dx, , , ] <- Tab.4d
    rm(Tab.4d)

    # Tabulate number of vehicles by vehicle type and powertrain
    Tab.3d <- tabulate(rep(SynPop..$Houseid, SynPop..$Hhvehcnt),
                       list(rep(SynPop..$District, SynPop..$Hhvehcnt),
                            unlist(SynPop..$VehType[HasVeh.Hh]), 
                            unlist(SynPop..$Powertrain[HasVeh.Hh])),
                       list(Dx, Vt, Pt ),
                       "Count")
    Hh_$NumPowertrain.DiVtPt[Dx, , ] <<- Tab.3d
    #Hh_$NumPowertrain.DiVtPt[Dx, , ] <- Tab.3d
    rm(Tab.3d)

    # Tabulate VMT powered by hydrocarbon fuels by vehicle type and powertrain
    Tab.3d <- tabulate(unlist(SynPop..$HcVehDvmt[HasVeh.Hh]),
                       list(rep(SynPop..$District, SynPop..$Hhvehcnt),
                            unlist(SynPop..$VehType[HasVeh.Hh]), 
                            unlist(SynPop..$Powertrain[HasVeh.Hh])),
                       list(Dx, Vt, Pt),
                       "Sum")
    Hh_$HcDvmt.DiVtPt[Dx, , ] <<- Tab.3d
    #Hh_$HcDvmt.DiVtPt[Dx, , ] <- Tab.3d
    rm(Tab.3d)

    # Tabulate VMT powered by electricity by vehicle type and powertrain 
    Tab.3d <- tabulate(unlist(SynPop..$EvVehDvmt[HasVeh.Hh]),
                       list(rep(SynPop..$District, SynPop..$Hhvehcnt),
                            unlist(SynPop..$VehType[HasVeh.Hh]), 
                            unlist(SynPop..$Powertrain[HasVeh.Hh])),
                       list(Dx, Vt, Pt),
                       "Sum")
    Hh_$EvDvmt.DiVtPt[Dx, , ] <<- Tab.3d
    #Hh_$EvDvmt.DiVtPt[Dx, , ] <- Tab.3d
    rm(Tab.3d)
      
    # Tabulate fuel consumed by vehicle type and powertrain 
    Tab.3d <- tabulate(unlist(SynPop..$HcVehDvmt[HasVeh.Hh]) / unlist(SynPop..$VehMpg[HasVeh.Hh]), 
                       list(rep(SynPop..$District, SynPop..$Hhvehcnt),
                            unlist(SynPop..$VehType[HasVeh.Hh]), 
                            unlist(SynPop..$Powertrain[HasVeh.Hh])),
                       list(Dx, Vt, Pt),
                       "Sum")
    Hh_$Fuel.DiVtPt[Dx, , ] <<- Tab.3d
    #Hh_$Fuel.DiVtPt[Dx, , ] <- Tab.3d
    rm(Tab.3d)
    
    # Close local function
    } ) 
  
    gc()
  
  # Close loop through metropolitan divisions 	
  }
  
  #Calculate several totals for commercial service vehicles
  #========================================================
  Filename <- paste( OutYearDir, "/", "CommServ_.RData", sep="" )
  load( Filename )
  CommServ_$CommVehCo2e <- sum( CommServ_$CommServAutoEvCo2e.MdDt ) + 
                           sum( CommServ_$CommServAutoHcCo2e.MdDt ) + 
                           sum( CommServ_$CommServLtTruckEvCo2e.MdDt ) + 
                           sum( CommServ_$CommServLtTruckHcCo2e.MdDt )
  CommServ_$CommVehFuel <- sum( CommServ_$CommServAutoFuel.MdDt ) + 
                           sum( CommServ_$CommServLtTruckFuel.MdDt )
  CommServ_$CommVehHcDvmt <- sum( CommServ_$CommServAutoHcDvmt.MdDt ) +
                             sum( CommServ_$CommServLtTruckHcDvmt.MdDt ) 
  CommServ_$CommVehEvDvmt <- sum( CommServ_$CommServAutoEvDvmt.MdDt ) +
                             sum( CommServ_$CommServLtTruckEvDvmt.MdDt ) 

  #Calculate metropolitan area roadway critical air pollutants from light duty vehicles
  #====================================================================================
  HcDvmt <- sum( Hh_$HcDvmt.DiIgDtPp, na.rm=TRUE ) + CommServ_$CommVehHcDvmt
  EvDvmt <- sum( Hh_$EvDvmt.DiIgDtPp, na.rm=TRUE ) + CommServ_$CommVehEvDvmt
  PropHcDvmt <- HcDvmt / ( HcDvmt + EvDvmt )
  HcRoadDvmt <- PropHcDvmt * Metropolitan_$Dvmt.Ty["LtVeh"]
  Metropolitan_$LtVehRoadCritAirPol.Po <- calcEmissions( HcRoadDvmt, yr )
  rm(HcDvmt, EvDvmt, PropHcDvmt, HcRoadDvmt) 
     
  #Save the results
  #================
  Filename <- paste( OutYearDir, "/", "Hh_.RData", sep="" )
  save( Hh_, file=Filename )
  Filename <- paste( OutYearDir, "/", "CommServ_.RData", sep="" )
  save( CommServ_, file=Filename )
  Filename <- paste( OutYearDir, "/", "Metropolitan_.RData", sep="" )
  save( Metropolitan_, file=Filename )
  Filename <- paste( OutYearDir, "/", "Inputs_.RData", sep="" )
  save( Inputs_, file=Filename )
  Filename <- paste( OutYearDir, "/", "Model_.RData", sep="" )
  save( Model_, file=Filename )
  
  #End the loop through years
  #==========================
  
  print( Sys.time() )
  
}
detach(Model_)
detach(Inputs_)
detach(GreenSTEP_)
detach(Abbr_)
rm(list=ls()[!(ls() %in% c("Dir_", "RunYears"))])


