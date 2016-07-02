#===============
#GreenSTEP_Sim.r
#===============
#Regional Strategic Planning Model (RSPM) GreenSTEP Version
#Copyright 2009 - 2015, Oregon Department of Transportation
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
#http://www.apache.org/licenses/LICENSE-2.0
#Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
#Version: 3.5
#Date: 1/10/16

#Description
#===========

#This module performs all of the household microsimulation calculations for determining household income, vehicle ownership, household travel and vehicle characteristics and use. The results are aggregated to arrays by county, income and development type and saved to disk.

#Changes from statewide version 3.0 to create the metropolitan version
#---------------------------------------------------------------------

# Notes on geography. 
#* The state version's geographic units are state, economic region, metropolitan area and county. Counties are strict subdivisions of the economic regions which are strict subdivisions of the state. Metropolitan areas can overlap counties. The economic regions are only used for the purpose of establishing a county percapita income from the state percapita income. The county_groups.csv which is input as the CountyGroups.. object contains the cross references between counties, economic regions, and metropolitan areas.
#* The metropolitan version's geographic units are model area, metropolitan division, and district. The model area is the greater metropolitan area: the area within the metropolitan urbanized area and surrounding rural and town areas. This geographic level corresponds to the state geographic level for the model inputs and program code. The metropolitan division represents large divisions of the metropolitan area that may be treated differently in terms of large scale land use and transportation policy (e.g. Eugene vs. Springfield). The divisions may correspond to Census PUMAs (Public Use Microdata Areas). This level corresponds to the metropolitan area level in the state model. This level also corresponds to the economic regions in the state model. This enables differences in income levels of different parts of the metropolitan area to be recognized. The district level represents different communities (for lack of a better term) within each metropolitan division. This level corresponds to the county level for program code.

# This metropolitan model version of the script no longer assigns any household or land use characteristics to household. All of that is done by the "GreenSTEP_Hh_Synthesis.r" script.

# The light truck proportions inputs are county level in the statewide GreenSTEP and district level in the Metro version of metropolitan GreenSTEP. This has been changed to be a metropolitan division level input in this script.

# The travel demand management (TDM) inputs were metropolitan level inputs in the statewide GreenSTEP and in the Metro version of metropolitan GreenSTEP. This has been changed to a district level input.

# In the statewide version, the metropolitan type households were treated differently than the rural and town because metropolitan households travel were influenced by metropolitan area characteristics (e.g. transit service, freeway supply) whereas town and rural were not. However, for metropolitan GreenSTEP the town and rural households are located in proximity to the metropolitan area and are likely to be affected by the same attributes as the metropolitan households. Therefore in this metropolitan version, the metropolitan area auto ownership and DVMT models are applied to all households.

#==============================================================================================
#STEP 1: CALCULATE TRANSPORTATION SYSTEM CHARACTERISTICS & AVERAGE EMISSION RATES FOR ALL YEARS
#==============================================================================================

#Population is tabulated by county and year for all years so that population growth rates may be calculated to use to calculate factors that are based on growth rates. Annual growth of freeway and arterial lane miles is calculated so that the annual construction costs may be calculated. This is used to calculate whether fuel taxes are sufficient to cover costs and how much VMT taxes should be raised to cover costs.

# Make a scenario outputs directory
if( !file.exists( OutputDir ) ) {
  dir.create( OutputDir )
}
# Make a vector of years that are past years (base year and before )
PastYears. <- as.character( as.numeric( Yr )[ as.numeric( Yr ) <= as.numeric( BaseYear ) ] )

# Tabulate population and income and calculate population change
#---------------------------------------------------------------
# Load file if already tabulated, otherwise tabulate
Pop.MdDtYr <- array( 0, dim=c( length(Md), length(Dt), length(Yr) ), dimnames=list( Md, Dt, Yr ) )
Pop.DiDtYr <- array( 0, dim=c( length(Di), length(Dt), length(Yr) ), dimnames=list( Di, Dt, Yr ) )
Inc.MdDtYr <- array( 0, dim=c( length(Md), length(Dt), length(Yr) ), dimnames=list( Md, Dt, Yr ) )
HsldDir <- paste( ModelDir, "SynHsld", sep="/" )
for( yr in Yr ) {
  # Identify inputs and outputs and check whether model results already exist
  OutYearDir <- paste( OutputDir, "/Year", yr, sep="" )
  AlreadyRun <- file.exists( OutYearDir )
  if( AlreadyRun ) {
    InYearDir <- OutYearDir
  } else {
    InYearDir <- paste( HsldDir, "/Year", yr, sep="" )
    dir.create( OutYearDir )
  }
  for( md in Md ) {
    # Load synthetic population data for metropolitan division
    PopFileName <- paste( InYearDir, "/", md, ".RData", sep="" )
    SynPop.. <- assignLoad( PopFileName )
    # Tabulate total population by development type
    Pop.Dx <- tapply( SynPop..$Hhsize, SynPop..$DevType, sum )
    Pop.MdDtYr[ md, names( Pop.Dx ), yr ] <- Pop.Dx
    rm( Pop.Dx )
    # Tabulate total population by district and development type
    Pop.DxDt <- tapply( SynPop..$Hhsize, list( SynPop..$District, SynPop..$DevType ), sum )
    Pop.DxDt[ is.na( Pop.DxDt ) ] <- 0
    Pop.DiDtYr[ rownames( Pop.DxDt ), colnames( Pop.DxDt ), yr ] <- Pop.DxDt
    rm( Pop.DxDt )
    # Tabulate total household income by development type 			
    Inc.Dx <- tapply( SynPop..$Hhincttl, SynPop..$DevType, sum )
    Inc.MdDtYr[ md, names( Inc.Dx ), yr ] <- Inc.Dx
    # Copy the synthetic population files to the outputs directory if not already run
    if( !AlreadyRun ) {
      PopFileName <- paste( OutYearDir, "/", md, ".RData", sep="" )
      save( SynPop.., file=PopFileName )
    } 
    # Clean up
    rm( PopFileName, SynPop.. )
  }
  rm( InYearDir, OutYearDir, md )
}
rm( HsldDir, yr )
# Summarize for whole metropolitan area
Pop.DtYr <- colSums( Pop.MdDtYr )
Inc.DtYr <- colSums( Inc.MdDtYr )
# Calculate metropolitan area population growth by year
MetroPop.Yr <- colSums( Pop.DtYr )
## Calculate the growth in metropolitan population between model years
#MetroPopGrowth.Yx <- diff( MetroPop.Yr ) 
## Calculate the number of years between model years
#NumYears.Yx <- diff( as.numeric( Yr ) ) 
## Calculate the annual growth in population
#AnnMetroPopGrowth.Ya <- rep( MetroPopGrowth.Yx / NumYears.Yx, NumYears.Yx ) 
#Ya <- (as.numeric(Yr[1])+1):(as.numeric(Yr[length(Yr)]))
#names( AnnMetroPopGrowth.Ya ) <- Ya
## Calculate the annual population by year
#MetroPop.Ya <- MetroPop.Yr[1] + cumsum( AnnMetroPopGrowth.Ya )
#rm(MetroPopGrowth.Yx,  

#Calculate freeway lane miles and the annual cost of construction
#----------------------------------------------------------------
NumYears.Yx <- diff( as.numeric( Yr ) ) 
Ya <- (as.numeric(Yr[1])+1):(as.numeric(Yr[length(Yr)]))
# Calculate freeway lane-miles
FwyLnMi.Yr <- BaseFwyLnMi * FreewayGrowth.Yr[Yr]
FwyLnMiGrowth.Yx <- diff( FwyLnMi.Yr ) 
# Calculate cost to build added freeway lane-miles
FwyUnitCost.Yr <- 1000 * Costs.YrCs[Yr,"FwyLnMi"] #Note: Costs.YrCs[Yr,"FwyLnMi"] is in $1000s
FwyAveUnitCost.Yx <- (FwyUnitCost.Yr[-1] + FwyUnitCost.Yr[-length(FwyUnitCost.Yr)]) / 2
FwyGrowthCost.Yx <- FwyLnMiGrowth.Yx * FwyAveUnitCost.Yx
FwyGrowthCost.Ya <- rep( FwyGrowthCost.Yx / NumYears.Yx, NumYears.Yx )  
names(FwyGrowthCost.Ya) <- Ya
# Extract the values for the model years
FwyGrowthCost.Yr <- c( 0, FwyGrowthCost.Ya[Yr[-1]] )
names( FwyGrowthCost.Yr ) <- Yr
# Calculate the per capita freeway supply
# Units are freeway lane-miles per 1000 persons
FwyLnMiCap.Yr <- 1000 * FwyLnMi.Yr / MetroPop.Yr
# Clean up
rm( FwyLnMi.Yr, FwyLnMiGrowth.Yx, FwyUnitCost.Yr, FwyAveUnitCost.Yx, FwyGrowthCost.Yx, FwyGrowthCost.Ya )

#Calculate arterial lane miles and the annual cost of construction
#----------------------------------------------------------------
# Calculate arterial lane-miles
ArtLnMi.Yr <- BaseArtLnMi * ArterialGrowth.Yr[Yr]
ArtLnMiGrowth.Yx <- diff( ArtLnMi.Yr ) 
# Calculate cost to build added arterial lane-miles
ArtUnitCost.Yr <- 1000 * Costs.YrCs[Yr,"ArtLnMi"] #Note: Costs.YrCs[Yr,"ArtLnMi"] is in $1000s
ArtAveUnitCost.Yx <- (ArtUnitCost.Yr[-1] + ArtUnitCost.Yr[-length(ArtUnitCost.Yr)]) / 2
ArtGrowthCost.Yx <- ArtLnMiGrowth.Yx * ArtAveUnitCost.Yx
ArtGrowthCost.Ya <- rep( ArtGrowthCost.Yx / NumYears.Yx, NumYears.Yx )  
names(ArtGrowthCost.Ya) <- Ya
# Extract the values for the model years
ArtGrowthCost.Yr <- c( 0, ArtGrowthCost.Ya[Yr[-1]] )
names( ArtGrowthCost.Yr ) <- Yr
# Calculate the per capita arterial supply
# Units are arterial lane-miles per 1000 persons
ArtLnMiCap.Yr <- 1000 * ArtLnMi.Yr / MetroPop.Yr
# Clean up
rm( ArtLnMi.Yr, ArtLnMiGrowth.Yx, ArtUnitCost.Yr, ArtAveUnitCost.Yx, ArtGrowthCost.Yx, ArtGrowthCost.Ya )

# Calculate the total annual cost for adding lane miles
#------------------------------------------------------
AnnLnMiAddCosts.Yr <- FwyGrowthCost.Yr + ArtGrowthCost.Yr		
rm( FwyGrowthCost.Yr, ArtGrowthCost.Yr )

# Calculate the public transit supply
#------------------------------------
TranRevMi.Yr <- BaseTranRevMi..[,"Total"] * unlist( TransitParm..["RevMiGrowth",] )[Yr]
RailRevMi.Yr <- TranRevMi.Yr * unlist( TransitParm..["PropRail",] )[Yr]
BusRevMi.Yr <- TranRevMi.Yr - RailRevMi.Yr
TranRevMiCap.Yr <- TranRevMi.Yr / MetroPop.Yr
rm( MetroPop.Yr )

#Calculate unit emissions for fuel and electricity for year
#----------------------------------------------------------
# Calculate average fuel CO2e per gallon for light duty vehicles by vehicle type and year 
AveFuelCo2e.LdYr <- sapply( Yr, function(x) {
  calcAveFuelCo2e( x, Fuels..Yr=AutoLtTrkFuels..Yr, Co2..Yr=FuelCo2..Yr, 
                   MjPerGallon=121, OutputType="MetricTons" )
} )
# Calculate average electricity CO2e per Kwh by district and year
AveElectricCo2e.DiYr <- sapply( Yr, function(x) {
  calcAveElectricCo2e( x, Co2.CoYr=PowerCo2.DiYr, OutputType="MetricTons" )
} )

# Save the results
#-----------------
Filename <- paste( OutputDir, "/Pop.DtYr.RData", sep="" )
save( Pop.DtYr, file=Filename ); rm( Filename )
Filename <- paste( OutputDir, "/Pop.DiDtYr.RData", sep="" )
save( Pop.DiDtYr, file=Filename ); rm( Filename )
Filename <- paste( OutputDir, "/Inc.DtYr.RData", sep="" )
save( Inc.DtYr, file=Filename ); rm( Filename )
Filename <- paste( OutputDir, "/FwyLnMiCap.Yr.RData", sep="" )
save( FwyLnMiCap.Yr, file=Filename ); rm( Filename )
Filename <- paste( OutputDir, "/ArtLnMiCap.Yr.RData", sep="" )
save( ArtLnMiCap.Yr, file=Filename ); rm( Filename )
Filename <- paste( OutputDir, "/TranRevMiCap.Yr.RData", sep="" )
save( TranRevMiCap.Yr, file=Filename ); rm( Filename )
Filename <- paste( OutputDir, "/BusRevMi.Yr.RData", sep="" )
save( BusRevMi.Yr, file=Filename ); rm( Filename )
Filename <- paste( OutputDir, "/RailRevMi.Yr.RData", sep="" )
save( RailRevMi.Yr, file=Filename ); rm( Filename )


#===========================
#RUN THE MODEL FOR EACH YEAR
#===========================

#The model iterates through each forecast year. The results for the year are stored in a list with a component for each year.

for( yr in RunYears ) {
  
  RunYearDir <- paste( OutputDir, "/Year", yr, sep="" )
  
  #Set a random seed to make run replicable
  #----------------------------------------
  #If the UsedSavedRandomSeed input parameter is TRUE a saved random seed will be retrieved
  #Otherwise a new random seed will be set and saved
  RandomSeedFile <- paste( InputDir, "/RandomSeedValue", yr, ".RData", sep="" )
  if( UseSavedRandomSeed & file.exists( RandomSeedFile ) ) {
    RandomSeedValue <- assignLoad( RandomSeedFile )
  } else {
    RandomSeedValue <- sample( 1:1000000, 1 )
    save( RandomSeedValue, file=RandomSeedFile )
  }
  #Set random seed for model run
  set.seed( RandomSeedValue )
  
  #================================================================================
  #STEP 2: SIMULATE HOUSEHOLD TRAVEL CHARACTERISTICS FOR EACH METROPOLITAN DIVISION 
  #================================================================================
  
  StartTime <- Sys.time()
  print( "Simulation of household travel characteristics" )
  
  # Iterate through metropolitan divisions
  #=======================================
  
  for( md in Md ) {
    
    print( md )

    # Load metropolitan division file
    Filename <- paste( RunYearDir, "/", md, ".RData", sep="" )
    SynPop.. <- assignLoad( Filename )
    
    # Assign block group density variable to accommodate new model
    SynPop..$Hbppopdn <- SynPop..$Htppopdn
    
    # Identify the districts in the metropolitan division
    Dx <- Di[ Di %in% rownames( DistrictGroups.. )[ DistrictGroups..$Division == md ] ]
    
    # Identify group quarters households
    IsGroupPop. <- SynPop..$HouseType == "GQ"
    
    #Step : Add the freeway and transit supply data to the SynPop..
    #================================================================
    
    SynPop..$Fwylnmicap <- 0
    SynPop..$Fwylnmicap <- FwyLnMiCap.Yr[ yr ]
    SynPop..$Tranmilescap <- 0
    SynPop..$Tranmilescap <- TranRevMiCap.Yr[ yr ]
    
    #Step : Identify households affected by travel demand management or vehicle o&m programs
    #=========================================================================================
    
    # Identify ECO and IMP households
    #--------------------------------
    # Identify ECO households
    SynPop..$NumEco <- 0
    ModelVar. <- c( "DrvAgePop", "Houseid" )
    for( dx in Dx ) {
      IsDistrict. <- SynPop..$District == dx
      if( sum( IsDistrict. > 5) ){
        SynPop..$NumEco[ IsDistrict. ] <- idEcoWorkers( SynPop..[ IsDistrict., ModelVar. ],
                                                        PropWrkEco=PropWrkEco.MdYr[ md, yr ] )
      }
      rm( IsDistrict. )
    }
    rm( ModelVar. )
    # Identify IMP households
    SynPop..$ImpHh <- 0
    ModelVar. <- c( "Htppopdn", "Urban", "Houseid" )
    for( dx in Dx ) {
      IsDistrict. <- SynPop..$District == dx
      if( any( IsDistrict. ) ){
        ImpHh_ <- idImpHouseholds( SynPop..[ IsDistrict., ModelVar. ],
                                   ImpPropGoal=ImpPropGoal.DiYr[ dx, yr ] )
        SynPop..$ImpHh[ IsDistrict. ] <- ImpHh_$ImpHh
        rm( ImpHh_ )
      }
      rm( IsDistrict. )
    }
    rm( ModelVar. )
   
    # Identify eco-driver and low rolling-resistance tire households
    #---------------------------------------------------------------
    # Note: reason for form of 1st argument is to pass a data frame with
    # minimal size to the functions. 
    ModelVar. <- "Houseid"
    TempInputData.. <- data.frame(SynPop..[ ,"Houseid" ] )
    SynPop..$IsEcoDriver <- idEcoDriverHh( TempInputData.., 
                                           EcoTire..Yr[ yr, "EcoDrvProp" ] )
    SynPop..$IsLowRollTire <- idLowRollTire( TempInputData.., 
                                             EcoTire..Yr[ yr, "LowRollProp" ] )
    rm( ModelVar., TempInputData.. )
    
    #Step 2b: Calculate vehicle ownership
    #====================================
    
    # Calculate initial vehicle ownership
    #------------------------------------
    # Initialize Hhvehcnt and VehPerDrvAgePop variables
    SynPop..$Hhvehcnt <- 0
    SynPop..$VehPerDrvAgePop <- 0
    # Predict vehicle ownership for standard households
    if( any( !IsGroupPop. ) ) {
      ModelVar. <- c( "Hhincttl", "Htppopdn", "Tranmilescap", "Urban", 
                      "Fwylnmicap", "OnlyElderly", "DrvLevels", "DrvAgePop" )  
      MetroVehOwn_ <- predictVehOwn( SynPop..[ !IsGroupPop., ModelVar. ],
                                     Model_=VehicleOwnModels_, Type="Metro" )
      rm( ModelVar. )
    }
    # Predict ownership for persons in group quarters
    if( any( IsGroupPop. ) ) {
      GroupPopAge. <- apply( SynPop..[ IsGroupPop., Ap ], 1, function(x) Ap[ which( x != 0 ) ] )
      AutoOwnRate. <- unlist( lapply( GroupAutoOwnership_Va..[ GroupPopAge. ], function(x) x[md,yr] ) )
      GroupVeh. <- sapply( AutoOwnRate., function(x) sample( c(1,0), 1, prob=c(x,1-x) ) )
      rm( GroupPopAge., AutoOwnRate. )
    }
    # Assign values to SynPop.. and return the result
    if( any( !IsGroupPop. ) ) {
      SynPop..$Hhvehcnt[ !IsGroupPop. ] <- MetroVehOwn_$NumVeh
      SynPop..$VehPerDrvAgePop[ !IsGroupPop. ] <- MetroVehOwn_$VehRatio
    }
    if( any( IsGroupPop. ) ) {
      SynPop..$Hhvehcnt[ IsGroupPop. ] <- GroupVeh.
      SynPop..$VehPerDrvAgePop[ IsGroupPop. ] <- GroupVeh.
    }
    # Clean up
    if( exists( "MetroVehOwn_" ) ) rm( MetroVehOwn_ )
    if( exists( "GroupVeh." ) ) rm( GroupVeh. )

    #Step 2c: 1st DVMT calculation
    #=============================

    SynPop..$BaseCostPerMi <- 4 / 100
    SynPop..$FutrCostPerMi <- 4 / 100
    ModelVar. <- c(
      "Hhincttl", "Hbppopdn", "Hhvehcnt", "Tranmilescap", "Fwylnmicap", 
      "DrvAgePop", "Hhsize", "Age0to14", "Age15to19", "Age20to29", "Age30to54", 
      "Age55to64", "Age65Plus", "Urban"
    )
    SynPop..$Dvmt <- 0
    SynPop..$Dvmt <- 
      predictAveDvmt(SynPop..[, ModelVar.], DvmtLmModels_, "Metro")$DvmtAve
    rm(ModelVar.)

    #Step 2d: Apply parking model to identify parkers and calculate daily parking costs
    #==================================================================================
    
    # Calculate parking costs for households that live in metropolitan areas
    SynPop..$DailyPkgCost <- 0
    SynPop..$CashOutIncAdj <- 0
    ModelVar. <- c( "DrvAgePop", "Houseid", "Dvmt", "Hhvehcnt" ) 
    Parkers_ <- idPayingParkers( SynPop..[ , ModelVar. ],
                                 PropWrkPkg=PkgParm_Va..$PropWrkPkg[ md, yr ],
                                 PropWrkChrgd=PkgParm_Va..$PropWrkChrgd[ md, yr ],
                                 PropCashOut=PkgParm_Va..$PropCashOut[ md, yr ],
                                 PropOthChrgd=PkgParm_Va..$PropOthChrgd[ md, yr ],
                                 PkgCost=PkgParm_Va..$PkgCost[ md, yr ],
                                 PropWrkTrav=0.22, WrkDaysPerYear=260 )
    PkgCosts_ <- calcParkCostAdj( SynPop..[ , ModelVar. ], Parkers_ )
    SynPop..$DailyPkgCost <- PkgCosts_$DailyPkgCost
    SynPop..$CashOutIncAdj <- PkgCosts_$CashOutIncAdj
    rm( Parkers_, PkgCosts_, ModelVar. )
    gc()

    #Step 2e: Calculate car service use, adjust vehicle ownership and VMT
    #====================================================================
    
    SynPop..$CarSvcAvail <- 
      calcCarSvcAvail(SynPop.., CarSvcAvail.DiYr[,yr])
    ModelVar. <- c(
      "Hhincttl", "Hbppopdn", "Hhvehcnt", "Tranmilescap", "Fwylnmicap", 
      "DrvAgePop", "Hhsize", "Age0to14", "Age15to19", "Age20to29", "Age30to54", 
      "Age55to64", "Age65Plus", "Urban", "DailyPkgCost", "CarSvcAvail"
      )
    VehUse_ <- 
      calcVehicleUse(SynPop..[, ModelVar.],
                     HhVehOwnParm_ = as.list(t(HhVehOwnParm..)[yr,]),
                     CarSvcCostParm_ = as.list(t(CarSvcCostParm..)[yr,]))
    SynPop..$Dvmt <- VehUse_$HhAuDvmt + VehUse_$HhNAuDvmt
    SynPop..$AuDvmt <- VehUse_$HhAuDvmt
    SynPop..$NAuDvmt <- VehUse_$HhNAuDvmt
    SynPop..$CarSvcDvmt <- VehUse_$CarSvcDvmt
    SynPop..$Hhvehcnt <- VehUse_$NumAuVeh + VehUse_$NumNAuVeh
    SynPop..$NumAuVeh <- VehUse_$NumAuVeh
    SynPop..$NumNAuVeh <- VehUse_$NumNAuVeh
    SynPop..$CarSvcBaseCost <- VehUse_$CarSvcMileCost
    SynPop..$CarSvcRepoProp <- VehUse_$RepoProp
    CarSvcIsAV <- VehUse_$CarSvcIsAV

    rm(ModelVar., VehUse_)

    #Step 2f: Calculate non-price TDM and light-weight vehicle DVMT adjustment factors
    #=================================================================================
    
    # Calculate the TDM adjustment factor
    #------------------------------------
    TdmAdjDvmt.Hh <- SynPop..$Dvmt
    ModelVar. <- c( "Dvmt", "NumEco", "ImpHh" )
    for( dx in Dx ) {
      IsDistrict. <- SynPop..$District == dx 
      if( any( IsDistrict. ) ) {
        TdmAdjDvmt.Hh[ IsDistrict. ] <- adjDvmtEcoImp( SynPop..[ IsDistrict., ModelVar. ],
                                                       EcoReduction=TdmParm.[ "EcoReduction" ],
                                                       ImpReduction=TdmParm.[ "ImpReduction" ] )
      }
      rm( IsDistrict. )
    }
    TdmAdjFactor.Hh <- TdmAdjDvmt.Hh / SynPop..$Dvmt
    TdmAdjFactor.Hh[ SynPop..$Dvmt == 0 ] <- 1
    
    # Calculate the light vehicle adjustment factor
    #----------------------------------------------
    # Predict light vehicle ownership
    LtVehOwn.Hh <- rep( 0, nrow( SynPop.. ) )
    SynPop..$LogDen <- log( SynPop..$Htppopdn )                 
    ModelVar. <- c( "LogDen", "Hhsize", "Hhincttl", "Age15to19", "Age20to29", "Age30to54", 
                    "Age55to64", "Age65Plus", "VehPerDrvAgePop", "DrvAgePop" )
    for( dx in Dx ) {
      IsDistrict. <- SynPop..$District == dx
      if( sum( IsDistrict. ) > 5 ) {
        LtVehOwn.Hh[ IsDistrict. ] <- predictLightVehicles( SynPop..[ IsDistrict., ModelVar. ],
                                                            LtVehOwnModels_=LtVehOwnModels_, Type="Metro",
                                                            TargetProp=LtVehParm_Va..$OwnRatio[ dx, yr ] )
      }
      rm( IsDistrict. )
    }
    SynPop..$LtVehCnt <- LtVehOwn.Hh
    rm( LtVehOwn.Hh, ModelVar. )
    
    # Predict light vehicle DVMT
    #---------------------------
    LtVehDvmt.Hh <- SynPop..$Dvmt
    SynPop..$LogDen <- log( SynPop..$Htppopdn )
    SynPop..$LogSize <- log( SynPop..$Hhsize )
    SynPop..$LogDvmt <- log( SynPop..$Dvmt )
    ModelVar. <- c( "Hhincttl", "LogDen", "LogSize", "Urban", "LogDvmt", "Dvmt", "LtVehCnt",
                    "DrvAgePop" )
    for( dx in Dx ) {
      IsDistrict. <- SynPop..$District == dx
      if( any( IsDistrict. ) ) {
        LtVehDvmt.Hh[ IsDistrict. ] <- calcLtVehDvmt( SynPop..[ IsDistrict., ModelVar. ], 
                                                      AveSovPropModels_, 
                                                      Threshold=LtVehParm_Va..$Threshold[ dx, yr ],
                                                      PropSuitable=LtVehParm_Va..$PropSuitable[ dx, yr ], 
                                                      Sharing=FALSE )
      }
    }
    # Calculate adjustment factor
    LtVehAdjFactor.Hh <- ( SynPop..$Dvmt - LtVehDvmt.Hh ) / SynPop..$Dvmt
    LtVehAdjFactor.Hh[ SynPop..$Dvmt == 0 ] <- 1
    
    # Calculate overall adjustment factor 
    #------------------------------------
    SynPop..$LtVehAdjFactor <- LtVehAdjFactor.Hh  #Save the factor in SynPop..
    SynPop..$TdmAdjFactor <- TdmAdjFactor.Hh  #Save the factor in SynPop..
    SynPop..$TdmLtVehAdjFactor <- TdmAdjFactor.Hh * LtVehAdjFactor.Hh  #Save the factor in SynPop..
    rm( LtVehDvmt.Hh, TdmAdjFactor.Hh, LtVehAdjFactor.Hh, ModelVar.,
        TdmAdjDvmt.Hh )
    SynPop..$LogSize <- NULL
    SynPop..$LogDvmt <- NULL
    
    #Step 2g: Calculate the 95th percentile and maximum DVMT from the adjusted DVMT
    #==============================================================================
    
    SynPop..$MaxDvmt <- 0
    SynPop..$Dvmt95 <- 0
    MetroMax95th_ <- predictMaxDvmt( SynPop..[ , c( "Dvmt", "MaxDvmt", "Dvmt95" ) ],
                                       DvmtLmModels_, "Metro" )
    SynPop..$MaxDvmt <- MetroMax95th_$DvmtMax
    SynPop..$Dvmt95 <- MetroMax95th_$Dvmt95th
    rm(MetroMax95th_)
    gc()
    
    #Step 2h: Calculate Walk, Bike, & Transit Trips
    #==============================================
    # These are performance measures. They do not affect DVMT calculations.
    ModelVar. <- 
      c("Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", 
        "Age65Plus", "Hhsize", "Hhincttl", "Hbppopdn", "Tranmilescap",
        "Urban", "Dvmt", "Hhvehcnt") 
    AltModeTrips_ <- 
      calcAltModeTrips(SynPop..[,ModelVar.], AltModeModels_, "Metro")
    SynPop..$AveWalkTrips <- AltModeTrips_$Walk
    SynPop..$AveBikeTrips <- AltModeTrips_$Bike
    SynPop..$AveTransitTrips <- AltModeTrips_$Transit
    rm(ModelVar., AltModeTrips_)
    
    # Remove variables from SynPop.. not needed
    #------------------------------------------
    SynPop..$PowPerCapInc <- NULL
    
    # Save results
    #-------------			
    Filename <- paste( RunYearDir, "/", md, ".RData", sep="" )
    save( SynPop.., file=Filename, compress=TRUE )
    rm( SynPop.. )
    gc()
    
    # End loop through divisions
    gc()
  }
  print( StartTime )
  print( Sys.time() )
  
  
  #==================================================
  #STEP 3: SIMULATE HOUSEHOLD VEHICLE CHARACTERISTICS
  #==================================================
  
  StartTime <- Sys.time()
  print( "Simulation of household vehicle characteristics" )
  
  # Iterate through metropolitan divisions
  #=======================================
  
  for( md in Md ) {
    
    print( md )
    
    # Load metropolitan division synthetic household file
    Filename <- paste( RunYearDir, "/", md, ".RData", sep="" )
    SynPop.. <- assignLoad( Filename )
    
    #Step 3a: Calculate vehicle types, ages, initial fuel economy, and assign vehicle DVMT
    #=====================================================================================
    
    # Predict light truck ownership and vehicle ages
    #-----------------------------------------------
    HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
    # Apply vehicle type model
    ModelVar. <- c( "Hhincttl", "Htppopdn", "Urban", "Hhvehcnt", "Hhsize" ) 
    SynPop..$VehType <- NA
    VehTypeResults_ <- predictLtTruckOwn( SynPop..[ HasVeh.Hh, ModelVar. ], Model_=LtTruckModels_,
                                          TruckProp=LtTruckProp.MdYr[md,yr] )
    SynPop..$VehType[ HasVeh.Hh ] <- VehTypeResults_
    rm( ModelVar. )
    # Apply vehicle age model
    ModelVar. <- c( "IncGrp", "Hhvehcnt", "VehType" )
    SynPop..$VehAge <- NA 
    VehTypeAgeResults_ <- calcVehicleAges( SynPop..[ HasVeh.Hh, ModelVar. ], VProp_=VehProp_,
                                           AdjRatio=AgeAdj.YrTy[yr,] )
    rm( ModelVar. )
    # Add type and age model results to the TestHh..
    SynPop..$VehAge[ HasVeh.Hh ] <- VehTypeAgeResults_$VehAge
    SynPop..$VehAge[ SynPop..$Hhvehcnt == 0 ] <- NA
    rm( VehTypeAgeResults_ )
    gc()
    
    # Assign initial fuel economy and DVMT to vehicles
    #-------------------------------------------------
    # Assign fuel economy to vehicles
    HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
    SynPop..$VehMpg <- NA
    ModelVar. <- c( "VehType", "VehAge", "Hhvehcnt" )
    SynPop..$VehMpg[ HasVeh.Hh ] <-assignFuelEconomy( SynPop..[ HasVeh.Hh, ModelVar. ],
                                                      AutoLtTrkMpg..Yr, CurrYear=yr )
    rm( ModelVar. )
    # Assign vehicle mileage proportions to household vehicles
    SynPop..$DvmtProp <- NA
    ModelVar. <- c( "Hhvehcnt", "Houseid" ) 
    SynPop..$DvmtProp[ HasVeh.Hh ] <- apportionDvmt( SynPop..[ HasVeh.Hh, ],
                                                     DP_=DvmtProp_ )
    rm( ModelVar. )
    # Assign vehicle mileage to household vehicles
    SynPop..$VehDvmt <- calcVehDvmt( SynPop..$Dvmt, SynPop..$DvmtProp )
    gc()
    
    #Step 3b: Identify HEVs & PHEVs
    #==============================
    
    # Apply HEV/PHEV model
    HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
    SynPop..$Htppopdn <- SynPop..$Hbppopdn
    ModelVar. <- 
      c( "Houseid", "Hhvehcnt", "VehType", "VehAge", "VehDvmt", "DevType", 
         "Hhincttl", "Htppopdn", "Hhsize", "Age0to14", "Age65Plus",
         "Tranmilescap", "Urban", "VehMpg" )
    PhevResults_ <- 
      assignPhev( SynPop..[ HasVeh.Hh, ModelVar. ], 
                  PhevRangeProp..Yr=PhevRangeProp..Yr, CurrYear=yr,
                  PhevPropModel_=PhevMilePropModel_, 
                  HevMpgProp..Yr=HevMpgProp..Yr )
    rm( ModelVar. )
    
    # Update SynPop.. data
    SynPop..$VehDvmt[ HasVeh.Hh ] <- PhevResults_$VehDvmt_
    SynPop..$DvmtProp[ HasVeh.Hh ] <- PhevResults_$DvmtProp_
    SynPop..$EvVehDvmt <- NA
    SynPop..$EvVehDvmt[ HasVeh.Hh ] <- PhevResults_$EvVehDvmt_
    SynPop..$HcVehDvmt <- NA
    SynPop..$HcVehDvmt[ HasVeh.Hh ] <- PhevResults_$HcVehDvmt_
    SynPop..$VehMpg[ HasVeh.Hh ] <- PhevResults_$VehMpg_
    SynPop..$VehMpkwh <- NA
    SynPop..$VehMpkwh[ HasVeh.Hh ] <- PhevResults_$VehMpkwh_
    SynPop..$Powertrain <- NA
    SynPop..$Powertrain[ HasVeh.Hh ] <- PhevResults_$Powertrain_
    rm( PhevResults_, HasVeh.Hh )
    gc()
    
    #Step 3c: Identify EVs
    #=====================
    
    # Apply EV model	
    ModelVar. <- c( "Houseid", "Hhvehcnt", "VehType", "VehAge", "VehDvmt", "Dvmt95", 
                    "DvmtProp", "Powertrain", "VehMpg", "VehMpkwh", "EvVehDvmt", "HcVehDvmt" )
    HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
    HasDvmt.Hh <- SynPop..$Dvmt > 0
    EvResults_ <- assignEv( SynPop..[ HasVeh.Hh & HasDvmt.Hh, ModelVar. ], EvRangeProp..Yr=EvRangeProp..Yr,
                            CurrYear=yr )
    SynPop..$EvVehDvmt[ HasVeh.Hh & HasDvmt.Hh ] <- EvResults_$EvVehDvmt_
    SynPop..$HcVehDvmt[ HasVeh.Hh & HasDvmt.Hh ] <- EvResults_$HcVehDvmt_
    SynPop..$VehMpg[ HasVeh.Hh & HasDvmt.Hh ] <- EvResults_$VehMpg_
    SynPop..$VehMpkwh[ HasVeh.Hh & HasDvmt.Hh ] <- EvResults_$VehMpkwh_
    SynPop..$Powertrain[ HasVeh.Hh & HasDvmt.Hh ] <- EvResults_$Powertrain_
    rm( EvResults_, HasVeh.Hh, HasDvmt.Hh, ModelVar. )
    
    #Calculate vehicle depreciation expenses
    #=======================================
    
    SynPop..$DepExp <- 0
    HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
    ModelVar. <- c( "Houseid", "Hhvehcnt", "VehType", "VehAge" )
    SynPop..$DepExp[ HasVeh.Hh ] <- calcVehDepreciationExp( SynPop..[ HasVeh.Hh, ModelVar. ] )
    rm( HasVeh.Hh, ModelVar. )
    gc()
    
    #Assign PAYD Insurance
    #=====================
    
    ModelVar. <- c( "Houseid", "Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", 
                    "Age65Plus", "Dvmt", "Hhvehcnt", "Hhincttl", "VehType", "VehAge" )
    PaydWeights.Hh <- estPaydWeights( Data..=SynPop..[ModelVar.], yr=yr)
    PaydWeights.Hh[is.na(PaydWeights.Hh)] <- 0
    SynPop..$Payd <- selectFromWeights(PaydWeights.Hh, PropToSelect=Payd..Yr["Proportion", yr])
    rm(ModelVar., PaydWeights.Hh)
    gc()
    
    # Save the synthetic households for the metropolitan division
    #============================================================
    Filename <- paste( RunYearDir, "/", md, ".RData", sep="" )
    save( SynPop.., file=Filename, compress=TRUE )
    rm( SynPop.. )
    
    # End loop through metropolitan divisions
    gc()
  }
  print( StartTime )
  print( Sys.time() )
  
  
  #=====================================================
  #STEP 4: EQUILIBRATE DVMT, COSTS, REVENUES, CONGESTION 
  #=====================================================
  
  # Loop to equilibrate DVMT, travel costs, revenues and congestion
  #----------------------------------------------------------------
  
  #Create list to store DVMT, cost totals
  Pop.MdDt <- apply( Pop.MdDtYr, c(1,2), sum )
  Dvmt.MdDt <- Pop.MdDt * 0
  AuDvmt.MdDt <- Pop.MdDt * 0
  Va <- c( "Dvmt", "AdjDvmt", "CongPrice", "FuelCost", "PowerCost", "RoadUseTax", "CarbonTax", "AddedExtTax", 
           "PaydCost", "TotExtCost", "HhTotCost", "FutrCostPerMi", "VehOwnExp", "TotRoadCost" )
  CostSummary.MdVa <- array( 0, dim=c( length(Md), length(Va) ), dimnames=list(Md,Va) )
  AveCongTaxPerMi <- 0
  ExtraModCost <- 0
  
  # Run 4 iterations if VMT tax surcharge is calculated, otherwise run 2 iterations.
  if( CalcVmtSurcharge ) {
    It <- 1:4
  } else {
    It <- 1:2
  }
  VmtSurcharge.It <- numeric( length(It) )
  rm( Va ) 
  
  for( it in It ) {
    
    StartTime <- Sys.time()
    print( paste( "Iteration", it, "Calculate emissions and cost and adjust DVMT" ) )
    
    #Steps 4a & 4b: calculate energy consumption, CO2e, production, household costs, and adjust DVMT from costs
    #==========================================================================================================
    for( md in Md ) {
      
      print( md )
      
      # Identify the districts in the metropolitan division
      Dx <- Di[ Di %in% rownames( DistrictGroups.. )[ DistrictGroups..$Division == md ] ]
      
      # Load division file
      Filename <- paste( RunYearDir, "/", md, ".RData", sep="" )  
      SynPop.. <- assignLoad( Filename )
      
      #Step 4a: Calculate fuel & electricity consumption, CO2e production, & household costs
      #=====================================================================================

      #Calculate household vehicle fuel & electricity consumption and CO2e production
      #------------------------------------------------------------------------------
      #Calculate consumption and production at a household level
      ModelVar. <- c( "Hhvehcnt", "HcVehDvmt", "VehMpg", "VehType", "EvVehDvmt",
                      "VehMpkwh", "Dvmt" )
      for( dx in Dx ) {
        IsDistrict. <- SynPop..$District == dx 
        if (sum(IsDistrict. > 0)) {
          FuelElecCo2e_ <- calcVehFuelElecCo2( SynPop..[ IsDistrict., ModelVar. ], AveFuelCo2e.=AveFuelCo2e.LdYr[,yr],
                                               AveElectricCo2e=AveElectricCo2e.DiYr[dx,yr], CsharEffFactor=1 )
          SynPop..$FuelGallons[ IsDistrict. ] <- FuelElecCo2e_$FuelGallons
          SynPop..$FuelCo2e[ IsDistrict. ] <- FuelElecCo2e_$FuelCo2e
          SynPop..$ElecKwh[ IsDistrict. ] <- FuelElecCo2e_$ElecKwh
          SynPop..$ElecCo2e[ IsDistrict. ] <- FuelElecCo2e_$ElecCo2e
          rm( FuelElecCo2e_ )
        }
      }
      rm( ModelVar. )
      gc()
      
      #Calculate car service vehicle fuel & electricity consumption and CO2e production
      #--------------------------------------------------------------------------------
      #Calculate average MPG, MPkWh, fuel CO2e, & power CO2e
      CarSvcFuelElecCo2Rate_ <- 
        calcCarSvcFuelElecCo2Rates( Year = yr, 
                                    CarSvcCostParm.. = CarSvcCostParm.., 
                                    CarSvcLtTruckProp.Yr = CarSvcLtTruckProp.Yr, 
                                    CarSvcPtProp..Yr = CarSvcPtProp..Yr,
                                    AutoLtTrkMpg..Yr = AutoLtTrkMpg..Yr, 
                                    EvRangeProp..Yr = EvRangeProp..Yr, 
                                    HevMpgProp..Yr = HevMpgProp..Yr,
                                    FuelCo2..Yr = FuelCo2..Yr, 
                                    PowerCo2.DiYr = PowerCo2.DiYr)
      #Calculate amounts of fuel and power consumed, and CO2e produced
      CarSvcFuelElecCo2_ <- calcCarSvcFuelElecCo2()
      SynPop..$CarSvcFuelGal <- CarSvcFuelElecCo2_$CarSvcFuelGal
      SynPop..$CarSvcElecKwh <- CarSvcFuelElecCo2_$CarSvcElecKwh
      SynPop..$CarSvcFuelCo2e <- CarSvcFuelElecCo2_$CarSvcFuelCo2e
      SynPop..$CarSvcElecCo2e <- CarSvcFuelElecCo2_$CarSvcElecCo2e
      #Clean up
      rm(CarSvcFuelElecCo2Rate_, CarSvcFuelElecCo2_)
      
      #Calculate household costs
      #-------------------------
      
      # Identify congestion price for metropolitan area if any
      CongPrice <- AveCongTaxPerMi
      # Identify the VmtSurcharge calculated to balance costs and revenues
      if( it == 1 ) {
        VmtSurcharge <- 0
      } else {
        VmtSurcharge <- VmtSurcharge.It[ it - 1 ]   # VmtSurcharge is value calculated in previous iteration
      }
      # Run household travel cost model  
      ModelVar. <- 
        c( "Dvmt", "FuelGallons", "FuelCo2e", "ElecCo2e", "ElecKwh", "DevType", 
           "Payd", "DailyPkgCost", "Hhvehcnt", "DepExp", "Hhincttl",
           "CarSvcFuelGal", "CarSvcFuelCo2e", "CarSvcElecCo2e", "CarSvcElecKwh",
           "CarSvcDvmt", "CarSvcBaseCost")
      Costs_ <- calcCosts( Data..=SynPop..[ , ModelVar. ], 
                           Costs.=Costs.YrCs[ yr, ], 
                           PaydRate=Payd..Yr[ "RatePerMile", yr ],
                           CongPrice=CongPrice,
                           VmtSurcharge=VmtSurcharge, 
                           ExtraModCost=ExtraModCost,
                           AVPkgDiscProp=HhVehOwnParm..["AVPkgDiscProp",yr] )
      rm( VmtSurcharge )	
      # Add selected cost data to household records
      SynPop..$FutrCostPerMi <- Costs_$FutrCostPerMi
      SynPop..$TotExtCost <- Costs_$TotExtCost
      SynPop..$HhTotCost <- Costs_$HhTotCost
      SynPop..$VehOwnExp <- Costs_$VehOwnExp
      
      # Add sums to DVMT and cost summary
      Dvmt.MdDt[ md, Dt] <- 
        tapply( SynPop..$Dvmt + SynPop..$CarSvcDvmt * (1 + SynPop..$CarSvcRepoProp), 
                SynPop..$DevType, sum, na.rm=TRUE )[Dt]
      Dvmt.MdDt[ is.na( Dvmt.MdDt ) ] <- 0
      CostSummary.MdVa[ md, "Dvmt" ] <- sum( Dvmt.MdDt[md,] )
      CostSummary.MdVa[ md, "FuelCost" ] <- sum( Costs_$FuelCost )  
      CostSummary.MdVa[ md, "PowerCost" ] <- sum( Costs_$PowerCost ) 
      CostSummary.MdVa[ md, "RoadUseTax" ] <- sum( Costs_$RoadUseTax )
      CostSummary.MdVa[ md, "CarbonTax" ] <- sum( Costs_$CarbonTax )
      CostSummary.MdVa[ md, "AddedExtTax" ] <- sum( Costs_$AddedExtTax )
      CostSummary.MdVa[ md, "PaydCost" ] <- sum( Costs_$PaydCost )
      CostSummary.MdVa[ md, "TotExtCost" ] <- sum( Costs_$TotExtCost )
      CostSummary.MdVa[ md, "HhTotCost" ] <- sum( Costs_$HhTotCost )
      CostSummary.MdVa[ md, "FutrCostPerMi" ] <- sum(Costs_$HhTotCost) / sum(Dvmt.MdDt[md,])
      CostSummary.MdVa[ md, "VehOwnExp" ] <- sum( Costs_$VehOwnExp )
      CostSummary.MdVa[ md, "TotRoadCost" ] <- sum( Costs_$TotRoadCost )
      rm( Costs_, ModelVar. )
      gc()
      
      
      #Step 4b: Calculate DVMT with new costs and reallocate to vehicles
      #=================================================================
      
      # Recalculate DVMT
      #-----------------
      PrevDvmt.Hh <- SynPop..$Dvmt
      ModelVar. <- c( "Dvmt", "CarSvcDvmt", "AuDvmt", "NAuDvmt", 
                      "BaseCostPerMi", "FutrCostPerMi", "LtVehAdjFactor",
                      "TdmLtVehAdjFactor", "Hhincttl", "CashOutIncAdj")
      Bgt_ <- 
        calcAdjAveDvmt( SynPop..[ , ModelVar. ], BudgetProp=BudgetProp, 
                        AnnVmtInflator=AnnVmtInflator, TrnstnProp=1 )
      SynPop..$Dvmt <- Bgt_$HhDvmt
      SynPop..$AuDvmt <- Bgt_$AuDvmt
      SynPop..$NAuDvmt <- Bgt_$NAuDvmt
      SynPop..$CarSvcDvmt <- Bgt_$CarSvcDvmt
      SynPop..$LtVehDvmt <- Bgt_$LtVehDvmt
      
      #Calculate 95th percentile and maximum DVMT
      #------------------------------------------
      SynPop..$MaxDvmt <- 0
      SynPop..$Dvmt95 <- 0
      MetroMax95th_ <- 
        predictMaxDvmt( SynPop..[ , c( "Dvmt", "MaxDvmt", "Dvmt95" ) ], DvmtLmModels_, "Metro" )
      SynPop..$MaxDvmt <- MetroMax95th_$DvmtMax
      SynPop..$Dvmt95 <- MetroMax95th_$Dvmt95th
      rm( MetroMax95th_ )
      gc()
      
      # Split adjusted DVMT among vehicles
      #-----------------------------------
      DvmtAdjFactor.Hh <- SynPop..$Dvmt / PrevDvmt.Hh
      HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
      ModelVar. <- c( "VehDvmt", "HcVehDvmt", "EvVehDvmt" )
      AdjDvmt_ <- allocateAdjDvmt( SynPop..[ HasVeh.Hh, ModelVar. ], DvmtAdjFactor.Hh[ HasVeh.Hh ] )
      SynPop..$VehDvmt[ HasVeh.Hh ] <- AdjDvmt_$VehDvmt
      SynPop..$EvVehDvmt[ HasVeh.Hh ] <- AdjDvmt_$EvVehDvmt
      SynPop..$HcVehDvmt[ HasVeh.Hh ] <- AdjDvmt_$HcVehDvmt
      rm( DvmtAdjFactor.Hh, HasVeh.Hh, ModelVar., AdjDvmt_ )
      gc()
      
      # Tabulate DVMT to use in congestion analysis
      #--------------------------------------------
      Dvmt.MdDt[md,Dt] <- 
        tapply( SynPop..$Dvmt + SynPop..$CarSvcDvmt * (1 + SynPop..$CarSvcRepoProp), 
                SynPop..$DevType, sum, na.rm=TRUE )[Dt]
      Dvmt.MdDt[ is.na( Dvmt.MdDt ) ] <- 0
      CostSummary.MdVa[ md, "AdjDvmt" ] <- sum( SynPop..$Dvmt, na.rm=TRUE )

      #Tabulate autonomous vehicle VMT
      #-------------------------------
      AuDvmt.MdDt[md,Dt] <- 
        tapply( SynPop..$AuDvmt + SynPop..$CarSvcDvmt * CarSvcIsAV, 
                SynPop..$DevType, sum, na.rm=TRUE )[Dt]        
      AuDvmt.MdDt[ is.na( AuDvmt.MdDt ) ] <- 0  

      # Save the household dataset
      #---------------------------
      Filename <- paste( RunYearDir, "/", md, ".RData", sep="" )
      save( SynPop.., file=Filename, compress=TRUE )
      rm( SynPop.. )
      gc()
      gc()
      print( memory.size() )
      
      # End iteration through metropolitan divisions
    }
    
    # Save the tabulations of Dvmt and Costs
    #---------------------------------------
    Filename <- paste( RunYearDir, "/", "Dvmt.MdDt", ".RData", sep="" )
    save( Dvmt.MdDt, file=Filename )
    rm( Filename )
    Filename <- paste( RunYearDir, "/", "AuDvmt.MdDt", ".RData", sep="" )
    save( AuDvmt.MdDt, file=Filename )
    rm( Filename )
    Filename <- paste( RunYearDir, "/", "CostSummary.MdVa", ".RData", sep="" )
    save( CostSummary.MdVa, file=Filename )
    rm( Filename )
    print( StartTime )
    print( Sys.time() )
    
    
    #Step 4c: Calculate Effects of Congestion
    #========================================
    
    local( {
      
      # Load data summaries
      #--------------------
      # Load income
      Filename <- paste( OutputDir, "/Inc.DtYr.RData", sep="" )
      Inc.DtYr <- assignLoad( Filename )
      # Load population
      Filename <- paste( OutputDir, "/Pop.DtYr.RData", sep="" )
      Pop.DtYr <- assignLoad( Filename )
      # Calculate population change from base year
      PopChangeRatio.Dt <- Pop.DtYr[ , yr ] / Pop.DtYr[ , BaseYear ]
      PopChangeRatio.Dt[ is.na( PopChangeRatio.Dt ) ] <- 0
      # Load metropolitan transportation summaries
      Filename <- paste( OutputDir, "/FwyLnMiCap.Yr.RData", sep="" )        
      FwyLnMiCap.Yr <- assignLoad( Filename )
      Filename <- paste( OutputDir, "/ArtLnMiCap.Yr.RData", sep="" )
      ArtLnMiCap.Yr <- assignLoad( Filename )
      Filename <- paste( OutputDir, "/TranRevMiCap.Yr.RData", sep="" )
      TranRevMiCap.Yr <- assignLoad( Filename )
      Filename <- paste( OutputDir, "/BusRevMi.Yr.RData", sep="" )
      BusRevMi.Yr <- assignLoad( Filename )
      Filename <- paste( OutputDir, "/RailRevMi.Yr.RData", sep="" )
      RailRevMi.Yr <- assignLoad( Filename )
      rm( Filename )
      # Load DVMT
      Filename <- paste( RunYearDir, "/", "Dvmt.MdDt", ".RData", sep="" )
      Dvmt.MdDt <- assignLoad( Filename )
      rm( Filename )
      
      # Calculate truck VMT for the metropolitan area
      #----------------------------------------------
      # Calculate growth in total percapita income from base year
      # Calculate change in income
      IncGrowth <- PerCapInc.Yr[yr] / PerCapInc.Yr[BaseYear]
      # Calculate truck DVMT
      TruckDvmt <- IncGrowth * TruckVmtGrowthMultiplier * BaseTruckVmt / 365
      # Allocate truck VMT to metropolitan areas for later congestion calculation
      TruckDvmt <- TruckDvmt * MpoBaseDvmtParm..Ma$PropTruckDvmt
      
      # Calculate bus DVMT for the metropolitan area
      #---------------------------------------------
      # Calculate bus DVMT
      BusDvmt <- BusRevMi.Yr[yr] * TranAdjFactor / 365
      
      # Calculate light vehicle DVMT for the metropolitan area
      #-------------------------------------------------------
      # Sum household light vehicle DVMT
      HhDvmt <- sum( Dvmt.MdDt )
      # Calculate commercial service vehicle DVMT
      if (CommVehDvmtMethod == "HouseholdIncome") {
        CommVehDvmt.MdDt <- calcCommVehTravelFromHhIncome(
          Dvmt.MdDt, CommVmtFactor, Inc.MdDtYr[,,yr])$CommVehDvmt.MdDt
      }
      if (CommVehDvmtMethod == "HouseholdDvmt") {
        CommVehDvmt.MdDt <- calcCommVehTravelFromHhDvmt(
          Dvmt.MdDt, CommVmtFactor)$CommVehDvmt.MdDt
      }
      CommVehDvmt <- sum( CommVehDvmt.MdDt )
      # Calculate total light vehicle DVMT that is on metropolitan area roadways
      LtVehDvmt <- ( HhDvmt + CommVehDvmt ) * LtVehDvmtFactor.Ma
      
      # Calculate total DVMT by type
      #-----------------------------
      Dvmt.Ty <- unlist(c( LtVehDvmt, TruckDvmt, BusDvmt ))
      names( Dvmt.Ty ) <- c( "LtVeh", "Truck", "Bus" ) 

      # Automated vehicle capacity expansion factor
      #--------------------------------------------
      FwyLnMiMult <- 
        calcFwyLnMiMult(sum(AuDvmt.MdDt) / sum(Dvmt.MdDt))

      # Initialize arrays to store results
      #-----------------------------------
      MpgMpkwhAdj.Pt <- numeric( length(Pt) ); names( MpgMpkwhAdj.Pt ) <- Pt
      VehHr.Ty <- numeric( length(Ty) ); names( VehHr.Ty ) <- Ty
      AveSpeed.Ty <- numeric( length(Ty) ); names( AveSpeed.Ty ) <- Ty
      FfVehHr.Ty <- numeric( length(Ty) ); names( FfVehHr.Ty ) <- Ty
      DelayVehHr.Ty <- numeric( length(Ty) ); names( DelayVehHr.Ty ) <- Ty
      CongVmt.ClFcTy <- array( 0, dim=c(length(Cl), length(Fc), length(Ty)), 
                               dimnames=list(Cl,Fc,Ty) )
      AveCongTaxPerMi <- 0

      # Calculate effects of congestion on speed and emissions
      #-------------------------------------------------------
      # Make an array of congestion prices
      CongPrice.ClFc <- array( 0, dim=c( length(Cl), length(Fc) ), dimnames=list( Cl, Fc ) )
      CongPrice.ClFc[ "Sev", "Fwy" ] <- CongPriceParm_Va..$FwySev[ 1, yr ]
      CongPrice.ClFc[ "Ext", "Fwy" ] <- CongPriceParm_Va..$FwyExt[ 1, yr ]
      CongPrice.ClFc[ "Sev", "Art" ] <- CongPriceParm_Va..$ArtSev[ 1, yr ]
      CongPrice.ClFc[ "Ext", "Art" ] <- CongPriceParm_Va..$ArtExt[ 1, yr ]
      
      # Calculate congestion results
      CongResults_ <- calcCongestion( 
        CongModel_ = CongModel_, 
        Dvmt.Ty = Dvmt.Ty, 
        PerCapFwy = FwyLnMiCap.Yr[ yr ] * FwyLnMiMult,                           
        PerCapArt = ArtLnMiCap.Yr[ yr ], 
        Pop = sum( Pop.DtYr[ , yr ] ),                                 
        BasePop = sum( Pop.DtYr[ , BaseYear ] ),
        FwyArtProp = MpoBaseDvmtParm..Ma[ Ma, "FwyArtProp" ], 
        BusVmtSplit.Fc = TruckBusFcDvmtSplit_Va..$BusVmt[ Ma, ], 
        TruckVmtSplit.Fc = TruckBusFcDvmtSplit_Va..$TruckVmt[ Ma, ],
        OpsDeployParm_Va.MaYr = OpsDeployParm_Va.MaYr, 
        SmoothEcoDriveParm_Va.. = SmoothEcoDriveParm_Va..,
        OtherOps_Yr.LvTy = OtherOps_Yr.LvTy, 
        CongPrice.ClFc = CongPrice.ClFc, 
        CongEfficiency.YrPt = CongEfficiency.YrPt, 
        ValueOfTime = ValueOfTime,
        ma=Ma )
      
      # Save the results
      #-----------------
      Filename <- paste( RunYearDir, "/", "CongResults_", ".RData", sep="" )
      save( CongResults_, file=Filename )
      
      # Return results to the enclosing environment
      #--------------------------------------------
      CommVehDvmt.MdDt <<- CommVehDvmt.MdDt
      CommVehDvmt <<- CommVehDvmt
      TruckDvmt <<- TruckDvmt
      BusDvmt <<- BusDvmt
      MpgMpkwhAdj.Pt <<- CongResults_$MpgMpkwhAdj.Pt
      AveCongTaxPerMi <<- CongResults_$AveCongTaxPerMi
      LtVehDvmt <<- LtVehDvmt
      HhRoadDvmt <<- HhDvmt * LtVehDvmtFactor.Ma
      Dvmt.Ty <<- Dvmt.Ty
      
    } )
    
    #Step 4d: Calculate Commercial Service Vehicle Fuel Consumption, Emissions, Costs
    #================================================================================
    
    # Store all the commercial service vehicle calculations in a list
    CommServ_ <- list()
    # Calculate CS DVMT by county and development type
    if (CommVehDvmtMethod == "HouseholdIncome") {
      CommServ_ <- c( CommServ_, calcCommVehTravelFromHhIncome(
        Dvmt.MdDt, CommVmtFactor, Inc.MdDtYr[,,yr]) )
    }
    if (CommVehDvmtMethod == "HouseholdDvmt") {
      CommServ_ <- c( CommServ_, calcCommVehTravelFromHhDvmt(
        Dvmt.MdDt, CommVmtFactor) )
    }
    # Calculate DVMT by vehicle type a vehicle age characteristics
    CommServ_ <- c( CommServ_, with( CommServ_, calcCommVehTypeAgeProp( 
      CommVehDvmt.MdDt, yr, CommServiceLtTruckProp.Yr,
      VehProp_$AgCumProp.AgTy, AgeAdj.YrTy ) ) )
    # Calculate the powertrain proportions, MPG and MPKWH
    CommServ_ <- c( CommServ_, with( CommServ_, calcCommVehPowertrainMpgMpkwh( 
      CommServicePtProp..Yr, CommServAutoAgProp.Ag, 
      CommServLtTruckAgProp.Ag, AutoLtTrkMpg..Yr,
      HevMpgProp..Yr, EvRangeProp..Yr, MpgMpkwhAdj.Pt ) ) )
    # Calculate DVMT powered by hydrocarbons vs. electricity            
    CommServ_ <- c( CommServ_, with( CommServ_, calcCommVehHcEvDvmt( 
      CommServAutoDvmt.MdDt, CommServAutoProp.Pt,
      CommServLtTruckDvmt.MdDt, CommServLtTruckProp.Pt ) ) )
    # Calculate daily electricity and power consumption
    CommServ_ <- c( CommServ_, with( CommServ_, calcFuelElectricityUse( 
      CommServAveAutoMpg, CommServAveLtTruckMpg,
      CommServAveAutoMpkwh, CommServAveLtTruckMpkwh,
      CommServAutoHcDvmt.MdDt, CommServAutoEvDvmt.MdDt,
      CommServLtTruckHcDvmt.MdDt, CommServLtTruckEvDvmt.MdDt ) ) ) 
    # Calculate average emissions per gallon of fuel consumed
    CommServ_ <- c( CommServ_, calcAveFuelCarbonIntensity(
      yr, CommServiceFuels..Yr, FuelCo2..Yr ) )
    # Calculate emissions                                        
    CommServ_ <- c( CommServ_, with( CommServ_, calcCommVehEmissions(
      yr, MjPerGallon, PowerCo2.DiYr,
      AveAutoFuelCo2, AveLtTruckFuelCo2,
      CommServAutoFuel.MdDt, CommServLtTruckFuel.MdDt,
      CommServAutoPower.MdDt, CommServLtTruckPower.MdDt ) ) )
    # Calculate commercial vehicle costs
    CommServ_ <- c( CommServ_, with( CommServ_, calcCommVehCosts( 
      yr, Costs.YrCs, it, 
      CommVehDvmt.MdDt, CommServAutoDvmt.MdDt, 
      CommServLtTruckDvmt.MdDt, CommServAutoFuel.MdDt, 
      CommServLtTruckFuel.MdDt, CommServAutoPower.MdDt, 
      CommServLtTruckPower.MdDt, CommServAutoHcCo2e.MdDt,
      CommServLtTruckHcCo2e.MdDt, CommServAutoEvCo2e.MdDt,
      CommServLtTruckEvCo2e.MdDt ) ) )
    # Calculate commercial auto and light truck emission rates by vehicle age
    CommServ_ <- c( CommServ_, with( CommServ_, calcCommVehEmissionRatesByAge( 
      yr, CommServAutoProp.AgPt, CommServLtTruckProp.AgPt,
      CommServAutoMpgMpkwh.AgPt, CommServLtTruckMpgMpkwh.AgPt,
      AveAutoFuelCo2, AveLtTruckFuelCo2, MjPerGallon, PowerCo2.DiYr,
      CommServAutoHcDvmt.MdDt, CommServAutoEvDvmt.MdDt,
      CommServLtTruckHcDvmt.MdDt, CommServLtTruckEvDvmt.MdDt ) ) ) 
    # Save the income factor for calculating commercial vehicle travel
    Filename <- paste( ModelDir, "/", "CommVehDvmtPerInc", ".RData", sep="" )
    CommServ_$CommVehDvmtPerInc <- assignLoad( Filename )
    # Save the results
    Filename <- paste( RunYearDir, "/", "CommServ_", ".RData", sep="" )
    save( CommServ_, file=Filename )
    
    #Step 4e: Calculate Total Costs and the VMT Surcharge to Pay for Infrastructure
    #==============================================================================
    
    # Calculate total costs
    #----------------------
    # Calculate total household road cost, adjusting for DVMT adjustment
    DvmtAdjRatio <- sum( CostSummary.MdVa[ , "AdjDvmt" ] ) / sum( CostSummary.MdVa[ , "Dvmt" ] )  
    TotHhRoadCost <- sum( CostSummary.MdVa[ , "TotRoadCost" ] ) * DvmtAdjRatio
    # Calculate light vehicle DVMT
    LtVehDvmt <- sum( CostSummary.MdVa[ , "AdjDvmt" ] ) + sum( CommServ_$CommVehDvmt.MdDt )
    # First iteration, calculate the extra modernization cost for new lanes (ExtraModCost)
    if( it == 1 ) {
      HvyVehDvmtEq <- TruckDvmt * CongModel_$Pce.Ty["Truck"] + sum( BusDvmt ) * CongModel_$Pce.Ty["Bus"]
      LtVehAddCostProp <- LtVehDvmt / ( LtVehDvmt + HvyVehDvmtEq ) 
      LnMiAddCost <- LtVehAddCostProp * AnnLnMiAddCosts.Yr[yr] / 365
      ExtraModCost <- LnMiAddCost / LtVehDvmt 
      TotRoadCost <- TotHhRoadCost + CommServ_$CommServCosts.[ "TotRoadCost" ] + LnMiAddCost
      rm( HvyVehDvmtEq, LtVehAddCostProp, LnMiAddCost )
      # Otherwise sum household & commercial vehicle costs because they include the added lane-mile costs 
    } else {
      TotRoadCost <- TotHhRoadCost + CommServ_$CommServCosts.[ "TotRoadCost" ]
    }
    
    # Calculate total revenues
    #-------------------------
    # Calculate total household revenues, adjusting for DVMT adjustment
    TotHhRoadUseTax <- sum( CostSummary.MdVa[ , "RoadUseTax" ] ) * DvmtAdjRatio
    # Add in estimated congestion tax if 1st iteration (since costs calculated before congestion tax)
    if( it == 1 ) {
      TotHhRoadUseTax <- TotHhRoadUseTax + sum( HhRoadDvmt * AveCongTaxPerMi )
    }			
    # Add in commercial light service vehicle
    TotRoadUseTax <- TotHhRoadUseTax + CommServ_$CommServCosts.["RoadUseTax"]
    
    # Compare total costs to revenues and calculate VMT surcharge to pay for system
    #------------------------------------------------------------------------------
    # This procedure calculates how much to increase a VMT tax to pay for system costs
    # It includes guards to keep the procedure from adding a negative surcharge that counteracts
    # VMT tax assumptions that are inputs to the model.
    # Calculate the gap between taxes and costs
    TaxGap <- TotRoadCost - TotRoadUseTax
    if( CalcVmtSurcharge ) {
      # If this is the first iteration, the tax gap per mile is added to the starting surcharge of 0
      if( it == 1 ) {
        # Calculate a VmtSurcharge only if there is a positive tax gap
        if( TaxGap > 0 ) {
          VmtSurcharge.It[ it ] <- TaxGap / LtVehDvmt
          # If the tax gap is negative, the VMT surcharge is zero
        } else {
          VmtSurcharge.It[ it ] <- 0
        }
        # If later iteration, add the calculated tax gap per mile to the previous surcharge
      } else {
        # If the VmtSurcharge for the previous iteration is positive, 
        # calculate the added surcharge for the iteration
        if( VmtSurcharge.It[ it - 1 ] > 0 ) { 
          VmtSurcharge.It[ it ] <- VmtSurcharge.It[ it - 1 ] +  TaxGap / LtVehDvmt
          # Otherwise the VmtSurcharge is zero
        } else {
          VmtSurcharge.It[ it ] <- 0
        }
      }
      # End calculation of VMT surcharge (if calculation is to be done)
    }				
    
    # Clean up
    #---------
    RoadCostSummary. <- c( TotRoadCost=TotRoadCost, TotRoadUseTax=TotRoadUseTax, TaxGap=TaxGap )
    Filename <- paste( RunYearDir, "/", "RoadCostSummary.", ".RData", sep="" )
    save( RoadCostSummary., file=Filename )
    rm( DvmtAdjRatio, TotHhRoadCost, LtVehDvmt, TotRoadCost, TotHhRoadUseTax, TotRoadUseTax )			
    gc()
    print( memory.size() )
    
    
    #Step 4f: Adjust MPG & MPKWH for ecodriving and low rolling resistance tires
    #===========================================================================
    
    # Only do this once, on the first iteration
    # Otherwise ecodriving would keep increasing MPG with each iteration
    if( it == 1 ) {
      
      StartTime <- Sys.time()
      print( paste( "Iteration", it, "Adjust MPG & MPKWH for ecodriving etc." ) )
      
      for( md in Md ) {
        
        print( md )
        
        # Load household file for metropolitan division
        Filename <- paste( RunYearDir, "/", md, ".RData", sep="" )
        SynPop.. <- assignLoad( Filename )
        rm( Filename )
        
        # Calculate adjustments
        HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
        ModelVar. <- c( "Houseid", "Hhvehcnt", "IsEcoDriver", "IsLowRollTire", "Powertrain", 
                        "VehMpg", "VehMpkwh" )
        
        # Calculate adjusted MPG and emissions
        MpgMpkwhAdj_ <- adjEcoTire( Data..=SynPop..[ HasVeh.Hh, ModelVar. ], 
                                    MpgMpkwhAdj.Pt=MpgMpkwhAdj.Pt, 
                                    TireMpgImp=EcoTire..Yr[yr,"TireMpgImp"], 
                                    TireMpkwhImp=EcoTire..Yr[yr,"TireMpkwhImp"] )
        rm( ModelVar. )
        
        # Assign to households
        SynPop..$VehMpg[ HasVeh.Hh ] <- MpgMpkwhAdj_$VehMpg_
        SynPop..$VehMpkwh[ HasVeh.Hh ] <- MpgMpkwhAdj_$VehMpkwh_
        rm( HasVeh.Hh, MpgMpkwhAdj_ )
        
        # Save the household dataset
        Filename <- paste( RunYearDir, "/", md, ".RData", sep="" )
        save( SynPop.., file=Filename, compress=TRUE )
        rm( SynPop.. )
        gc()
        gc()
        print( memory.size() )
        
      }
      
      print( StartTime )
      print( Sys.time() )
      
    }			
    
    Filename <- paste( RunYearDir, "/", "VmtSurcharge.It", ".RData", sep="" )
    save( VmtSurcharge.It, file=Filename )
    rm( Filename )
    # End of for loop to equilibrate DVMT, congestion, costs and road use taxes
  }
  
  
  #===========================================================================
  #STEP 5: CALCULATE METROPOLITAN AREA HEAVY VEHICLE CONSUMPTION AND EMISSIONS
  #===========================================================================
  
  # Calculate truck and bus age distributions
  #------------------------------------------
  # Calculate the truck age distribution
  TruckAgProp.Ag <- adjustHvyVehAgeDistribution( TruckBusAgeDist.AgTy[ , "Truck" ], 
                                                 AdjRatio=AgeAdj.YrTy[ yr, "Truck" ] )
  # Calculate bus age distribution
  BusAgProp.Ag <- adjustHvyVehAgeDistribution( TruckBusAgeDist.AgTy[ , "Bus" ], 
                                               AdjRatio=AgeAdj.YrTy[ yr, "Bus" ] )
  
  # Calculate truck and bus fuel economy
  #-------------------------------------
  # Calculate truck fuel economy
  TruckMpg <- assignHvyVehFuelEconomy( TruckAgProp.Ag, Mpg..Yr=HvyVehMpgMpk..Yr,
                                       Type="Truck_MPG", CurrYear=yr )
  # Calculate bus fuel economy
  BusMpg <- assignHvyVehFuelEconomy( BusAgProp.Ag, Mpg..Yr=HvyVehMpgMpk..Yr, Type="Bus_MPG", 
                                     CurrYear=yr )
  # Adjust fuel economy to account for congestion, eco-driving, etc for metropolitan areas
  TruckMpg <- MpgMpkwhAdj.Pt[ "TruckIce" ] * TruckMpg		
  BusMpg <- MpgMpkwhAdj.Pt[ "BusIce" ] * BusMpg
  
  # Calculate truck fuel consumption by fuel type
  #----------------------------------------------
  # Calculate overall fuel consumption
  TruckFuel <- Dvmt.Ty["Truck"] / TruckMpg
  rm( TruckMpg )
  # Calculate fuel consumption by type
  TruckFuelProp.Ft <- numeric(5)
  names( TruckFuelProp.Ft ) <- Ft
  TruckFuelInput. <- unlist( HvyTruckFuels..Yr[yr,] )
  PropDiesel <- 1 - TruckFuelInput.[ "PropGas" ] - TruckFuelInput.[ "PropCng" ]
  TruckFuelProp.Ft[ "ULSD" ] <- PropDiesel * ( 1 - TruckFuelInput.[ "DieselPropBio" ] )
  TruckFuelProp.Ft[ "Biodiesel" ] <- PropDiesel * ( TruckFuelInput.[ "DieselPropBio" ] )
  TruckFuelProp.Ft[ "Gasoline" ] <- ( TruckFuelInput.[ "PropGas" ] ) *
    ( 1 - TruckFuelInput.[ "GasPropEth" ] )
  TruckFuelProp.Ft[ "Ethanol" ] <- ( TruckFuelInput.[ "PropGas" ] ) *
    ( TruckFuelInput.[ "GasPropEth" ] )
  TruckFuelProp.Ft[ "CNG" ] <- ( TruckFuelInput.[ "PropCng" ] )
  TruckFuel.Ft <- TruckFuel * TruckFuelProp.Ft
  rm( TruckFuelInput., PropDiesel, TruckFuelProp.Ft )
  
  # Calculate Bus Fuel Consumption and Emissions
  #---------------------------------------------
  # Calculate overall fuel consumption
  BusPropFuel <- 1 - BusPropElectric.Yr[yr]
  BusFuel <- Dvmt.Ty["Bus"] * BusPropFuel / BusMpg
  # Calculate fuel consumption by type
  BusFuelProp.Ft <- numeric(5)
  names( BusFuelProp.Ft ) <- Ft
  BusFuelInput.Fp <- BusFuels.FpYr[,yr]
  PropDiesel <- 1 - BusFuelInput.Fp[ "PropGas" ] - BusFuelInput.Fp[ "PropCng" ]
  BusFuelProp.Ft[ "ULSD" ] <- PropDiesel * ( 1 - BusFuelInput.Fp[ "DieselPropBio" ] )
  BusFuelProp.Ft[ "Biodiesel" ] <- PropDiesel * ( BusFuelInput.Fp[ "DieselPropBio" ] )
  BusFuelProp.Ft[ "Gasoline" ] <- ( BusFuelInput.Fp[ "PropGas" ] ) *
    ( 1 - BusFuelInput.Fp[ "GasPropEth" ] )
  BusFuelProp.Ft[ "Ethanol" ] <- ( BusFuelInput.Fp[ "PropGas" ] ) *
    ( BusFuelInput.Fp[ "GasPropEth" ] )
  BusFuelProp.Ft[ "CNG" ] <- ( BusFuelInput.Fp[ "PropCng" ] )
  BusFuel.Ft <- BusFuel * BusFuelProp.Ft
  rm( BusMpg, BusFuelInput.Fp, PropDiesel )
  
  # Calculate emissions per gallon of fuel consumed
  #------------------------------------------------
  FuelCo2.Ft <- numeric(5)
  names( FuelCo2.Ft ) <- Ft
  FuelCo2Input. <- unlist( Inputs_$FuelCo2..Yr[ yr, ] )
  FuelCo2.Ft[ "ULSD" ] <- FuelCo2Input.[ "ULSD" ]
  FuelCo2.Ft[ "Biodiesel" ] <- FuelCo2Input.[ "Biodiesel" ]
  if( yr == "1990" ) {
    FuelCo2.Ft[ "Gasoline" ] <- FuelCo2Input.[ "RFG" ]
  } else {
    FuelCo2.Ft[ "Gasoline" ] <- FuelCo2Input.[ "CARBOB" ]
  }
  FuelCo2.Ft[ "Ethanol" ] <- FuelCo2Input.[ "Ethanol" ]
  FuelCo2.Ft[ "CNG" ] <- FuelCo2Input.[ "Cng" ]
  
  # Calculate truck and bus emissions
  #----------------------------------
  # Calculate truck emissions
  TruckMj.Ft <- TruckFuel.Ft * MjPerGallon
  TruckCo2e.Ft <- TruckMj.Ft * FuelCo2.Ft / 1000000
  TruckCo2e <- sum( TruckCo2e.Ft )
  rm( TruckMj.Ft, TruckCo2e.Ft )
  # Calculate bus emissions
  BusMj.Ft <- BusFuel.Ft * MjPerGallon
  BusHcCo2e <- sum( BusMj.Ft * FuelCo2.Ft / 1000000 )
  rm( BusMj.Ft, FuelCo2.Ft )
  
  # Calculate bus and rail emissions from electricity consumption
  #--------------------------------------------------------------
  # Calculate DVMT and power consumed
  RailDvmt <- RailRevMi.Yr[yr] * TranAdjFactor / 365
  RailPower <- RailDvmt / HvyVehMpgMpk..Yr[yr,"Train_MPkWh"]
  BusEvDvmt <- Dvmt.Ty["Bus"] * BusPropElectric.Yr[yr]
  BusPower <- BusEvDvmt / HvyVehMpgMpk..Yr[yr,"Bus_MPkWh"]
  RailBusPower <- RailPower + BusPower
  rm(RailDvmt, RailPower, BusEvDvmt, BusPower)
  # Calculate average emissions per kwh
  PowerCo2 <- sum( PowerCo2.DiYr[,yr] * rowSums( Pop.DiDtYr[,,yr] ) ) / sum( Pop.DiDtYr[,,yr] )
  # Calculate total emissions by metropolitan area
  RailBusEvCo2e <- RailBusPower * PowerCo2 / 2204.62262
  rm( PowerCo2 )
  
  # Save the results
  #-----------------
  Filename <- paste( RunYearDir, "/", "TruckFuel.Ft", ".RData", sep="" )
  save( TruckFuel.Ft, file=Filename )
  Filename <- paste( RunYearDir, "/", "BusFuel.Ft", ".RData", sep="" )
  save( BusFuel.Ft, file=Filename )
  Filename <- paste( RunYearDir, "/", "TruckCo2e", ".RData", sep="" )
  save( TruckCo2e, file=Filename )
  Filename <- paste( RunYearDir, "/", "BusHcCo2e", ".RData", sep="" )
  save( BusHcCo2e, file=Filename )
  Filename <- paste( RunYearDir, "/", "RailBusPower", ".RData", sep="" )
  save( RailBusPower, file=Filename )
  Filename <- paste( RunYearDir, "/", "RailBusEvCo2e", ".RData", sep="" )
  save( RailBusEvCo2e, file=Filename )
  rm( TruckFuel.Ft, BusFuel.Ft, TruckCo2e, BusHcCo2e, RailBusPower, RailBusEvCo2e )
  
  
  #=========================
  #END THE LOOP FOR THE YEAR
  #=========================
  
  # Clean up
  gc()
  
}

