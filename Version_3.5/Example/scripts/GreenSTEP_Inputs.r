#GreenSTEP_Inputs.r
#==================
#Regional Strategic Planning Model (RSPM) GreenSTEP Version
#Copyright 2009 - 2015, Oregon Department of Transportation
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
#http://www.apache.org/licenses/LICENSE-2.0
#Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
#Version: 3.5
#Date: 6/22/15

#Description
#===========

#This scripts loads all of the model objects and data needed to run the GreenSTEP model.

#Changes in version 3.0
#----------------------

#4/26/13 BG -  This version uses several new abbreviations relevant to the metropolitan model. Md means metropolitan district and refers to large subdivisions of the metropolitan area. Di means districts and refers to small subdivisions of the metropolitan area (e.g. census tract). Names have been changed to refer to metropolitan divisions and districts as appropriate and to eliminate references to county which are a holdover from the statewide version.

#7/31/13 BG - Removed legacy inputs that are no longer used. Also moved some of the data stored in Inputs_ to Model_.


#Define naming vectors
#=====================

Abbr_ <- list()

# Geographic abbreviations
setwd( ModelDir )
Filename <- "district_groups.csv"
DistrictGroups.. <- read.csv( Filename, as.is=TRUE )
setwd( RunDir )

# District names
Abbr_$Di <- DistrictGroups..$District

# Metropolitan division names
Abbr_$Md <- sort( unique( DistrictGroups..$Division ) )

# Metropolitan area names
Abbr_$Ma <- sort( unique( DistrictGroups..$Msa ) )
rm( Filename, DistrictGroups.. )

# Categories for the age of persons
Abbr_$Ap <- c( "Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus" )

# Categories for urban types ( metropolitan vs. other urban )
Abbr_$Ut <- c( "Metropolitan", "Town" )

# Categories for all development types ( metropolitan, town, rural )
Abbr_$Dt <- c( "Metropolitan", "Town", "Rural" )

# Categories for housing types ( single family, mobile home, 5+ unit apartments, 2-4 unit apartments, single family attached )
Abbr_$Ht <- c( "A24", "A5P", "SFA", "SFD", "MH" )

# Income group
Abbr_$Ig <- c( "0to20K", "20Kto40K", "40Kto60K", "60Kto80K", "80Kto100K", "100KPlus" )

# Fuel Type
Abbr_$Ft <- c( "ULSD", "Biodiesel", "Gasoline", "Ethanol", "CNG" )

# Congestion levels
Abbr_$Cl <- c( "None", "Mod", "Hvy", "Sev", "Ext" )

# Types of vehicles
Abbr_$Ty <- c("LtVeh","Truck","Bus")

# Functional class of roadways
Abbr_$Fc <- c("Fwy","Art","Other")

# Powertrain types
Abbr_$Pt <- c( "LdIceEco", "LdIceNonEco", "LdHev", "LdFcv", "LdEv", "TruckIce", "TruckEv", "BusIce", "BusEv" )

attach( Abbr_ )


#Read in global values
#=====================
#The global_values.txt file contains various parameters used by the model.

source( paste( ModelDir, "/global_values.txt", sep="" ) )


#Load estimated models and functions
#===================================
#The submodels of GreenSTEP are combinations of data and functions. For example, the submodel that adjusts household income distributions based on changes in the distribution of the household age structure consists of a matrix of probabilities and a function to calculate income adjustments based on the matrix of probabilities and other inputs. All of the submodels are contained in one .RData file that is composed of a list containing the data and functions for the submodels. This list is attached to the workspace so that each submodel can be called using it's individual name.

# Load the estimated model
setwd( ModelDir )
load( "GreenSTEP_.RData" )
# Change the congestion model Lambda values to the input parameters
GreenSTEP_$CongModel_$Lambda.Ma <- Lambda.Ma
# Attach GreenSTEP_ and reset current directory
attach( GreenSTEP_ )
setwd( RunDir )


#Load model starting inventory data
#==================================
#The starting inventories for the model are also contained in the model directory. Once finalized, this data should not change because it reflects existing conditions.

setwd( ModelDir )

Model_ <- list()

# Base year freeway and arterial lane miles by metropolitan area
Filename <- "lane_miles.csv"
TempInput.. <- read.csv(Filename)
Model_$BaseFwyLnMi <- unlist(TempInput..["Fwy"])
Model_$BaseArtLnMi <- TempInput..[,"Art"]
rm( Filename, TempInput.. )

# Crosswalk between districts, divisions, and metropolitan areas
Filename <- "district_groups.csv"
DistrictGroups.. <- read.csv( Filename, as.is=TRUE, row.names=1 )
Model_$DistrictGroups.. <- DistrictGroups..
rm( Filename, DistrictGroups.. )

# Load group household auto ownership rates
Filename <- paste( "group_auto_ownership.csv", sep="" )
TempInput.. <- read.csv( Filename )
Va <- TempInput..$Value
GroupAutoOwnership_Va.. <- split( TempInput..[,-1], Va )
GroupAutoOwnership_Va.. <- lapply( GroupAutoOwnership_Va.., function(x) {
  RowNames. <- x[,1]
  Parm.. <- x[,-1]
  rownames( Parm.. ) <- RowNames.
  ColNames. <- colnames( Parm.. )
  ColNames. <- gsub( "X", "", ColNames. )
  colnames( Parm.. ) <- ColNames.
  Parm.. } )
Model_$GroupAutoOwnership_Va.. <- GroupAutoOwnership_Va..
rm( Filename, TempInput.., Va, GroupAutoOwnership_Va.. )

# Factor to convert metropolitan household DVMT to metropolitan light vehicle road DVMT
Filename <- "hh_dvmt_to_road_dvmt.csv"
TempInput.. <- read.csv( Filename )
LtVehDvmtFactor.Ma <- TempInput..[,2]
names( LtVehDvmtFactor.Ma ) <- TempInput..[,1]
Model_$LtVehDvmtFactor.Ma <- LtVehDvmtFactor.Ma[ Ma ]
rm( Filename, TempInput.., LtVehDvmtFactor.Ma )

# Proportions of households in districts living in urban mixed use neighborhoods
Filename <- "land_forecasts/metropolitan_urban_type_proportions.csv"
TempInput.. <- read.csv( Filename )
Model_$UrbanTypeProp.DiYr <- TempInput..[,2:ncol(TempInput..)]
colnames( Model_$UrbanTypeProp.DiYr ) <- gsub("X", "", colnames( Model_$UrbanTypeProp.DiYr ))
rownames( Model_$UrbanTypeProp.DiYr ) <- TempInput..[,1]
rm( Filename )

# Base year parameters for metropolitan light vehicle DVMT, truck DVMT proportions,
# and proportions of DVMT on freeways and arterials
Filename <- "mpo_base_dvmt_parm.csv"
MpoBaseDvmtParm..Ma <- read.csv( Filename, row.names=1 )
Model_$MpoBaseDvmtParm..Ma <- MpoBaseDvmtParm..Ma[ Ma, ]
rm( Filename, MpoBaseDvmtParm..Ma )

# Statewide real per capita income
Filename <- "per_cap_inc.csv"
TempInput.. <- read.csv( Filename )
Model_$PerCapInc.Yr <- TempInput..$Income
names( Model_$PerCapInc.Yr ) <- TempInput..$Year 
rm( Filename, TempInput.. )

# Population targets for household size and for 1-person households
Filename <- "pop_targets.csv"
TempInput.. <- read.csv( Filename )
Va <- TempInput..$Value
PopTargets_Va.. <- split( TempInput..[,-1], Va )
PopTargets_Va.. <- lapply( PopTargets_Va.., function(x) {
  RowNames. <- x[,1]
  Parm.. <- x[,-1]
  rownames( Parm.. ) <- RowNames.
  ColNames. <- colnames( Parm.. )
  ColNames. <- gsub( "X", "", ColNames. )
  colnames( Parm.. ) <- ColNames.
  Parm..
} )
Model_$PopTargets_Va.. <- PopTargets_Va..
rm( Filename, TempInput.., Va, PopTargets_Va.. )

# Regional proportion of statewide real per capita income
Filename <- "regional_inc_prop.csv"
Model_$IncomeProp.YrMd <- as.matrix( read.csv( Filename, row.names=1 ) )
rm( Filename )

# Travel demand management parameters for the proportional reduction in VMT by program area
Filename <- "tdm_parameters.csv"
Temp.. <- read.csv( Filename )
Model_$TdmParm. <- Temp..$Value
names( Model_$TdmParm. ) <- Temp..$Parm
rm( Filename, Temp.. )

# Base year metropolitan transit revenue miles by type and metropolitan area
Filename <- "transit_revenue_miles.csv"
TempInput.. <- read.csv( Filename, row.names=1 )
Model_$BaseTranRevMi.. <- TempInput..
rm( Filename, TempInput.. )

# Proportions of truck and bus DVMT by functional class
Filename <- "truck_bus_fc_dvmt_split.csv"
TempInput.. <- read.csv( Filename )
Va <- TempInput..$Value
TruckBusFcDvmtSplit_Va.. <- split( TempInput..[,-1], Va )
TruckBusFcDvmtSplit_Va.. <- lapply( TruckBusFcDvmtSplit_Va.., function(x) {
  RowNames. <- x[,1]
  Parm.. <- x[,-1]
  rownames( Parm.. ) <- RowNames.
  ColNames. <- colnames( Parm.. )
  ColNames. <- gsub( "X", "", ColNames. )
  colnames( Parm.. ) <- ColNames.
  Parm..
} )
Model_$TruckBusFcDvmtSplit_Va.. <- TruckBusFcDvmtSplit_Va..
rm( Filename, TempInput.., Va, TruckBusFcDvmtSplit_Va.. )

# Set the working directory back to the run directory
setwd( RunDir )

attach( Model_ )

#Load the policy inputs
#======================

# Not necessary to load scenario inputs if only household synthesis is to be done
if( RunTravel | RunOutputs ) {
  
  setwd( InputDir )
  
  Inputs_ <- list()
  
  # Factors for adjusting 95 percentile age of vehicles
  Filename <- "age_adj.csv"
  Inputs_$AgeAdj.YrTy <- as.matrix( read.csv( Filename, row.names=1 ) )
  rm( Filename )
  
  # Auto and light-truck fuel type mix
  Filename <- "auto_lighttruck_fuel.csv"
  Inputs_$AutoLtTrkFuels..Yr <- read.csv( Filename, row.names=1 )
  rm( Filename )
  
  # Household light duty vehicle MPG & electric power consumption (MPKwh) by vehicle year and type
  Filename <- "auto_lighttruck_mpg.csv"
  Inputs_$AutoLtTrkMpg..Yr <- read.csv( Filename, row.names=1 )
  rm( Filename )
  
  #Bus fuel type mix, hydrocarbon by type and electric
  Filename <- "bus_fuels.csv"
  TempInput.. <- read.csv( Filename, row.names=1, as.is=TRUE )
  names(TempInput..) <- gsub("X", "", names(TempInput..))
  Inputs_$BusPropElectric.Yr <- unlist(TempInput..["PropElectric", ])
  Fp <- c( "PropGas", "PropCng", "DieselPropBio", "GasPropEth" )
  Inputs_$BusFuels.FpYr <- as.matrix(TempInput..[Fp, ])
  rm( Filename, TempInput.. )
  
  # Carshare input parameters
  Filename <- "carshare.csv"
  CarshareRates.DiYr <- as.matrix( read.csv( Filename, row.names=1 ) )
  colnames(CarshareRates.DiYr) <- gsub("X", "", colnames(CarshareRates.DiYr))
  Inputs_$CarshareRates.DiYr <- CarshareRates.DiYr[Di,Yr]
  rm(Filename, CarshareRates.DiYr)
  
  # Commercial service vehicle fuels
  Filename <- "comm_service_fuel.csv"
  Inputs_$CommServiceFuels..Yr <- read.csv( Filename, row.names=1 )
  rm( Filename )
  
  # Commercial service vehicle light truck proportions
  Filename <- "comm_service_lttruck_prop.csv"
  TempInput.. <- read.csv( Filename, as.is=TRUE )
  CommServiceLtTruckProp.Yr <- TempInput..$LtTruckProp
  names( CommServiceLtTruckProp.Yr ) <- TempInput..$Year
  Inputs_$CommServiceLtTruckProp.Yr <- CommServiceLtTruckProp.Yr
  rm( Filename, TempInput.., CommServiceLtTruckProp.Yr )
  
  # Commercial service vehicle powertrain proportions
  Filename <- "comm_service_pt_prop.csv"
  Inputs_$CommServicePtProp..Yr <- read.csv( Filename, row.names=1 )
  rm( Filename )
  
  # Congestion pricing input parameters
  Filename <- "congestion_charges.csv"
  TempInput.. <- read.csv( Filename )
  Va <- TempInput..$Value
  CongPriceParm_Va.. <- split( TempInput..[,-1], Va )
  CongPriceParm_Va.. <- lapply( CongPriceParm_Va.., function(x) {
    RowNames. <- x[,1]
    Parm.. <- x[,-1]
    rownames( Parm.. ) <- RowNames.
    ColNames. <- colnames( Parm.. )
    ColNames. <- gsub( "X", "", ColNames. )
    colnames( Parm.. ) <- ColNames.
    Parm..
  } )
  Inputs_$CongPriceParm_Va.. <- CongPriceParm_Va..
  rm( Filename, TempInput.., Va, CongPriceParm_Va.. )
  
  # Congestion efficiency of vehicles by year (Yr) and powertrain (Pt)
  Filename <- "cong_efficiency.csv"
  Inputs_$CongEfficiency.YrPt <- as.matrix( read.csv( Filename, row.names=1 ) )
  rm( Filename )
  
  # Costs for fuel, electricity and optional VMT-based charges
  Filename <- "costs.csv"
  Costs..Yr <- read.csv( Filename, row.names=1 )
  Costs.YrCs <- as.matrix( Costs..Yr )
  Inputs_$Costs.YrCs <- Costs.YrCs
  rm( Filename, Costs.YrCs )
  
  # Eco-driving and low rolling-resistance tire inputs
  Filename <- "eco_tire.csv"
  TempInput.. <- read.csv( Filename, row.names=1 )
  Inputs_$EcoTire..Yr <- TempInput..
  rm( Filename, TempInput.. )
  
  # Electric vehicle range and proportion of VMT in range traveled by electricity
  Filename <- "ev_characteristics.csv"
  Inputs_$EvRangeProp..Yr <- read.csv( Filename, row.names=1 )
  rm( Filename )
  
  # Fuel types and carbon emissions by vehicle type
  Filename <- "fuel_co2.csv"
  Inputs_$FuelCo2..Yr <- read.csv( Filename, row.names=1 )
  rm( Filename )
  
  #Freeway and arterial lane miles as a proportion of base year lane miles
  Filename <- "lane_mile_growth.csv"
  TempInput.. <- read.csv(Filename, row.names=1)
  names(TempInput..) <- gsub("X", "", names(TempInput..))
  Inputs_$FreewayGrowth.Yr <- unlist(TempInput..["Fwy",])
  Inputs_$ArterialGrowth.Yr <- unlist(TempInput..["Art",])
  rm(Filename, TempInput..)
  
  # Heavy truck fuel type mix
  Filename <- "heavy_truck_fuel.csv"
  Inputs_$HvyTruckFuels..Yr <- read.csv( Filename, row.names=1 )
  rm( Filename )
  
  # Hybrid electric vehicle characteristics
  Filename <- "hev_characteristics.csv"
  Inputs_$HevMpgProp..Yr <- read.csv( Filename, row.names=1 )
  rm( Filename )
  
  #Heavy vehicle MPG & power consumption by vehicle year and type:
  Filename <- "hvy_veh_mpg_mpk.csv"
  Inputs_$HvyVehMpgMpk..Yr <- read.csv( Filename, row.names=1 )
  rm( Filename )
  
  # Proportion of households participating in individualized marketing program (IMP)
  Filename <- "imp_prop_goal.csv"
  ImpPropGoal.DiYr <- as.matrix( read.csv( Filename, row.names=1 ) )
  colnames( ImpPropGoal.DiYr ) <- gsub( "X", "", colnames( ImpPropGoal.DiYr ) )
  Inputs_$ImpPropGoal.DiYr <- ImpPropGoal.DiYr
  rm( Filename, ImpPropGoal.DiYr )
  
  # Light weight vehicles input parameters
  Filename <- "light_vehicles.csv"
  TempInput.. <- read.csv( Filename, row.names=1 )
  Inputs_$LtVehParm_Va.. <- list()
  Vars. <- c( "OwnRatio", "Threshold", "PropSuitable" )
  for( va in Vars. ) {
    Inputs_$LtVehParm_Va..[[va]] <- TempInput..[, grep( va, colnames( TempInput.. ) ) ]
    Prefix <- paste( va, ".", sep="" )
    colnames( Inputs_$LtVehParm_Va..[[va]] ) <- gsub( Prefix, "", colnames( Inputs_$LtVehParm_Va..[[va]] ) )
  }
  rm( Filename, TempInput.., Vars. )
  
  # Proportions of vehicles that are light trucks
  Filename <- "lttruck_prop.csv"
  TempInput.. <- read.csv( Filename )
  Inputs_$LtTruckProp.MdYr <- TempInput..[,2:ncol(TempInput..)]
  colnames( Inputs_$LtTruckProp.MdYr ) <- gsub("X", "", colnames( Inputs_$LtTruckProp.MdYr ))
  rownames( Inputs_$LtTruckProp.MdYr ) <- TempInput..[,1]
  rm( Filename, TempInput.. )
  
  # Operations program deployment inputs
  Filename <- "ops_deployment.csv"
  TempInput.. <- read.csv( Filename )
  Va <- TempInput..$Value
  OpsDeployParm_Va.. <- split( TempInput..[,-1], Va )
  OpsDeployParm_Va.MaYr <- lapply( OpsDeployParm_Va.., function(x) {
    RowNames. <- x[,1]
    Parm.. <- x[,-1]
    rownames( Parm.. ) <- RowNames.
    ColNames. <- colnames( Parm.. )
    ColNames. <- gsub( "X", "", ColNames. )
    colnames( Parm.. ) <- ColNames.
    Parm..
  } )
  Inputs_$OpsDeployParm_Va.MaYr <- OpsDeployParm_Va.MaYr
  rm( Filename, TempInput.., Va, OpsDeployParm_Va.., OpsDeployParm_Va.MaYr )
  
  # Proportions of households who optimize
  Filename <- "optimize.csv"
  TempInput.. <- read.csv( Filename )
  Inputs_$OptimProp.Yr <- TempInput..$OptimProp
  names( Inputs_$OptimProp.Yr ) <- TempInput..$Year
  rm( Filename, TempInput.. )
  
  # Other operations deployment inputs
  Filename <- "other_ops.csv"
  TempInput_ <- as.list( read.csv( Filename, as.is=TRUE ) )
  Ty <- TempInput_[[1]][ c(1,5,9,13) ]
  Lv <- TempInput_[[2]][ 1:4 ]
  OtherOps_Yr.LvTy <- lapply( TempInput_[ 3:length( TempInput_ ) ], function(x) {
    array( x, dim=c( length(Lv), length(Ty) ), dimnames=list( Lv, Ty ) ) } )
  names( OtherOps_Yr.LvTy ) <- gsub( "X", "", names( OtherOps_Yr.LvTy ) )
  Inputs_$OtherOps_Yr.LvTy <- OtherOps_Yr.LvTy
  rm( Filename, TempInput_, OtherOps_Yr.LvTy, Ty, Lv )                   
  
  # Parking pricing
  Filename <- "parking.csv"
  TempInput.. <- read.csv( Filename )
  Va <- TempInput..$Value
  PkgParm_Va.. <- split( TempInput..[,-1], Va )
  PkgParm_Va.. <- lapply( PkgParm_Va.., function(x) {
    RowNames. <- x[,1]
    Parm.. <- x[,-1]
    rownames( Parm.. ) <- RowNames.
    ColNames. <- colnames( Parm.. )
    ColNames. <- gsub( "X", "", ColNames. )
    colnames( Parm.. ) <- ColNames.
    Parm..
  } )
  Inputs_$PkgParm_Va.. <- PkgParm_Va..
  rm( Filename, TempInput.., Va, PkgParm_Va.. )
  
  # PAYD input parameters
  Filename <- "payd.csv"
  TempInput.. <- read.csv( Filename, row.names=1 )
  Inputs_$Payd..Yr <- TempInput..
  rm( Filename, TempInput.. )
  
  # Load the plug in hybrid vehicle data
  Filename <- "phev_characteristics.csv"
  Inputs_$PhevRangeProp..Yr <- read.csv( Filename, row.names=1 )
  rm( Filename )
  
  # Carbon emissions per KWH
  Filename <- "power_co2.csv"
  PowerCo2.DiYr <- as.matrix( read.csv( Filename, row.names=1 ) )
  colnames( PowerCo2.DiYr  ) <- gsub( "X", "", colnames( PowerCo2.DiYr ) )
  Inputs_$PowerCo2.DiYr <- PowerCo2.DiYr
  rm( Filename, PowerCo2.DiYr )
  
  # Proportion of workers at employers with strong employee commute options (ECO) program
  Filename <- "prop_wrk_eco.csv"
  PropWrkEco.MdYr <- as.matrix( read.csv( Filename, row.names=1 ) )
  colnames( PropWrkEco.MdYr ) <- gsub( "X", "", colnames( PropWrkEco.MdYr ) )
  Inputs_$PropWrkEco.MdYr <- PropWrkEco.MdYr
  rm( Filename, PropWrkEco.MdYr )
  
  # Speed smoothing and ecodriving inputs
  Filename <- "speed_smooth_ecodrive.csv"
  TempInput.. <- read.csv( Filename )
  Va <- TempInput..$Value
  SmoothEcoDriveParm_Va.. <- split( TempInput..[,-1], Va )
  SmoothEcoDriveParm_Va.. <- lapply( SmoothEcoDriveParm_Va.., function(x) {
    RowNames. <- x[,1]
    Parm.. <- x[,-1]
    rownames( Parm.. ) <- RowNames.
    ColNames. <- colnames( Parm.. )
    ColNames. <- gsub( "X", "", ColNames. )
    colnames( Parm.. ) <- ColNames.
    Parm..
  } )
  Inputs_$SmoothEcoDriveParm_Va.. <- SmoothEcoDriveParm_Va..
  rm( Filename, TempInput.., Va, SmoothEcoDriveParm_Va.. )
  
  #Per capita transit growth and proportion that is rail
  Filename <- "transit_growth.csv"
  TempInput.. <- read.csv( Filename, row.names=1 )
  names(TempInput..) <- gsub("X", "", names(TempInput..))
  Inputs_$TransitParm.. <- TempInput..
  rm( Filename, TempInput.. )
  
  setwd( RunDir )
  attach( Inputs_ )
  
  # End if statement !OnlySynthesizeHh
}