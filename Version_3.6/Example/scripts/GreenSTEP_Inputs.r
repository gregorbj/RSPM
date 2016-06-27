#GreenSTEP_Inputs.r
#==================
#Regional Strategic Planning Model (RSPM) GreenSTEP Version
#Copyright 2016, Brian Gregor
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use
#this file except in compliance with the License. You may obtain a copy of the
#License at
#http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or
#agreed to in writing, software distributed under the License is distributed on
#an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
#or implied.  See the License for the specific language governing permissions
#and limitations under the License.
#Version: 3.6
#Date: 6/25/16


#Description
#===========

#This scripts loads all of the model objects and data needed to run the RSPM
#model version 3.6. This is a complete rewrite of the process of loading model
#parameters and scenario inputs. Several functions were written to manage the
#loading and processing of model and input data in a standardized way. This
#greatly reduced code redundancy that was in the earlier script.

#In addition, functionality was added to adjust monetary inputs to a consistent
#year. Previously, the model was written so that all monetary values had to be
#expressed in 2005 dollar values. This reflected the GHG mitigation planning
#approach in Oregon and was a year close to the year of the NHTS model
#estimation data (2001). There were several limitations to the previous
#approach. First, inputs denominated in 2005 dollars, while close to 2001 values
#are not equal to 2001 values, so there is some inconsistency in values. Second,
#the logic of using a 2005 dollar year becomes obscure as time goes on and as
#the model is applied outside of Oregon. Third, it placed a burden on the user
#to deflate all inputs to 2005 values. The new processes handles all of this in
#code. The model user is responsible for specifying the following 3 data items
#and the model handles the money conversions to a consistent year:
#1. The 'BaseCostYear' which is the year of the data used in model estimation.
## Since the current version of the RSPM used the 2001 NHTS, the BaseCostYear is
## 2001. This value is specified in the 'global_values.txt' file.
#2. The 'dollar-year' for each data item that is a monetary value. For
## example, if the starting year (i.e. the base year) for a modeling study is
## 2015 and if parking costs were collected for that year and all future parking
## cost assumptions are expressed in those terms, then the 'dollar-year' is
## 2015. Every input file that contains monetary values must include a column
## named 'CostDataYear' which identifies the 'dollar-year' for the money values.
## This approach allows users to combine data from different sources that are
## denominated in different year values.
#3. A consumer price index file which specifies the consumer price index by
## year covering the range of all years that dollar values need to be converted
## between. This file is named 'cpi.txt' and it specifies the year in the first
## column and the corresponding consumer price index in the second column.


#Define functions used in script
#===============================

#Function to load the consumer price index table
#-----------------------------------------------
loadCpiTable <- function(CpiFilePath) {
  Input.. <- read.csv(CpiFilePath)
  Cpi.Yr <- Input..$CPI
  names(Cpi.Yr) <- Input..$Year
  Cpi.Yr
}

#Function to adjust a vector or array of money values between years
#------------------------------------------------------------------
adjMoneyVal <- function(Value, InYear, OutYear, Cpi.Yr) {
  if (!is.array(Value)) {
    InputLengths. <- c(length(Value), length(InYear), length(OutYear))
    OkLengths. <- c(1, max(InputLengths.))
    if (!all(InputLengths. %in% OkLengths.)) {
      stop("The lengths of Value, InYear, and OutYear can only be 1 or the length of the longest one.")
    }
  } else {
    if ((length(InYear) != 1) | (length(OutYear) != 1)) {
      stop("When Value is a matrix or array, InYear and OutYear must be single values.")
    }
    
  }
  InYear <- as.character(InYear)
  OutYear <- as.character(OutYear)
  Value * Cpi.Yr[OutYear] / Cpi.Yr[InYear]
}

#Function to adjust money values in a data frame created from loading
#a properlyformatted input file that identifies the 'DataCostYear'
#-----------------------------------------------------------------
adjMoneyValDF <- function(DF.., InYear., OutYear, Cpi.Yr) {
  apply(DF.., 2, function(x) {
    Vals. <- x[!is.na(InYear.)]
    DataYr. <- InYear.[!is.na(InYear.)]
    AdjVals. <- adjMoneyVal(Vals., DataYr., OutYear, Cpi.Yr)
    x[!is.na(InYear.)] <- AdjVals.
    x
  })
}

#Function to read in an input file and create a specified data structure
#-----------------------------------------------------------------------
readDataFile <- 
  function(FilePath, HasRowNameCol = FALSE, 
           ConvertYearNames = TRUE, OrderRowsBy = NA, OutputAs = "DataFrame", 
           ValueNames = NA, ConvertMoney = FALSE, OutYear = NA) {
    #Read file
    Inp.. <- read.csv(FilePath)
    #Set column names so that year names are not altered
    ColNames. <- unlist(strsplit(readLines(FilePath)[1], ","))
    names(Inp..) <- ColNames.
    #Convert money values 
    if (ConvertMoney) {
      if (is.na(OutYear)) {
        stop("In order to convert money values an OutYear must be provided.")
      } else {
        if (!is.numeric(as.numeric(OutYear))) {
          stop("OutYear must be a 4 digit year.")
        }
        if (!("CostDataYear" %in% ColNames.)) {
          stop("Can't convert money when missing 'CostDataYear' in data file.")
        }
        InYear. <- Inp..$CostDataYear
        Inp.. <- Inp..[, -ncol(Inp..)]
        if (HasRowNameCol & (OutputAs != "DataFrameList")) {
          Inp..[,-1] <- adjMoneyValDF(Inp..[,-1], InYear. = InYear., OutYear = OutYear, 
                                      Cpi.Yr = Model_$Cpi.Yr)
        } 
        if (HasRowNameCol & (OutputAs == "DataFrameList")) {
          Inp..[,-(1:2)] <- adjMoneyValDF(Inp..[,-(1:2)], InYear. = InYear., OutYear = OutYear, 
                                          Cpi.Yr = Model_$Cpi.Yr)               
        }
        if (!HasRowNameCol & (OutputAs == "DataFrameList")){
          Inp..[,-1] <- adjMoneyValDF(Inp..[,-1], InYear. = InYear., OutYear = OutYear, 
                                      Cpi.Yr = Model_$Cpi.Yr)        
        }
        if (!HasRowNameCol & (OutputAs != "DataFrameList")) {
          Inp.. <- adjMoneyValDF(Inp.., InYear. = InYear., OutYear = OutYear, 
                                 Cpi.Yr = Model_$Cpi.Yr) 
        }
      }
    }
    #Format DataFrame and Matrix output
    if (OutputAs %in% c("DataFrame", "Matrix")) {
      if (HasRowNameCol) {
        if (ncol(Inp..) == 2) {
          DataName <- ColNames.[2]
          Inp_ <- list()
          Inp_[[DataName]] <- Inp..[,2]
          names(Inp_[[DataName]]) <- Inp..[,1]
          Inp.. <- data.frame(Inp_)
        } else {
          rownames(Inp..) <- Inp..[,1]
          Inp.. <- Inp..[,-1]
        }
        if (!all(is.na(OrderRowsBy))) {
          if (all(rownames(Inp..) %in% OrderRowsBy) & 
              all(OrderRowsBy %in% rownames(Inp..))) {
            Inp.. <- Inp..[OrderRowsBy,]
          } else {
            stop("OrderRowsBy is not consistent with row names.")
          }
        }    
      }
      if (OutputAs == "DataFrame") {
        Result <- Inp..
      } else {
        Result <- as.matrix(Inp..)
      }
    }
    #Format Values output
    if (OutputAs == "Values") {
      if (nrow(Inp..) != 1) {
        stop("If output is Values, then file can only have one row of data.")
      }
      if (all(is.na(ValueNames))) {
        Result <- as.list(Inp..)
      } else {
        Result <- as.list(Inp..)
        if (!all(names(Result) %in% names(ValueNames))) {
          stop("ValueNames are not named to match names in input file.")
        } else {
          names(Result) <- ValueNames[names(Result)]
        }
      }
    }
    #Format DataFrameList output
    if (OutputAs == "DataFrameList") {
      ListNames. <- Inp..[,1]
      Inp_ <- split(Inp..[,-1], ListNames.)
      if (HasRowNameCol) {
        Inp_ <- lapply(Inp_, function(x){
          rownames(x) <- x[,1]
          x[,-1]
        })
      }
      Result <- Inp_
    }
    #Format ColumnToVector output
    if (OutputAs == "ColumnToVector") {
      if (HasRowNameCol) {
        Result <- Inp..[,2]
        names(Result) <- Inp..[,1]
      }
    }
    #Format RowToVector output
    if (OutputAs == "RowToVector") {
      Result <- unlist(Inp..)
    }
    Result
  }


#Define naming vectors
#=====================
#The model code uses standardized names for data dimensions for a number of
#purposes such as ordering vectors and matrices so that the elements correspond.
#The data dimensions that these naming vectors relate to are given 2-character 
#names that are used in the naming of a data object to describe the object's 
#structure. The naming vectors are collected in a list named 'Abbr_'.

#Initialize the naming list
Abbr_ <- list()
#Load file of geographical names
setwd(ModelDir)
Filename <- "district_groups.csv"
DistrictGroups.. <- read.csv( Filename, as.is=TRUE )
setwd(RunDir)
#District names
Abbr_$Di <- DistrictGroups..$District
#Metropolitan division names
Abbr_$Md <- sort(unique(DistrictGroups..$Division))
#Metropolitan area names
Abbr_$Ma <- sort(unique(DistrictGroups..$Msa))
rm(Filename, DistrictGroups..)
#Ages of persons category names
Abbr_$Ap <- 
  c("Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus")
#Urban type category names (metropolitan vs. other urban)
Abbr_$Ut <- c("Metropolitan", "Town")
#Development type category names (metropolitan, town, rural)
Abbr_$Dt <- c("Metropolitan", "Town", "Rural")
#Housing type category names (e.g. SF = single family, MH = mobile home, 
#A5P = 5+ unit apartments, A24 = 2-4 unit apartments, 
#SFA = single family attached)
Abbr_$Ht <- c("A24", "A5P", "SFA", "SFD", "MH")
#Income group category names
Abbr_$Ig <- 
  c("0to20K", "20Kto40K", "40Kto60K", "60Kto80K", "80Kto100K", "100KPlus")
#Fuel type names
Abbr_$Ft <- c("ULSD", "Biodiesel", "Gasoline", "Ethanol", "CNG")
#Congestion level category names
Abbr_$Cl <- c("None", "Mod", "Hvy", "Sev", "Ext")
#Types of vehicles
Abbr_$Ty <- c("LtVeh","Truck","Bus")
#Functional class of roadways
Abbr_$Fc <- c("Fwy","Art","Other")
#Powertrain types for vehicles
Abbr_$Pt <- 
  c("LdIceEco", "LdIceNonEco", "LdHev", "LdFcv", "LdEv", "TruckIce", "TruckEv", 
    "BusIce", "BusEv" )
#Attach the list so that the dimension names can be accessed directly
attach( Abbr_ )


#Read in global values
#=====================
#The global_values.txt file contains various parameters used by the model.
source( paste( ModelDir, "/global_values.txt", sep="" ) )


#Load estimated models and functions
#===================================
#The submodels of the RSPM are combinations of data and functions. For example,
#the submodel that adjusts household income distributions based on changes in
#the distribution of the household age structure consists of a matrix of
#probabilities and a function to calculate income adjustments based on the
#matrix of probabilities and other inputs. All of the submodels are contained in
#one .RData file that is composed of a list containing the data and functions
#for the submodels. This list is named 'GreenSTEP' owing to the roots of the 
#RSPM in the earlier GreenSTEP model. This list is attached to the workspace so 
#that each submodel can be called using it's individual name.

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
#The starting inventories for the model are also contained in the model
#directory. Once finalized, this data should not change because it reflects
#existing conditions.

#Change working directory and initialize list
setwd(ModelDir)
Model_ <- list()
#Consumer price index
Model_$Cpi.Yr <- loadCpiTable("cpi.csv")
#Crosswalk between districts, divisions, and metropolitan areas
Model_$DistrictGroups.. <-
  readDataFile("district_groups.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Load group household auto ownership rates
Model_$GroupAutoOwnership_Va.. <-
  readDataFile("group_auto_ownership.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrameList")
#Factor to convert metropolitan household DVMT to metropolitan light vehicle road DVMT
Model_$LtVehDvmtFactor.Ma <-
  readDataFile("hh_dvmt_to_road_dvmt.csv",
               HasRowNameCol = TRUE,
               OutputAs = "ColumnToVector")
#Base year freeway and arterial lane miles by metropolitan area
Model_ <- c(Model_,
            readDataFile(
              "lane_miles.csv",
              OutputAs = "Values",
              ValueNames = c(Fwy = "BaseFwyLnMi", Art = "BaseArtLnMi")
            ))
#Base year parameters for truck DVMT proportions and freeway/arterial DVMT proportions
Model_$MpoBaseDvmtParm..Ma <-
  readDataFile("mpo_base_dvmt_parm.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Statewide real per capita income
Model_$PerCapInc.Yr <-
  readDataFile(
    "per_cap_inc.csv",
    HasRowNameCol = TRUE,
    OutputAs = "RowToVector",
    ConvertMoney = TRUE,
    OutYear = BaseCostYear
  )
#Population targets for household size and for 1-person households
Model_$PopTargets_Va.. <-
  readDataFile("pop_targets.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrameList")
#Regional proportion of statewide real per capita income
Model_$IncomeProp.YrMd <-
  readDataFile("regional_inc_prop.csv",
               HasRowNameCol = TRUE,
               OutputAs = "Matrix")
#Travel demand management parameters for the reduction in VMT by program type
Model_$TdmParm. <-
  readDataFile("tdm_parameters.csv",
               HasRowNameCol = TRUE,
               OutputAs = "ColumnToVector")
#Base year metropolitan transit revenue miles by type and metropolitan area
Model_$BaseTranRevMi.. <-
  readDataFile("transit_revenue_miles.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Proportions of truck and bus DVMT by functional class
Model_$TruckBusFcDvmtSplit_Va.. <-
  readDataFile("truck_bus_fc_dvmt_split.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrameList")
#Proportions of households in districts living in urban mixed use neighborhoods
Model_$UrbanTypeProp.DiYr <-
  readDataFile(
    "land_forecasts/metropolitan_urban_type_proportions.csv",
    HasRowNameCol = TRUE,
    OutputAs = "Matrix"
  )
#Set the working directory back to the run directory and attach Model list
setwd(RunDir)
attach(Model_)


#Load scenario inputs
#====================
#These inputs represent different scenarios.

#Change working directory and initialize list
setwd(InputDir)
Inputs_ <- list()
#Factors for adjusting 95 percentile age of vehicles
Inputs_$AgeAdj.YrTy <-
  readDataFile("age_adj.csv", HasRowNameCol = TRUE, OutputAs = "Matrix")
#Auto and light-truck fuel type mix
Inputs_$AutoLtTrkFuels..Yr <-
  readDataFile("auto_lighttruck_fuel.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Household light duty vehicle MPG & MPKwh by vehicle year and type
Inputs_$AutoLtTrkMpg..Yr <-
  readDataFile("auto_lighttruck_mpg.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Bus fuel type mix, hydrocarbon by type and electric
Data <-
  readDataFile("bus_fuels.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
Inputs_$BusPropElectric.Yr <- unlist(Data["PropElectric", ])
Fp <- c("PropGas", "PropCng", "DieselPropBio", "GasPropEth")
Inputs_$BusFuels.FpYr <- as.matrix(Data[Fp,])
rm(Data, Fp)
#Car service availability
Inputs_$CarSvcAvail.DiYr <-
  readDataFile("car_svc_avail.csv",
               HasRowNameCol = TRUE,
               OutputAs = "Matrix")
#Car service cost parameters
Inputs_$CarSvcCostParm.. <-
  readDataFile(
    "car_svc_cost_parm.csv",
    HasRowNameCol = TRUE,
    OutputAs = "DataFrame",
    ConvertMoney = TRUE,
    OutYear = BaseCostYear
  )
#Car service light truck proportion
Inputs_$CarSvcLtTruckProp.Yr <-
  readDataFile("car_svc_lttruck_prop.csv",
               HasRowNameCol = TRUE,
               OutputAs = "ColumnToVector")
#Car service powertrain proportions
Inputs_$CarSvcPtProp..Yr <-
  readDataFile("car_svc_pt_prop.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Commercial service vehicle fuels
Inputs_$CommServiceFuels..Yr <-
  readDataFile("comm_service_fuel.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Commercial service vehicle light truck proportions
Inputs_$CommServiceLtTruckProp.Yr <-
  readDataFile("comm_service_lttruck_prop.csv",
               HasRowNameCol = TRUE,
               OutputAs = "ColumnToVector")
#Commercial service vehicle powertrain proportions
Inputs_$CommServicePtProp..Yr <-
  readDataFile("comm_service_pt_prop.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Congestion pricing input parameters
Inputs_$CongPriceParm_Va.. <-
  readDataFile(
    "congestion_charges.csv",
    HasRowNameCol = TRUE,
    OutputAs = "DataFrameList",
    ConvertMoney = TRUE,
    OutYear = BaseCostYear
  )
#Congestion efficiency of vehicles by year (Yr) and powertrain (Pt)
Inputs_$CongEfficiency.YrPt <-
  readDataFile("cong_efficiency.csv",
               HasRowNameCol = TRUE,
               OutputAs = "Matrix")
#Costs for fuel, electricity and optional VMT-based charges
Inputs_$Costs.YrCs <-
  t(
    readDataFile(
      "costs.csv",
      HasRowNameCol = TRUE,
      OutputAs = "Matrix",
      ConvertMoney = TRUE,
      OutYear = BaseCostYear
    )
  )
#Eco-driving and low rolling-resistance tire inputs
Inputs_$EcoTire..Yr <-
  readDataFile("eco_tire.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Electric vehicle range and proportion of VMT in range traveled by electricity
Inputs_$EvRangeProp..Yr <-
  readDataFile("ev_characteristics.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Fuel types and carbon emissions by vehicle type
Inputs_$FuelCo2..Yr <-
  readDataFile("fuel_co2.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Freeway and arterial lane miles as a proportion of base year lane miles
Data <-
  readDataFile("lane_mile_growth.csv",
               HasRowNameCol = TRUE,
               OutputAs = "Matrix")
Inputs_$FreewayGrowth.Yr <- Data["Fwy", ]
Inputs_$ArterialGrowth.Yr <- Data["Art", ]
rm(Data)
#Heavy truck fuel type mix
Inputs_$HvyTruckFuels..Yr <-
  readDataFile("heavy_truck_fuel.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Hybrid electric vehicle characteristics
Inputs_$HevMpgProp..Yr <-
  readDataFile("hev_characteristics.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Household vehicle cost parameters
Inputs_$HhVehOwnParm.. <-
  readDataFile(
    "hh_veh_own_cost_parm.csv",
    HasRowNameCol = TRUE,
    OutputAs = "DataFrame",
    ConvertMoney = TRUE,
    OutYear = BaseCostYear
  )
#Heavy vehicle MPG & power consumption by vehicle year and type:
Inputs_$HvyVehMpgMpk..Yr <-
  readDataFile("hvy_veh_mpg_mpk.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Proportion of households participating in individualized marketing program (IMP)
Inputs_$ImpPropGoal.DiYr <-
  readDataFile("imp_prop_goal.csv",
               HasRowNameCol = TRUE,
               OutputAs = "Matrix")
#Light weight vehicles input parameters
TempInput.. <- read.csv("light_vehicles.csv", row.names = 1)
Inputs_$LtVehParm_Va.. <- list()
Vars. <- c("OwnRatio", "Threshold", "PropSuitable")
for (va in Vars.) {
  Inputs_$LtVehParm_Va..[[va]] <-
    TempInput..[, grep(va, colnames(TempInput..))]
  Prefix <- paste(va, ".", sep = "")
  colnames(Inputs_$LtVehParm_Va..[[va]]) <-
    gsub(Prefix, "", colnames(Inputs_$LtVehParm_Va..[[va]]))
}
rm(TempInput.., Vars.)
#Proportions of vehicles that are light trucks
Inputs_$LtTruckProp.MdYr <-
  readDataFile("lttruck_prop.csv",
               HasRowNameCol = TRUE,
               OutputAs = "Matrix")
#Operations program deployment inputs
Inputs_$OpsDeployParm_Va.MaYr <-
  readDataFile("ops_deployment.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrameList")
#Other operations deployment inputs
Inputs_$OtherOps_Yr.LvTy <-
  readDataFile("other_ops.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrameList")
Inputs_$OtherOps_Yr.LvTy <-
  lapply(Inputs_$OtherOps_Yr.LvTy, function(x) as.matrix(x))
#Parking pricing
Inputs_$PkgParm_Va.. <-
  readDataFile(
    "parking.csv",
    HasRowNameCol = TRUE,
    OutputAs = "DataFrameList",
    ConvertMoney = TRUE,
    OutYear = BaseCostYear
  )
#PAYD input parameters
Inputs_$Payd..Yr <-
  readDataFile(
    "payd.csv",
    HasRowNameCol = TRUE,
    OutputAs = "DataFrame",
    ConvertMoney = TRUE,
    OutYear = BaseCostYear
  )
#Plug in hybrid vehicle data
Inputs_$PhevRangeProp..Yr <-
  readDataFile("phev_characteristics.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Carbon emissions per KWH
Inputs_$PowerCo2.DiYr <-
  readDataFile("power_co2.csv",
               HasRowNameCol = TRUE,
               OutputAs = "Matrix")
#Proportion of workers at employers with strong employee commute options program
Inputs_$PropWrkEco.MdYr <-
  readDataFile("prop_wrk_eco.csv",
               HasRowNameCol = TRUE,
               OutputAs = "Matrix")
#Speed smoothing and ecodriving inputs
Inputs_$SmoothEcoDriveParm_Va.. <-
  readDataFile("speed_smooth_ecodrive.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrameList")
#Per capita transit growth and proportion that is rail
Inputs_$TransitParm.. <-
  readDataFile("transit_growth.csv",
               HasRowNameCol = TRUE,
               OutputAs = "DataFrame")
#Return to run directory and attach Inputs_ list
setwd( RunDir )
attach( Inputs_ )
