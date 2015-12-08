#===========
#GreenSTEP.r
#===========
#Regional Strategic Planning Model (RSPM) GreenSTEP Version
#Copyright 2009 - 2015, Oregon Department of Transportation
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
#http://www.apache.org/licenses/LICENSE-2.0
#Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
#Version: 3.5
#Date: 6/22/15

#Description
#===========

#This is the main script for running the GreenSTEP Model. This script sets up run parameters and calls modules which implement various portions of the GreenSTEP model. These modules include:

#1) The GreenSTEP_Inputs.r module loads all of the model objects and data needed to run the GreenSTEP model.

#2) The "GreenSTEP_Hh_Synthesis.r" module generates synthetic households for each county and year from population forecasts of persons by age group. The synthetic households have age structure, income, driver, housing type, and location characteristics. For the base year a calibration script is run which estimates division and district income weights which are used in the household income model and the household location model.

#3) The GreenSTEP_Sim.r module performs all of the household microsimulation calculations for determining vehicle ownership, household travel, vehicle characteristics, vehicle emissions, etc.

#4) The GreenSTEP_Sim_Outputs.r module computes summary output tables from the household simulation results. These tables are used by other scripts which produce evaluation measures.

#Read in parameters that are unique to the model run
#===================================================

#These define whether the scenario is a base scenario (addressing past conditions, e.g. 1990, 1995, 2000 and 2005) or a future scenario, the name of the base scenario, and the years to model. These parameters are stored in a text file in the inputs directory.

source( "run_parameters.txt" )

#Identify directory locations for model, inputs, etc.
#====================================================

# Make a list to store the directory references
Dir_ <- list()
# Directory references are made with respect to the run directory
Dir_$RunDir <- getwd()
# The scenario inputs directory location
Dir_$InputDir <- "scenario/inputs"
# The scenario outputs directory location
Dir_$OutputDir <- "scenario/outputs"
# Directory containing model objects and inputs
Dir_$ModelDir <- "model"
# Directory where model scripts are located
Dir_$ScriptDir <- "scripts"
attach( Dir_ )


#Define function to load an RData object to an object name
#=========================================================

assignLoad <- function(filename){
  load(filename)
  get(ls()[ls() != "filename"])
}


#Run the GreenSTEP_Inputs.r script
#=================================

#The GreenSTEP_Inputs.r script loads all of the data objects needed to run the model.

source( paste( ScriptDir, "/GreenSTEP_Inputs.r", sep="" ) )


#Run the GreenSTEP_Hh_Synthesis.r script if necessary
#====================================================

#The GreenSTEP_Hh_Synthesis.r script generates "synthetic" households for each metropolitan division and assigns each to a district and establishes all of the household demographic, economic, and land use characteristics. Either of two scripts are run in this step. For the base year a calibration script is run. This script calibrates a set of district weights that determine the relative attractiveness of zones. The calibration script estimates the weights so that the average per capita incomes of households assigned to districts matches the base year inventory of average per capita income by district. It is important that after the base year is run the calibrated weights (saved in "model/calibrated_income_weights.csv") are used to update the weights used for model runs for other periods (stored in "model/land_forecasts/income_weights.csv"). For other years, a non-calibrating script is run.   

if( RunHhSynthesis ) {
  for( yr in RunYears ) {
    if( yr == BaseYear ) {
      source( paste( ScriptDir, "/GreenSTEP_Hh_Synthesis_Cal.r", sep="" ) )
      if( PlotCalibration ) {
        if( !file.exists( paste( ModelDir, "Documentation", sep="/" ) ) ) {
          dir.create( paste( ModelDir, "Documentation", sep="/" ) )
        }
        source( paste( ScriptDir, "/view_calibrated_hh.r", sep="" ) )
      }
    } else {
      source( paste( ScriptDir, "/GreenSTEP_Hh_Synthesis.r", sep="" ) )
    }
  } 
}

#Run the GreenSTEP_Sim.r script
#==============================

#The GreenSTEP_Sim.r module performs all of the household microsimulation calculations for determining vehicle ownership, household travel and vehicle characteristics and use. The results are aggregated to arrays by county, income and development type and saved to disk.

if( RunTravel ) {
  source( paste( ScriptDir, "/GreenSTEP_Sim.r", sep="" ) )
}

#Run the GreenSTEP_Sim_Outputs.r script
#======================================

#The GreenSTEP_Sim_Outputs.r module computes summary output tables from the household simulation results. These tables are used by the GreenSTEP_Emissions.r module and the GreenSTEP_Analysis.r module.

if( RunOutputs ) {
  source( paste( ScriptDir, "/GreenSTEP_Sim_Outputs.r", sep="" ) )
  source( paste( ScriptDir, "/calc_summary_measures.r", sep="" ) )
}	

