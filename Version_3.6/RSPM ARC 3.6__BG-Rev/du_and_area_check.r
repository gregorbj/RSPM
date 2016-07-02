#check_units_and_land_supply.r


#Load the Run Parameters
#=======================
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


#Check land supply and dwelling unit consistency
#===============================================

#Create log file
sink("DU_and_Area_Check.txt")
cat("Check of Dwelling Unit and Land Supply Inputs for Consistency")
cat("\n\n")

#Iterate through years

for (yr in Yr) {
  
  # Load and process the dwelling unit file
  DuFileName <- paste( ModelDir, "/du_forecasts/", "du_", yr, ".csv", sep="" )
  TempInput.. <- read.csv( DuFileName )
  Districts. <- TempInput..$District
  Units_Di.. <- split( TempInput..[,-1], Districts. )
  Units_Di.DtHt <- lapply( Units_Di.., function(x) {
    Units.DtHt <- as.matrix( x[,-1] )
    rownames( Units.DtHt ) <- x[,1]
    Units.DtHt[Dt,Ht]
  })
  Units.DiDt <- do.call(rbind, lapply(Units_Di.DtHt, rowSums))
  rm( DuFileName, TempInput.., Districts., Units_Di.. )
  
  # Load and process the land supply file
  LuFileName <- paste( ModelDir, "/land_forecasts/land_supply.csv", sep="" )
  TempInput.. <- read.csv( LuFileName )
  Districts. <- TempInput..$District
  YearColName <- paste( "X", yr, sep="" )
  LandArea.DiDt <- matrix( TempInput..[,YearColName], nrow=length(Di), byrow=TRUE )
  rownames( LandArea.DiDt ) <- Districts.[ c( 1, 1 + cumsum(rep(3,length(Di)-1))) ]
  colnames( LandArea.DiDt ) <- TempInput..$Type[1:3]
  LandArea.DiDt <- LandArea.DiDt[Di,Dt]
  if( AreaUnits == "acres" ) {
    LandArea.DiDt <- LandArea.DiDt / 640 # convert to square miles
  } 
  rm( LuFileName, TempInput.., Districts., YearColName )

  # Identify presence of land supply and dwelling units
  HasDu.DiDt <- Units.DiDt > 0
  HasArea.DiDt <- LandArea.DiDt > 0
  
  # Identify where there are units and no land supply
  HasDuAndNoArea.DiDt <- HasDu.DiDt & !HasArea.DiDt
  DiWithDuAndNoArea. <- Di[apply(HasDuAndNoArea.DiDt, 1, any)]
  
  # Identify where there is land supply and no units
  HasAreaAndNoDu.DiDt <- HasArea.DiDt & !HasDu.DiDt
  DiWithAreaAndNoDu. <- Di[apply(HasAreaAndNoDu.DiDt, 1, any)]
  
  # Make record of results
  cat(paste("Check of", yr, "Dwelling Unit and Land Supply Consistency"))
  cat("\n\n")
  if (sum(HasDuAndNoArea.DiDt) == 0) {
    cat("All districts and development types having units also have land area.")
    cat("\n\n")
  } else {
    cat("The following districts have dwelling units for a development type but no land area for that development type.")
    cat("\n")
    cat(DiWithDuAndNoArea.)
    cat("\n\n")
  }
}

sink()

