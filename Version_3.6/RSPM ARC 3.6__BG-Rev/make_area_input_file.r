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


#Make temporary 2050 land use inputs
#===================================

yr <- "2050"

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
rm( LuFileName, TempInput.., Districts., YearColName )

# Identify presence of land supply and dwelling units
HasDu.DiDt <- Units.DiDt > 0
HasArea.DiDt <- LandArea.DiDt > 0

# Identify where there are units and no land supply
HasDuAndNoArea.DiDt <- HasDu.DiDt & !HasArea.DiDt
HasDuAndNoArea.Di <- apply(HasDuAndNoArea.DiDt, 1, any)

# Calculate total area by district
LandArea.Di <- rowSums(LandArea.DiDt)

# Select the data for the non-matching districts
Units.DxDt <- Units.DiDt[HasDuAndNoArea.Di,]
LandArea.DxDt <- LandArea.DiDt[HasDuAndNoArea.Di,]
LandArea.Dx <- LandArea.Di[HasDuAndNoArea.Di]
Dx <- rownames(Units.DxDt)
Dw <- Dx[Units.DxDt[,"Metropolitan"] > 0]
FunkyUnits.DwDt <- Units.DxDt[Dw,]
head(FunkyUnits.DwDt)

FixFunkyUnits.DwDt <- FunkyUnits.DwDt
FixFunkyUnits.DwDt[,"Metropolitan"] <- 0

FixUnits.DiDt <- Units.DiDt
FixUnits.DiDt[Dw,] <- FixFunkyUnits.DwDt[Dw,]

# Identify presence of land supply and dwelling units
HasDu.DiDt <- FixUnits.DiDt > 0
HasArea.DiDt <- LandArea.DiDt > 0

# Identify where there are units and no land supply
HasDuAndNoArea.DiDt <- HasDu.DiDt & !HasArea.DiDt
HasDuAndNoArea.Di <- apply(HasDuAndNoArea.DiDt, 1, any)
any(HasDuAndNoArea.Di)

# Create revised dataset
DuFileName <- paste( ModelDir, "/du_forecasts/", "du_", yr, ".csv", sep="" )
NewDuInput.. <- read.csv( DuFileName, as.is = TRUE )
NewDuInput..$District <- as.character(NewDuInput..$District)
NewDuInput..[(NewDuInput..$District %in% Dw) & (NewDuInput..$Type == "Metropolitan"), 3:8] <- 
    0 * NewDuInput..[(NewDuInput..$District %in% Dw) & (NewDuInput..$Type == "Metropolitan"), 3:8]

# Write to file
DuFileName <- paste( ModelDir, "/du_forecasts/", "du_rev_", yr, ".csv", sep="" )
write.table( NewDuInput.., file = DuFileName, col.names = TRUE, row.names = FALSE, sep = "," )

head(Units.DxDt[Temp.,])

summary(Units.DiDt[HasDuAndNoArea.Di,"Metropolitan"])

# Calculate substitute values for non-matching districts
MetroInvDensity.Dy <- 
  LandArea.DiDt[!HasDuAndNoArea.Di, "Metropolitan"] /
  Units.DiDt[!HasDuAndNoArea.Di, "Metropolitan"]
MeanInvDensity <- mean(MetroInvDensity.Dy)
LandAreaSub.DxDt <- LandArea.DxDt
LandAreaSub.DxDt[,"Metropolitan"] <- Units.DxDt[,"Metropolitan"] * MeanInvDensity

Temp. <- Units.DiDt[!HasDuAndNoArea.Di, "Metropolitan"]

# Identify districts that only have one housing unit development type
OnlyOneType.Di <- apply(Units.DiDt, 1, function(x) sum(x > 0) == 1)

# Make sure that districts that have only one type have consistent area
LandArea.DiDt[OnlyOneType.Di & HasDuAndNoArea.Di,] <- 
  sweep(HasDuAndNoArea.DiDt[OnlyOneType.Di & HasDuAndNoArea.Di,], 1, 
        LandArea.Di[OnlyOneType.Di & HasDuAndNoArea.Di], "*")

sum(HasDuAndNoArea.Di)

HasArea.DiDt <- LandArea.DiDt > 0

# Identify where there are units and no land supply
HasDuAndNoArea.DiDt <- HasDu.DiDt & !HasArea.DiDt
HasDuAndNoArea.Di <- apply(HasDuAndNoArea.DiDt, 1, any)

sum(HasDuAndNoArea.Di)
