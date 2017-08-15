#========================
#GreenSTEP_Hh_Synthesis.r
#========================
#Regional Strategic Planning Model (RSPM) GreenSTEP Version
#Copyright 2009 - 2015, Oregon Department of Transportation
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
#http://www.apache.org/licenses/LICENSE-2.0
#Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
#Version: 3.5
#Date: 6/22/15

#Description
#===========

#This script takes population projections by age cohort, county and year and makes synthetic households that are used in the GreenSTEP model. The attributes of the synthetic households include:
#* The number of persons by age category;
#* Household size
#* Household income and income category
#* Age of household head
#* Housing type
#* Development type
#* District
#* Population density
#* Urban mixed-use designation

#Households (i.e. persons) in group quarters are created separately from other households.

#The synthetic households do not need to change except where different population and/or land use assumptions are being modeled. Most scenario analysis will be built off of a just a few population and land use scenarios. The population synthesizer is not rerun if population synthesis has already occurred as indicated by the presence of the directories which store the population synthesis results. This approach will limit the effect of stochastic processes in the model on the outcomes.

#The script stores the synthetic households in the "model/SynHsld" directory. The directory will be created if it does not exist. The synthetic households for a particular run year are stored in directory named "YearXXXX" where XXXX is the four digit year. The script checks whether the directory exists and will  not create synthetic households if the directory already exists. Therefore, if the user desires to recreate synthetic households for a particular year, then they need to delete the directory for that year.

local( {
  
  # Create a directory to store the created households if one does not exist
  HsldDir <- paste( ModelDir, "SynHsld", sep="/" )
  if( !file.exists( HsldDir ) ) dir.create( HsldDir )
  
  # Run rest of script if synthetic households have not already been created
  # Check is based on whether directory has been created
  YearDir <- paste( HsldDir, "/Year", yr, sep="" )
  if( file.exists( YearDir ) ) {
    print( paste( "The directory '", YearDir, "' already exists. Stopping synthetic household generation for ", yr, ".", sep="" ) )
  } else {
    
    # Create directory to store results 
    dir.create( YearDir )
    
    #Set a random seed to make run replicable
    #========================================
    #If the UsedSavedRandomSeed input parameter is TRUE a saved random seed will be retrieved
    #Otherwise a new random seed will be set and saved
    RandomSeedFile <- paste( ModelDir, "/RandomSeedValue", yr, ".RData", sep="" )
    if( UseSavedRandomSeed & file.exists( RandomSeedFile ) ) {
      RandomSeedValue <- assignLoad( RandomSeedFile )
    } else {
      RandomSeedValue <- sample( 1:1000000, 1 )
      save( RandomSeedValue, file=RandomSeedFile )
    }
    #Set random seed for model run
    set.seed( RandomSeedValue )
    
    #Create synthetic households
    #===========================
    
    # Load the dwelling unit and land supply inputs
    #----------------------------------------------
    # Load the dwelling unit file
    DuFileName <- paste( ModelDir, "/du_forecasts/", "du_", yr, ".csv", sep="" )
    TempInput.. <- read.csv( DuFileName )
    Districts. <- TempInput..$District
    Units_Di.. <- split( TempInput..[,-1], Districts. )
    Units_Di.DtHt <- lapply( Units_Di.., function(x) {
      Units.DtHt <- as.matrix( x[,-1] )
      rownames( Units.DtHt ) <- x[,1]
      Units.DtHt[Dt,Ht]
    })
    GroupUnits.DiDt <- do.call( rbind, lapply( Units_Di.., function(x) x$GQ ) )
    colnames( GroupUnits.DiDt ) <- Dt
    rm( DuFileName, TempInput.., Districts., Units_Di.. )
    # Load the land supply file
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
    # Load the district income weights
    IncWtFileName <- paste( ModelDir, "/land_forecasts/income_weights.csv", sep="" )
    TempInput.. <- read.csv( IncWtFileName )
    Districts. <- TempInput..$District
    YearColName <- paste( "X", yr, sep="" )
    IncWt.DiDt <- matrix( TempInput..[,YearColName], nrow=length(Di), byrow=TRUE )
    rownames( IncWt.DiDt ) <- Districts.[ c( 1, 1 + cumsum(rep(3,length(Di)-1))) ]
    colnames( IncWt.DiDt ) <- TempInput..$Type[1:3]
    IncWt.DiDt <- IncWt.DiDt[Di,Dt] 
    rm( IncWtFileName, TempInput.., Districts., YearColName )
    
    # Use the population by age cohort to create synthetic households
    #----------------------------------------------------------------
    # Load the population file for the year
    PopFileName <- paste( ModelDir, "/pop_forecasts/", "pop_by_age_", yr, ".csv", sep="" )
    Pop..MdAp <- read.csv( PopFileName, row.names=1 )[Md,]
    # Create a list to hold the results where each component of the list
    # is a matrix of
    Hsld_Md.HhAp <- list()
    # Create the synthetic households for each metropolitan division
    for( md in Md ) {
      if( yr == BaseYear ) {
        HhSizeTargets.Md <- BaseHhSize.Md
      } else {
        HhSizeTargets.Md <- PopTargets_Va..$AveHhSize[Md,yr]
        names( HhSizeTargets.Md ) <- Md 
      }
      Hsld_Md.HhAp[[md]] <- createHhByAge( unlist( Pop..MdAp[md,] ), HtProb.HtAp, 
                                           TargetHhSize=HhSizeTargets.Md[md], 
                                           TargetProp1PerHh=PopTargets_Va..$Prop1PerHh[md,yr] )[[1]]
    }
    
    # Use the group population by age cohort to create synthetic households in group quarters
    #----------------------------------------------------------------------------------------
    # Check whether there is group population file
    GroupPopFileName <- paste( ModelDir, "/pop_forecasts/", "group_pop_by_age_", yr, ".csv", sep="" )
    # If the file exists, make the group population households
    if( file.exists( GroupPopFileName ) ) {
      GroupPop..MdAp <- read.csv( GroupPopFileName, row.names=1 )[Md,]
      GroupHsld_Md.HhAp <- list()
      for( md in Md ) {
        HsldType.HtAg <- array( 0, dim=c( length(Ap), length(Ap) ) )
        diag( HsldType.HtAg ) <- unlist( GroupPop..MdAp[md,] )
        GroupHh_Ht.HhAp <- apply( HsldType.HtAg, 1, function(x) { 
          matrix( rep( x / sum(x), sum(x) ), ncol=length(Ap), byrow=TRUE ) } )
        if( length( GroupHh_Ht.HhAp ) > 0 ) { 
          GroupHsld_Md.HhAp[[md]] <- do.call( rbind, GroupHh_Ht.HhAp )
          colnames( GroupHsld_Md.HhAp[[md]] ) <- Ap
        }
        rm( HsldType.HtAg, GroupHh_Ht.HhAp )
      }
      rm( GroupPopFileName, GroupPop..MdAp )
    }  
    
    # Iterate through metropolitan divisions and add all synthetic household attributes
    #==================================================================================
    
    # Initialize object to store number of housing units that need to be added to the inventory
    UnitsAdd_Di.DtHt <- lapply( Units_Di.DtHt, function(x) x * 0 )
    
    # Begin iteration through metropolitan divisions     
    for( md in Md ) {
      
      print( md )
      
      # Assign housing characteristics for standard households
      #=======================================================
      
      # Add household size, income, household head and driver attributes
      #-----------------------------------------------------------------
      # Extract the synthetic population for the metropolitan division
      SynPop.. <- data.frame( Hsld_Md.HhAp[[md]] )
      # Give each household a unique id
      SynPop..$Houseid <- 1:nrow( SynPop.. )
      # Calculate the household size
      SynPop..$Hhsize <- rowSums( SynPop..[ , Ap ] )
      # Predict income
      IncomeProp <- IncomeProp.YrMd[yr,md]
      # Add the power transformed per capita income for the metropolitan division
      Pow <- 0.4
      SynPop..$PowPerCapInc <- ( PerCapInc.Yr[yr] * IncomeProp )^Pow
      # Predict household income
      SynPop..$Hhincttl <- predictIncome( SynPop.. )
      SynPop..$PowPerCapInc <- NULL
      # Create floor income (to prevent negatives)
      MinInc <- quantile( SynPop..$Hhincttl, prob=0.01 )
      SynPop..$Hhincttl[ SynPop..$Hhincttl < MinInc ] <- MinInc
      # Classify households according to income group
      MaxInc <- max( SynPop..$Hhincttl )
      IncBreaks. <- c( 0, 20000, 40000, 60000, 80000, 100000, MaxInc )
      SynPop..$IncGrp <- cut( SynPop..$Hhincttl, breaks=IncBreaks., labels=Ig,
                              include.lowest=TRUE )
      # Determine age of household head (oldest person) and assign numerical value
      AgeOfHead. <- c(1:nrow(SynPop..))
      AgeOfHead.[SynPop..$Age0to14>0] <- "Age0to14"
      AgeOfHead.[SynPop..$Age15to19>0] <- "Age15to19"
      AgeOfHead.[SynPop..$Age20to29>0] <- "Age20to29"
      AgeOfHead.[SynPop..$Age30to54>0] <- "Age30to54"
      AgeOfHead.[SynPop..$Age55to64>0] <- "Age55to64"
      AgeOfHead.[SynPop..$Age65Plus>0] <- "Age65Plus"
      SynPop..$AgeOfHead <- sapply(AgeOfHead., function(x) switch(x, Age0to14=1, 
                                                                  Age15to19=1, Age20to29=2, Age30to54=3, 
                                                                  Age55to64=4, Age65Plus=5)
      )   
      rm( AgeOfHead. )
      # Calculate driving age population
      SynPop..$DrvAgePop <- rowSums( SynPop..[ , Ap[-1] ] )
      # Create a variable identifying driver population levels
      DrvLevels. <- c( 0, 1, 2, max( SynPop..$DrvAgePop ) )
      SynPop..$DrvLevels <- as.character( cut( SynPop..$DrvAgePop, breaks=DrvLevels.,
                                               labels=c( "Drv1", "Drv2", "Drv3Plus" ) ) )
      # Identify households having only elderly persons
      SynPop..$OnlyElderly <- as.numeric( SynPop..$DrvAgePop == SynPop..$Age65Plus )
      
      # Assign housing type to households
      #----------------------------------
      # Identify the districts in the metropolitan division
      Dx <- rownames( DistrictGroups.. )[ DistrictGroups..$Division == md ]
      # Calculate the number of units in each development type (excluding student group quarters)
      Units.Ht <- colSums( do.call( rbind, lapply( Units_Di.DtHt[Dx], colSums ) ) )[ Ht ]
      # Add some temporary variables used by the housing type model
      SynPop..$Income <- SynPop..$Hhincttl
      SynPop..$HhSize <- SynPop..$Hhsize
      # If number of units is not adequate to satisfy number of households, increase units
      if( nrow( SynPop.. ) != sum( Units.Ht ) ) {
        # Calculate deficit
        NeededUnits <- nrow( SynPop.. ) - sum( Units.Ht )
        # Increase units by type in proportion to input proportions
        AddUnits. <- sample( Ht, abs( NeededUnits ), replace=TRUE, prob=Units.Ht/sum(Units.Ht) )
        AddUnits.Ht <- sign( NeededUnits ) * sapply( Ht, function(x) sum( AddUnits. == x ) )
        Units.Ht <- Units.Ht + AddUnits.Ht
        # Allocate added units to districts in proportion to representation in districts
        for( ht in Ht[ Units.Ht > 0 ] ) {
          Units.DxDt <- do.call( rbind, lapply( Units_Di.DtHt[Dx], function(x) x[,ht] ) )
          UnitsProb.DxDt <- Units.DxDt / sum( Units.DxDt )
          UnitsAdd.. <- expand.grid( rownames( Units.DxDt ), colnames( Units.DxDt ) )
          names( UnitsAdd.. ) <- c( "Dx", "Dt" )
          UnitsAdd..$Dx <- as.character( UnitsAdd..$Dx )
          UnitsAdd..$Dt <- as.character( UnitsAdd..$Dt )
          UnitsAdd..$NumAdd <- 0
          UnitsAdd. <- sign( NeededUnits ) * table( sample( 1:nrow(UnitsAdd..), abs( AddUnits.Ht[ht] ), 
                                                            replace=TRUE, prob=as.vector( UnitsProb.DxDt ) ) )
          UnitsAdd..$NumAdd[ as.numeric( names( UnitsAdd. ) ) ] <- UnitsAdd.
          for( j in 1:nrow(UnitsAdd..) ) {
            dxj <- UnitsAdd..$Dx[j]
            dtj <- UnitsAdd..$Dt[j]
            UnitsAdd_Di.DtHt[[dxj]][dtj,ht] <- UnitsAdd..$NumAdd[j]
            Units_Di.DtHt[[dxj]][dtj,ht] <- Units_Di.DtHt[[dxj]][dtj,ht] + UnitsAdd_Di.DtHt[[dxj]][dtj,ht]
            #Set any negative values to 0
            Units_Di.DtHt <- lapply(Units_Di.DtHt, function(x) {
              x[x < 0] <- 0
              x
            })
          }
        }
        # Finish calculation of added units
      }
      # Apply the housing type model
      SynPop..$HouseType <- predictBldgType(Data..=SynPop.., Models_=BldgTypeModels_,
                                            BldgTypeNum.=Units.Ht, SaveCalibModels=FALSE, Division=md )
      # Remove unneeded columns from SynPop..
      SynPop..$Income <- SynPop..$HhSize <- NULL
      
      # Allocate households to districts and development types
      #-------------------------------------------------------        
      # Initialize the capacity of the districts and development types to number of units
      # Put in data frame with a vector of values
      Capacity.. <- expand.grid( Dt, Ht, Dx )
      names( Capacity.. ) <- c( "Dt", "Ht", "Dx" )
      Capacity..$Units <- do.call( c, lapply( Units_Di.DtHt[Dx], as.vector ) )
      # Set up households for assignment
      SynPop.. <- SynPop..[ rev( order( SynPop..$Hhincttl ) ), ]
      SynPop..$DevType <- NA
      SynPop..$District <- NA
      # Split households by housing type
      SynPop_Ht.. <- split( SynPop.., SynPop..$HouseType )
      # Get district weights for allocating households to districts
      Wts.DtDx <- t( IncWt.DiDt[ Dx, ] )          
      # Iterate through housing types and allocate households to districts and development types
      for( ht in Ht[ Units.Ht > 0 ] ) {
        # Make index vectors for development types and districts
        DevTypes. <- Capacity..$Dt[ Capacity..$Ht == ht ]
        Districts. <- Capacity..$Dx[ Capacity..$Ht == ht ]
        # Make corresponding vector of income weights
        Wts. <- as.vector( Wts.DtDx )
        # Target household size
        TargetHhSize. <- Capacity..$TargetHhSize[ Capacity..$Ht == ht ]
        # Calculate starting inventory of units
        Units. <- Capacity..$Units[ Capacity..$Ht == ht ]
        # Calculate number of households to locate
        N <- nrow( SynPop_Ht..[[ht]] )
        # Initialize vector to hold set of district and development type choices
        Choices. <- numeric( N )
        # Iterate through households of the chosen housing type and choose district and development type
        for( i in 1:N ) {
          # Calculate probability of each choice based on the number of units and the income weights 
          Prob. <- Units. * Wts. / sum( Units. * Wts. )
          # Choose the index to the corresponding DevTypes. and Districts. vectors
          Choice <- sample( 1:length(Units.), 1, prob=Prob. )
          # Update the inventory of units
          Units.[ Choice ] <- Units.[ Choice ] - 1
          # Retain the choice
          Choices.[i] <- Choice
          # Clean up
          rm( Prob., Choice )          
        }
        # Assign the development type to the household
        SynPop_Ht..[[ht]]$DevType <- DevTypes.[ Choices. ]
        # Assign the district to the household
        SynPop_Ht..[[ht]]$District <- Districts.[ Choices. ]
        # End loop through housing type
      }
      # Combine back into one list and randomize
      SynPop.. <- do.call( rbind, SynPop_Ht.. )
      SynPop..$DevType <- as.character( SynPop..$DevType )
      SynPop..$District <- as.character( SynPop..$District )
      
      # Assign housing characteristics for group quarters population
      #=============================================================
      # Do this only if the metropolitan division has group quarters population
      if ( md %in% names( GroupHsld_Md.HhAp ) ) {
        # Extract the synthetic population for the metropolitan division
        GroupSynPop.. <- data.frame( GroupHsld_Md.HhAp[[md]] )
        # Give each household a unique id
        GroupSynPop..$Houseid <- (nrow(SynPop..)+1):(nrow( SynPop.. )+nrow(GroupSynPop..) )
        # Calculate the household size
        GroupSynPop..$Hhsize <- 1
        # Load household income data
        Filename <- paste( ModelDir, "/group_hh_income.csv", sep="" )
        TempInput.. <- read.csv( Filename )
        Va <- TempInput..$Value
        GroupIncome_Va.. <- split( TempInput..[,-1], Va )
        GroupIncome_Va.. <- lapply( GroupIncome_Va.., function(x) {
          RowNames. <- x[,1]
          Parm.. <- x[,-1]
          rownames( Parm.. ) <- RowNames.
          ColNames. <- colnames( Parm.. )
          ColNames. <- gsub( "X", "", ColNames. )
          colnames( Parm.. ) <- ColNames.
          Parm.. } )
        rm( Filename, TempInput.., Va )
        # Add household income to records
        HsldAge. <- apply( GroupSynPop..[ ,Ap ], 1, function(x) Ap[as.logical(x)] )
        Hhincttl. <- unlist( lapply( GroupIncome_Va..[ HsldAge. ], function(x) x[md,yr] ) )
        GroupSynPop..$Hhincttl <- Hhincttl.
        # Classify households according to income group
        GroupSynPop..$IncGrp <- cut( GroupSynPop..$Hhincttl, breaks=IncBreaks., labels=Ig,
                                     include.lowest=TRUE )
        # Classify age of household head
        GroupSynPop..$AgeOfHead <- HsldAge.
        # Calculate driving age population
        GroupSynPop..$DrvAgePop <- rowSums( GroupSynPop..[ , Ap[-1] ] )
        # Create a variable identifying driver population levels
        GroupSynPop..$DrvLevels <- "Drv1"
        # Identify households having only elderly persons
        GroupSynPop..$OnlyElderly <- as.numeric( GroupSynPop..$DrvAgePop == GroupSynPop..$Age65Plus )
        # Identify housing type
        GroupSynPop..$HouseType <- "GQ"
        # Check whether there are sufficient housing units for group quarters households
        if( sum( GroupUnits.DiDt ) < sum( GroupSynPop..[,Ap] ) ) {
          stop( "Insufficient housing for group population. Check Inputs" )
        }
        # Randomize order
        GroupSynPop.. <- GroupSynPop..[ sample( 1:nrow(GroupSynPop..), nrow(GroupSynPop..) ), ]
        # Allocate group households to group quarters
        Capacity.. <- expand.grid( Dt, Dx )
        names( Capacity.. ) <- c( "Dt", "Dx" )
        Capacity..$Units <- as.vector( t( GroupUnits.DiDt[Dx,] ) )
        Capacity.. <- Capacity..[ Capacity..$Units != 0, ]
        # Calculate starting inventory of units
        Units. <- Capacity..$Units
        # Calculate number of households to locate
        N <- nrow( GroupSynPop.. )
        # Initialize vector to hold set of district and development type choices
        Choices. <- numeric( N )
        # Iterate through households of the chosen housing type and choose district and development type
        for( i in 1:N ) {
          # Calculate probability of each choice based on the number of units and the income weights 
          Prob. <- Units. / sum( Units. )
          # Choose the index to the corresponding DevTypes. and Districts. vectors
          Choice <- sample( 1:length(Units.), 1, prob=Prob. )
          # Update the inventory of units
          Units.[ Choice ] <- Units.[ Choice ] - 1
          # Retain the choice
          Choices.[i] <- Choice
          # Clean up
          rm( Prob., Choice )          
        }
        # Assign the development type to the household
        GroupSynPop..$DevType <- Capacity..$Dt[ Choices. ]
        # Assign the district to the household
        GroupSynPop..$District <- Capacity..$Dx[ Choices. ]
        # Combine dataframes
        SynPop.. <- rbind( SynPop.., GroupSynPop.. )
        rm( GroupSynPop.. ) 
        # End of if block
      }             
      
      # Combine the ordinary and qroup quarters households and apply land use characteristics
      #======================================================================================
      
      # Assign population density to the household
      #-------------------------------------------
      # Add up population by district and development type
      Pop.DxDt <- array( 0, dim=c( length(Dx), length(Dt) ), dimnames=list( Dx, Dt ) )
      Pop.2d <- tapply( SynPop..$Hhsize, list( SynPop..$District, SynPop..$DevType ), sum )
      Pop.DxDt[ rownames( Pop.2d ), colnames( Pop.2d ) ] <- Pop.2d
      Pop.DxDt[ is.na( Pop.DxDt ) ] <- 0
      PopDen.DxDt <- Pop.DxDt / LandArea.DiDt[Dx,Dt]
      PopDen.DxDt[ is.nan( PopDen.DxDt ) ] <- 0
      # Assign population density to households
      SynPop..$Htppopdn <- NA
      IsMetropolitan. <- SynPop..$DevType == "Metropolitan"
      SynPop..$Htppopdn[ IsMetropolitan. ] <- PopDen.DxDt[ SynPop..$District[ IsMetropolitan. ], "Metropolitan" ]
      IsTown. <- SynPop..$DevType == "Town"
      SynPop..$Htppopdn[ IsTown. ] <- PopDen.DxDt[ SynPop..$District[ IsTown. ], "Town" ]
      IsRural. <- SynPop..$DevType == "Rural"
      SynPop..$Htppopdn[ IsRural. ] <- PopDen.DxDt[ SynPop..$District[ IsRural.], "Rural" ]      
      rm( IsMetropolitan., IsTown., IsRural. )
      # Calculate the natural log of density
      SynPop..$LogDen <- log( SynPop..$Htppopdn )
      
      # Add the urban mixed-use designation to the household
      #-----------------------------------------------------
      # Determine the urban mixed-use probability for metropolitan proportions of districts  
      UrbMixProb.Dx <- predictUrbanMix( Density.Di=PopDen.DxDt[,"Metropolitan"], UrbanMixModel=UrbanMixModel, 
                                        UrbProp.Di=UrbanTypeProp.DiYr[Dx,yr] )
      names( UrbMixProb.Dx ) <- Dx
      # Assign a value for each household
      SynPop..$Urban <- 0
      UrbMixProb.Hh <- UrbMixProb.Dx[ SynPop..$District ]
      IsMetro. <- SynPop..$DevType == "Metropolitan"
      SynPop..$Urban[ IsMetro. ] <- sapply( UrbMixProb.Hh, function(x) {
        sample( c(1,0), 1, prob=c(x,1-x) ) } )[ IsMetro. ]
      rm( UrbMixProb.Dx, UrbMixProb.Hh, IsMetro. )
      
      # Save the results for the year
      #------------------------------
      # Randomize order
      SynPop.. <- SynPop..[ sample( 1:nrow(SynPop..), nrow(SynPop..) ), ]
      # Save file
      SaveFileName <- paste( YearDir, "/", md, ".RData", sep="" )
      save( SynPop.., file=SaveFileName, compress=TRUE )
      rm( SynPop.. )
      
      # End iteration for metropolitan division  
    }
    
    # Save a record of the housing units added to the inventory
    if( exists( "UnitsAdd_Di.DtHt" ) ){
      WarnOptions <- options( "warn" )
      options( warn=-1 )
      UnitsAdd.. <- data.frame( do.call( rbind, UnitsAdd_Di.DtHt ) )
      options( WarnOptions )
      UnitsAdd.. <- cbind( District=rep( Di, each=3 ), Type=rep( Dt, length(Di) ), UnitsAdd.. )
      FileName <- paste( YearDir, "/added_housing_units.csv", sep="" )  
      write.table( UnitsAdd.., file=FileName, row.names=FALSE, col.names=TRUE, sep="," )
      rm( FileName, UnitsAdd.., UnitsAdd_Di.DtHt )
    }   
    
    # End else statement (generating households if directory has not been created)
  }
  
  # Exit local function    
} )

