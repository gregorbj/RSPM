# RSPM
Regional Strategic Planning Model

This repository contains code, a working example, and documentation of the Regional Strategic Planning Model (RSPM). The RSPM is an offshoot the the GreenSTEP model. 

**Version 3.5**  
RSPM V3.5 is documented in "GreenSTEP-RSPM_Documentation_20151220.docx" file. Documentation is for both the GreenSTEP model and RSPM. Changes in V3.5 from previous versions are documented in the "RSPM_V3.5_Changes.docx" file. V3.5 model outputs are documented in the "RSPM_V3.5_Output_Documentation.docx" file.

**Version 3.6 Changes**  
This version includes new modules that model:  
- Potential effects of autonomous vehicles and shared autonomous vehicles on auto ownership, use of car services, vehicle use costs, vehicle travel, and roadway capacity.
- Revised cost and emissions models which account for autonomous vehicle use.  
- Walking, bicycling, and public transit trip models - hurdle models that predict the number of trips as a function of household, land use, service characteristics, and household DVMT.
- Automatic conversion of currency amounts between years - input files identify the data year; input values converted to model estimation year to run model; output values are converted to model base year.
The autonomous vehicle model is documented in the "RSPM_V3.6_AV-Model_Design.html" and "RSPM_V3.6_AV-Model_Code&Testing.html" files. Documentation of the walk/bike/transit trip models is contained in the "RSPM_V3.6_Alt_Modes_Models.html" file.

**Version 3.8 Changes**
RSPM V3.8 includes changes to the code to handle lane-miles and transit revenue-miles. Lane-miles are input directly by model run year rather than having base year values that are scaled by growth factors. Transit revenue miles are handled the same way. Bus-equivalency factors are used to convert transit revenue miles by transit mode to bus-equivalent revenue miles. Following are model and input file changes:
- model/lane_miles.csv - REMOVED
- model/transit_revenue_miles.csv - REMOVED
- model/revenue_mile_factors.csv - ADDED
- scenario/inputs/lane_mile_growth.csv - REMOVED
- scenario/inputs/lane_miles.csv - ADDED
- scenario/inputs/transit_growth.csv - REMOVED
- scenario/inputs/transit_service.csv - ADDED
