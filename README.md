# arena-r
  The function in _arena-survey-analysis.R_ is used to compute statistical results for (active) area-based quantitative variables in Open Foris Arena.
  It also computes area estimates for given dimensions, either as combination of dimensions or separately. 
  The result statistics contain means, totals, and associated standard deviations, variances, confidence intervals, and sample sizes.  
  The results will be written into CSV files.
  
  This function can be called in Arena data processing chain, after running 'persist-results.R'
  or through a separate UI in Arena (to be implemented later).
   
  About this script:
  The script first creates a list of data frames ('result_cat') for statistical analysis. This is input data for the analysis part. 
  This object contains "per hectare" data summed up to the base unit level, grouped by entities. It contains all categorical, taxonomic and boolean attributes as dimensions.
  
  Required R packages (with dependencies): dplyr, rlang, stringr, tidyr, srvyr, survey
  
  Created by:   Lauri Vesa (FAO), Javier Garcia Perez (FAO), Stefano Ricci (FAO)
