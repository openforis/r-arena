# arena-r
  The function in _arena-survey-analysis.R_ is used to compute statistical results for area-based quantitative variables in Open Foris Arena.
  It also computes area estimates for given combination of dimensions. 
  The statistics contain means, totals, and associated standard deviations, variances, confidence intervals, and the sample sizes.  
  The results will be written into several CSV files.
  
  This function can be called in Arena data processing chain, after running 'persist-results.R'
  or through a separate UI in Arena (to be implemented later).
   
  The script creates first a list of data frames for statistical analysis called 'result_cat' 
  This object contains "per hectare" data at the base unit level for all categorical, taxonomic and boolean attributes across area-based variables, grouped by entities.   
  
  Required R packages (with dependencies): dplyr, rlang, stringr, tidyr, srvyr, survey
  
  Created by:   Lauri Vesa (FAO), Javier Garcia Perez (FAO), Stefano Ricci (FAO)
