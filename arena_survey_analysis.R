
# *** call :

#arena_process_response <- arenaAnalytics( )
#print( arena_process_response) 



#########################################################################

arenaAnalytics <- function(  ) {
  #**********************************************************************************************
  #
  # The function is used to compute statistical results for area-based quantitative variables in Open Foris Arena.
  # It also computes area estimates for given combination of dimensions. 
  # The statistics contain means, totals, and associated standard error, variance, and confidence intervals.  
  # The result will be written into several CSV files.
  #
  # This function can be called in Arena data processing chain, after running 'persist-results.R'
  # or through a separate UI in Arena (to be implemented later).
  # 
  # The script creates first a list of data frames for statistical analysis called 'result_cat' 
  # This object contains "per hectare" data at the base unit level for all categorical, taxonomic and boolean attributes across area-based (active) variables, grouped by entities.   
  # Note: reported result entities' names we can get as follows: names(result_cat)
  #
  # Required R packages (with dependencies): dplyr, stringr, srvyr, survey, rlang
  #
  # Created by:   Lauri Vesa, FAO
  #               Javier Garcia Perez, FAO
  #               
  # Last update:  15.10.2023
  # Notice: Methods for computing results with "post-stratification" are not yet working. 
  # 
  #**********************************************************************************************
  
  tryCatch( usePackage('tidyr'),
            error = function(e){ library('tidyr')
            })
  
  conversion_HierarchicalCodeAttributes <- function( df_data ) {
    # This function is used to recode hierarchical categorical data, so that a child category item gets all parents' codes, separated with astersk (*)
    # for example: '1' Province_A: '1' District_A, '2' District_B  ==> Districts can be recoded as: '1*1' District_A, '1*2' District_B   
    # New codes are valid only during the run time, and they are not returned back into the Arena database
    if ( is.null( df_data ))  return( df_data )
    if ( nrow( df_data ) == 0 | is.null( arena.chainSummary$categoryAttributeAncestors )) return( df_data )
    if ( length( arena.chainSummary$categoryAttributeAncestors$attribute ) == 0)          return( df_data )
    
    categoryNames = unique(arena.chainSummary$categoryAttributeAncestors$categoryName)
    
    for (j in 1: length(categoryNames)) {
      cat_table <- arena.chainSummary$categoryAttributeAncestors %>% filter(categoryName==categoryNames[j]) %>%
        arrange(categoryLevel)
      for ( i in 1 : length( cat_table$attribute )) {
        if ( cat_table$attribute[[i]] %in% names( df_data )) {
          varname <- cat_table$attribute[[i]]
          df_data <- df_data %>%
            unite( !!varname, any_of( c( cat_table$ancestors[[i]][i], varname )), sep = "*", remove=FALSE ) 
        }
      }}
    return( df_data )
  } # conversion_HierarchicalCodeAttributes
  
  
  # START
  # define a folder for output files
  if ( !exists('user_file_path')) user_file_path <- './user_output/'
  # create a folder for files to be exported
  if ( !dir.exists( user_file_path )) dir.create( user_file_path, showWarnings = FALSE )
  
  
  # set  options, see more at https://r-survey.r-forge.r-project.org/survey/html/surveyoptions.html
  #options(dplyr.summarise.inform      = FALSE)
  options( survey.ultimate.cluster     = FALSE)
  options( survey.adjust.domain.lonely = FALSE)
  options( survey.lonely.psu           = "remove")  
  options( digits = 10)
  old_sigfig      <- options("pillar.sigfig") # https://github.com/gergness/srvyr/blob/main/vignettes/srvyr-vs-survey.Rmd
  options( "pillar.sigfig" = 5)
  showStatisticsInResults = TRUE # standard deviation, variance, and conf. intervals are shown in the result tables
  
  # read JSON file 
  chain_summary_json <-  paste(getwd(), 'chain_summary.json', sep = .Platform$file.sep)
  if ( file.exists( chain_summary_json ))  arena.chainSummary <- jsonlite::fromJSON( chain_summary_json )
  
  
  # check analysis parameters, if any
  arena.analyze   <- list(entity = '', dimensions = '', filter = "", reportingMethod = '2')
  
  if ( !is.null( arena.chainSummary$analysis )) {
    if ( !is.null( arena.chainSummary$analysis$entity) & !is.null( arena.chainSummary$analysis$dimensions )) {
      arena.analyze$entity       <- trimws( arena.chainSummary$analysis$entity )
      arena.analyze$dimensions   <- trimws( arena.chainSummary$analysis$dimensions )
      
      # drop out totally blank (NA) columns
      drop_names <- get( arena.analyze$entity) %>% select( where( ~all( is.na(.)))) %>% names()
      # assign( arena.analyze$entity,        get( arena.analyze$entity ) %>% select( -any_of( drop_names)))
      # assign( arena.chainSummary$baseUnit, get( arena.chainSummary$baseUnit ) %>% select( -any_of( drop_names)))
      if ( length( drop_names) > 0) {
        arena.chainSummary$analysis$dimensions <- arena.chainSummary$analysis$dimensions[!arena.chainSummary$analysis$dimensions %in% drop_names ]
        arena.analyze$dimensions               <- arena.analyze$dimensions[ !arena.analyze$dimensions %in% drop_names ]
      }   
      
      if ( !is.null( arena.chainSummary$analysis$filter))          arena.analyze$filter            <- trimws( arena.chainSummary$analysis$filter )  
      if ( toupper( arena.analyze$filter) == "NOSTATISTICS" | toupper( arena.analyze$filter) == "NO STATISTICS" ) {
        showStatisticsInResults = FALSE
        arena.analyze$filter = ""
      }
      if ( !is.null( arena.chainSummary$analysis$reportingMethod)) arena.analyze$reportingMethod   <- trimws( arena.chainSummary$analysis$reportingMethod )  
      
      if ( is.null( arena.analyze$entity) | arena.analyze$entity =="" | is.na( arena.analyze$entity ) | length( arena.analyze$entity ) == 0 ) {
        return( "Arena Analytics: No entity to report" )
      } else if ( is.null( arena.analyze$dimensions ) | is.na( arena.analyze$entity ) | length( arena.analyze$entity ) == 0 ){
        return( "Arena Analytics: No dimension to report" )
      } else {
        arena.analyze$dimensions_datatypes   <- c()
        arena.analyze$dimensions_at_baseunit <- c()
        
        # drop out totally blank (NA) columns
        drop_names <- get( arena.analyze$entity) %>% select( where( ~all( is.na(.)))) %>% names()
        assign( arena.analyze$entity,        get( arena.analyze$entity ) %>% select( -any_of( drop_names)))
        assign( arena.chainSummary$baseUnit, get( arena.chainSummary$baseUnit ) %>% select( -any_of( drop_names)))
        arena.analyze$dimensions <- arena.analyze$dimensions[!arena.analyze$dimensions %in% drop_names]
        
        entity_datatype <- lapply(get( arena.analyze$entity), class)
        
        for ( j in (1 : length( arena.analyze$dimensions ))){
          arena.analyze$dimensions_datatypes[[j]]   <- ifelse( arena.analyze$dimensions[[j]] %in% names( get( arena.analyze$entity)),
                                                               as.character( entity_datatype[arena.analyze$dimensions[[j]]]), "character")
          arena.analyze$dimensions_datatypes[[j]]   <- ifelse(  paste0( arena.analyze$dimensions[[j]], "_scientific_name") %in% names( get( arena.analyze$entity)), "taxon", arena.analyze$dimensions_datatypes[[j]] )
          arena.analyze$dimensions_at_baseunit[[j]] <- unlist( ifelse( arena.analyze$dimensions[[j]] %in% names( get( arena.chainSummary$baseUnit)), TRUE, FALSE))
        }
        arena.analyze$dimensions_datatypes   <- as.character( arena.analyze$dimensions_datatypes)
        arena.analyze$dimensions_at_baseunit <- as.logical( arena.analyze$dimensions_at_baseunit) 
        arena.analyze$dimensions_baseunit    <- as.character( unlist( Map(`[`, arena.analyze$dimensions, arena.analyze$dimensions_at_baseunit)))
        # change comma to dot (if used as decimal separator)
        if (is.null( arena.chainSummary$analysis$reportingArea)) arena.chainSummary$analysis$reportingArea <- 100
        arena.chainSummary$analysis$reportingArea <- stringr::str_replace( arena.chainSummary$analysis$reportingArea, ",", ".")
        arena.analyze$reportingArea               <- as.numeric( paste0( "0", trimws( arena.chainSummary$analysis$reportingArea ))) 
        rm(entity_datatype)
      }
    } else {
      return( "Arena Analytics: No entity or dimensions to report" )
    }
  } else {
    return( "Arena Analytics: No entity to report" )
  } 
  
  
  # Initial processing with JSON data. 
  # a) no base unit -> no sampling design 
  if ( arena.chainSummary$baseUnit == "" )                       arena.chainSummary$samplingDesign              <- FALSE
  # b) stratum attribute is missing
  if ( is.null( arena.chainSummary$stratumAttribute ))            arena.chainSummary$stratumAttribute            <- ""
  # nonresponse bias correction is missing
  if ( is.null( arena.chainSummary$analysis$nonResponseBiasCorrection ))   arena.chainSummary$analysis$nonResponseBiasCorrection   <- FALSE
  
  #############################
  # SAMPLING DESIGN EXISTS. ###
  # Compute expansion factors, sum of area-based variables & weights up to base unit level, and non-response bias corrections
  if ( arena.chainSummary$samplingDesign ) {
    
    # get base unit data into a data frame
    df_base_unit                  <- get( arena.chainSummary$baseUnit )
    df_base_unit$weight[ is.na( df_base_unit$weight)] <- 0
    df_base_unit                  <- conversion_HierarchicalCodeAttributes( df_base_unit)
    
    # take a copy of weight. Non-response bias correction may change weights. 
    df_base_unit$weight_original_ <- df_base_unit$weight 
    # Key attribute names: base unit and clustering attributes
    base_UUID_                    <- paste0( arena.chainSummary$baseUnit, "_uuid")
    cluster_UUID_                 <- ifelse( arena.chainSummary$clusteringEntity != "", paste0( arena.chainSummary$clusteringEntity, "_uuid"), "")    
    # Stratification check: method, attribute and areas
    arena.stratification          <- ifelse(( arena.chainSummary$samplingStrategy==3 | arena.chainSummary$samplingStrategy==4 ) & arena.chainSummary$stratumAttribute != "", TRUE, FALSE)
    arena.strat_attribute         <- ifelse( arena.stratification, arena.chainSummary$stratumAttribute, "")
    
    
    # parameters for post-stratification
    arena.post_stratification  <- FALSE
    ps.weights                 <- NULL 
    # if post-stratification attribute is missing, set ""
    if ( is.null( arena.chainSummary$postStratificationAttribute )) arena.chainSummary$postStratificationAttribute <- ""
    
    # post-stratification attribute is given
    if ( arena.chainSummary$postStratificationAttribute != "" ) {
      arena.post_stratification         <- TRUE
      
      # create a new flat post-stratification category table
      if ( arena.chainSummary$postStratificationAttributeCategory != "" ) {
        arena.postcategory_table        <- as.data.frame( categories[[ arena.chainSummary$postStratificationAttributeCategory ]])
        
        # if hierarchical table, take data from the correct level 
        if (('level_1_code' %in% names( arena.postcategory_table )) & ('area_cumulative' %in% names( arena.postcategory_table))) {
          # set missing area to 0
          if ( anyNA( arena.postcategory_table$area_cumulative)) arena.postcategory_table$area_cumulative[ is.na( arena.postcategory_table$area_cumulative) ] <- 0
          arena.postcategory_table$area_cumulative <- as.numeric( arena.postcategory_table$area_cumulative )
          arena.postcategory_table                 <- arena.postcategory_table %>%
            filter(level == arena.chainSummary$postStratificationAttributeCategoryLevel) %>%
            select( code = code_joint, label, area = area_cumulative)
        }
        
        if ('area' %in% names( arena.postcategory_table)) {           # get ps weights as areas
          if ( anyNA( arena.postcategory_table$area)) arena.postcategory_table$area[ is.na( arena.postcategory_table$area) ] <- 0
          arena.postcategory_table$area <- as.numeric( arena.postcategory_table$area )
          
          ps.weights  <- arena.postcategory_table %>% 
            select( postStratificationAttribute = code, Freq = area) 
        } 
      } # arena.chainSummary$postStratificationAttributeCategory != ""
    } # (arena.chainSummary$postStratificationAttribute != "")
    
    
    aoi_df                           <- NULL
    arena.stratification_area_exists <- FALSE
    
    
    if ( arena.stratification ) {
      aoi_df        <- as.data.frame( categories[[ arena.chainSummary$stratumAttributeCategory ]])
      if ( arena.chainSummary$analysis$nonResponseBiasCorrection ) {
        if (  'design_psu' %in% names( aoi_df) & !'design_ssu' %in% names( aoi_df)) aoi_df$design_ssu <- 0
        if ( !'design_psu' %in% names( aoi_df) &  'design_ssu' %in% names( aoi_df)) aoi_df$design_psu <- 0
        if (  'design_psu' %in% names( aoi_df) &  'design_ssu' %in% names( aoi_df)) {
          aoi_df$design_psu <- as.numeric( aoi_df$design_psu)
          aoi_df$design_ssu <- as.numeric( aoi_df$design_ssu)
        } else {
          arena.chainSummary$analysis$nonResponseBiasCorrection <- FALSE
          aoi_df$design_psu <- 0
          aoi_df$design_ssu <- 0
        }
      } else {
        aoi_df$design_psu <- 0
        aoi_df$design_ssu <- 0
      }
      
      if ( arena.post_stratification )  aoi_df$area <- NULL 
      
      # if hierarchical table, take data from the correct level
      if (('level_1_code' %in% names( aoi_df )) & ('area_cumulative' %in% names( aoi_df ))) {
        
        # set missing area to 0
        if ( anyNA( aoi_df$area_cumulative)) aoi_df$area_cumulative[ is.na( aoi_df$area_cumulative) ] <- 0
        aoi_df$area_cumulative <- as.numeric( aoi_df$area_cumulative )
        if ( !all( aoi_df$area_cumulative == 0)) arena.stratification_area_exists <- TRUE
        
        aoi_df      <- aoi_df %>%
          filter( level == arena.chainSummary$stratumAttributeCategoryLevel) %>%
          select( code = code_joint, label, area=area_cumulative, design_psu, design_ssu)
        
      } else if (('level_1_code' %in% names( aoi_df)) & 'area' %in% names( aoi_df)) {    # flat lookup table with area
        if ( anyNA( aoi_df$area)) aoi_df$area[ is.na( aoi_df$area) ] <- 0
        aoi_df$area <- as.numeric( aoi_df$area )
        if ( !all( aoi_df$area == 0)) arena.stratification_area_exists <- TRUE
        
        if ('code_joint' %in% names(aoi_df )) {
          aoi_df      <- aoi_df %>%
            dplyr::filter(level == arena.chainSummary$stratumAttributeCategoryLevel) %>%
            dplyr::select( code = code_joint, label, area, design_psu, design_ssu)
        } else {
          aoi_df      <- aoi_df %>%
            dplyr::filter(level == arena.chainSummary$stratumAttributeCategoryLevel) %>%
            dplyr::select( code, label, area, design_psu, design_ssu)
        }
        
      } else if ( 'area' %in% names( aoi_df)) {    # flat lookup table with area, no "level"
        if ( anyNA( aoi_df$area)) aoi_df$area[ is.na( aoi_df$area) ] <- 0
        aoi_df$area <- as.numeric( aoi_df$area )
        if ( !all( aoi_df$area == 0)) arena.stratification_area_exists <- TRUE
        
        aoi_df <- aoi_df %>%
            select( code, label, area, design_psu, design_ssu)

      } else if ( arena.analyze$reportingArea > 0 ) {
          aoi_df$area <- 0.0
      } else if ( !arena.post_stratification ) {
          print(paste0( "''area' column for strata missing in table '", arena.chainSummary$stratumAttributeCategory, "'. Stratification cannot be applied!!")) 
          arena.stratification <- FALSE
      } else {
          arena.stratification <- FALSE
      }
    } 
    
    # CLUSTER SAMPLING: assign  cluster weights. Base units' weights are re-scaled so that the sum of weights in a cluster is equal to 1. 
    
    if (cluster_UUID_ != "" ) {
      
      df_base_unit <- df_base_unit %>% 
        dplyr::group_by( !!! syms( cluster_UUID_ ))                          %>%
        dplyr::summarize( sum_weight_ = sum( weight ), cluster_count_ = n() )   %>%
        dplyr::select( all_of( cluster_UUID_), sum_weight_, cluster_count_ )    %>%
        dplyr::right_join( df_base_unit, by = cluster_UUID_ )                
      
      if (!arena.chainSummary$analysis$clusteringVariances) df_base_unit$weight = df_base_unit$weight / df_base_unit$sum_weight_ 
    } 
    
    
    # NO NONRESPONSE CORRECTION, or DEFAULT NON-RESPONSE CORRECTION VALUES
    df_base_unit$arena_psu_correction       <- 1  # correction factor for PSUs
    df_base_unit$arena_ssu_correction       <- 1  # correction factor for SSUs
    
    
    # A. MISSING SECONDARY SAMPLING UNITS (SSUs): nonresponse bias correction, naive imputation method
    if ( arena.stratification & !is.null( aoi_df) & cluster_UUID_ != "" & arena.chainSummary$analysis$nonResponseBiasCorrection) {  # aoi exist
      aoi_df[ arena.strat_attribute ] <- aoi_df$code
      
      if ('design_ssu' %in% names( aoi_df)) {
        aoi_df$design_ssu[ is.na( aoi_df$design_ssu)] <- 0
        
        if ( sum( aoi_df$design_ssu ) > 0) {        
          df_base_unit$arena_ssu_correction <- NULL
          arena_cluster_statistics <- aoi_df                              %>%
            dplyr::select( all_of( arena.strat_attribute), design_ssu )   %>%
            dplyr::right_join( df_base_unit                               %>% 
                                 dplyr::filter( weight > 0 )                     %>%
                                 dplyr::group_by( !!! syms( arena.strat_attribute), across( all_of( cluster_UUID_ )))       %>%
                                 dplyr::summarize( cluster_count_ = n(), sum_weight_ = sum( weight )), by = arena.strat_attribute)  %>%
            dplyr::mutate( arena_ssu_correction = ifelse( design_ssu > 0 & sum_weight_ > 0, ( design_ssu / cluster_count_) * sum_weight_, 1)) # this works if a full base unit weight is 1 !!
          
          
          # check whether some clusters are split over more than 1 stratum
          if ( nrow( arena_cluster_statistics) != nrow( unique( arena_cluster_statistics[cluster_UUID_] ))) {
            # list of clusters belonging to multiple strata
            analyze_overlaps <- arena_cluster_statistics         %>%
              dplyr::group_by( across( all_of( cluster_UUID_ ))) %>%
              dplyr::summarize( c_count = n() )                  %>%
              dplyr::filter( c_count > 1 )                       %>%
              pull( cluster_UUID_ ) 
            
            # fix this later, overlaps get all 1:
            arena_cluster_statistics$arena_ssu_correction <- with( arena_cluster_statistics,
                                                                   ifelse( cluster_UUID_ %in% analyze_overlaps, 1, arena_ssu_correction)) 
            
            df_base_unit <- df_base_unit %>%
              dplyr::left_join( arena_cluster_statistics %>% 
                                  dplyr::select( !!! syms( arena.strat_attribute), all_of( cluster_UUID_), arena_ssu_correction), by = c( arena.strat_attribute, cluster_UUID_))
            
          } else {  # no clusters split across strata
            df_base_unit <- df_base_unit                        %>%
              dplyr::left_join( arena_cluster_statistics        %>% 
                                  dplyr::select( all_of( cluster_UUID_), arena_ssu_correction), by = cluster_UUID_)
          }
        }
      }
      
    } else if (cluster_UUID_ != "" & arena.chainSummary$analysis$nonResponseBiasCorrection & !arena.chainSummary$analysis$clusteringVariances) {
      # non-stratified cluster sampling with non-response bias correction (for missing samples in clusters)
      ssu_max = max( df_base_unit$cluster_count_ )
      df_base_unit$arena_ssu_correction <- ifelse( ssu_max > 0 & df_base_unit$sum_weight_ > 0, ( ssu_max / df_base_unit$cluster_count_ ) , 1 )
    }
    
    # 1. non-stratified sampling, compute expansion factor for the base unit
    # 2. stratified sampling,  bias correction for missing clusters/plots
    
    # B STRATIFIED SAMPLING, MISSING PRIMARY SAMPLING UNITS (PSUs): nonresponse bias correction, naive imputation method
    if ( arena.stratification & !is.null( aoi_df ) & arena.chainSummary$analysis$nonResponseBiasCorrection ) {  
      aoi_df[ arena.strat_attribute ] <- aoi_df$code
      
      if ( 'design_psu' %in% names( aoi_df) ) {
        aoi_df$design_psu[ is.na( aoi_df$design_psu) ] <- 0
        
        if ( sum( aoi_df$design_psu) > 0) {
          df_base_unit$arena_psu_correction    <- NULL
          
          # clustered sampling
          if ( cluster_UUID_ != "" ) {
            arena_psu_statistics <- df_base_unit                                                   %>% 
              dplyr::filter( weight > 0 )                                                          %>%
              dplyr::group_by( !!! syms( arena.strat_attribute), across( all_of( cluster_UUID_ ))) %>%
              dplyr::summarize( cluster_count = n(), by = arena.strat_attribute )                  %>%
              dplyr::group_by( !!! syms( arena.strat_attribute ))                                  %>%
              dplyr::summarize( psu_count = n() )                                                  %>%
              dplyr::left_join( aoi_df                                                             %>% 
                                  dplyr::select( all_of( arena.strat_attribute), design_psu ), by = arena.strat_attribute) %>%
              dplyr::mutate( arena_psu_correction = design_psu / psu_count )                       %>%
              dplyr::select( all_of( arena.strat_attribute), arena_psu_correction )                %>%
              as.data.frame()
          } else {
            # not clustered
            arena_psu_statistics <- df_base_unit                  %>% 
              dplyr::filter( weight > 0 )                         %>%
              dplyr::group_by( !!! syms( arena.strat_attribute )) %>%
              dplyr::summarize( baseunit_count = n() )            %>%
              dplyr::left_join( aoi_df                            %>% 
                                  dplyr::select( all_of( arena.strat_attribute), design_psu), by = arena.strat_attribute ) %>%
              dplyr::mutate( arena_psu_correction = design_psu / baseunit_count )    %>%
              dplyr::select( all_of( arena.strat_attribute), arena_psu_correction )  %>%
              as.data.frame()
          } 
          
          # join PSU correction with base unit table 
          df_base_unit <- df_base_unit %>%
            dplyr::left_join( arena_psu_statistics, by = arena.strat_attribute)
        } # if (sum(aoi_df$design_psu > 0)
      }
    }
    
    # add SSU and PSU non-response bias corrections to weight
    df_base_unit$weight <- df_base_unit$weight * df_base_unit$arena_ssu_correction * df_base_unit$arena_psu_correction
    
    
    # calculate expansion factors for AOIs. AOI table exists
    df_base_unit$exp_factor_ <- NULL
    
    if ( arena.stratification & !is.null( aoi_df)) {
      aoi_df[ arena.strat_attribute ] <- aoi_df$code
      
      arena.expansion_factor <- df_base_unit                         %>%             # Do: missing stratum in base unit?
        dplyr::filter( weight > 0 )                                  %>%
        dplyr::group_by( across( all_of( arena.strat_attribute )))   %>%
        dplyr::summarize( aoi_weight_ = sum( weight ))               %>% 
        dplyr::left_join( aoi_df                                     %>% 
                            dplyr::select( all_of( arena.strat_attribute), area), by = arena.strat_attribute ) %>% 
        data.frame()
      
      if ( all( aoi_df$area == 0) & ( arena.analyze$reportingArea > 0 )) arena.expansion_factor$area <-  arena.analyze$reportingArea / sum( arena.expansion_factor$aoi_weight_) *  arena.expansion_factor$aoi_weight_ 
      
      arena.expansion_factor$exp_factor_ <- arena.expansion_factor$area / arena.expansion_factor$aoi_weight_ 
      
      df_base_unit <- df_base_unit                                   %>%
        dplyr::left_join( arena.expansion_factor                     %>%
                            dplyr::select( !!! syms( arena.strat_attribute), exp_factor_), by = arena.strat_attribute) %>%
        dplyr::mutate( exp_factor_ = exp_factor_ * weight)
      
    } else if ( arena.stratification) { 
      arena.expansion_factor <- df_base_unit                         %>%
        dplyr::filter( weight > 0 )                                  %>%
        dplyr::group_by( across( all_of( arena.strat_attribute )))   %>%
        dplyr::summarize( aoi_weight_ = sum( weight ))               %>% 
        data.frame()
      
      if ( arena.analyze$reportingArea > 0 ) arena.expansion_factor$area <-  arena.analyze$reportingArea / sum( arena.expansion_factor$aoi_weight_) *  arena.expansion_factor$aoi_weight_ 
      arena.expansion_factor$exp_factor_ <- arena.expansion_factor$area / arena.expansion_factor$aoi_weight_ 
      
      df_base_unit <- df_base_unit                                   %>%
        dplyr::left_join( arena.expansion_factor                     %>%
                            dplyr::select( !!! syms( arena.strat_attribute), exp_factor_), by = arena.strat_attribute) %>%
        dplyr::mutate( exp_factor_ = exp_factor_ * weight)
      
    } else if ( arena.analyze$reportingArea > 0 ) {
      df_base_unit$exp_factor_   <- arena.analyze$reportingArea / sum( df_base_unit$weight) * df_base_unit$weight
      
    } else {
      # no stratification, no AOI data 
      df_base_unit$exp_factor_   <- df_base_unit$weight 
    }
    
    df_base_unit$exp_factor_[ is.na( df_base_unit$exp_factor_) ] <- 0
    
    ##########################################################################################
    
    
    
    # get a list of active area-based result variables (of all entities)
    # parents of base units cannot have area-based variables!
    result_entities <- arena.chainSummary$resultVariables %>%
      dplyr::filter( areaBased == TRUE & active == TRUE & stringr::str_detect(entityPath, arena.chainSummary$baseUnit))  %>%
      #      filter(entity != arena.chainSummary$baseUnit) %>%  
      select( entity ) %>% 
      unique()         %>%
      pull()
    
    # next loop over result entities, collects a list of all categorical, taxonomic and boolean variables
    # get: entity, base_UUID_, exp_factor_, and combinations for all categorical variables in the data
    result_cat_attributes <- list()
    result_cat            <- list()
    base_unit.results     <- list()
    cluster.results       <- list()
    
    # ADD area estimates based on exp_factor
    
    # Quantitative result variables against categorical and taxonomic data
    for ( i in (1:length(result_entities))) {
      
      # PART 1.compute sums (per AOIs) and means (/ha) across all categorical variables 
      # drop out columns where all data is NA.  https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
      
      df_entitydata              <- get(result_entities[[i]])
      df_entitydata              <- conversion_HierarchicalCodeAttributes( df_entitydata )
      
      
      result_cat_attributes[[i]] <- ( df_entitydata  %>% select( where( ~!all( is.na(.)))) %>% select( where( ~is.character(.))) )  %>%
        select(ends_with("_label") | ends_with("_scientific_name")) %>% 
        names()
      
      boolean_list <-  ( df_entitydata  %>% select( where( ~!all( is.na(.)))) %>% select( where( ~is.logical(.))) )  %>%
        # select(where(~all( .=="false" | .=="true"))) %>% 
        # select(where(~!all(.=="false")))             %>%
        # select(where(~!all(.=="true")))              %>% 
        names()
      
      if (length(boolean_list) > 0) result_cat_attributes[[i]] <- unique( c(result_cat_attributes[[i]], boolean_list))
      #        rm(boolean_list)
      
      # Remove: _label, _scientific_name 
      result_cat_attributes[[i]] <- ifelse( stringr::str_sub(result_cat_attributes[[i]], -6, -1)  == "_label",           stringr::str_sub(result_cat_attributes[[i]], 0, -7),  result_cat_attributes[[i]] )  
      result_cat_attributes[[i]] <- ifelse( stringr::str_sub(result_cat_attributes[[i]], -16, -1) == "_scientific_name", stringr::str_sub(result_cat_attributes[[i]], 0, -17), result_cat_attributes[[i]] )  
      
      # join parents' categorical result attributes with entity data
      # add categorical result variables
      # 1) search parents' names
      parent_names               <- df_entitydata %>% 
        dplyr::select(ends_with("_uuid"), -ends_with("_file_uuid"), -record_uuid, -paste0(result_entities[[i]], "_uuid")) %>% 
        names %>%
        stringr::str_sub(., 0,-6)
      
      
      # 2) join parents' cat. data
      if (length(parent_names) > 0) {
        res_cat                    <- arena.chainSummary$resultVariables %>% 
          dplyr::filter( type == "C" & active == TRUE & entity %in% parent_names)   %>% 
          dplyr::select( entity, name) 
        if ( nrow( res_cat) > 0) {
          for (j in (1:nrow(res_cat) )) {
            df_join       <- get( res_cat$entity[j]) %>% select( paste0( res_cat$entity[j], "_uuid"), res_cat$name[j] ) 
            df_entitydata <- df_entitydata %>%
              dplyr::left_join( df_join, by = paste0( res_cat$entity[j], "_uuid")) 
            
            rm( df_join )
          }
        }
      } 
      
      # 3) list of names: add base unit id, and categorical result variables (of parents & selected entity)
      res_cat         <- arena.chainSummary$resultVariables %>% 
        dplyr::filter( type=="C" & active==TRUE & entity %in% c(parent_names, result_entities[[i]]) ) %>% 
        pull( name)
      
      result_cat_attributes[[i]] <- unique( c(base_UUID_, result_cat_attributes[[i]], res_cat) )
      
      resultVariables <- arena.chainSummary$resultVariables %>%
        dplyr::filter( areaBased == TRUE & active == TRUE & entity == result_entities[[i]]) %>%
        dplyr::mutate( name = paste0( name, "_ha")) %>%
        pull( name) 
      
      
      if (result_entities[[i]] != arena.chainSummary$baseUnit) {
        
        # get a list of base unit IDs that are not in entity data (i.e. treeless plots)
        missing_ids     <- setdiff( unique( unlist( df_base_unit %>% filter( weight > 0) %>% select( all_of( base_UUID_)) )), unique( unlist( df_entitydata[ base_UUID_] )))
        
        if ( length( missing_ids) > 0) {
          # get list of attributes that exist in base unit data
          names_in_data   <- intersect( names( df_entitydata), names( df_base_unit)) 
          
          df_base_unit2   <- subset( df_base_unit,  eval( parse( text = base_UUID_)) %in% missing_ids)
          result_cat[[i]] <- bind_rows( df_entitydata, df_base_unit2 %>% select( all_of( names_in_data)) )
          result_cat[[i]] <- result_cat[[i]]  %>% 
            dplyr::mutate( across( where( is.numeric), ~tidyr::replace_na(., 0)))
          rm( names_in_data); rm( df_base_unit2)
        } else {
          result_cat[[i]] <- df_entitydata
        }
        
        rm( missing_ids )
        
        # Compute mean and total for each result category group
        result_cat[[i]] <- result_cat[[i]]                             %>%
          dplyr::right_join( df_base_unit %>% select( all_of( base_UUID_), exp_factor_), by = base_UUID_ ) %>% # join expansion factor
          dplyr::group_by(  across( result_cat_attributes[[i]] ))      %>%
          dplyr::summarize( across(.cols= all_of(resultVariables), 
                                   list( Total = ~sum(exp_factor_ * .x, na.rm = TRUE), Mean = ~sum(.x, na.rm = TRUE) ),  
                                   .names = "{.col}.{.fn}"), 
                            entity_count_ = n() )                      %>%
          data.frame() 
          # %>%
          # in cases where reporting entity contains an categ. attribute and missing base unit ids, there are NAs. These are removed. 
          # drop_na()
        
      } else { # entity is the base unit
        result_cat[[i]] <- df_entitydata                               %>%
          dplyr::left_join( df_base_unit %>% select( all_of(base_UUID_), exp_factor_ ), by = base_UUID_) %>%
          dplyr::group_by(  across( result_cat_attributes[[i]] ))      %>%
          dplyr::summarize( across( .cols= all_of( resultVariables), 
                                    list( Total = ~sum(exp_factor_ * .x, na.rm = TRUE), Mean = ~sum( .x, na.rm = TRUE) ),  
                                    .names = "{.col}.{.fn}"), 
                            entity_count_ = n() )                      %>%
          data.frame()
      }
      
      
      # add weight, exp_factor_; AND IF EXISTS: cluster_UUID_, arena.strat_attribute (This is actually already in dataframe because it is categorical!) 
      temp_list_variables <- c(base_UUID_, "weight", "exp_factor_")
      if ( cluster_UUID_ != ""    &  !( cluster_UUID_ %in% names(result_cat[[i]])) )        temp_list_variables <- c( temp_list_variables, cluster_UUID_)
      if ( arena.stratification  &  !( arena.strat_attribute %in% names(result_cat[[i]])) ) temp_list_variables <- c( temp_list_variables, arena.strat_attribute)
      
      result_cat[[i]] <- result_cat[[i]] %>%
        dplyr::left_join( df_base_unit %>% select( all_of( temp_list_variables)), by = base_UUID_)
      
      rm( temp_list_variables )
      
      ## PART 2. compute sum of per hectare results at the base unit level for each result variable
      base_unit.results[[i]] <- df_entitydata %>%
        # Add expansion factor for all result entities
        dplyr::right_join(( df_base_unit %>% select( all_of( base_UUID_), weight, exp_factor_)), by = base_UUID_) %>%
        dplyr::group_by( across( all_of(base_UUID_ ))) %>%
        dplyr::summarize( across( .cols= all_of( resultVariables),
                                  list( Total = ~sum( exp_factor_ * .x, na.rm = TRUE), Mean = ~sum( .x, na.rm = TRUE) ),
                                  .names = "{.col}.{.fn}"),
                          item_count = n() )
      
      # join results with the clone of base unit
      df_base_unit <- df_base_unit %>%
        dplyr::left_join( base_unit.results[[i]], by = base_UUID_)
      
      
      ## PART 3. compute sum of per hectare results at the cluster level for each result variable
      
      if ( cluster_UUID_ != "" ) {
        
        df_entitydata$weight = NULL
        cluster.results[[i]] <- df_entitydata %>%
          # Add expansion factor for all result entities
          #        dplyr::left_join( cluster.weights, by = cluster_UUID_) %>%
          dplyr::right_join(( df_base_unit %>% select( all_of( base_UUID_), weight, exp_factor_)), by = base_UUID_) %>%
          dplyr::group_by( across( all_of( cluster_UUID_ )))                                                        %>%
          dplyr::summarize( across(.cols= all_of( resultVariables ),
                                   list( Total = ~sum( exp_factor_ * .x, na.rm = TRUE), Mean = ~mean( .x, na.rm = TRUE)),
                                   .names = "{.col}.{.fn}"),
                            sum_weight = sum( weight) )
      }      
      
      rm(resultVariables)
    }
    
    names(result_cat) <- result_entities    
    
  } # END OF (arena.chainSummary$samplingDesign == TRUE)
  
  
  
  ###############################################################################################
  # SAMPLING STRATEGIES AND RELIABILITY
  
  # do analysis based on reportingMethod
  # '1' = Combination of dimensions (default)
  # '2' = Dimensions separately
  
  # ****************************************************
  # do first filtering, if filter clause exists. If error, ignore a filter rule      
  processMessage <- tryCatch({ if ( arena.analyze$filter != "" ) {  
    df_analysis_combined <- df_analysis_combined %>%
      filter( eval( parse( text = arena.analyze$filter )))
    processMessage
  }},
  warning = function( w ) { 
    cat("Error in filter clause - Filter not applied!") 
    return("Error in filter clause - Filter not applied.")},
  error   = function( e ) { 
    cat("Error in filter clause - Filter not applied!")
    return("Error in filter clause - Filter not applied.")}
  )
  # ****************************************************
  
  
  arena.analyze$dimensions_input <- arena.analyze$dimensions
  if ( arena.analyze$reportingMethod == '1' ) arena.reportingLoops = 1
  if ( arena.analyze$reportingMethod == '2' ) arena.reportingLoops = length( arena.analyze$dimensions )
  
  
  # get all files in the main output folder
  f <- list.files(user_file_path, full.names = TRUE, recursive = FALSE)
  f <- f[!file.info(f)$isdir]
  
  # remove the files in the main output folder
  if (length(f)) file.remove(f)
  rm(f)
  
  out_path  <- "dimensions/"
  if ( dir.exists( paste0( user_file_path, out_path ))) unlink(paste0(user_file_path, out_path), recursive = TRUE)
  dir.create( paste0( user_file_path, out_path ), showWarnings = FALSE )
  
  
  for ( rep_loop in (1 : arena.reportingLoops )) {
    if ( arena.analyze$reportingMethod == '2' ) {
      arena.analyze$dimensions <- arena.analyze$dimensions_input[rep_loop]
      out_path                 <- paste0( "dimensions/", arena.analyze$dimensions, "/")
      if ( dir.exists( paste0( user_file_path, arena.analyze$dimensions ))) unlink( paste0( user_file_path, arena.analyze$dimensions), recursive = TRUE)
      dir.create( paste0( user_file_path, "dimensions/", arena.analyze$dimensions ), showWarnings = FALSE )
    } 
    
    # get labels to the categorical result variables, [1]: input attribute, [2]: result attribute 
    result_labels           <- list() 
    
    result_names_category_1 <- setdiff(   arena.analyze$dimensions, arena.chainSummary$resultVariables$name) # cat. attributes in input data 
    result_names_category_2 <- intersect( arena.analyze$dimensions, arena.chainSummary$resultVariables$name) # cat. attributes as result attributes
    
    # join stratification attribute labels
    if ( arena.stratification ) {
      if (!(arena.strat_attribute %in% result_names_category_1) & !(arena.strat_attribute %in% result_names_category_2)) {
        result_names_category_1 <- c( result_names_category_1, arena.strat_attribute )
      } 
    }
    
    if (length( result_names_category_1 ) > 0) {
      df_cat_report <- get( arena.analyze$entity) 
      df_cat_report <- conversion_HierarchicalCodeAttributes( df_cat_report )
      
      df_cat_report <- df_cat_report %>%  
        select( all_of(result_names_category_1), any_of( paste0( result_names_category_1,"_label")), any_of( paste0( result_names_category_1,"_scientific_name"))) %>%
        distinct()
      
      for ( i in (1 : length(result_names_category_1))) {
        label_column   <- paste0(result_names_category_1[i], "_label")
        species_column <- paste0(result_names_category_1[i], "_scientific_name")
        
        # in case of boolean data, label is same as data value
        if ( !(label_column %in% names( df_cat_report))) df_cat_report[ label_column ] <- df_cat_report[ result_names_category_1[i] ]
        # but in case of taxon data, label is the scientific name
        if ( species_column %in% names( df_cat_report))  df_cat_report[ label_column ] <- df_cat_report[ species_column ]
        
        result_labels[[i]]  <- df_cat_report %>% 
          select( code = result_names_category_1[i], label = all_of(label_column) ) %>%
          distinct() %>%
          arrange( .[1])
        
        # if species code is blank, then here is given a new label
        result_labels[[i]]$label[result_labels[[i]]$code == ""] <- "No code" 
      }
      rm( df_cat_report ); rm( label_column ); rm( species_column )
    }
    
    if ( length( result_names_category_2 ) > 0) {
      if (!is.na(result_names_category_2)) {
        dataindex <- length(result_labels) 
        
        df_cat_report <- arena.chainSummary$resultVariables                     %>%
          filter( type == "C" & active == TRUE & name %in% result_names_category_2) %>%
          select( categoryName ) 
        
        for ( i in (1:nrow( df_cat_report))) {
          result_labels[[ dataindex + i]]  <- 
            categories[ df_cat_report$categoryName[[i]] ] %>% 
            as.data.frame()                               %>%
            select( ends_with('.code'), ends_with('.label'))
          
          names(result_labels[[dataindex + i]]) <- c("code","label") 
        }
    }}
    
    names(result_labels) <- c( result_names_category_1, result_names_category_2 )
    
    if (exists("result_names_category_1")) rm(result_names_category_1) 
    if (exists("result_names_category_2")) rm(result_names_category_2) 
    
    # get analysis data 
    df_analysis <- as.data.frame( result_cat[[arena.analyze$entity]] )
    
    # get categorical variables
    cat_names_uuid <- result_cat[[ arena.analyze$entity ]] %>%
      data.frame()               %>%
      select( where( is.character))    %>%
      select(ends_with("_uuid")) %>%
      names()
    
    # get numeric variables
    cat_names_num <- result_cat[[ arena.analyze$entity ]] %>%
      data.frame()                                        %>%
      select( where( is.numeric))                               %>%
      select( -weight, -exp_factor_, -entity_count_)       %>%
      names() 
    
    
    #*# stratification attribute name into Dimensions
    if ( arena.stratification ) arena.analyze$dimensions <- unique( c( arena.analyze$dimensions, arena.strat_attribute))
    
    if ( arena.post_stratification  ) {
      if ( is.null( ps.weights)) {    # get ps weights from proportions in data
        ps.weights <- df_base_unit                                                    %>%
          dplyr::group_by( across( arena.chainSummary$postStratificationAttribute ))  %>%
          dplyr::summarize( Freq = sum( exp_factor_))                                 %>%              
          data.frame()
        
        names(ps.weights)[[1]] <- "postStratificationAttribute" 
      }
      if ( !"NoData_" %in% ps.weights$code & anyNA( df_base_unit[ arena.chainSummary$postStratificationAttribute ]) ) ps.weights <- rbind( ps.weights, c("NoData_", 0.001))
      ps.weights$Freq <- as.numeric( ps.weights$Freq )
      
      
      # add a static column name 'postStratificationAttribute'
      for ( i in (1:length(result_cat))) {
        result_cat[[i]]$postStratificationAttribute <- result_cat[[i]][ arena.chainSummary$postStratificationAttribute ][[1]] 
        result_cat[[i]]$postStratificationAttribute[ is.na(result_cat[[i]]$postStratificationAttribute) ] <- "NoData"
      } 
      
      arena.analyze$dimensions          <- unique( c( arena.analyze$dimensions,          "postStratificationAttribute" ))
      arena.analyze$dimensions_baseunit <- unique( c( arena.analyze$dimensions_baseunit, "postStratificationAttribute" ))
    }
    
    if ( is.null( processMessage )) processMessage = ""
    
    dimension_names <- unique( c( cat_names_uuid, arena.analyze$dimensions))
    if ( arena.stratification )      dimension_names <- unique( c( dimension_names, arena.strat_attribute))
    if ( arena.post_stratification ) dimension_names <- unique( c( dimension_names, arena.chainSummary$postStratificationAttribute ))
    
    
    # Data into the analysis
    df_analysis_combined <- result_cat[[ arena.analyze$entity ]]
    
    # compress data in cluster sampling
    # (entity_count_ : number of base units in a cluster)
    if ( cluster_UUID_ != "" & !arena.chainSummary$analysis$clusteringVariances ) {
      
      ids_2_survey          <- NULL
      dimension_names       <- dimension_names[ !stringr::str_detect( dimension_names, pattern = base_UUID_) ] # remove a list element
      cat_names_uuid        <- cat_names_uuid[  !stringr::str_detect( cat_names_uuid,  pattern = base_UUID_) ]
      
      df_analysis_combined  <- df_analysis_combined                               %>%
        dplyr::select( -all_of( base_UUID_))                                      %>% 
        dplyr::group_by( across( all_of( cluster_UUID_ )))                        %>%
        dplyr::mutate( across( ends_with('.Total'), ~sum(  ., na.rm = TRUE )))    %>%
        dplyr::mutate( across( ends_with('.Mean'),  ~mean( ., na.rm = TRUE )))    %>%
        dplyr::mutate( weight = sum( weight), exp_factor_ = sum( exp_factor_), entity_count_ = sum( entity_count_)) %>% 
        distinct( !!! syms( cluster_UUID_), .keep_all = TRUE)                     %>%
        data.frame()
      
      df_analysis_weights <- df_analysis_combined %>%
        select( all_of( cluster_UUID_), weight, exp_factor_ )
      
    } else if ( cluster_UUID_ != "" & arena.chainSummary$analysis$clusteringVariances ) {
      ids_2_survey        <- cluster_UUID_
      
      df_analysis_weights  <- df_analysis_combined                              %>% 
        distinct( !!! syms( base_UUID_), .keep_all = T)                         %>% 
        dplyr::select( all_of( base_UUID_), weight, exp_factor_)
      
    } else {
      ids_2_survey          <- NULL
      
      df_analysis_weights  <- df_analysis_combined                              %>% 
        distinct( !!! syms( base_UUID_), .keep_all = T)                         %>% 
        dplyr::select( all_of( base_UUID_), weight, exp_factor_)
    } 
    
    
    df_analysis_area    <- df_analysis_combined                                 %>%
      dplyr::filter( weight > 0 )                                               %>%
      dplyr::group_by(  across( all_of( dimension_names )))                     %>%
      dplyr::summarize( across( .cols = all_of( cat_names_num), 
                                list( Total = ~sum( .x, na.rm = TRUE )),  
                                .names = "{.col}") )                             %>%
      data.frame()
    
    
    df_analysis_combined <- df_analysis_combined                                %>%
      dplyr::filter( weight > 0 )                                               %>%
      dplyr::group_by(  across( unique( c( cat_names_uuid, arena.analyze$dimensions)))) %>%
      dplyr::summarize( across( .cols= all_of( cat_names_num), 
                                list( Total = ~sum( .x, na.rm = TRUE )),  
                                .names = "{.col}") )                             %>%
      data.frame()
    
    
    if ( cluster_UUID_ != "" & !arena.chainSummary$analysis$clusteringVariances ) {
      df_analysis_area <- df_analysis_area                             %>%
        dplyr::left_join( df_analysis_weights, by = cluster_UUID_)
      
      df_analysis_combined <- df_analysis_combined                     %>%  
        dplyr::left_join( df_analysis_weights, by = cluster_UUID_)
    } else {
      df_analysis_area <- df_analysis_area                             %>%
        dplyr::left_join( df_analysis_weights, by = base_UUID_)
      
      df_analysis_combined <- df_analysis_combined                     %>%  
        dplyr::left_join( df_analysis_weights, by = base_UUID_)
    }
    
    if ( all( df_analysis_combined$exp_factor_ == 0)) df_analysis_combined$exp_factor_ = df_analysis_combined$weight
    
    # missing stratum code set to "", in order to report these too
    stratum_2_survey <- NULL 
    
    if ( arena.stratification ) {
      if ( arena.strat_attribute != "") stratum_2_survey <- arena.strat_attribute
      df_analysis_area[ stratum_2_survey][ is.na( df_analysis_area[ stratum_2_survey]) ] <- ""
      df_analysis_combined[ stratum_2_survey][ is.na( df_analysis_combined[ stratum_2_survey]) ] <- ""
    }
    
    if ( arena.chainSummary$samplingStrategy %in% c(1:4)) { 
      # 1. SIMPLE RANDOM SAMPLING, cluster/non-cluster
      # 2. SYSTEMATIC SAMPLING, cluster/non-cluster 
      # 3. STRATIFIED RANDOM SAMPLING, cluster/non-cluster  
      # 4. STRATIFIED SYSTEMATIC SAMPLING, cluster/non-cluster
      
      arena.weights <- ifelse( arena.stratification & arena.stratification_area_exists, "exp_factor_", "weight") 
      
      # if stratification, compute SRS case. Needed to compute efficiency of stratification.
      if ( arena.stratification | arena.post_stratification ) {
        design_srvyr_SRS_total <- df_analysis_combined  %>%
          srvyr::as_survey_design(
            ids       = !!ids_2_survey,
            strata    = NULL,
            fpc       = NULL, 
            weights   = NULL, 
            variables = c( arena.analyze$dimensions, ends_with('.Total')) )
      }
      
      design_srvyr_mean <-  df_analysis_combined       %>%
        srvyr::as_survey_design(
          ids       = !!ids_2_survey,
          strata    = !!stratum_2_survey,
          fpc       = NULL, 
          weights   = !!arena.weights,  
          nest      = FALSE, # If TRUE, relabel cluster ids to enforce nesting within strata
          variables = c( arena.analyze$dimensions, ends_with('.Mean')) )
      
      design_srvyr_global_mean <- NULL
      if (( length( arena.analyze$dimensions_baseunit) > 0 &  arena.analyze$reportingMethod == '2' ) | ( all( arena.analyze$dimensions_at_baseunit) &  arena.analyze$reportingMethod == '1'))  {
        if (arena.analyze$reportingMethod == '2') {
          analyze_variables <- arena.analyze$dimensions
        } else {
          analyze_variables <- arena.analyze$dimensions_baseunit
        }
        
        design_srvyr_global_mean <-  df_analysis_combined   %>% 
          srvyr::as_survey_design(
            ids       = !!ids_2_survey,
            strata    = !!stratum_2_survey,
            fpc       = NULL, 
            weights   = !!arena.weights,  
            nest      = FALSE, # If TRUE, relabel cluster ids to enforce nesting within strata
            variables = c( all_of( analyze_variables), ends_with('.Mean')) )
      }
      
      design_srvyr_total <-  df_analysis_combined     %>%
        srvyr::as_survey_design(
          ids       = !!ids_2_survey,
          strata    = !!stratum_2_survey,
          fpc       = NULL, 
          weights   = NULL, 
          nest      = FALSE,
          variables = c( arena.analyze$dimensions, exp_factor_, ends_with('.Total')) )
      
      
      design_srvyr_area <-   df_analysis_area        %>%
        srvyr::as_survey_design(
          ids       = !!ids_2_survey,
          strata    = !!stratum_2_survey,
          fpc       = NULL, 
          weights   = NULL, 
          nest      = FALSE,
          variables = c( arena.analyze$dimensions, exp_factor_ ) )
      
    }
    
    
    # 5. DOUBLE PHASE * coming later)
    if ( arena.chainSummary$samplingStrategy == 5 ) design_srvyr <- ""
    
    # post-stratification
    # https://github.com/gergness/srvyr/issues/50
    # https://stats.oarc.ucla.edu/r/faq/how-do-i-analyze-survey-data-with-stratification-after-sampling-poststratification/
    
    if ( arena.post_stratification ) { 
      
      design_srvyr_mean  <- survey::postStratify( design_srvyr_mean, 
                                                  strata = ~postStratificationAttribute,
                                                  population = ps.weights,
                                                  partial = TRUE) #if TRUE, ignore population strata not present in the sample
      
      if (!is.null(design_srvyr_global_mean))  {
        design_srvyr_global_mean <- survey::postStratify( design_srvyr_global_mean, 
                                                          strata = ~postStratificationAttribute,
                                                          population = ps.weights,
                                                          partial = TRUE) #if TRUE, ignore population strata not present in the sample
      }
      
      design_srvyr_total <- survey::postStratify( design_srvyr_total, 
                                                  strata = ~postStratificationAttribute,
                                                  population = ps.weights,
                                                  partial = TRUE)
      
      design_srvyr_area <- survey::postStratify( design_srvyr_area, 
                                                 strata = ~postStratificationAttribute,
                                                 population = ps.weights,
                                                 partial = TRUE)
      
    }
    
    
    # Execute the SURVEY. The available functions are
    # survey_mean:     Calculate the survey mean of the entire population or by groups. Based on svymean. survey_mean(x, vartype = "ci")
    # survey_total:    Calculate the survey total of the entire population or by groups. Based on svytotal.
    # survey_ratio:    Calculate the ratio of 2 variables in the entire population or by groups. Based on svyratio.
    # survey_quantile: Calculate quantiles in the entire population or by groups. Based on svyquantile.
    # survey_median:   Calculate the median in the entire population or by groups. Based on svyquantile.
    # unweighted:      Calculate an unweighted estimate as you would on a regular tbl_df. Based on dplyr summarise.
    
    # NOTE: calculate the proportion or count in each group of a factor or character variable by leaving x empty in survey_mean() or survey_total()
    # https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html
    
    # drop out stratification attributes from result tables if these are not selected as dimensions  
    if ( arena.stratification & !( arena.strat_attribute %in% arena.analyze$dimensions_input )) {
      arena.analyze$dimensions          <- arena.analyze$dimensions[! arena.analyze$dimensions                   %in% c( arena.strat_attribute)]
      arena.analyze$dimensions_baseunit <- arena.analyze$dimensions_baseunit[! arena.analyze$dimensions_baseunit %in% c( arena.strat_attribute)]
    }
    
    if ( arena.post_stratification & !( arena.chainSummary$postStratificationAttribute %in% arena.analyze$dimensions_input ) ) {
      arena.analyze$dimensions          <- arena.analyze$dimensions[! arena.analyze$dimensions                   %in% c( arena.chainSummary$postStratificationAttribute, 'postStratificationAttribute')]
      arena.analyze$dimensions_baseunit <- arena.analyze$dimensions_baseunit[! arena.analyze$dimensions_baseunit %in% c( arena.chainSummary$postStratificationAttribute, 'postStratificationAttribute')]
    }
    
    # AREA 
    out_area <- design_srvyr_area                           %>%
      dplyr::group_by( across( arena.analyze$dimensions ))  %>%    
      dplyr::summarize( across( exp_factor_ ,      
                                list( ~survey_total(.) )))  %>%  
      as.data.frame(.)                                      %>%
      rename( area = exp_factor__1, area_sd = exp_factor__1_se)
    
    # MEANS (per hectares) for selected categories
    out_mean  <- design_srvyr_mean                          %>%
      dplyr::group_by( across( arena.analyze$dimensions ))  %>%     
      dplyr::summarize( across( ends_with(".Mean") ,     
                                list( tally = ~sum( !is.na(.)), ~survey_mean( ., na.rm = FALSE, vartype = c("se", "var", "ci"), proportion = FALSE, level=arena.chainSummary$analysis$pValue )))) %>% 
      as.data.frame(.)                                      %>%
      setNames( stringr::str_replace( names(.), ".Mean_2", ".Mean")) 
    

    # compute totals, multiple the means by areas
    out_mean_chr   <- out_mean %>% select( where( is.character))
    out_mean_num   <- out_mean %>% select( where( is.numeric), ends_with("_tally"))
    out_total      <- out_area$area * out_mean_num
    
    out_total      <- cbind( out_mean_chr, out_total ) 
    if (!("area" %in% names( out_total))) out_total$area <- out_area$area  
    rm( out_mean_num ); rm( out_mean_chr )      
    
    out_total     <- out_total %>% setNames( stringr::str_replace( names(.), ".Mean", ".Total"))
    
    
    # ALL DATA (totals). Total variances are correctly computed here also for stratified sampling
    jdesign <- update( design_srvyr_total, whole_area_ = 1 )
    
    out_global_total <- jdesign %>%
      dplyr::group_by( whole_area_ )   %>%       
      dplyr::summarize( across( c( exp_factor_, ends_with(".Total") ),      
                                list( ~survey_total( ., vartype = c("se", "var", "ci"), level=arena.chainSummary$analysis$pValue ))))         %>%  
      as.data.frame(.)  %>%
      setNames( stringr::str_replace( names(.), ".Total_1", ".Total"))
    
    if (!is.null(design_srvyr_global_mean))  {
      out_global_mean <- design_srvyr_global_mean  %>%
        dplyr::summarize( across( c( ends_with(".Mean") ),   
                                  list( ~survey_mean( ., na.rm = FALSE, vartype = c("se", "var", "ci"), proportion = FALSE, level=arena.chainSummary$analysis$pValue )))) %>% 
        as.data.frame(.)  %>%
        setNames( stringr::str_replace( names(.), ".Mean_1", ".Mean"))
    }
    
    out_global_total$tally <- nrow( df_base_unit %>% filter( weight>0 ) %>% select( all_of( base_UUID_)) %>% unique() )
    # drop out area estimates from this table
    out_global_total <- out_global_total %>% 
      select( -starts_with( "exp_factor_" )) 
    
    if ( arena.stratification | arena.post_stratification ) {
      
      # SRS: ALL DATA (totals) 
      jdesign       <- update( design_srvyr_SRS_total, whole_area_ = 1 )
      out_SRS_total <- jdesign                                      %>%
        dplyr::group_by( whole_area_ )                                     %>%         
        dplyr::summarize( across( c( ends_with(".Total") ),      
                                  list( ~survey_total( ., vartype = c("var") ))))  %>%  
        as.data.frame(.) 
      
      var_global_total <- out_global_total %>% select( ends_with("_var")) 
      var_SRS_total    <- out_SRS_total    %>% select( ends_with("_var"))
      var_efficiency   <- var_SRS_total / var_global_total 
      
      names(var_efficiency) = gsub(pattern = "_ha.Total_var", replacement = "", x = names(var_efficiency))
      rm(var_global_total); rm(var_SRS_total)
      
    }
    #######################################
    # remove extra '_tally' columns by groups, leave just one tally row
    if ("_tally" %in% stringr::str_sub( colnames(out_mean), -6, -1)) {
      tally_out          <- out_mean %>% select( ends_with("_tally"))  %>% select(1) 
      names( tally_out ) <- "tally"
      out_mean           <- out_mean %>% select( -ends_with("_tally")) %>% cbind( tally_out )
      rm( tally_out )
    }
    
    #  name "arena_post_stratum_attribute"
    # if (arena.post_stratification & !(arena.chainSummary$postStratificationAttribute %in% arena.analyze$dimensions)) {
    #     names(out_mean)[names(out_mean)   == 'postStratificationAttribute'] <- arena.chainSummary$postStratificationAttribute
    #     names(out_total)[names(out_total) == 'postStratificationAttribute'] <- arena.chainSummary$postStratificationAttribute
    # }
    
    out_mean$postStratificationAttribute         <- NULL
    out_total$postStratificationAttribute        <- NULL
    out_global_total$postStratificationAttribute <- NULL
    
    # function to join "result_labels" with output tables
    joinLabels <- function(result_labels, out_table) { 
      for ( i in (1:length(result_labels))) {
        if ( names(result_labels[i]) %in% names( out_table)) {
          out_table$code <- out_table[[ names( result_labels[i])]]
          out_table <- out_table                               %>% 
            dplyr::full_join( result_labels[[i]], by = "code") %>%
            dplyr::select(-code)
          
          out_table[[names(result_labels[i])]] <- out_table$label
          out_table$label                      <- NULL
          out_table$code                       <- NULL
        }
      }
      return(out_table)
    }
    
    out_mean  <- joinLabels( result_labels, out_mean )
    out_total <- joinLabels( result_labels, out_total )
    out_area  <- joinLabels( result_labels, out_area ) 
    
    #omit rows with NA 
    out_mean        <- na.omit( out_mean )
    out_total       <- subset( out_total, area > 0 )
    
    # rename columns
    out_mean  <- setNames( out_mean,  stringr::str_replace( names(out_mean),  "_ha.Mean_se",  ".sd"  ))
    out_mean  <- setNames( out_mean,  stringr::str_replace( names(out_mean),  "_ha.Mean_var", ".var"))
    out_mean  <- setNames( out_mean,  stringr::str_replace( names(out_mean),  "_ha.Mean_low", ".ci_lower"  ))
    out_mean  <- setNames( out_mean,  stringr::str_replace( names(out_mean),  "_ha.Mean_upp", ".ci_upper"))
    out_mean  <- setNames( out_mean,  stringr::str_replace( names(out_mean),  "_ha.Mean", ".mean"   )) 
    
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total_se", ".sd"  ))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total_var",".var"))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "area_se",      "area_sd"  ))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total_low", ".ci_lower"))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total_upp", ".ci_upper"))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total_globalAverage",".average"))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total", ".total" ))
    
    
    out_file <- list()
    
    out_file[[1]] <- paste0(user_file_path, out_path, arena.analyze$entity, " (", paste( arena.analyze$dimensions, collapse = " - "), ") --mean.csv")
    out_file[[2]] <- paste0(user_file_path, out_path, arena.analyze$entity, " (", paste( arena.analyze$dimensions, collapse = " - "), ") --total.csv")
    
    # drop out statistical variables from result tables
    if (showStatisticsInResults == FALSE & exists("out_mean"))  out_mean  <- out_mean  %>% select( -ends_with( c( ".sd", ".var",".area_sd",".ci_lower",".ci_upper")))
    if (showStatisticsInResults == FALSE & exists("out_total")) out_total <- out_total %>% select( -ends_with( c( ".sd", ".var",".area_sd",".ci_lower",".ci_upper")))
    
    
    tryCatch({if (exists('user_file_path') & exists("out_mean"))  write.csv(out_mean, out_file[[1]],  row.names = F)},
             warning = function( w ) { cat("No output - out_mean") },
             error   = function( e ) { cat("No output - out_mean")
             })
    
    tryCatch({if (exists('user_file_path') & exists("out_total")) write.csv(out_total, out_file[[2]], row.names = F)},
             warning = function( w ) { cat("No output - out_total") },
             error   = function( e ) { cat("No output - out_total")
             })
    
  } # for loop
  
  if ( arena.analyze$reportingMethod == '2' ) arena.analyze$dimensions <- arena.analyze$dimensions_input 
  
  out_file[[3]] <- paste0(user_file_path, arena.analyze$entity,  "--global_total.csv")
  out_file[[4]] <- paste0(user_file_path, arena.analyze$entity,  "--relative_efficiency.csv")
  out_file[[5]] <- paste0(user_file_path, arena.strat_attribute, "--PSU_nonresponse_correction.csv")
  out_file[[6]] <- paste0(user_file_path, arena.analyze$entity,  "--area_estimates.csv")
  out_file[[7]] <- paste0(user_file_path, arena.analyze$entity,  "--global_mean.csv")
  
  
  # rename columns
  if (exists("out_global_mean")) {
    out_global_mean  <- setNames( out_global_mean,  stringr::str_replace( names(out_global_mean),  "_ha.Mean_se",  ".sd"  ))
    out_global_mean  <- setNames( out_global_mean,  stringr::str_replace( names(out_global_mean),  "_ha.Mean_var", ".var"))
    out_global_mean  <- setNames( out_global_mean,  stringr::str_replace( names(out_global_mean),  "_ha.Mean_low", ".ci_lower"  ))
    out_global_mean  <- setNames( out_global_mean,  stringr::str_replace( names(out_global_mean),  "_ha.Mean_upp", ".ci_upper"))
    out_global_mean  <- setNames( out_global_mean,  stringr::str_replace( names(out_global_mean),  "_ha.Mean", ".mean"   )) 
  }
  
  #  out_global_total <- setNames( out_global_total, stringr::str_replace(names(out_global_total), "area_se",    "area_sd"))
  out_global_total <- setNames( out_global_total, stringr::str_replace( names(out_global_total), "_ha.Total_se",  ".sd"))
  out_global_total <- setNames( out_global_total, stringr::str_replace( names(out_global_total), "_ha.Total_var", ".var"))
  out_global_total <- setNames( out_global_total, stringr::str_replace( names(out_global_total), "_ha.Total_low", ".ci_lower"))
  out_global_total <- setNames( out_global_total, stringr::str_replace( names(out_global_total), "_ha.Total_upp", ".ci_upper"))
  out_global_total <- setNames( out_global_total, stringr::str_replace( names(out_global_total), "_ha.Total_globalAverage",".average"))
  out_global_total <- setNames( out_global_total, stringr::str_replace( names(out_global_total), "_ha.Total", ".total"))
  out_global_total$whole_area_ <- NULL
  

  # drop out statistical variables from result tables
  if (showStatisticsInResults == FALSE & exists("out_global_mean"))  out_global_mean  <- out_global_mean  %>% select( -ends_with( c( ".sd", ".var",".area_sd",".ci_lower",".ci_upper")))
  if (showStatisticsInResults == FALSE & exists("out_global_total")) out_global_total <- out_global_total %>% select( -ends_with( c( ".sd", ".var",".area_sd",".ci_lower",".ci_upper")))
  
  
  tryCatch({if (exists('user_file_path') & exists("out_global_total")) write.csv( out_global_total, out_file[[3]], row.names = F)},
           warning = function( w ) { cat("No output - out_global_total") },
           error   = function( e ) { cat("No output - out_global_total")
           })
  
  tryCatch({if (exists('user_file_path') & exists("out_global_mean")) write.csv( out_global_mean, out_file[[7]], row.names = F)},
           warning = function( w ) { cat("No output - out_global_mean") },
           error   = function( e ) { cat("No output - out_global_mean")
           })
  if ( exists("out_global_mean")) rm(out_global_mean)
  
  if ( arena.stratification | arena.post_stratification ) {
    tryCatch({if (exists('user_file_path') & exists("var_efficiency")) write.csv( var_efficiency, out_file[[4]], row.names = F)},
             warning = function( w ) { cat("No output - var_efficiency") },
             error   = function( e ) { cat("No output - var_efficiency")
             })
  }
  
  tryCatch({if ( exists('user_file_path') & exists("out_area")) write.csv( out_area, out_file[[6]], row.names = F)},
           warning = function( w ) { cat("No output - out_area") },
           error   = function( e ) { cat("No output - out_area")
           })
  
  if ( arena.chainSummary$analysis$nonResponseBiasCorrection) {
    if ( arena.stratification | arena.post_stratification ) {
      nonResponse_out1 <- df_base_unit %>% select( STRATUM = all_of( arena.strat_attribute), correction_factor = arena_ssu_correction ) %>% unique() %>% arrange( STRATUM)  
      tryCatch({if (exists('user_file_path') & exists("nonResponse_out1")) write.csv( nonResponse_out1, out_file[[5]], row.names = F)},
               warning = function( w ) { cat("No output - nonResponse_out1") },
               error   = function( e ) { cat("No output - nonResponse_out1")
               })
    }
  }
  
  
  # get results by sampling units out
  out_path <- paste0(user_file_path, "sampling unit results", "/")
  # create a folder for files to be exported
  if ( !dir.exists( out_path )) dir.create( out_path, showWarnings = FALSE )
  
  for ( i in 1:length( result_entities )) {
    outfile7              <- paste0( out_path, result_entities[[i]], "_base_unit_results.csv")
    base_unit.results_out <- base_unit.results[i] %>% as.data.frame() %>% select(-ends_with(".Total"))
    
    dimension_names <- arena.chainSummary$baseUnitEntityKeys
    if ( arena.stratification )      dimension_names <- unique( c( dimension_names, arena.strat_attribute))
    if ( arena.post_stratification ) dimension_names <- unique( c( dimension_names, arena.chainSummary$postStratificationAttribute ))
    
    
    # use original label for hier. categorical attribute data, on levels 2-.. 
    if (length( arena.chainSummary$categoryAttributeAncestors$attribute ) > 0) {
      category_attribute_ancestors <- intersect( dimension_names, arena.chainSummary$categoryAttributeAncestors$attribute)
      if ( length( category_attribute_ancestors ) > 0 ) {
        dimension_names <- setdiff( dimension_names, category_attribute_ancestors)
        dimension_names <- c( dimension_names, paste0( category_attribute_ancestors, "_label" ))
      }
    }
    
    base_unit.results_out <- df_base_unit %>% select( all_of( base_UUID_), all_of( dimension_names ), weight, exp_factor=exp_factor_) %>%
      dplyr::left_join( base_unit.results_out, by = base_UUID_) %>%
      dplyr::select( -all_of( base_UUID_))
    
    tryCatch({if (exists('user_file_path')) write.csv( base_unit.results_out, outfile7, row.names = F)},
             warning = function( w ) { cat("No output - base unit results") },
             error   = function( e ) { cat("No output - base unit results")
             })
    
    if ( cluster_UUID_ !="" ) {
      outfile8            <- paste0( out_path, result_entities[[i]], "_cluster_results.csv")
      cluster.results_out <- cluster.results[i] %>% as.data.frame() %>% select(-ends_with(".Total"))
      #cluster.results_out <- get( arena.chainSummary$clusteringEntity ) %>% select( cluster_UUID_, all_of( arena.chainSummary$clusteringEntityKeys )) %>%
      cluster.results_out <- df_base_unit %>% dplyr::select( all_of( cluster_UUID_), all_of( arena.chainSummary$clusteringEntityKeys )) %>%
        unique() %>%
        dplyr::left_join( cluster.results_out, by = cluster_UUID_) %>%
        dplyr::select( -all_of( cluster_UUID_ ))
      
      cluster.results_out[ is.na( cluster.results_out)] <- 0
      
      tryCatch({if (exists('user_file_path')) write.csv( cluster.results_out, outfile8, row.names = F)},
               warning = function( w ) { cat("No output - cluster results") },
               error   = function( e ) { cat("No output - cluster results")
               })
    }
    
  }
  
  if ( Sys.getenv("RSTUDIO_PROGRAM_MODE") == "server" & exists('user_file_path') ) { 
    # zip all files
    export_filename  <- paste0( user_file_path, 'arena_results_(', arena.chainSummary$surveyName, ').zip')
    files2zip        <- dir( user_file_path, full.names = TRUE )
    if ( length(files2zip) > 0 ) {
      zip(zipfile = export_filename, files = files2zip, mode = "cherry-pick")
      browseURL( export_filename )
    }
  }
  
  if ( Sys.getenv("RSTUDIO_PROGRAM_MODE") == "desktop" & exists('user_file_path') ) { 
    if ( Sys.info()['sysname']=="Windows" ) processMessage = paste0(" Result files in /Documents/arena/arena-",  arena.chainSummary$surveyName, "-DATE_TIME", "/user_output/")
  }
  
  processMessage = paste0("Arena Analytics: Process completed. ", processMessage )
  return( processMessage )
}

###################################################################
# END -------------------------------------------------------------
###################################################################

