
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
  # Last update:  12.10.2022
  #**********************************************************************************************
  
  tryCatch( usePackage('tidyr'),
            error = function(e){ library('tidyr')
            })
  
  # define a folder for output files
  if (!exists('user_file_path')) user_file_path <- './user_output/'
  # create a folder for files to be exported
  if (!dir.exists( user_file_path )) dir.create( user_file_path, showWarnings = FALSE )
  
  
  # set  options, see more at https://r-survey.r-forge.r-project.org/survey/html/surveyoptions.html
  #options(dplyr.summarise.inform      = FALSE)
  options(survey.ultimate.cluster     = FALSE)
  options(survey.adjust.domain.lonely = TRUE)
  options(survey.lonely.psu           = "adjust")  # alternatively "remove"
  options(digits = 10)
  old_sigfig      <- options("pillar.sigfig") # https://github.com/gergness/srvyr/blob/main/vignettes/srvyr-vs-survey.Rmd
  options("pillar.sigfig" = 5)
  
  # read JSON file 
  chain_summary_json <-  paste(getwd(), 'chain_summary.json', sep = .Platform$file.sep)
  if ( file.exists( chain_summary_json ))  arena.chainSummary <- jsonlite::fromJSON( chain_summary_json )
  
  
  # check analysis parameters, if any
  arena.analyze   <- list(entity = '', dimensions = '', filter = "", reportingMethod = '2')
  
  if (!is.null(arena.chainSummary$analysis)) {
    if (!is.null(arena.chainSummary$analysis$entity) & !is.null(arena.chainSummary$analysis$dimensions)) {
      arena.analyze$entity       <- trimws( arena.chainSummary$analysis$entity )
      arena.analyze$dimensions   <- trimws( arena.chainSummary$analysis$dimensions )
      if (!is.null(arena.chainSummary$analysis$filter))          arena.analyze$filter            <- trimws( arena.chainSummary$analysis$filter)  
      if (!is.null(arena.chainSummary$analysis$reportingMethod)) arena.analyze$reportingMethod   <- trimws( arena.chainSummary$analysis$reportingMethod)  
      
      if (is.null(arena.analyze$entity) | arena.analyze$entity =="" | is.na(arena.analyze$entity) | length(arena.analyze$entity)==0 ){
        return( "Arena Analytics: No entity to report" )
      }
      if (is.null(arena.analyze$dimensions) | is.na(arena.analyze$entity) | length(arena.analyze$entity)==0 ){
        return( "Arena Analytics: No dimension to report" )
      } else {
        arena.analyze$dimensions_datatypes   <- c()
        arena.analyze$dimensions_at_baseunit <- c()
        entity_datatype <- lapply(get( arena.analyze$entity), class)
        
        for (j in (1:length(arena.analyze$dimensions))){
          arena.analyze$dimensions_datatypes[[j]]   <- ifelse(arena.analyze$dimensions[[j]] %in% names(get(arena.analyze$entity)),
                                                              as.character(entity_datatype[arena.analyze$dimensions[[j]]]), "character")
          arena.analyze$dimensions_datatypes[[j]]   <- ifelse(  paste0(arena.analyze$dimensions[[j]], "_scientific_name") %in% names(get(arena.analyze$entity)), "taxon", arena.analyze$dimensions_datatypes[[j]] )
          arena.analyze$dimensions_at_baseunit[[j]] <- unlist(ifelse(arena.analyze$dimensions[[j]] %in% names(get(arena.chainSummary$baseUnit)), TRUE, FALSE))
        }
        arena.analyze$dimensions_datatypes   <- as.character(arena.analyze$dimensions_datatypes)
        arena.analyze$dimensions_at_baseunit <- as.logical(arena.analyze$dimensions_at_baseunit) 
        arena.analyze$dimensions_baseunit    <- as.character(unlist(Map(`[`, arena.analyze$dimensions, arena.analyze$dimensions_at_baseunit)))
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
  if (arena.chainSummary$baseUnit == "")                       arena.chainSummary$samplingDesign              <- FALSE
  # b) stratum attribute is missing
  if (is.null(arena.chainSummary$stratumAttribute))            arena.chainSummary$stratumAttribute            <- ""
  # nonresponse bias correction is missing
  if (is.null(arena.chainSummary$nonResponseBiasCorrection))   arena.chainSummary$nonResponseBiasCorrection   <- FALSE
  # sampling design lookup table is missing -> no bias correction
  if (is.null("categories$sampling_units_plan"))               arena.chainSummary$nonResponseBiasCorrection   <- FALSE
  
  # Reporting area table check
  arena.reporting_area <- ifelse(is.null(arena.chainSummary$reportingCategory), FALSE, ifelse(arena.chainSummary$reportingCategory$name == "", FALSE, TRUE))
  
  # SAMPLING DESIGN EXISTS. 
  # Compute expansion factors, sum of area-based variables & weights up to base unit level, and non-response bias corrections
  if (arena.chainSummary$samplingDesign) {
    
    # get base unit data into a data frame
    df_base_unit                  <- get( arena.chainSummary$baseUnit )
    
    # take a copy of weight. Non-response bias correction may change weights. 
    df_base_unit$weight_original_ <- df_base_unit$weight 
    # Key attribute names: base unit and clustering attributes
    base_uuid                     <- paste0( arena.chainSummary$baseUnit, "_uuid")
    cluster_uuid_                 <- ifelse( arena.chainSummary$clusteringEntity != "", paste0(arena.chainSummary$clusteringEntity, "_uuid"), "")    
    # Stratification check: method, attribute and areas
    arena.stratification          <- ifelse(( arena.chainSummary$samplingStrategy==3 | arena.chainSummary$samplingStrategy==4 ) & arena.chainSummary$stratumAttribute != "", TRUE, FALSE)
    arena.strat_attribute         <- ifelse( arena.stratification, arena.chainSummary$stratumAttribute, "")
    
    
    # Reporting areas, hierarchical table, AOI = Area of Interest
    if (arena.reporting_area) {
      aoi.attributes           <- arena.chainSummary$reportingCategory$attributes   # list, attributes joined with
      aoi.level_count          <- length( aoi.attributes ) 
      # get items from the lowest level with area
      aoi_df                   <- arena.chainSummary$reportingCategory$items        # levelIndex, level1code, label, level2code, ..., area
      


      aoi_df$area[ is.na(aoi_df$area) ] <- 1
      
      
      # 1. non-stratified sampling, compute expansion factor for the base unit
      if ( !arena.stratification ) {
        ifelse( sum(df_base_unit$weight) > 0, 
                df_base_unit$exp_factor_ <- sum( aoi_df$area[aoi_df$levelIndex == aoi.level_count], na.rm = TRUE  ) / sum(df_base_unit$weight), 0)
      } else {  
        # 2. stratified sampling, nonresponse bias correction
        if (arena.chainSummary$nonResponseBiasCorrection) {
          arena.samplingdesign_table                          <- as.data.frame(categories$sampling_units_plan) # read a lookup table
          level_name                                          <- paste0("level_", as.character(aoi.level_count), "_code")
          
          arena.samplingdesign_table[ arena.strat_attribute ] <- ifelse( !is.null(arena.samplingdesign_table$code), arena.samplingdesign_table['code'], 
                                                                         arena.samplingdesign_table[ level_name ] )
          rm(level_name)
          arena.samplingdesign_table$design_number_psu        <- as.numeric(arena.samplingdesign_table$design_number_psu)
          arena.samplingdesign_table$design_number_psu[is.na(arena.samplingdesign_table$design_number_psu)] <- 0
          arena.samplingdesign_table$design_number_ssu        <- as.numeric(arena.samplingdesign_table$design_number_ssu)
          
          
          df_base_unit[arena.strat_attribute]                 <- as.character( df_base_unit[ arena.strat_attribute ][,1] )
          arena.samplingdesign_table[ arena.strat_attribute ] <- as.character( arena.samplingdesign_table[ arena.strat_attribute ][,1])
          
          
          # A. MISSING CLUSTERS IN STRATA: nonresponse bias correction, naive imputation method
          
          # get number of accessible clusters by stratum and correction factor
          arena.samplingdesign_table <- arena.samplingdesign_table %>%
            left_join( df_base_unit                    %>%
                         filter( weight > 0 )                    %>%
                         group_by_at( arena.strat_attribute )    %>%
                         dplyr::summarize( cluster_count = n( ), sum_weight= sum(weight) ), by = arena.strat_attribute) %>%
            mutate( cluster_count            = ifelse( is.na(cluster_count ), 0, cluster_count) )      %>%
            mutate( arena_cluster_correction = ifelse( cluster_count>0 & design_number_psu>0, design_number_psu/cluster_count, 1))
          
          
          df_base_unit <- df_base_unit %>%
            left_join(arena.samplingdesign_table %>% select(arena.strat_attribute, arena_cluster_correction), by = arena.strat_attribute)
          
          
          # B. MISSING BASE UNITS IN CLUSTER: nonresponse bias correction, naive imputation method
          # clustered
          if (cluster_uuid_ != "" & !arena.chainSummary$clusteringVariances & all(!is.na(arena.samplingdesign_table$design_number_ssu))) {
            arena_cluster_statistics <- arena.samplingdesign_table %>%
              select( arena.strat_attribute, design_number_ssu )   %>%
              right_join( df_base_unit %>% 
                            filter( weight > 0 )   %>%
                            group_by( !!! syms(arena.strat_attribute), cluster_uuid_ ) %>%
                            dplyr::summarize(bu_count = n( ), sum_weight= sum(weight)) )     %>%
              mutate( arena_bu_correction = ifelse(!is.na(design_number_ssu) & design_number_ssu > 0, design_number_ssu/sum_weight, 1)) # this works if a full base unit weight is 1 !!
            
            # check whether some clusters are split over more than 1 stratum
            if (nrow(arena_cluster_statistics) != length(unique(arena_cluster_statistics$cluster_uuid_))) {
              # list of clusters belonging to multiple strata
              analyze_overlaps <- arena_cluster_statistics %>%
                group_by(cluster_uuid_ ) %>%
                dplyr::summarize(c_count =n()) %>%
                filter(c_count > 1)     %>%
                pull(cluster_uuid_) 
              
              # fix this later, overlaps get all 1:
              arena_cluster_statistics$arena_bu_correction <- with(arena_cluster_statistics,
                                                                   ifelse(cluster_uuid_ %in% analyze_overlaps, 1, arena_bu_correction)) 
              
              df_base_unit <- df_base_unit %>%
                left_join(arena_cluster_statistics %>% 
                            select(!!! syms(arena.strat_attribute),cluster_uuid_, arena_bu_correction), by = c(arena.strat_attribute,cluster_uuid_))
              
            } else {  # no clusters split across strata
              df_base_unit <- df_base_unit       %>%
                left_join(arena_cluster_statistics %>% 
                            select(cluster_uuid_, arena_bu_correction), by = cluster_uuid_)
            }
            
          } else { # stratification, no clustering
            df_base_unit$arena_bu_correction <- 1
          } 
          
        } else { # end of nonresponse correction
          
          # C. NO NONRESPONSE CORRECTION
          df_base_unit$arena_cluster_correction  <- 1
          df_base_unit$arena_bu_correction       <- 1
        }
        
        
        # 2a. nonresponse effect: adjust base unit weights
        if (arena.chainSummary$nonResponseBiasCorrection) {
          df_base_unit$weight <- df_base_unit$weight * df_base_unit$arena_cluster_correction
          # is the next line correct?
          if (arena.chainSummary$clusteringVariances == FALSE) df_base_unit$weight <- df_base_unit$weight * df_base_unit$arena_bu_correction
          # rescale back to max. 1
          #                df_base_unit$weight <- df_base_unit$weight / max(df_base_unit$weight)
        }
        
        
        # calculate expansion factor for AOIs. AOI table exists
        
        arena.expansion_factor <- df_base_unit %>%
          filter(weight>0)                     %>%
          group_by_at( arena.strat_attribute ) %>%
          dplyr::summarize( aoi_weight_ = sum( weight ), aoi_count_ =n()) 
        
        arena.expansion_factor <- arena.expansion_factor %>%
          left_join(aoi_df                               %>% 
                      filter(aoi_df$levelIndex == aoi.level_count) %>%
                      select(level1Code, area), by =structure(names=arena.strat_attribute, "level1Code" )) %>% #https://stackoverflow.com/questions/38503960/dplyr-join-two-tables-within-a-function-where-one-variable-name-is-an-argument-t
          mutate(exp_factor_ = area / aoi_weight_ )
        
        df_base_unit <- df_base_unit       %>%
          left_join(arena.expansion_factor %>%
                      select( !!! syms(arena.strat_attribute), exp_factor_), by = arena.strat_attribute) 
        
        # True expansion factor
        df_base_unit$exp_factor_ <- df_base_unit$exp_factor_ * df_base_unit$weight 
        
        
      } # end of stratified sampling
      
    } else { 
      # AOI table not defined
      # create a clone of base unit entity
      df_base_unit$exp_factor_               <- 1
      df_base_unit$arena_cluster_correction  <- 1
      
      # clustering, no stratification        
      if (cluster_uuid_ !="" & !arena.chainSummary$clusteringVariances) {
        max_weight_in_cluster <- df_base_unit %>% 
          group_by( cluster_uuid_ )            %>%
          dplyr::summarize(sum_weight= sum(weight))  %>%
          select(sum_weight)                  %>%
          max()
        
        df_base_unit$arena_bu_correction <- NULL
        
        df_base_unit <- df_base_unit %>% 
          group_by( cluster_uuid_ )  %>%
          dplyr::summarize(sum_weight= sum(weight))  %>%
          mutate( arena_bu_correction = max_weight_in_cluster/sum_weight) %>%
          select(-sum_weight) %>%
          right_join(df_base_unit, by=cluster_uuid_) 
        
        df_base_unit$arena_bu_correction[is.na(df_base_unit$arena_bu_correction)] <- 1
        
        df_base_unit$weight <- df_base_unit$weight * df_base_unit$arena_bu_correction
        
      } else {
        df_base_unit$arena_bu_correction <- 1
      }
    }
    
    
    # get a list of active area-based result variables (of all entities)
    # parents of base units cannot have area-based variables!
    result_entities <- arena.chainSummary$resultVariables %>%
      dplyr::filter( areaBased == TRUE & active == TRUE & stringr::str_detect(entityPath, arena.chainSummary$baseUnit))  %>%
      #      filter(entity != arena.chainSummary$baseUnit) %>%  
      select(entity) %>% 
      unique()       %>%
      pull()
    
    # next loop over result entities, collects a list of all categorical and taxonomic variables
    # get: entity, base_uuid, exp_factor_, and combinations for all categorical variables in the data
    result_cat_attributes <- list()
    result_cat            <- list()
    base_unit.results     <- list()
    cluster.results       <- list()
    
    # ADD area estimates based on exp_factor
    
    # Quantitative result variables against categorical and taxonomic data
    for (i in (1:length(result_entities))) {
      
      # PART 1.compute sums (per AOIs) and means (/ha) across all categorical variables 
      # drop out columns where all data is NA.  https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
      
      df_entitydata              <- get(result_entities[[i]])
      

      result_cat_attributes[[i]] <- ( df_entitydata  %>% select_if(~!all(is.na(.))) %>% select_if(~is.character(.)) )  %>%
        select(ends_with("_label") | ends_with("_scientific_name")) %>% 
        names()
      
      boolean_list <-  ( df_entitydata  %>% select_if(~!all(is.na(.))) %>% select_if(~is.logical(.)) )  %>%
        # select_if(~all( .=="false" | .=="true")) %>% 
        # select_if(~!all(.=="false"))             %>%
        # select_if(~!all(.=="true"))              %>% 
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
        select(ends_with("_uuid"), -ends_with("_file_uuid"), -record_uuid, -paste0(result_entities[[i]], "_uuid")) %>% 
        names %>%
        stringr::str_sub(., 0,-6)
      
      
      # 2) join parents' cat. data
      if (length(parent_names) > 0) {
        res_cat                    <- arena.chainSummary$resultVariables %>% 
          filter( type=="C" & active==TRUE & entity %in% parent_names)   %>% 
          select(entity, name) 
        if (nrow(res_cat) > 0) {
          for (j in (1:nrow(res_cat) )) {
            df_join       <- get( res_cat$entity[j]) %>% select(paste0(res_cat$entity[j], "_uuid"), res_cat$name[j] ) 
            df_entitydata <- df_entitydata %>%
              left_join(df_join, by = paste0(res_cat$entity[j], "_uuid")) 
            
            rm(df_join)
          }
        }
      } 
      
      # 3) list of names: add base unit id, and categorical result variables (of parents & selected entity)
      res_cat         <- arena.chainSummary$resultVariables %>% 
        filter(type=="C" & active==TRUE & entity %in% c(parent_names, result_entities[[i]]) ) %>% 
        pull(name)
      
      result_cat_attributes[[i]] <- unique( c(base_uuid, result_cat_attributes[[i]], res_cat) )
      
      resultVariables <- arena.chainSummary$resultVariables %>%
        dplyr::filter( areaBased == TRUE & active == TRUE & entity == result_entities[[i]]) %>%
        mutate(name = paste0(name, "_ha")) %>%
        pull(name) 
      
      
      if (result_entities[[i]] != arena.chainSummary$baseUnit) {
        
        # get a list of base unit IDs that are not in entity data (i.e. treeless plots)
        missing_ids     <- setdiff(unique(unlist(df_base_unit %>% filter(weight>0) %>% select(base_uuid) )), unique(unlist( df_entitydata[base_uuid] )))
        
        if (length(missing_ids) > 0) {
          # get list of attributes that exist in base unit data
          names_in_data   <- intersect(names(df_entitydata), names(df_base_unit)) 
          
          df_base_unit2   <- subset(df_base_unit,  eval(parse(text = base_uuid)) %in% missing_ids)
          result_cat[[i]] <- bind_rows(df_entitydata, df_base_unit2 %>% select(names_in_data) )
          result_cat[[i]] <- result_cat[[i]] %>% 
            mutate_if(is.numeric, ~tidyr::replace_na(., 0))
          rm(names_in_data); rm(df_base_unit2)
        } else {
          result_cat[[i]] <- df_entitydata
        }
        
        rm(missing_ids)
        
        result_cat[[i]] <- result_cat[[i]]                                                  %>%
          dplyr::right_join(df_base_unit %>% select(base_uuid, exp_factor_), by= base_uuid) %>% # join expansion factor
          group_by(  across( result_cat_attributes[[i]] ))                                  %>%
          dplyr::summarize( across(.cols= all_of(resultVariables), 
                            list(Total = ~sum(exp_factor_ * .x, na.rm = TRUE), Mean = ~sum(.x, na.rm = TRUE) ),  
                            .names = "{.col}.{.fn}"), 
                            entity_count_ = n() ) %>%
          data.frame()
        
      } else { # entity is the base unit
        result_cat[[i]] <- df_entitydata                   %>%
          group_by(  across( result_cat_attributes[[i]] )) %>%
          dplyr::summarize( across(.cols= all_of(resultVariables), 
                            list(Total = ~sum(exp_factor_ * .x, na.rm = TRUE), Mean = ~sum(.x, na.rm = TRUE) ),  
                            .names = "{.col}.{.fn}"), 
                            entity_count_ = n() )          %>%
          data.frame()
      }
      
      
      # add weight, exp_factor_; AND IF EXISTS: cluster_uuid_, arena.strat_attribute (This is actually already in dataframe because it is categorical!) 
      temp_list_variables <- c(base_uuid, "weight", "exp_factor_")
      if (cluster_uuid_ != ""    &  !(cluster_uuid_ %in% names(result_cat[[i]])) )          temp_list_variables <- c(temp_list_variables, cluster_uuid_)
      if (arena.stratification  &  !(arena.strat_attribute %in% names(result_cat[[i]])) ) temp_list_variables <- c(temp_list_variables, arena.strat_attribute)
      
      result_cat[[i]] <- result_cat[[i]] %>%
        left_join(df_base_unit %>% select( all_of(temp_list_variables)), by = base_uuid)
      
      rm( temp_list_variables )
      
      ## PART 2. compute sum of per hectare results at the base unit level for each result variable
      base_unit.results[[i]] <- df_entitydata %>%
        # Add expansion factor for all result entities
        dplyr::right_join((df_base_unit %>% select(base_uuid, weight, exp_factor_)), by = base_uuid) %>%
        group_by_at( base_uuid ) %>%
        dplyr::summarize(across(.cols= all_of(resultVariables),
                                list(Total = ~sum(exp_factor_ * .x, na.rm = TRUE), Mean = ~sum(.x, na.rm = TRUE) ),
                                .names = "{.col}.{.fn}"),
                         item_count = n() )
      
      # join results with the clone of base unit
      df_base_unit <- df_base_unit %>%
        dplyr::left_join( base_unit.results[[i]], by = base_uuid)
      
      
      ## PART 3. compute sum of per hectare results at the cluster level for each result variable
      
      if (cluster_uuid_ !="") {
        
        cluster.weights <- df_base_unit %>%
          filter(weight>0)              %>%
          select(cluster_uuid_, weight)  %>%
          group_by_at( cluster_uuid_ )   %>%
          dplyr::summarize( sumweight = sum(weight), n_baseunits = n() )
        
        cluster.results[[i]] <- df_entitydata %>%
          # Add expansion factor for all result entities
          dplyr::left_join(cluster.weights, by = cluster_uuid_) %>%
          dplyr::right_join((df_base_unit %>% select(base_uuid, weight, exp_factor_)), by = base_uuid) %>%
          group_by_at( cluster_uuid_ )                                                          %>%
          dplyr::summarize(across(.cols= all_of(resultVariables),
                                  list(Total = ~sum(exp_factor_ * .x, na.rm = TRUE), Mean = ~sum(.x, na.rm = TRUE)/max(sumweight)),
                                  .names = "{.col}.{.fn}"),
                           n_baseunits = max(n_baseunits), sum_weight = max(sumweight) )
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
  
  arena.analyze$dimensions_input <- arena.analyze$dimensions
  if ( arena.analyze$reportingMethod == '1' ) arena.reportingLoops = 1
  if ( arena.analyze$reportingMethod == '2' ) arena.reportingLoops = length(arena.analyze$dimensions)
  
  
  # get all files in the main output folder
  f <- list.files(user_file_path, full.names = TRUE, recursive = FALSE)
  f <- f[!file.info(f)$isdir]
  
  # remove the files in the main output folder
  if (length(f)) file.remove(f)
  rm(f)
  
  out_path  <- "dimensions/"
  if (dir.exists( paste0( user_file_path, out_path ))) unlink(paste0(user_file_path, out_path), recursive = TRUE)
  dir.create( paste0( user_file_path, out_path ), showWarnings = FALSE )
  
  
  for (rep_loop in (1:arena.reportingLoops)) {
    if (arena.analyze$reportingMethod == '2') {
      arena.analyze$dimensions <- arena.analyze$dimensions_input[rep_loop]
      out_path                 <- paste0("dimensions/", arena.analyze$dimensions, "/")
      if (dir.exists( paste0( user_file_path, arena.analyze$dimensions ))) unlink(paste0(user_file_path, arena.analyze$dimensions), recursive = TRUE)
      dir.create( paste0( user_file_path, "dimensions/", arena.analyze$dimensions ), showWarnings = FALSE )
    } 
    
    # get labels to the categorical result variables, [1]: input attribute, [2]: result attribute 
    result_labels           <- list() 
    result_names_category_1 <- intersect( arena.analyze$dimensions, names(get(arena.analyze$entity))) # cat. attributes in input data 
    result_names_category_2 <- setdiff(   arena.analyze$dimensions, names(get(arena.analyze$entity))) # cat. attributes not in input data, but as result attributes
    
    
    if (length( result_names_category_1 ) > 0) {
      df_cat_report <- get(arena.analyze$entity) %>% 
        select( all_of(result_names_category_1), any_of(paste0(result_names_category_1,"_label")), any_of(paste0(result_names_category_1,"_scientific_name"))) %>%
        distinct()
      
      for (i in (1:length(result_names_category_1))) {
        label_column   <- paste0(result_names_category_1[i], "_label")
        species_column <- paste0(result_names_category_1[i], "_scientific_name")
        
        # in case of boolean data, label is same as data value
        if ( !(label_column %in% names(df_cat_report))) df_cat_report[ label_column ] <- df_cat_report[ result_names_category_1[i] ]
        # but in case of taxon data, label is the scientific name
        if ( species_column %in% names(df_cat_report))  df_cat_report[ label_column ] <- df_cat_report[ species_column ]
        
        result_labels[[i]]  <- df_cat_report %>% 
          select(code = result_names_category_1[i], label = label_column ) %>%
          distinct() %>%
          arrange(.[1])
      }
      rm(df_cat_report); rm(label_column); rm(species_column)
    }
    
    if (length( result_names_category_2 ) > 0) {
      dataindex <- length(result_labels) 
      
      df_cat_report <- arena.chainSummary$resultVariables                     %>%
        filter( type=="C" & active==TRUE & name %in% result_names_category_2) %>%
        select( categoryName ) 
      
      for (i in (1:nrow(df_cat_report))) {
        result_labels[[dataindex + i]]  <- 
          categories[ df_cat_report$categoryName[[i]] ] %>% 
          data.frame                                    %>%
          select(last_col(1:0))  # selects last two columns
        
        names(result_labels[[dataindex + i]]) <- c("code","label") 
      }
    }
    
    names(result_labels) <- c( result_names_category_1, result_names_category_2 )
    
    if (exists("result_names_category_1")) rm(result_names_category_1) 
    if (exists("result_names_category_2")) rm(result_names_category_2) 
    
    # get analysis data 
    df_analysis <- as.data.frame( result_cat[[arena.analyze$entity]] )
    
    # get categorical variables
    cat_names_uuid <- result_cat[[ arena.analyze$entity ]] %>%
      data.frame()               %>%
      select_if(is.character)    %>%
      select(ends_with("_uuid")) %>%
      names()
    
    # get numeric variables
    cat_names_num <- result_cat[[ arena.analyze$entity ]] %>%
      data.frame()                                        %>%
      select_if(is.numeric)                               %>%
      select(-weight, -exp_factor_, -entity_count_)       %>%
      names() 
    
    
    # Compute statistics about accessible base units by clusters: only used to compute variances
    base_uuid     <- paste0( arena.chainSummary$baseUnit, "_uuid")
    cluster_uuid_  <- ifelse( arena.chainSummary$clusteringEntity != "", paste0(arena.chainSummary$clusteringEntity, "_uuid"), "")
    
    if ( cluster_uuid_ != "" & arena.chainSummary$clusteringVariances ) {
      cluster_statistics  <- result_cat[[ arena.analyze$entity ]] %>%
        data.frame()                                              %>%
        filter(weight > 0)                                        %>%
        distinct(!!! syms(base_uuid), .keep_all = TRUE)           %>%
        group_by_at( cluster_uuid_ )                               %>%
        dplyr::summarize( bu_count_ = n(), bu_sum_ = sum(weight), exp_factor_sum_ = sum(exp_factor_) )
      
      ids_2_survey       <- NULL
    } else if (cluster_uuid_ != "") {
      cluster_statistics <- NA
      ids_2_survey       <- cluster_uuid_ 
    } else {
      cluster_statistics <- NA
      ids_2_survey       <- NULL
    }
    
    # post-stratification attribute is missing
    if (is.null(arena.chainSummary$postStratificationAttribute)) arena.chainSummary$postStratificationAttribute <- ""
    
    if (arena.chainSummary$postStratificationAttribute != "") {
      arena.post_stratification         <- TRUE
      
      # post-stratification category table
      if (arena.chainSummary$postStratificationCategory != "") {
        arena.postcategory_table        <- as.data.frame( categories[[arena.chainSummary$postStratificationCategory]])
        if ('area' %in% names(arena.postcategory_table)) {
          arena.postcategory_table$area <- is.numeric(arena.postcategory_table$area)
          arena.postcategory_table$area[ is.na(arena.postcategory_table$area) ] <- 0
          ps.weights                    <- arena.postcategory_table        %>% 
            select(postStratificationAttribute = code, Freq = area) 
        } else {
          ps.weights <- df_base_unit                                       %>%
            group_by_at( arena.chainSummary$postStratificationAttribute )  %>%
            dplyr::summarize( Freq = sum(exp_factor_))                     %>%         
            data.frame()
          
          names(ps.weights)[[1]] <- "postStratificationAttribute" 
        }
      }
      
      # add a static column name 'postStratificationAttribute'
      for (i in (1:length(result_cat))) {
        result_cat[[i]]$postStratificationAttribute <- result_cat[[i]][ arena.chainSummary$postStratificationAttribute ][[1]] 
        result_cat[[i]]$postStratificationAttribute[ is.na(result_cat[[i]]$postStratificationAttribute) ] <- "NoData"
      } 
    } else {
      arena.post_stratification  <- FALSE
      ps.weights                 <- NULL 
    }
    
    if ( arena.stratification )       arena.analyze$dimensions <- unique(c(arena.analyze$dimensions, arena.strat_attribute))
    if ( arena.post_stratification  ) {
      arena.analyze$dimensions          <- unique( c(arena.analyze$dimensions,          "postStratificationAttribute" ))
      arena.analyze$dimensions_baseunit <- unique( c(arena.analyze$dimensions_baseunit, "postStratificationAttribute" ))
    }
    
    df_analysis_weights  <- result_cat[[arena.analyze$entity]] %>% distinct(!!! syms(base_uuid), .keep_all = T) %>% select(base_uuid, weight, exp_factor_)
    
    df_analysis_combined <- result_cat[[arena.analyze$entity]]
    
    # do first filtering, if filter clause exists. If error, ignore a filter rule      
    processMessage <- tryCatch({ if (arena.analyze$filter != "") {  
      df_analysis_combined <- df_analysis_combined %>%
        filter( eval(parse( text = arena.analyze$filter )))
      processMessage
    }},
    warning = function(w) { 
      cat("Error in filter clause - Filter not applied!") 
      return("Error in filter clause - Filter not applied.")},
    error   = function(e) { 
      cat("Error in filter clause - Filter not applied!")
      return("Error in filter clause - Filter not applied.")}
    )
    
    if (is.null( processMessage )) processMessage = ""
    
    dimension_names <- unique( c(cat_names_uuid, arena.analyze$dimensions_baseunit))
    if (arena.stratification)      dimension_names <- unique( c(dimension_names, arena.strat_attribute))
    if (arena.post_stratification) dimension_names <- unique( c(dimension_names, arena.chainSummary$postStratificationAttribute ))
    
    
    df_analysis_area <- df_analysis_combined                                    %>%
      filter( weight > 0 )                                                      %>%
      group_by(  across( dimension_names ))                                     %>%
      dplyr::summarize( across(.cols= all_of(cat_names_num), 
                               list(Total = ~sum(.x, na.rm = TRUE)),  
                               .names = "{.col}") )                             %>%
      ungroup()                                                                 %>%
      left_join(df_analysis_weights, by = base_uuid)
    
    
    df_analysis_combined <- df_analysis_combined                                %>%
      filter( weight > 0 )                                                      %>%
      group_by(  across( unique( c(cat_names_uuid, arena.analyze$dimensions)))) %>%
      dplyr::summarize( across(.cols= all_of(cat_names_num), 
                               list(Total = ~sum(.x, na.rm = TRUE)),  
                               .names = "{.col}") )                             %>%
      ungroup()                                                                 %>%
      left_join(df_analysis_weights, by = base_uuid)
    
    
    if (arena.chainSummary$samplingStrategy %in% c(1:4)) { 
      # 1. SIMPLE RANDOM SAMPLING, cluster/non-cluster
      # 2. SYSTEMATIC SAMPLING, cluster/non-cluster 
      # 3. STRATIFIED RANDOM SAMPLING, cluster/non-cluster  
      # 4. STRATIFIED SYSTEMATIC SAMPLING, cluster/non-cluster
      
      stratum_2_survey <- NULL # ifelse cannot assign NULL
      if (arena.strat_attribute != "") stratum_2_survey <- arena.strat_attribute
      
      # if stratification, compute SRS case. Needed to compute efficiency of stratification.
      if (arena.stratification | arena.post_stratification) {
        design_srvyr_SRS_total <- 
          df_analysis_combined     %>%
          srvyr::as_survey_design(
            ids       = !!ids_2_survey,
            strata    = NULL,
            fpc       = NULL, 
            weights   = NULL, 
            variables = c( arena.analyze$dimensions, ends_with('.Total')) )
      }
      
      design_srvyr_mean <- 
        df_analysis_combined     %>%
        srvyr::as_survey_design(
          ids       = !!ids_2_survey,
          strata    = !!stratum_2_survey,
          fpc       = NULL, 
          weights   = exp_factor_,  
          nest      = FALSE, # If TRUE, relabel cluster ids to enforce nesting within strata
          variables = c( arena.analyze$dimensions, ends_with('.Mean')) )
      
      design_srvyr_total <- 
        df_analysis_combined     %>%
        srvyr::as_survey_design(
          ids       = !!ids_2_survey,
          strata    = !!stratum_2_survey,
          fpc       = NULL, 
          weights   = NULL, 
          nest      = FALSE,
          variables = c( arena.analyze$dimensions, exp_factor_, ends_with('.Total')) )
      
      
      design_srvyr_area <- 
        df_analysis_area     %>%
        srvyr::as_survey_design(
          ids       = !!ids_2_survey,
          strata    = !!stratum_2_survey,
          fpc       = NULL, 
          weights   = NULL, 
          nest      = FALSE,
          variables = c( arena.analyze$dimensions_baseunit, exp_factor_ ) )
      
    }
    
    
    # 5. DOUBLE PHASE * coming later)
    if (arena.chainSummary$samplingStrategy == 5) design_srvyr <- ""
    
    # post-stratification
    # https://github.com/gergness/srvyr/issues/50
    # https://stats.oarc.ucla.edu/r/faq/how-do-i-analyze-survey-data-with-stratification-after-sampling-poststratification/
    
    if (arena.post_stratification) { 
      
      design_srvyr_mean  <- survey::postStratify( design_srvyr_mean, 
                                                  strata = ~postStratificationAttribute,
                                                  population = ps.weights,
                                                  partial = TRUE) #if TRUE, ignore population strata not present in the sample
      
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
    if (arena.stratification & !(arena.strat_attribute %in% arena.analyze$dimensions_input)) {
      arena.analyze$dimensions          <- arena.analyze$dimensions[! arena.analyze$dimensions                   %in% c(arena.strat_attribute)]
      arena.analyze$dimensions_baseunit <- arena.analyze$dimensions_baseunit[! arena.analyze$dimensions_baseunit %in% c(arena.strat_attribute)]
    }

    if (arena.post_stratification & !(arena.chainSummary$postStratificationAttribute %in% arena.analyze$dimensions_input) ) {
      arena.analyze$dimensions          <- arena.analyze$dimensions[! arena.analyze$dimensions                   %in% c(arena.chainSummary$postStratificationAttribute, 'postStratificationAttribute')]
      arena.analyze$dimensions_baseunit <- arena.analyze$dimensions_baseunit[! arena.analyze$dimensions_baseunit %in% c(arena.chainSummary$postStratificationAttribute, 'postStratificationAttribute')]
    }
    
    
    # MEANS (per hectares) for selected categories
    out_mean  <- design_srvyr_mean             %>%
      group_by_at( arena.analyze$dimensions )  %>%        # here comes grouping variable(s) 
      summarize_at( vars( ends_with(".Mean") ),     
                    funs( tally = sum(!is.na(.)), survey_mean(., na.rm = FALSE, vartype = c("se", "var", "ci"), proportion = FALSE, level=arena.chainSummary$pValue ))) %>% 
      as.data.frame(.) 
    
    names(out_mean) = gsub(pattern = "Mean_survey_", replacement = "", x = names(out_mean))
    sName           = df_analysis_combined %>% select(ends_with('.Mean')) %>% names() 
    if (length(sName) == 1) {
      sName = gsub("_ha.Mean", replacement="", sName )
      names(out_mean) = gsub(pattern = "survey_mean", replacement = sName, x = names(out_mean))
    }
    
    # TOTAL
    out_total <- design_srvyr_total           %>%
      group_by_at( arena.analyze$dimensions ) %>%    
      summarize_at( vars(area=exp_factor_, ends_with(".Total") ),      
                    funs( survey_total(., vartype = c("se", "var", "ci") )))         %>%  
      mutate(across(ends_with(".Total"), ~ .x/area, .names = "{col}_globalAverage")) %>%
      as.data.frame(.) 
    
    # AREA 
    out_area <- design_srvyr_area                      %>%
      group_by_at( arena.analyze$dimensions_baseunit ) %>%    
      summarize_at( vars(area=exp_factor_ ),      
                    funs( survey_total(.) ))           %>%  
      as.data.frame(.) 
    
    
    # ALL DATA (totals). Total variances are correctly computed here also for stratified sampling
    jdesign <- update( design_srvyr_total, whole_area_ = 1 )
    
    out_global_total <- jdesign %>%
      group_by( whole_area_ )   %>%       
      summarize_at( vars(area=exp_factor_, ends_with(".Total") ),      
                    funs( survey_total(., vartype = c("se", "var", "ci") )))         %>%  
      mutate(across(ends_with(".Total"), ~ .x/area, .names = "{col}_globalAverage")) %>%
      as.data.frame(.) 
    
    out_global_total$tally <- nrow( df_base_unit %>% filter(weight>0) %>% select_at(base_uuid) %>% unique() )
    # drop out area estimates from this table
    out_global_total <- out_global_total %>% 
      select(-starts_with("area")) 
    
    if (arena.stratification | arena.post_stratification) {
      
      # SRS: ALL DATA (totals) 
      jdesign       <- update( design_srvyr_SRS_total, whole_area_ = 1 )
      out_SRS_total <- jdesign                                      %>%
        group_by( whole_area_ )                                     %>%         
        summarize_at( vars( ends_with(".Total") ),      
                      funs( survey_total(., vartype = c("var") )))  %>%  
        as.data.frame(.) 
      
      var_global_total <- out_global_total %>% select(ends_with("_var")) 
      var_SRS_total    <- out_SRS_total    %>% select(ends_with("_var"))
      var_efficiency   <- var_SRS_total / var_global_total 
      
      names(var_efficiency) = gsub(pattern = "_ha.Total_var", replacement = "", x = names(var_efficiency))
      rm(var_global_total); rm(var_SRS_total)
      
    }
    #######################################
    # remove extra '_tally' columns by groups, leave just one tally row
    if ("_tally" %in% stringr::str_sub(colnames(out_mean), -6, -1)) {
      tally_out        <- out_mean %>% select(ends_with("_tally"))  %>% select(1) 
      names(tally_out) <- "tally"
      out_mean         <- out_mean %>% select(-ends_with("_tally")) %>% cbind(tally_out)
      rm(tally_out)
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
      for (i in (1:length(result_labels))) {
        if (names(result_labels[i]) %in% names(out_table)) {
          out_table$code <- out_table[[names(result_labels[i])]]
          out_table <- out_table                       %>% 
            inner_join(result_labels[[i]], by= "code") %>%
            select(-code)
          
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
    
    # rename columns
    out_mean  <- setNames( out_mean,  stringr::str_replace( names(out_mean),  "_ha.mean_se", ".sd"  ))
    out_mean  <- setNames( out_mean,  stringr::str_replace( names(out_mean),  "_ha.mean_var", ".var"))
    out_mean  <- setNames( out_mean,  stringr::str_replace( names(out_mean),  "_ha.mean_low",  ".ci_lower"  ))
    out_mean  <- setNames( out_mean,  stringr::str_replace( names(out_mean),  "_ha.mean_upp",  ".ci_upper"))
    out_mean  <- setNames( out_mean,  stringr::str_replace( names(out_mean),  "_ha.mean", ".mean"   )) 
    
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total_se",".sd"  ))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total_var",".var"))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "area_se", "area_sd"  ))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total_low", ".ci_lower"))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total_upp", ".ci_upper"))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total_globalAverage",".average"))
    out_total <- setNames( out_total, stringr::str_replace( names(out_total), "_ha.Total", ".total" ))
    
    
    out_file <- list()
    out_file[[1]] <- paste0(user_file_path, out_path, arena.analyze$entity, "_out_mean.csv")
    out_file[[2]] <- paste0(user_file_path, out_path, arena.analyze$entity, "_out_total.csv")
    
    tryCatch({if (exists('user_file_path') & exists("out_mean"))  write.csv(out_mean, out_file[[1]],  row.names = F)},
             warning = function(w) { cat("No output - out_mean") },
             error   = function(e) { cat("No output - out_mean")
             })
    
    tryCatch({if (exists('user_file_path') & exists("out_total")) write.csv(out_total, out_file[[2]], row.names = F)},
             warning = function(w) { cat("No output - out_total") },
             error   = function(e) { cat("No output - out_total")
             })
    
  } # for loop
  
  if (arena.analyze$reportingMethod == '2') arena.analyze$dimensions <- arena.analyze$dimensions_input 
  
  out_file[[3]] <- paste0(user_file_path, arena.analyze$entity, "_out_global_total.csv")
  out_file[[4]] <- paste0(user_file_path, arena.analyze$entity, "_relative_efficiency.csv")
  out_file[[5]] <- paste0(user_file_path, arena.analyze$entity, "_nonresponse_correction_by_stratum.csv")
  out_file[[6]] <- paste0(user_file_path, arena.analyze$entity, "_area_estimates.csv")
  
  # rename columns
  #  out_global_total <- setNames( out_global_total, stringr::str_replace(names(out_global_total), "area_se",    "area_sd"))
  out_global_total <- setNames( out_global_total, stringr::str_replace(names(out_global_total), "_ha.Total_se",  ".sd"))
  out_global_total <- setNames( out_global_total, stringr::str_replace(names(out_global_total), "_ha.Total_var", ".var"))
  out_global_total <- setNames( out_global_total, stringr::str_replace(names(out_global_total), "_ha.Total_low", ".ci_lower"))
  out_global_total <- setNames( out_global_total, stringr::str_replace(names(out_global_total), "_ha.Total_upp", ".ci_upper"))
  out_global_total <- setNames( out_global_total, stringr::str_replace(names(out_global_total), "_ha.Total_globalAverage",".average"))
  out_global_total <- setNames( out_global_total, stringr::str_replace(names(out_global_total), "_ha.Total", ".total"))
  out_global_total$whole_area_ <- NULL
  
  tryCatch({if (exists('user_file_path') & exists("out_global_total")) write.csv(out_global_total, out_file[[3]], row.names = F)},
           warning = function(w) { cat("No output - out_global_total") },
           error   = function(e) { cat("No output - out_global_total")
           })
  
  if (arena.stratification | arena.post_stratification) {
    tryCatch({if (exists('user_file_path') & exists("var_efficiency")) write.csv(var_efficiency, out_file[[4]], row.names = F)},
             warning = function(w) { cat("No output - var_efficiency") },
             error   = function(e) { cat("No output - var_efficiency")
             })
  }
  
  tryCatch({if (exists('user_file_path') & exists("out_area")) write.csv(out_area, out_file[[6]], row.names = F)},
           warning = function(w) { cat("No output - out_area") },
           error   = function(e) { cat("No output - out_area")
           })
  
  if (arena.chainSummary$nonResponseBiasCorrection & arena.strat_attribute !="") {
    nonResponse_out1 <- df_base_unit %>% select( STRATUM=arena.strat_attribute, correction_factor = arena_cluster_correction ) %>% unique() %>% arrange(STRATUM)  
    tryCatch({if (exists('user_file_path') & exists("nonResponse_out1")) write.csv(nonResponse_out1, out_file[[5]], row.names = F)},
             warning = function(w) { cat("No output - nonResponse_out1") },
             error   = function(e) { cat("No output - nonResponse_out1")
             })
  }
  
  
  # get results by sampling units out
  out_path <- paste0(user_file_path, "sampling unit results", "/")
  # create a folder for files to be exported
  if (!dir.exists( out_path )) dir.create( out_path, showWarnings = FALSE )
  
  for (i in 1:length(result_entities)) {
    outfile7              <- paste0( out_path, result_entities[[i]], "_base_unit_results.csv")
    base_unit.results_out <- base_unit.results[i] %>% as.data.frame() %>% select(-ends_with(".Total"))
    
    dimension_names <- arena.chainSummary$baseUnitEntityKeys
    if (arena.stratification)      dimension_names <- unique( c(dimension_names, arena.strat_attribute))
    if (arena.post_stratification) dimension_names <- unique( c(dimension_names, arena.chainSummary$postStratificationAttribute ))
    
    
    base_unit.results_out <- df_base_unit %>% select(base_uuid, all_of( dimension_names ), weight) %>%
      dplyr::left_join( base_unit.results_out, by = base_uuid) %>%
      select(-base_uuid)
    
    tryCatch({if (exists('user_file_path')) write.csv(base_unit.results_out, outfile7, row.names = F)},
             warning = function(w) { cat("No output - base unit results") },
             error   = function(e) { cat("No output - base unit results")
             })
    
    if (cluster_uuid_ !="") {
        outfile8            <- paste0( out_path, result_entities[[i]], "_cluster_results.csv")
        cluster.results_out <- cluster.results[i] %>% as.data.frame() %>% select(-ends_with(".Total"))
        cluster.results_out <- get( arena.chainSummary$clusteringEntity ) %>% select(cluster_uuid_, all_of( arena.chainSummary$clusteringEntityKeys )) %>%
          dplyr::left_join( cluster.results_out, by = cluster_uuid_) %>%
          select(-cluster_uuid_)
        
        cluster.results_out[is.na(cluster.results_out)] <- 0
        
        tryCatch({if (exists('user_file_path')) write.csv(cluster.results_out, outfile8, row.names = F)},
                 warning = function(w) { cat("No output - cluster results") },
                 error   = function(e) { cat("No output - cluster results")
                 })
    }
    
  }
  
  if ( Sys.getenv("RSTUDIO_PROGRAM_MODE") == "server" & exists('user_file_path') ) { 
    # zip all files
    export_filename  <- paste0( user_file_path, 'arena_results.zip')
    files2zip        <- dir( user_file_path, full.names = TRUE )
    if ( length(files2zip)>0 ) {
      zip(zipfile = export_filename, files = files2zip, mode = "cherry-pick")
      browseURL( export_filename )
    }
  }
  
  if ( Sys.getenv("RSTUDIO_PROGRAM_MODE") == "desktop" & exists('user_file_path') ) { 
    if ( Sys.info()['sysname']=="Windows" ) processMessage = " Result files in /Documents/arena/SURVEYNAME/user_output/"
  }
  
  processMessage = paste0("Arena Analytics: Process completed. ", processMessage )
  return( processMessage )
}

###################################################################
# END -------------------------------------------------------------
###################################################################

