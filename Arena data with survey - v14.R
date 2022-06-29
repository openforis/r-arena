

tryCatch( usePackage('dplyr'),
          error = function(e){ library('dplyr')
          })

tryCatch( usePackage('stringr'),
          error = function(e){ library('stringr')
          })

# define a folder for output files
if (!exists('user_file_path')) user_file_path <- './user_output/'
# create a folder for files to be exported
if (!dir.exists( user_file_path )) dir.create( user_file_path, showWarnings = FALSE )


###############################################################################################
######## UI: no action
########
###############################################################################################

# set  options, see more at https://r-survey.r-forge.r-project.org/survey/html/surveyoptions.html
#options(dplyr.summarise.inform      = FALSE)
options(survey.ultimate.cluster     = FALSE)
options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu           = "adjust")  # alternatively "remove"
options(digits = 10)
old_sigfig <- options("pillar.sigfig") # https://github.com/gergness/srvyr/blob/main/vignettes/srvyr-vs-survey.Rmd
options("pillar.sigfig" = 5)

# read JSON file 
chain_summary_json <-  paste(getwd(), 'chain_summary.json', sep = .Platform$file.sep)
if ( file.exists( chain_summary_json ))  arena.chainSummary <- jsonlite::fromJSON( chain_summary_json )

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
    cluster_uuid                  <- ifelse( arena.chainSummary$clusteringEntity != "", paste0(arena.chainSummary$clusteringEntity, "_uuid"), "")    
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
                     summarise( cluster_count = n( ), sum_weight= sum(weight) ), by = arena.strat_attribute) %>%
                  mutate( cluster_count            = ifelse( is.na(cluster_count ), 0, cluster_count) )      %>%
                  mutate( arena_cluster_correction = ifelse( cluster_count>0 & design_number_psu>0, design_number_psu/cluster_count, 1))
                

                df_base_unit <- df_base_unit %>%
                  left_join(arena.samplingdesign_table %>% select(arena.strat_attribute, arena_cluster_correction), by = arena.strat_attribute)

                
                # B. MISSING BASE UNITS IN CLUSTER: nonresponse bias correction, naive imputation method
                # clustered
                if (cluster_uuid != "" & !arena.chainSummary$clusteringVariances & all(!is.na(arena.samplingdesign_table$design_number_ssu))) {
                    arena_cluster_statistics <- arena.samplingdesign_table %>%
                      select( arena.strat_attribute, design_number_ssu )   %>%
                      right_join( df_base_unit %>% 
                        filter( weight > 0 )   %>%
                        group_by( !!! syms(arena.strat_attribute), cluster_uuid ) %>%
                        summarise(bu_count = n( ), sum_weight= sum(weight)) )     %>%
                      mutate( arena_bu_correction = ifelse(!is.na(design_number_ssu) & design_number_ssu > 0, design_number_ssu/sum_weight, 1)) # this works if a full base unit weight is 1 !!
                    
                    # check whether some clusters are split over more than 1 stratum
                    if (nrow(arena_cluster_statistics) != length(unique(arena_cluster_statistics$cluster_uuid))) {
                          # list of clusters belonging to multiple strata
                          analyze_overlaps <- arena_cluster_statistics %>%
                            group_by(cluster_uuid ) %>%
                            summarize(c_count =n()) %>%
                            filter(c_count > 1)     %>%
                            pull(cluster_uuid) 
                          
                          # fix this later, overlaps get all 1:
                          arena_cluster_statistics$arena_bu_correction <- with(arena_cluster_statistics,
                                ifelse(cluster_uuid %in% analyze_overlaps, 1, arena_bu_correction)) 
                          
                          df_base_unit <- df_base_unit %>%
                            left_join(arena_cluster_statistics %>% 
                                        select(!!! syms(arena.strat_attribute),cluster_uuid, arena_bu_correction), by = c(arena.strat_attribute,cluster_uuid))
                          
                    } else {  # no clusters split across strata
                      df_base_unit <- df_base_unit       %>%
                      left_join(arena_cluster_statistics %>% 
                                  select(cluster_uuid, arena_bu_correction), by = cluster_uuid)
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
                summarize( aoi_weight_ = sum( weight ), aoi_count_ =n()) 
              
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
         if (cluster_uuid !="" & !arena.chainSummary$clusteringVariances) {
            max_weight_in_cluster <- df_base_unit %>% 
              group_by( cluster_uuid )            %>%
              summarise(sum_weight= sum(weight))  %>%
              select(sum_weight)                  %>%
              max()

           df_base_unit$arena_bu_correction <- NULL
           
           df_base_unit <- df_base_unit %>% 
              group_by( cluster_uuid )  %>%
              summarise(sum_weight= sum(weight))  %>%
              mutate( arena_bu_correction = max_weight_in_cluster/sum_weight) %>%
              select(-sum_weight) %>%
              right_join(df_base_unit, by=cluster_uuid) 
          
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
    
    # ADD area estimates based on exp_factor

    # Quantitative result variables against categorical and taxonomic data
    for (i in (1:length(result_entities))) {

      # PART 1.compute sums (per AOIs) and means (/ha) across all categorical variables 
      # drop out columns where all data is NA.  https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
      
      df_entitydata              <- get(result_entities[[i]])

      result_cat_attributes[[i]] <- ( df_entitydata  %>% select_if(~!all(is.na(.))) %>% select_if(~is.character(.)) )  %>%
        select(ends_with("_label") | ends_with("_scientific_name")) %>% 
        names()

      boolean_list <-  ( df_entitydata  %>% select_if(~!all(is.na(.))) %>% select_if(~is.character(.)) )  %>%
        select_if(~all( .=="false" | .=="true")) %>% 
        select_if(~!all(.=="false"))             %>%
        select_if(~!all(.=="true"))              %>% 
        names()
      
      if (length(boolean_list) > 0) result_cat_attributes[[i]] <- unique( c(result_cat_attributes[[i]], boolean_list))
      rm(boolean_list)
      
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
          result_cat[[i]] <- df_entitydata  %>%
            # join expansion factor
            dplyr::right_join(df_base_unit %>% select(base_uuid, exp_factor_), by= base_uuid) %>%  
            group_by(  across( result_cat_attributes[[i]] )) %>%
            summarise( across(.cols= all_of(resultVariables), 
                             list(Total = ~sum(exp_factor_ * .x, na.rm = TRUE), Mean = ~sum(.x, na.rm = TRUE) ),  
                             .names = "{.col}.{.fn}"), 
                             entity_count_ = n() ) %>%
            data.frame()
        
        } else { # entity is the base unit
          result_cat[[i]] <- df_entitydata  %>%
            group_by(  across( result_cat_attributes[[i]] )) %>%
            summarise( across(.cols= all_of(resultVariables), 
                              list(Total = ~sum(exp_factor_ * .x, na.rm = TRUE), Mean = ~sum(.x, na.rm = TRUE) ),  
                              .names = "{.col}.{.fn}"), 
                              entity_count_ = n() ) %>%
            data.frame()
        }
      

      # add weight, exp_factor_; AND IF EXISTS: cluster_uuid, arena.strat_attribute (This is actually already in dataframe because it is categorical!) 
      temp_list_variables <- c(base_uuid, "weight", "exp_factor_")
      if (cluster_uuid != ""    &  !(cluster_uuid %in% names(result_cat[[i]])) )          temp_list_variables <- c(temp_list_variables, cluster_uuid)
      if (arena.stratification  &  !(arena.strat_attribute %in% names(result_cat[[i]])) ) temp_list_variables <- c(temp_list_variables, arena.strat_attribute)
            
      result_cat[[i]] <- result_cat[[i]] %>%
        left_join(df_base_unit %>% select( all_of(temp_list_variables)), by = base_uuid)
      
      rm( temp_list_variables )
      
      # # PART 2. compute sum of per hectare results at the base unit level for each result variable
      base_unit.results[[i]] <- df_entitydata %>%
        # Add expansion factor for all result entities
        dplyr::right_join(df_base_unit %>% 
                          select(base_uuid, exp_factor_), by = base_uuid) %>%
        group_by_at( base_uuid ) %>%
        summarise(across(.cols= all_of(resultVariables),
                          list(Total = ~sum(exp_factor_ * .x, na.rm = TRUE), Mean = ~sum(.x, na.rm = TRUE) ),
                          .names = "{.col}.{.fn}"),
                          arena_count = n() )
      
      
      # join results with the clone of base unit
      df_base_unit <- df_base_unit %>%
          dplyr::left_join( base_unit.results[[i]], by = base_uuid)

      rm(resultVariables)
    }
    
    names(result_cat) <- result_entities    
    
} # END OF (arena.chainSummary$samplingDesign == TRUE)



###############################################################################################
# SAMPLING STRATEGIES AND RELIABILITY
# if (arena.chainSummary$samplingDesign==TRUE)
#
# input data: result_cat (list of dataframes), df_base_unit (dataframe), arena.chainSummary 
# note: array of  result entities' names we get as follows: names(result_cat)
###############################################################################################
###############################################################################################
######## UI: 1) select from a dropdown list [names(result_cat)] one entity to analyze: the list contains the base unit names, and all (child) entities' names (i.e. entities with area-based variables) 
########     Dimensions: drag and drop from a list containing all categorical attributes + taxonomic attributes of the selected entity (1 - multiple)

arena.analyze   <- list(entity = "tree", dimensions = c("cluster_stratum", "cluster_province", "plot_fra", "plot_landuse"), filter = "plot_landuse == '1'", categories = categories)

# add labels to the categorical result variables

result_labels           <- list() # [1]: input attribute, [2]: result attribute 
result_names_category_1 <- intersect( arena.analyze$dimension, names(get(arena.analyze$entity)))
result_names_category_2 <- setdiff(   arena.analyze$dimension, names(get(arena.analyze$entity)))

if (length( result_names_category_1 ) > 0) {
    df_cat_report <- get(arena.analyze$entity) %>% 
      select( all_of(result_names_category_1), any_of(paste0(result_names_category_1,"_label"))) %>%
      distinct()
    
    for (i in (1:length(result_names_category_1))) {
      result_labels[[i]]  <- df_cat_report %>% 
        select(code = result_names_category_1[i], label = paste0(result_names_category_1[i], "_label")) %>%
        distinct() %>%
        arrange(.[1])
    }
    rm(df_cat_report)
}

if (length( result_names_category_2 ) > 0) {
    dataindex <- length(result_labels) 
  
    df_cat_report <- arena.chainSummary$resultVariables %>%
      filter( type=="C" & active==TRUE & name %in% result_names_category_2) %>%
      select( categoryName ) 
    
    for (i in (1:nrow(df_cat_report))) {
      result_labels[[dataindex + i]]  <- 
        categories[ df_cat_report$categoryName[[i]] ] %>% 
        data.frame  %>%
        select(2,3)
    
      names(result_labels[[dataindex + i]]) <- c("code","label") 
    }
}

names(result_labels) <- c( result_names_category_1, result_names_category_2 )

if (exists("result_names_category_1")) rm(result_names_category_1) 
if (exists("result_names_category_2")) rm(result_names_category_2) 

#
########     Measures:   "Area" + all area-based attributes of the selected entity (1 - multiple)
########     Filter:     drag and drop from a list containing all categorical attributes + taxonomic attributes of the selected entity (1 - multiple)
#######
#######      Button (action): submit selections as arguments to an R function, run it
###############################################################################################



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
     data.frame()               %>%
     select_if(is.numeric)      %>%
     select(-weight, -exp_factor_, -entity_count_)     %>%
     names() 
   

    # Compute statistics about accessible base unit by clusters: only used to compute variances
    # what is NFMA method? Base unit is plot section, not a plot?
    base_uuid     <- paste0( arena.chainSummary$baseUnit, "_uuid")
    cluster_uuid  <- ifelse( arena.chainSummary$clusteringEntity != "", paste0(arena.chainSummary$clusteringEntity, "_uuid"), "")
    
    if ( cluster_uuid != "" & arena.chainSummary$clusteringVariances ) {
      cluster_statistics  <- result_cat[[ arena.analyze$entity ]]   %>%
        data.frame()                                    %>%
        filter(weight > 0)                              %>%
        distinct(!!! syms(base_uuid), .keep_all = TRUE) %>%
        group_by_at( cluster_uuid )                     %>%
        summarise( bu_count_ = n(), bu_sum_ = sum(weight), exp_factor_sum_ = sum(exp_factor_) )
      
      ids_2_survey       <- NULL
    } else if (cluster_uuid != "") {
      cluster_statistics <- NA
      ids_2_survey       <- cluster_uuid 
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
          arena.postcategory_table        <- as.data.frame( arena.analyze$categories[[arena.chainSummary$postStratificationCategory]])
          if ('area' %in% names(arena.postcategory_table)) {
            arena.postcategory_table$area <- is.numeric(arena.postcategory_table$area)
            arena.postcategory_table$area[ is.na(arena.postcategory_table$area) ] <- 0
            ps.weights                    <- arena.postcategory_table %>% select(postStratificationAttribute = code, Freq = area) 
          } else {
            ps.weights <- df_base_unit                                          %>%
                 group_by_at( arena.chainSummary$postStratificationAttribute )  %>%
                 summarize( Freq = sum(exp_factor_))                            %>%         
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
    if ( arena.post_stratification  ) arena.analyze$dimensions <- unique(c(arena.analyze$dimensions, "postStratificationAttribute"))
    
    
    df_analysis_weights <- result_cat[[1]] %>% distinct(!!! syms(base_uuid), .keep_all = T) %>% select(base_uuid, weight, exp_factor_)
    
    df_analysis_combined <- result_cat[[1]]                                     %>%
      filter( eval(parse( text = arena.analyze$filter )))                       %>%
      filter( weight > 0 )                                                      %>%
      group_by(  across( unique( c(cat_names_uuid, arena.analyze$dimensions)))) %>%
      summarise( across(.cols= all_of(cat_names_num), 
                        list(Total = ~sum(.x, na.rm = TRUE)),  
                        .names = "{.col}") )                                    %>%
      ungroup()                                                                 %>%
      left_join(df_analysis_weights, by = base_uuid)
    
    
    # Shiny, see https://stackoverflow.com/questions/57853627/moveable-multiple-items-in-r-shiny-boxes-something-similar-to-attached-screens
    
    
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
            weights   = weight,  
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
    
    
      # MEANS (per hectares) for selected categories
      out_mean  <- design_srvyr_mean             %>%
        group_by_at( arena.analyze$dimensions )  %>%        # here comes grouping variable(s) 
        summarize_at( vars( ends_with(".Mean") ),     
                     funs( tally = sum(!is.na(.)), survey_mean(., na.rm = FALSE, vartype = c("se", "var", "ci"), proportion = FALSE, level=arena.chainSummary$pValue ))) %>% 
        as.data.frame(.) 
      
      names(out_mean) = gsub(pattern = "Mean_survey_", replacement = "", x = names(out_mean))

      
      # TOTAL
      out_total <- design_srvyr_total           %>%
        group_by_at( arena.analyze$dimensions ) %>%          # here comes grouping variable(s)  
        summarize_at( vars(area=exp_factor_, ends_with(".Total") ),      
                      funs( survey_total(., vartype = c("se", "var", "ci") ))) %>%  
        mutate(across(ends_with(".Total"), ~ .x/area, .names = "{col}_globalAverage")) %>%
        as.data.frame(.) 
      

      # ALL DATA (totals). Total variances are correctly computed here also for stratified sampling
      jdesign <- update( design_srvyr_total, whole_area_ = 1 )
      
      out_global_total <- jdesign %>%
        group_by( whole_area_ )   %>%          # here comes grouping variable(s)  
        summarize_at( vars(area=exp_factor_, ends_with(".Total") ),      
                      funs( survey_total(., vartype = c("se", "var", "ci") ))) %>%  
        mutate(across(ends_with(".Total"), ~ .x/area, .names = "{col}_globalAverage")) %>%
        as.data.frame(.) 
      

      if (arena.stratification | arena.post_stratification) {
          
          # SRS: ALL DATA (totals) 
          jdesign       <- update( design_srvyr_SRS_total, whole_area_ = 1 )
          out_SRS_total <- jdesign  %>%
            group_by( whole_area_ ) %>%          # here comes grouping variable(s)  
            summarize_at( vars( ends_with(".Total") ),      
                          funs( survey_total(., vartype = c("var") ))) %>%  
            as.data.frame(.) 
          
          var_global_total <- out_global_total %>% select(ends_with("_var"), -area_var) 
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
            out_table$code <- out_table[[names(result_labels[i])]]
            out_table <- out_table %>% 
              inner_join(result_labels[[i]], by= "code") %>%
              select(-code)
            
            out_table[[names(result_labels[i])]] <- out_table$label
            out_table$label <- NULL
            out_table$code  <- NULL
          }
          return(out_table)
      }
      
      out_mean  <- joinLabels( result_labels, out_mean )
      out_total <- joinLabels( result_labels, out_total )
      

      tryCatch({if (exists('user_file_path') & exists("out_mean"))  write.csv(out_mean,  paste0(user_file_path, "out_mean.csv"),  row.names = F)},
               warning = function(w) { cat("No output - out_mean") },
               error   = function(e) { cat("No output - out_mean")
               })
      
      tryCatch({if (exists('user_file_path') & exists("out_total")) write.csv(out_total, paste0(user_file_path, "out_total.csv"), row.names = F)},
               warning = function(w) { cat("No output - out_total") },
               error   = function(e) { cat("No output - out_total")
               })
      
      tryCatch({if (exists('user_file_path') & exists("out_global_total")) write.csv(out_global_total, paste0(user_file_path, "out_global_total.csv"), row.names = F)},
               warning = function(w) { cat("No output - out_global_total") },
               error   = function(e) { cat("No output - out_global_total")
               })

      if (arena.stratification | arena.post_stratification) {
        tryCatch({if (exists('user_file_path') & exists("var_efficiency")) write.csv(var_efficiency, paste0(user_file_path, "relative_efficiency.csv"), row.names = F)},
                 warning = function(w) { cat("No output - var_efficiency") },
                 error   = function(e) { cat("No output - var_efficiency")
                 })
      }
      
      if (arena.chainSummary$nonResponseBiasCorrection & arena.strat_attribute !="") {
        nonResponse_out1 <- df_base_unit %>% select( STRATUM=arena.strat_attribute, correction_factor = arena_cluster_correction ) %>% unique() %>% arrange(STRATUM)  
        tryCatch({if (exists('user_file_path') & exists("nonResponse_out1")) write.csv(nonResponse_out1,  paste0(user_file_path, "nonresponse_correction_by_stratum.csv"), row.names = F)},
                 warning = function(w) { cat("No output - nonResponse_out1") },
                 error   = function(e) { cat("No output - nonResponse_out1")
                 })
      }
      
      if ( Sys.getenv("RSTUDIO_PROGRAM_MODE") == "server" & exists('user_file_path') ) { 
        # zip all files
        export_filename  <- paste0( user_file_path, 'arena_results.zip')
        if (file.exists( export_filename )) file.remove( export_filename )
        files2zip        <- dir( user_file_path, full.names = TRUE )
        if (!is.empty(files2zip)) {
          zip(zipfile = export_filename, files = files2zip, mode = "cherry-pick")
          browseURL( export_filename )
        }
      }
      
  #    return("Process OK")
# }

###################################################################
# END -------------------------------------------------------------
###################################################################
###################################################################
  