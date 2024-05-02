#############################################################################
## This function is for computing area estimates of two-phase sampling with data from Open Foris Arena.
#
# This function can be called in the Arena's data processing chain, after running 'persist-results.R'.
# 
# Required R packages (with dependencies): tidyverse, rlang, forestinventory
#
# Created by:   Lauri Vesa, FAO
#               Anibal Cuchietti, FAO
#               
# Last update:  2.5.2024
# 
#############################################################################


ronak_func <- function( df, joint_category_column, report_categories ) {   
  # https://stackoverflow.com/questions/67996444/how-do-i-create-a-binary-column-based-off-characters-in-another-column-in-r
  
  df <- df %>%
    mutate( TEMP_phase2 = ifelse( phase != 2 , NA, !!sym( joint_category_column) ))  %>% 
    mutate( col = paste0( "joint_ph2_", sub( '\\d+\\.', '', TEMP_phase2)))           %>%
    pivot_wider( names_from = col, values_from = col, 
                 values_fn = length, values_fill = 0)                                %>%
    dplyr::select( -TEMP_phase2 )                                                    %>%
    
    # For each joint category type this is a dummy variable as an indicator of the
    # number of plots of each joint (e.g. land use) category according to the 1st phase (N1)
    mutate(col = paste0( "joint_ph1_", sub( '\\d+\\.', '', !!sym(joint_category_column) ))) %>%
    pivot_wider( names_from = col, values_from = col, 
                 values_fn = length, values_fill = 0) %>%
    as.data.frame()
  
  if ( all(report_categories != "" )) {
    for (s_column in report_categories) { 
      df <- df %>%
        mutate( TEMP_phase2 = ifelse( phase != 2 , NA, !!sym( s_column )))            %>%
        mutate( col = paste0( "report_ph2_", s_column, "__", sub( '\\d+\\.', '', TEMP_phase2)))  %>%
        pivot_wider( names_from = col, values_from = col, 
                     values_fn = length, values_fill = 0)                             %>%
        dplyr::select( -TEMP_phase2 )                                                 %>%
        as.data.frame()
    }
  }
  
  df
}


arenaTwoPhaseSampling_area_estimates <- function() {
  # PACKAGES
  
  usePackage("tidyverse")
  usePackage("forestinventory")
  
  #######################################
  # FUNCTIONS
  
  source("https://raw.githubusercontent.com/openforis/r-arena/master/two_phase_ratio_estimator.R")
  source("https://raw.githubusercontent.com/openforis/r-arena/master/arena_survey_analysis.R")
  
  
  #######################################
  # read JSON file data
  # if argument missing, set predefined dimension list to NULL. Dimensions are read from JSON (=arena.chainSummary) 
  dimension_list_arg <- NULL
  server_report_step <- "last"
  
  # Read json data
  # KORJAA: Jos DIMENSION puuttuu: arenaReadJSON ei palauta mitään!
  
  arena_return        <- arenaReadJSON( dimension_list_arg )
  if (is.list(arena_return))  {
    arena.analyze       <- arena_return[[1]]
    arena.chainSummary  <- arena_return[[2]]
  }
  rm(arena_return)
  
  #
  # Read commonAttribute (= joint category)
  joint_category_          <- arena.chainSummary$commonAttribute
  
  # TAKE OUT NEXT 2 LINES AFTER UI COMPLETE
  if ( is.null(joint_category_) ) joint_category_  <- "rs_land_use"
  if ( joint_category_ == "")     joint_category_  <- "rs_land_use"
  
  # Note, take parentCode from arena.schemaSummary in order to select the correct level!
  cat_level <- arena.schemaSummary     %>% 
    filter( name == joint_category_)  %>% 
    select( categoryName, parentCode) %>%
    mutate( level = ifelse( is.na(parentCode) | parentCode == "", 1, as.integer( stringr::str_sub( categoryName, -4, -2)))) %>%
    select( level)                    %>%
    pull()
  
  if (cat_level > 1) print(paste0("WARNING: the common attribute is in a hierarchical table at level ", cat_level, ". It is assumed that all that category level codes are unique!"))
  
  
  # get joint category label
  joint_category_label_    <- paste(joint_category_, "label", sep = "_")
  joint_category_Tablename <- arena.schemaSummary$categoryName[ arena.schemaSummary$name == joint_category_ ]
  # take out [level_code]
  joint_category_Tablename <- trimws( stringr::word( joint_category_Tablename, 1, sep= "\\["))
  
  joint_category_Table <- categories[[ joint_category_Tablename ]] %>%
    select(-uuid)               %>%
    filter( level == cat_level) %>%
    mutate( code = as.character(code))  # Note: here we assume that all codes are unique !
  
  rm( joint_category_Tablename )
  
  ##########################################################################
  # get domain/stratum attribute
  DomainAttribute_phase1 <- ""
  df_Domain              <- NA
  
  
  if (arena.chainSummary$stratumAttribute != "" ) {
    df_Domain <- categories[[ arena.chainSummary$stratumAttributeCategory ]] %>%
      select( -uuid)
    
    if ('code_joint' %in% names( df_Domain)) {
      df_Domain$Domain_code = as.character( df_Domain$code_joint)
    } else {
      df_Domain$Domain_code = as.character( df_Domain$code)
    }  
    
    # get variable name in 1st phase sample data (sampling_point_data)
    domainDefaultValueExpression <- arena.schemaSummary %>% 
      filter(name == arena.chainSummary$stratumAttribute) %>%
      select( defaultValue) %>% 
      pull() %>% trimws()
    
    # Get domain attribute name in phase1 table. Note. This method does not work if domain is on level 2 or above! It must on level 1.  
    if ( stringr::str_detect( domainDefaultValueExpression, "categoryItemProp")) {
      domainDefaultValueExpression = stringr::str_sub( domainDefaultValueExpression, 18, )
      st = stringr::str_split( domainDefaultValueExpression, ",")[[1]][[2]]
      st = trimws( gsub("'", '', st)) # remove single quotation marks
      
      DomainAttribute_phase1 <- st
      rm(st); rm(domainDefaultValueExpression)
    } else {
      DomainAttribute_phase1 <- arena.chainSummary$stratumAttribute
    }
  } 
  
  
  #########################################################################
  # Get phase 1 data
  data_phase1 <- categories[[ arena.chainSummary$phase1Category ]]  
  if ( is.null( data_phase1)) cat("Missing 1st phase category table!")
  
  
  if ('code_joint' %in% colnames(data_phase1)) {
    data_phase1$plot_id_ = data_phase1$code_joint
  } else {
    data_phase1$plot_id_ = data_phase1$level_1_code
  }
  
  data_phase1 <- data_phase1 %>% 
    filter(as.numeric(level_2_code) >= 1) %>%
    dplyr::select(level_1_code,
                  level_2_code, 
                  phase, 
                  plot_id_, 
                  all_of( joint_category_),
                  any_of( DomainAttribute_phase1)) %>%
    mutate(across(everything(), as.character), phase = as.integer(phase))
  
  if (DomainAttribute_phase1 %in% names(data_phase1)) {
    data_phase1$DOMAIN                              <- data_phase1[[ DomainAttribute_phase1 ]]
    data_phase1[[ DomainAttribute_phase1 ]]         <- NULL
  }
  
  
  cluster_key_  <- NA
  if (arena.chainSummary$clusteringEntity != "") cluster_key_ <- arena.chainSummary$clusteringEntityKeys
  
  baseunit_key_ <- setdiff( arena.chainSummary$baseUnitEntityKeys, cluster_key_ )
  if ( all( is.na(cluster_key_))) cluster_key_ <- ""
  
  
  
  # Get active quantitative area-based categorical result variables from the Arena survey
  result_attribute_names <- arena.chainSummary$resultVariables %>%
    filter( active == TRUE & type == "Q" & areaBased == TRUE)  %>%
    mutate( name = ifelse(type == "Q", paste( name, "ha", sep = "_"), name)) %>%
    select( name) %>%
    pull()
  
  reporting_categories        <- arena.analyze$dimensions # arena.chainSummary$analysis$dimensions
  reporting_categories_labels <- ""
  computed_result_categories  <- data.frame()
  
  if (all( reporting_categories != "")) {
    reporting_categories_labels  <- paste( reporting_categories, "label", sep = "_") 
    computed_result_categories   <- arena.chainSummary$resultVariables %>%
      filter( active == TRUE & type == "C")     %>%
      filter( name %in% reporting_categories) %>%
      select( name, table = categoryName ) 
  }
  
  # Get phase 2 data
  data_phase2 <- get( arena.analyze$entity ) 
  
  if ( arena.chainSummary$stratumAttribute != "") data_phase2$DOMAIN <- data_phase2[[ arena.chainSummary$stratumAttribute ]]
  
  # Add labels for computed categorical result variables. These are not in the inputted data. 
  if ( nrow( computed_result_categories) > 0) {
    for (i in(1:nrow( computed_result_categories) )) {
      n = computed_result_categories$name[i]
      t = categories[[ computed_result_categories$table[i] ]] %>%
        dplyr::select( !!quo_name(n) := code, label)          %>%   # https://stackoverflow.com/questions/49650394/how-to-rename-a-variable-using-a-dynamic-name-and-dplyr
        rename( !!paste0(colnames(.)[1], "_label") := 2)      %>%   # https://stackoverflow.com/questions/72957517/how-to-rename-a-column-using-string-from-another-column-in-r-dplyr
        mutate( across( everything(), as.character))
      
      n2 = colnames(t)[2]
      
      if ((n %in% colnames(data_phase2)) & !(n2 %in% colnames(data_phase2))) {
        data_phase2 <- data_phase2 %>%
          dplyr::left_join( t, by = colnames(t)[1])
      }
      rm(n); rm(n2); rm(t)
    }
  }
  
  # Copy cluster key attribute into "level_1_code"
  if ( all( cluster_key_ != "")) {
    cluster_attribute        <- "level_1_code"
    data_phase2 <- data_phase2 %>% unite(col = level_1_code, all_of( cluster_key_), sep = "*", remove = FALSE, na.rm = TRUE)
  } else {
    cluster_attribute        <- NA
    data_phase2$level_1_code <- "" 
  }  
  
  # Create "level_2_code" 
  data_phase2 <- data_phase2 %>% unite(col = level_2_code, all_of( baseunit_key_), sep = "*", remove = FALSE, na.rm = TRUE)
  
  
  data_phase2 <- data_phase2 %>% 
    dplyr::select(level_1_code, level_2_code, 
                  all_of( result_attribute_names), 
                  any_of( reporting_categories), 
                  any_of( reporting_categories_labels),
                  any_of( "DOMAIN"), 
                  any_of( arena.chainSummary$stratumAttribute),
                  all_of( joint_category_), 
                  all_of( joint_category_label_ )) %>%
    mutate(phase = as.integer(2), plot_id_ = paste(level_1_code, level_2_code, sep = "*"))
  
  
  # Merge the data of both phases 
  data_complete <- merge( data_phase1, data_phase2,
                          by = c("level_1_code", "level_2_code", "phase", "plot_id_", joint_category_, "DOMAIN"), 
                          all.x = TRUE, all.y = TRUE)
  
  
  ######################
  
  #### Two-phase land use type area ####
  # Creation of the dummy variables for each land use  
  # Data frame field plots and selected attributes only 
  if (arena.chainSummary$stratumAttribute != "") {
    cat( paste("Domain/Stratum column in 1st phase table: ", DomainAttribute_phase1))
    cat( paste("Domain/Stratum column in 2nd phase table: ", arena.chainSummary$stratumAttribute))
    cat( "Only area estimates can be currently computed.")
    cat( "Processing..")
    cat("\n")
  }
  
  DomainClasses        <- ""
  if (DomainAttribute_phase1 != "") DomainClasses <- sort( unique( data_complete$DOMAIN ))
  
  if (arena.chainSummary$stratumAttribute == "") {
    DomainClasses <- c("0") 
    df_Domain <- data.frame(
      Domain_code = "0",
      area = 100.0,
      label = "Unstratified")
    
    if (arena.analyze$reportingArea != "") df_Domain$area <- as.numeric( arena.analyze$reportingArea) 
  }
  
  jointCategoryClasses  <- sort( unique( data_complete[[ joint_category_ ]]))
  
  twophase_data         <- data_complete 
  twophase_data         <- twophase_data %>% 
    mutate( across( where(is.character), ~ replace_na(.x, "NA")))
  
  twophase_data         <- ronak_func( twophase_data, joint_category_, reporting_categories )   
  # names(twophase_data) 
  
  
  #### AREA ####
  # Estimation of area proportions
  
  i_count = 0
  
  for (i_Domain in (1: length( DomainClasses))) {
    
    Domain_area   <- df_Domain                        %>% 
      filter(Domain_code == DomainClasses[i_Domain])  %>% 
      select( area )                                  %>% 
      as.numeric()
    
    twophase_data_subset <- twophase_data
    # get subset (i.e. Domain data)
    if (arena.chainSummary$stratumAttribute != "") twophase_data_subset <- twophase_data %>% filter(DOMAIN == DomainClasses[i_Domain])
    
    for (i in (1: length( jointCategoryClasses)) ) {
      i_count            <- i_count + 1 
      col_ph1            <- paste("joint_ph1", jointCategoryClasses[i], sep = "_") 
      col_ph2            <- paste("joint_ph2", jointCategoryClasses[i], sep = "_")
      analysisExpression <- as.formula( paste( col_ph2, "~", col_ph1))  # https://stackoverflow.com/questions/52413017/create-formula-call-from-character-string
      
      # compute count of samples in the land category class
      samples_n1     <- sum( twophase_data_subset[[col_ph1]])
      
      two_phase_area <- forestinventory::twophase( formula = analysisExpression,
                                                   data = twophase_data_subset, 
                                                   list(phase.col = "phase", terrgrid.id = 2), cluster = cluster_attribute)
      
      # Summary of results
      sum_Area <- two_phase_area$estimation %>%
        mutate( Report_label      = joint_category_Table %>% filter(code == jointCategoryClasses[i]) %>% select(label) %>% pull()) %>%
        mutate( Cover_percent     = estimate * 100)                               %>%  
        mutate( Cover_SE          = sqrt((estimate * (1 - estimate)/n1)) * 100)   %>%
        mutate( Total_area_ha     = (Cover_percent/100) * Domain_area)           %>% 
        mutate( Area_SE           = sqrt(g_variance * Domain_area^2))            %>% 
        mutate( Domain_code      = DomainClasses[i_Domain])                    %>%
        mutate( Report_code       = jointCategoryClasses[i])                      %>%
        left_join( df_Domain %>% select(Domain_code, Domain_label = label), by = "Domain_code")
      
      sum_Area            = sum_Area[ , c(12:14, 7:11)]
      sum_Area            = cbind( sum_Area, two_phase_area$samplesizes)
      sum_Area$samples_n1 = samples_n1
      
      if (i_count == 1) {
        result_Area <- sum_Area 
      } else {
        result_Area <- bind_rows( result_Area, sum_Area)
      }
      
      rm(col_ph1); rm(col_ph2); rm(analysisExpression)
    }
  }
  
  row.names(result_Area) <- NULL  
  AREA_Results           <- result_Area
  print( AREA_Results )
  
  # define a folder for output files
  if ( !exists('user_file_path')) user_file_path <- './user_output/'
  # create a folder for files to be exported
  if ( !dir.exists( user_file_path )) dir.create( user_file_path, showWarnings = FALSE )
  out_path  <- "two-phase/"
  dir.create( paste0(user_file_path,"/", out_path), showWarnings = FALSE )
  
  out_file_name <- paste0(user_file_path, out_path, "Arena_2phase_resuls_by_common_attribute.csv")
  tryCatch({if (exists('user_file_path') & exists("AREA_Results"))  write.csv(AREA_Results, out_file_name,  row.names = F)},
           warning = function( w ) { cat("No output - AREA_Results") },
           error   = function( e ) { cat("No output - AREA_Results")
           })
  
  # avoid using common, domain, clustering and base unit attribute as dimensions. Take these out.
  reporting_categories <- reporting_categories[!reporting_categories %in% c(joint_category_, arena.chainSummary$stratumAttribute, baseunit_key_, cluster_key_)]
  if (length( reporting_categories) == 0) reporting_categories = ""
  
  
  if ( all( reporting_categories != "")) {
    # Estimation of area proportions by reporting categories
    i_count   <- 0
    
    # loop across strata
    for (i_Domain in (1: length( DomainClasses))) {
      
      if (arena.chainSummary$stratumAttribute != "") twophase_data_subset <- twophase_data_subset %>% filter(DOMAIN == DomainClasses[i_Domain])
      
      twophase_data_subset2 <- twophase_data_subset[ twophase_data_subset$DOMAIN == DomainClasses[i_Domain], ]
      
      # get subset (i.e. Domain data)
      Domain_label        <- df_Domain    %>% filter(Domain_code == DomainClasses[i_Domain]) %>% select(label) %>% pull()
      
      # loop across (common) land categories in Domain [i_Domain]  
      for (i in (1: length( jointCategoryClasses ))) {
        
        reportingCategoryClasses  <- twophase_data_subset %>%
          select( all_of( reporting_categories )) 
        
        jointCategory_label <- joint_category_Table %>% filter(code == jointCategoryClasses[i]) %>% select(label) %>% pull()
        
        land_use_area       <- AREA_Results$Total_area_ha[ AREA_Results$Domain_code == DomainClasses[i_Domain] & AREA_Results$Report_code == jointCategoryClasses[i] ]
        land_use_cover      <- AREA_Results$Cover_percent[ AREA_Results$Domain_code == DomainClasses[i_Domain] & AREA_Results$Report_code == jointCategoryClasses[i] ]
        
        # loop across dimensions in land category i
        for (j in (1: length( reporting_categories ))) {
          
          Out_message <- paste0(jointCategory_label, " -- ", reporting_categories[j]) 
          if (arena.chainSummary$stratumAttribute != "") Out_message <- paste0( Domain_label, " -- ", Out_message) 
          cat( paste0("Computing dimension:  ", Out_message, "\n"))
          
          
          unique_values_by_col <- sort( unique( reportingCategoryClasses[[j]], na.last = T))
          
          # loop over classes in dimension j
          for (k in (1:length( unique_values_by_col ))) {
            
            i_count            <- i_count + 1 
            
            attribute_label    <- twophase_data_subset %>% filter( eval( parse( text = reporting_categories[j])) == unique_values_by_col[k] ) %>%
              select(any_of(reporting_categories_labels[j])) %>% unique() %>% pull()
            
            col_ph1                         <- paste("joint_ph1",   jointCategoryClasses[i], sep = "_") 
            joint_ph2                       <- paste("joint_ph2",   jointCategoryClasses[i], sep = "_")
            col_ph2                         <- paste("report_ph2_", reporting_categories[j], "__", unique_values_by_col[k], sep = "")
            twophase_data_subset$col_phase2 <- twophase_data_subset[[ col_ph2 ]] * twophase_data_subset[[ joint_ph2 ]]   
            
            analysisExpression <- as.formula( paste( 'col_phase2 ~', col_ph1))  
            
            n_count                      <- sum( twophase_data_subset$col_phase2 )
            b_small_area_estimation      <- FALSE
            twophase_data_subset$issmall <- NULL
            
            # global estimator
            if (!b_small_area_estimation) two_phase_area <- forestinventory::twophase( formula = analysisExpression,
                                                                                       data = twophase_data_subset, 
                                                                                       list(phase.col = "phase", terrgrid.id = 2), cluster = cluster_attribute)
            
            # Summary of results
            sum_Area <- two_phase_area$estimation %>%
              mutate( Cover_percent     = ((estimate * 100) * 100) / land_use_cover)                       %>%  
              mutate( Cover_SE          = suppressWarnings( sqrt((estimate * (1 - estimate)/n1)) * 100))   %>%
              mutate( Total_area_ha     = (Cover_percent/100) * land_use_area)                             %>% 
              mutate( Area_SE           = suppressWarnings( sqrt(g_variance * land_use_area^2)))           %>% 
              mutate( Domain_code      = DomainClasses[i_Domain])                                       %>%
              mutate( Report_code       = jointCategoryClasses[i])                                         %>%
              mutate( Dimension_code    = unique_values_by_col[k])                                         %>%
              mutate( Domain_label            = Domain_label,
                      Report_label             = jointCategory_label, 
                      Dimension_label          = attribute_label,    
                      Domain_attribute_name   = arena.chainSummary$stratumAttribute, 
                      Report_attribute_name    = joint_category_, 
                      Dimension_attribute_name = reporting_categories[j]) 
            
            
            sum_Area          = sum_Area[ , c(17:19, 11:16, 7:10)]
            sum_Area$n2_count = n_count
            
            if (i_count == 1) {
              result_Area <- sum_Area 
            } else {
              result_Area <- bind_rows( result_Area, sum_Area)
            }
            
            rm(col_ph1); rm(col_ph2); rm(analysisExpression)
          }
        }
      }
    }
    
    row.names(result_Area) <- NULL
    result_Area            <- result_Area %>% arrange(Domain_attribute_name, Dimension_attribute_name)
    
    out_file_name <- paste0(user_file_path, out_path, "Arena_2phase_resuls_by_dimensions.csv")
    tryCatch({if (exists('user_file_path') & exists("result_Area"))  write.csv(result_Area, out_file_name,  row.names = F)},
             warning = function( w ) { cat("No output - Results by dimensions") },
             error   = function( e ) { cat("No output - Results by dimensions")
             })
    
    
  } # if (all( reporting_categories != ""))
  
}