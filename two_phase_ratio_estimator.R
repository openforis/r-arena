#### Post stratification results with statistical estimators of variance
# Source: Radim Adolt, Forest specialist consultant for FAO
# Edited by: Anibal Cuchietti and Lauri Vesa, FAO
# Last edited: 13 March, 2024
#
# To properly estimate the proportion of an attribute together with another post stratification variable 
# (such as ownership, stand origin, etc.) an extra estimate is defined as the ratio of the corresponding two-phase estimated areas.
# 

fn_two_phase_ratio_estimator <- function( nominator, denominator ) {
  t1  <- (nominator$estimation)$estimate
  t2  <- (denominator$estimation)$estimate
  R12 <- t1/t2
  
  u <- nominator$Rc_x_hat - denominator$mean_Rc_x_hat*R12
  nominator_variable <- 
    substring(toString(nominator$input$formula),
    gregexpr(" "  , toString(nominator$input$formula))[[1]][1]+1,
    gregexpr(", " , toString(nominator$input$formula))[[1]][2]-1) 
  
  nominator_variable_index <- 
    match( nominator_variable, names(nominator$input$data)) 
  
  denominator_variable <- 
    substring( toString( denominator$input$formula),
              gregexpr(" "  , toString(denominator$input$formula))[[1]][1]+1,
              gregexpr(", " , toString(denominator$input$formula))[[1]][2]-1) 
  
  denominator_variable_index <- 
    match(denominator_variable, names(denominator$input$data)) 
  
  nominator2phase_data <- 
    nominator$input$data[ nominator$input$data$phase==2,]
  
  denominator2phase_data <- 
    denominator$input$data[ denominator$input$data$phase==2,]
  
  y1      <- nominator2phase_data[, nominator_variable_index]
  y2      <- denominator2phase_data[, denominator_variable_index]
  cluster <- nominator2phase_data$level_1_code
  
  sum_y1_per_cluster <- aggregate(y1, by=list(cluster=cluster), sum)[,2]
  sum_y2_per_cluster <- aggregate(y2, by=list(cluster=cluster), sum)[,2]
  nplots_per_cluster <- aggregate(y2, by=list(cluster=cluster), length)[,2]
  
  y1c <- sum_y1_per_cluster / nplots_per_cluster
  y2c <- sum_y2_per_cluster / nplots_per_cluster
  
  n1 <- (nominator$estimation)$n1
  n2 <- (nominator$estimation)$n2
  
  v1 <- 1 / t2^2 * (1 - n2/n1) / n2 / (n2-1) / mean(nplots_per_cluster)^2 * 
          sum(nplots_per_cluster^2 * (u - sum(u*nplots_per_cluster) / 
          sum(nplots_per_cluster))^2)
    
  v2 <- 1 / t2^2 / n1 / (n2 - 1) / mean(nplots_per_cluster)^2 *
          sum(nplots_per_cluster^2 * (y1c - R12*y2c)^2)
  
  return (list(R12 = R12, var = v1 + v2, stderr = sqrt(v1 + v2)))
}