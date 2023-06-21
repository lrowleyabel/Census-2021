collapse_age_groups<- function(start_groups, target_groups){
  
  # Create dataframe of taregt groups with group name, lower bound and upper bound
  start_lower_bounds<- lapply(start_groups, FUN = function(age_group){
    if (str_detect(age_group, "under|less|lower|younger")){
      return(0)
    } else {
      return(as.numeric(str_extract(age_group, "[0-9]{1,3}")))  
    }
  })%>%
    unlist()
  
  start_upper_bounds<- lapply(start_groups, FUN = function(age_group){
    if (str_detect(age_group, "over|more|higher|older")){
      return(150)
    } else {
      extracted_ages<- str_extract_all(age_group, "[0-9]{1,3}")%>%
        unlist()
      if (length(extracted_ages)>1){
        return(as.numeric(extracted_ages[[2]]))
      } else {
        return(as.numeric(extracted_ages[[1]]))
      }
    }
  })%>%
    unlist()
  
  start_df<- data.frame(
    start_name = start_groups,
    start_lower = start_lower_bounds,
    start_upper = start_upper_bounds
  )
  
  # Create dataframe of taregt groups with group name, lower bound and upper bound
  target_lower_bounds<- lapply(target_groups, FUN = function(age_group){
    if (str_detect(age_group, "under|less|lower|younger")){
      return(0)
    } else {
      return(as.numeric(str_extract(age_group, "[0-9]{1,3}")))  
    }
  })%>%
    unlist()
  
  target_upper_bounds<- lapply(target_groups, FUN = function(age_group){
    if (str_detect(age_group, "over|more|higher|older")){
      return(150)
    } else {
      extracted_ages<- str_extract_all(age_group, "[0-9]{1,3}")%>%
        unlist()
      if (length(extracted_ages)>1){
        return(as.numeric(extracted_ages[[2]]))
      } else {
        return(as.numeric(extracted_ages[[1]]))
      }
    }
  })%>%
    unlist()
  
  target_df<- data.frame(
    target_name = target_groups,
    target_lower = target_lower_bounds,
    target_upper = target_upper_bounds
  )
  
  # Loop through start groups
  matched_names<- lapply(1:nrow(start_df), FUN = function(i){
    
    # Loop through target groups
    for (j in 1:nrow(target_df)){
      
      # Get start and target bounds
      start_lower<- start_df$start_lower[i]
      start_upper<- start_df$start_upper[i]
      target_lower<- target_df$target_lower[j]
      target_upper<- target_df$target_upper[j]
      
      # If start lower bound is greater than or equal to current target lower bound
      # and start upper bound is less than or equal to current target upper bound
      # then return current target group name
      
      if (start_lower >= target_lower){
        if (start_upper <= target_upper){
          return(target_df$target_name[j])
        }
      }
      
    }
  })%>%
    unlist()
  
  # Error if groups not matched
  if (length(matched_names) != length(start_groups)){
    stop("Some age groups not matched")
  }
  
  # Return matched groups
  return(matched_names)
  
}

