# ----------------------------
# Demo OPUF survey analysis
# Paul Schneider
# University of Sheffield
# 11 December 2021
# ----------------------------
#
# This R script gives a short demo of how 
# personal utility functions are constructed 
# from OPUF survey responses.
#


# load the demo survey data
dat = readRDS("./demo_data.RDS")

# load descriptive system functions
source("./R/ds.R")                # general function to create descriptive system (ds) 
source("./R/ds_placeholder.R")    # placeholder ds with generic labels

ds = placeholderGen()
ds_expanded = dsExpand(ds)


# SURVEY RESPONSE DATA DETAILS ------
  # the loaded data contains the user's responses to each task
  names(dat)
  
  # 1. own health state (unordered)
  unlist(dat$rateOwn_res)
  
  # 2. VAS
  dat$rateOwn_vas
  
  # 3. level rating 
  names(dat$lvlRate) <- c("no","slight","moderate","severe","extreme")
  100 - dat$lvlRate
  
  # 4. dimension ranking (from 'best' to worst)
  dat$dimRank
  
  # 5. dimension swing weigthing (unorderd)
  dat$swingRate
  
  # 6. validation DCE responses
    # choice scenarios and choices
    dat$anyDCE
    # difficulty of the choice sets
    dat$dce_difficulties
    # where on the utility scale does the choice set lie?
    dat$dce_quants
    
  # 7. DeadDce (position of dead task)
    # choice trail
    dat$deadDCE_trail
    # the health state that was identified as being approx. equal to being dead:
    dat$hs_eq_dead
    
  # 8. deadVAS (if applicable)
  dat$dead_vas_hs
    
  # 9. demographic survey responses (unordered)
  dat$survey_res
  
  # 10. time stamps
  dat$tab_times
  
    # check duration per task
    tab_times <- unlist(dat$tab_times)
    tab_times <- tab_times[order(tab_times)]
    tab_names <- names(tab_times)
    tab_names <- gsub('[0-9]+[_]', "", tab_names)
    tab_names <- unique(tab_names)
    tab_times <- diff(tab_times) # NB: if user revisits a page, times need to be aggregated
    names(tab_times) = tab_names[-length(tab_names)]
    round(tab_times) # in seconds (as you can see, this was a very quick run through)
    round(sum(tab_times) / 60,1) # in minutes
  
# CONSTRUCTING A PUF ----
    dim_order <- c("A","B","C","D","E")
    lvls_labels <- c("no","slight","moderate","severe","extreme")
    
    d_weights <- unlist(dat$swingRate) / 100
    names(d_weights) <- gsub("swingRate_","",names(d_weights))
    d_weights <- d_weights[match(dim_order, names(d_weights))]
    
    l_scores = dat$lvlRate / 100
    l_scores = 1 - l_scores # recode as utilities rather than disutilities
    
    # 1-0 scaled coefficient matrixc
    scaled_mat = matrix(unlist(lapply(l_scores, function(x) x * d_weights)), ncol = 5, byrow = T)
    colnames(scaled_mat) <- dim_order
    rownames(scaled_mat) <- lvls_labels
    scaled_mat
    
    # assigning a 1-0 scaled value to 
    # all health state in the descriptive system
    ds_expanded_collapsed <- apply(ds_expanded, 1, paste0, collapse = "")
    scaled_vals = c(0)
    for (j in 1:ncol(ds_expanded)) {
      scaled_vals = scaled_vals + scaled_mat[ds_expanded[, j], j]
    }
    scaled_vals = scaled_vals / max(scaled_vals)
    names(scaled_vals) = ds_expanded_collapsed
    
    # the best health state (level 1 on all 5 dimensions) has a value of 1
    head(scaled_vals)
    # the worst health state (level 5 on all 5 dimensions) has a value of 0
    tail(scaled_vals)
    
    # Anchoring / rescaling to map values on to the utility scale with 
    # 1 = full health; 0 = dead; negative values = states worse than dead
    
    # ALTERNATIVE A: if anchoring via 'position of dead':
      dead_equal = paste0(dat$hs_eq_dead, collapse = "")
      val_hs_eq_dead = ds_expanded_collapsed == dead_equal 
      scale_d = scaled_vals[1] - scaled_vals[val_hs_eq_dead]
      values_d = scaled_vals - scaled_vals[val_hs_eq_dead]
      anchored_values = (values_d / scale_d)
      
      # now best health state has a value of 1
      head(anchored_values)
      # and the worst health state has a value of -0.32
      tail(anchored_values)
      
    # # ALTERNATIVE B: if anchoring via 'dead VAS task':
    #   val_pit = dat$dead_vas_hs 
    #   # val_pit = 1.5  # # example: user sets '55555' to a value of 15 of 100 
    #   pit_index = length(scaled_vals)
    #   scale_t = (1-(val_pit/10))/(1-scaled_vals[pit_index])
    #   values_d = (1 - scaled_vals)*scale_t
    #   anchored_values = 1-values_d  
  
      
      
# predict the value of a particulat health state
    pred_hs = "12345"
    hs_index = match(pred_hs,ds_expanded_collapsed)
    anchored_values[pred_hs]
  

# check user's consistency in DCE tasks
  # predict value of DCE choice sets
    hs_a <- apply(dat$anyDCE[,1:5], 1, paste0, collapse = "")
    hs_index_a = match(hs_a,ds_expanded_collapsed)
    pred_a = anchored_values[hs_index_a]
    
    hs_b <- apply(dat$anyDCE[,6:10], 1, paste0, collapse = "")
    hs_index_b = match(hs_b,ds_expanded_collapsed)
    pred_b = anchored_values[hs_index_b]
    
    # predicted choices
    pred_choices = c("A","B")[as.numeric(pred_a > pred_b)+1]
    pred_choices
    
    # actual choices
    dat$anyDCE[,11]
    
    
    
  # visualise PUF
    library(ggplot2)
    
    # limit the number of health states to 25
    sample_hs <- round(seq(1,length(anchored_values), length.out = 25))
    hs_ranks = rank(-anchored_values, ties.method = "random")
    indices = hs_ranks %in% sample_hs
    
    ggplot() +
      geom_hline(yintercept = 0, size = 0.5, color = "gray") +
      geom_point(aes(x = hs_ranks[indices], y= anchored_values[indices]),shape = 10, col = "cadetblue", fill ="red") +
      geom_line(aes(x = hs_ranks[indices], y= anchored_values[indices]), col = "cadetblue")  +
      scale_x_continuous(
        breaks = sample_hs, 
        labels = ds_expanded_collapsed[sample_hs], 
        minor_breaks = NULL, name = "Health state indices") +
      ylab("Personal utility values") +
      theme_minimal()+ 
      scale_color_viridis_b(begin = 1,end = 0) +
      coord_cartesian(ylim=c(-0.5,1)) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45))

    
  