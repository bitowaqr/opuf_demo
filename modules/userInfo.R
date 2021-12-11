# utility functions to generate 1-0 scaled and anchored
# utility functions, predict health state values, and retrive
# responses to save them as RDS


# generate 1-0 scaled PUF
scaledFit = function(input,ds){
        d_weights = unlist(lapply(names(ds), function(x) isolate(input[[paste0("swingRate_", x)]]))) / 100
        l_scores = c(isolate(100-input$lvlRate[5:1])) / 100
        mat = matrix(unlist(lapply(l_scores, function(x) x * d_weights)), ncol = 5, byrow = T)
        return(mat)
}

# gemerate anchored PUF
anchoredFit = function(ds_expanded,scaled_hs_values,hs_eq_dead, dead_vas_hs = NULL,dead_vas_value = NULL){

    if (sum(hs_eq_dead) > 5) {
        # print("USING DEAD DCE")
        val_hs_eq_dead = apply(ds_expanded, 1, function(x) {
            paste0(x, collapse = "") == paste0(hs_eq_dead, collapse = "")
        })
        scale_d = scaled_hs_values[1] - scaled_hs_values[val_hs_eq_dead]
        values_d = scaled_hs_values - scaled_hs_values[val_hs_eq_dead]
        anchored_values = (values_d / scale_d)

    } else {
        # print("USING TTO")
        val_hs_tto = apply(ds_expanded, 1, function(x) {
            paste0(x, collapse = "") == paste0(dead_vas_hs, collapse = "")
            })
        
        
        scale_t = (1-(dead_vas_value/10))/(1-scaled_hs_values[val_hs_tto])
        values_d = (1 - scaled_hs_values)*scale_t
        anchored_values = 1-values_d  
        
    }

  return(anchored_values)
}


# predicts puf health state utility for hs based on responses
    # hs must be a vector of strings (eg. c("11111","22222", ...))
pufPred = function(ds_expanded,scaled_hs_values,hs_eq_dead, dead_vas_hs = NULL,dead_vas_value = NULL, new.hs = c(1,2,3,4,5)){

    if (sum(hs_eq_dead) > 5) {
        # print("USING DEAD DCE")
        val_hs_eq_dead = apply(ds_expanded, 1, function(x) {
            paste0(x, collapse = "") == paste0(hs_eq_dead, collapse = "")
        })
        scale_d = scaled_hs_values[1] - scaled_hs_values[val_hs_eq_dead]
        values_d = scaled_hs_values - scaled_hs_values[val_hs_eq_dead]
        anchored_values = (values_d / scale_d)

    } else {
        # print("USING TTO")
        val_hs_tto = apply(ds_expanded, 1, function(x) {
            paste0(x, collapse = "") == paste0(dead_vas_hs, collapse = "")
            })
        
        scale_t = (1-(dead_vas_value/10))/(1-scaled_hs_values[val_hs_tto])
        values_d = (1 - scaled_hs_values)*scale_t
        anchored_values = 1-values_d  
    }
    
    ds_exp_collapsed = apply(ds_expanded, 1, function(x) paste0(x, collapse = ""))
    res = c()
    for(i in seq_along(new.hs)){
        index = which(ds_exp_collapsed == new.hs[i])
        res = c(res, anchored_values[index])
    }
    

  return(res)
}


scaledRank = function(ds_expanded,model,return_rank = T){
    
    val = c(0)
    for (i in 1:ncol(ds_expanded)) {
        val = val + model[ds_expanded[, i], i]
    }
    if(return_rank){
         hs_ranks = rank(val, ties.method = "random")
         return(hs_ranks) # return the rank of each hs in ds_expanded
    } else {
        val = 1 - val / max(val)
        return(val) # return the scaled/unanchored value of each hs
    }

}


grabVals = function(str,input){
        res_names = grep(str, names(input), value = T)
        res_list = lapply(res_names, function(x) {input[[x]]})
        names(res_list) = res_names
        return(res_list)
    }


# retrive responses from inputs and reactive objects, print results, and save on drive
collect_responses = function(input,ds,deadDCE_trail,puf,tab_times,file_prefix = NULL, save_to_drive = T){

    user_puf_res = list(
        
      # 2+3
      rateOwn_res = grabVals("rateOwn_", input), 
      rateOwn_vas = input$rateOwnVas, 
      # 4
      lvlRate = c(isolate(100-input$lvlRate[5:1])),
      # 5 
      dimRank = input$dimRank_ranking,
      # 6
      swingRate = grabVals("swingRate_", input),
      # 7
      anyDCE = puf[["anyDCE"]],
      dce_quants = puf[["dce_quants"]],
      dce_difficulties = puf[["dce_difficulties"]],
      # 8
      hs_eq_dead = puf[["pos_dead"]],
      deadDCE_trail = deadDCE_trail,
      # 9
      dead_vas_hs = puf[["dead_vas_hs"]],
      dead_vas_value = puf[["dead_vas_value"]],
      # 10
      survey_res = grabVals("survey_", input),
      # times
      tab_times = grabVals("",tab_times)
    )

    print(user_puf_res)  
    
    if(save_to_drive){
      # fileformat: opuf_yearmonthday_hourminute.RDS
      file_name = paste0("./output/opuf_",format(Sys.time(), format  = "%Y%b%d_%H%M"),".RDS")
      saveRDS(user_puf_res, file_name)
      print(paste0("OPUF survey responses in: ", file_name))
    }
    
}
