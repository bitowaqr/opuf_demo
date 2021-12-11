

dimGen <- function(name, dim_id, levels, 
                         #type = "ordinal",
                         #rateOwn_args = NULL, dimRank_args = NULL,
                         #topSwing_args = NULL, swingRate_args = NULL, 
                         #lvlRate_args = NULL, lvlsRate_args=NULL,
                          ... ){
  
  
  dimension <- list(dim_id = dim_id,
                    name = name,
                    levels = levels,
                    # type = type,
                    #rateOwn_args = rateOwn_args,
                    #dimRank_args = dimRank_args,
                    #topSwing_args = topSwing_args,
                    #swingRate_args = swingRate_args,
                    #lvlRate_args = lvlRate_args,
                    #lvlsRate_args = lvlsRate_args,
                    ... = ...                     # can add any other method arguments       
                    )
  
  class(dimension) <- append("dimension", class(dimension))
  
  return(dimension)
  
}





dsGen <- function(dimension_list, expand_ds = F){
  
  if(1==2){stop("checks must be performed here.")}
  
  
  ds <- list()
  for(l in dimension_list){
    ds[[l$dim_id]] <- l
  }
  
  if(sum(duplicated(names(ds)))>0){stop("Dimension IDs must be unique")}
  
  if(expand_ds){print("expand ds")}
  
  class(ds) <- append("ds", class(ds))
  
  return(ds)
}


eq5d5l_expand = function() {
  temp = lapply(1:5, function(x) 1:5)
  temp = expand.grid(temp)
  colnames(temp) = c("MO", "SC", "UA", "PD", "AD")
  return(temp)
}

