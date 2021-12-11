

dimGen <- function(name, dim_id, levels, ... ){
  
  
  dimension <- list(dim_id = dim_id,
                    name = name,
                    levels = levels,
                    ... = ...                     # can add any other method arguments       
                    )
  
  class(dimension) <- append("dimension", class(dimension))
  
  return(dimension)
  
}





dsGen <- function(dimension_list){
  
  ds <- list()
  for(l in dimension_list){
    ds[[l$dim_id]] <- l
  }
  
  class(ds) <- append("ds", class(ds))
  
  return(ds)
}


dsExpand = function(ds) {
  
  temp = list()
  dim_ids = c()
  for(i in seq_along(ds)){
    dim_ids[i] = ds[[i]]$dim_id
    temp[[i]] = 1:length(ds[[i]]$levels)
  }
  
  temp = expand.grid(temp)
  colnames(temp) = dim_ids
  return(temp)
}

