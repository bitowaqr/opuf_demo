
placeholderGen <- function(){
  A <- dimGen(
    name = "DIMENSION A", name2 = "DIMENSION A", dim_id = "A",col = "#26384E",
    levels = c(
      "I have no problems with DIMENSION A",
      "I have slight problems with DIMENSION A",
      "I have moderate problems with DIMENSION A",
      "I have severe problems with DIMENSION A",
      "I have extreme problems with DIMENSION A"
    )
  )
  
  B <- dimGen(name = "DIMENSION B",name2="DIMENSION B",dim_id="B",col = "#1AB471",
               levels = c(
                 "I have no problems with DIMENSION B",
                 "I have slight problems with DIMENSION B",
                 "I have moderate problems with DIMENSION B",
                 "I have severe problems with DIMENSION B",
                 "I have extreme problems with DIMENSION B"
                 ))
  
  C <- dimGen(name = "DIMENSION C",name2 = "DIMENSION C",dim_id = "C",col = "#00AEEF",
               levels = c(
                 "I have no problems with DIMENSION C",
                 "I have slight problems with DIMENSION C",
                 "I have moderate problems with DIMENSION C",
                 "I have severe problems with DIMENSION C",
                 "I have extreme problems with DIMENSION C"
                 ))
  
  
  D <- dimGen(name = "DIMENSION D",name2="DIMENSION D",dim_id = "D",col = "#F15A22",
               levels =  c(
                 "I have no problems with DIMENSION D",
                 "I have slight problems with DIMENSION D",
                 "I have moderate problems with DIMENSION D",
                 "I have severe problems with DIMENSION D",
                 "I have extreme problems with DIMENSION D"
                 ))
  
  
  E <- dimGen(name = "DIMENSION E",name2 = "DIMENSION E",dim_id = "E",col = "#FFCB05",
               levels = c(
                 "I have no problems with DIMENSION E",
                 "I have slight problems with DIMENSION E",
                 "I have moderate problems with DIMENSION E",
                 "I have severe problems with DIMENSION E",
                 "I have extreme problems with DIMENSION E"
                          ),
               lvlsRate_args = c("No health problems",
                                 "Slight health problems",
                                 "Moderate health problems",
                                 "Severe health problems",
                                 "Extreme health problems"
                                 )
               )
  
  placeholder_ds <- dsGen(list(A,B,C,D,E)) 
  
  return(placeholder_ds)
}


# placeholderGen()







