
placeholderGen <- function(){
  MO <- dimGen(
    name = "DIMENSION A", name2 = "DIMENSION A", dim_id = "MO",col = "#26384E",
    levels = c(
      "I have no problems with DIMENSION A",
      "I have slight problems with DIMENSION A",
      "I have moderate problems with DIMENSION A",
      "I have severe problems with DIMENSION A",
      "I have extreme problems with DIMENSION A"
    )
  )
  
  SC <- dimGen(name = "DIMENSION B",name2="DIMENSION B",dim_id="SC",col = "#1AB471",
               levels = c(
                 "I have no problems with DIMENSION B",
                 "I have slight problems with DIMENSION B",
                 "I have moderate problems with DIMENSION B",
                 "I have severe problems with DIMENSION B",
                 "I have extreme problems with DIMENSION B"
                 ))
  
  UA <- dimGen(name = "DIMENSION C",name2 = "DIMENSION C",dim_id = "UA",col = "#00AEEF",
               levels = c(
                 "I have no problems with DIMENSION C",
                 "I have slight problems with DIMENSION C",
                 "I have moderate problems with DIMENSION C",
                 "I have severe problems with DIMENSION C",
                 "I have extreme problems with DIMENSION C"
                 ))
  
  
  PD <- dimGen(name = "DIMENSION D",name2="DIMENSION D",dim_id = "PD",col = "#F15A22",
               levels =  c(
                 "I have no problems with DIMENSION D",
                 "I have slight problems with DIMENSION D",
                 "I have moderate problems with DIMENSION D",
                 "I have severe problems with DIMENSION D",
                 "I have extreme problems with DIMENSION D"
                 ))
  
  
  AD <- dimGen(name = "DIMENSION E",name2 = "DIMENSION E",dim_id = "AD",col = "#FFCB05",
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
  
  placeholder_ds <- dsGen(list(MO,SC,UA, PD, AD)) 
  
  return(placeholder_ds)
}


# placeholderGen()







