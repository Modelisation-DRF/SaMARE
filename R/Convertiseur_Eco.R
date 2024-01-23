



ConvertisseurEco<- function (data){

  data$eco <- ifelse(data$Espece == "BOJ", 
                     ifelse(data$reg_eco %in% c("1a", "2a", "2b", "2c", "3a", "3b", "SV"), "3O",
                            ifelse(data$reg_eco %in% c("3c", "3d"), "3E",
                                   ifelse(data$reg_eco %in% c("4a", "4b"), "4O_b",
                                          ifelse(data$reg_eco %in% c("4c"), "4O_c", "4E")
                                   )
                            )
                     ),
                     ifelse(data$reg_eco %in% c("1a", "2a", "2b", "2c"), "2O",
                            ifelse(data$reg_eco %in% c("3a"), "3O_a",
                                   ifelse(data$reg_eco %in% c("3b", "3c", "SV"), "3O_b",
                                          ifelse(data$reg_eco %in% c("4a", "4b"), "4O_b",
                                                 ifelse(data$reg_eco %in% c("4c"), "4O_c", "4E")
                                          )
                                   )
                            )
                     )
  )

 return(data)
}







