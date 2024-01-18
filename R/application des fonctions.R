



























# Para.ConvMSCRVig<-read_delim("P:/F1272/CPF/SaMARE2018/Stagiaire informatique/Conversion_MSCR_1234/0_MatchModuleParametersUpdatedConvMSCR.csv",delim=";") %>% 
# filter(SubModuleID==17)  #parametres pour vigueur
# Para.ConvMSCRProd1024<-read_delim("P:/F1272/CPF/SaMARE2018/Stagiaire informatique/Conversion_MSCR_1234/0_MatchModuleParametersUpdatedConvMSCR.csv",delim=";") %>% 
#   filter(SubModuleID==18)  #parametres pour produits<24 cm
# Para.ConvMSCRProd24<-read_delim("P:/F1272/CPF/SaMARE2018/Stagiaire informatique/Conversion_MSCR_1234/0_MatchModuleParametersUpdatedConvMSCR.csv",delim=";") %>% 
#   filter(SubModuleID==19)  #parametre pour produits>24cm
# 
# predVig0<-ConvMSCRVig(Plac,Para.ConvMSCRVig)#fonction pour attribuer la vigueur numero 1 dans le doc Word
# 
# Plac<-Plac %>% 
#       mutate(vig0=predVig0,Alea=runif(n()),
#              vig0=ifelse(vig0>=Alea,"vig","NONVIG")) %>% 
#           select(-Alea)
# 
# predprod0<-ConvMSCRProd1024(Plac,Para.ConvMSCRProd1024) #fonction pour produits arbres < = 23.0  cm numeo 2 dans le doc Word 
# 
# Plac<-Plac %>% 
#   mutate(prod0=predprod0,Alea=runif(n()),
#          prod0=ifelse(GrEss %in% c("EPX","RES","SAB"),"resineux",
#                      ifelse(vig0>=Alea,"sciage","pate"))) %>% 
#   select(-Alea)
# 
# predprod0<-ConvMSCRProd24(Plac,Para.ConvMSCRProd24)#fonction pour produits arbres >23.0 cm numeo 3 dans le doc Word 
# 
# Plac<-Plac %>% 
#   mutate(prod0=predprod0,Alea=runif(n()),
#          prod0=ifelse(GrEss %in% c("EPX","RES","SAB"),"resineux",
#                      ifelse(prod0>=Alea,"sciage","pate"))) %>% 
#   select(-Alea)