
#' set les paramettres
#'
#' @param Data  data frame liste des donner de depart
#' @return
#' @examples Ess_groupe(arbres)

Ess_groupe<-function(Data){

            Data<-Data %>%
                 mutate (Essence=ifelse(Essence %in% c("CHG","CHR","CHB"),"CHX",
                           ifelse(Essence %in% c("EPB","EPN","EPR"),"EPX",
                                  ifelse(Essence %in% c("PEG","PET","PEB","PED"), "PEU",
                                         ifelse (Essence %in% c("PIR","PIB","PIS"),"PIN",Essence)))))
  suppressMessages(
            Data<-read_delim("Parametres/0_Species.csv", delim=";") %>%
                  rename(Essence=SpeciesName) %>%
                  inner_join(Data, by="Essence"))

  suppressMessages(
            Data<-read_delim("Parametres/0_Vegpot.csv", delim=";") %>%
                  rename(Veg_Pot=VegPotName) %>%
                  inner_join(Data))

  suppressMessages(
            Data<-read_delim("Parametres/0_MatchSpeciesGroups.csv", delim=";") %>%
                  select(-RelationHDSpeciesID,-VolumeSpeciesID,-HarvestSpeciesID) %>%
                  inner_join(Data))

  suppressMessages(
            Data<-read_delim("Parametres/0_SpeciesGroups.csv", delim=";")%>%
                  rename(GrEspece=SpeciesGroupName) %>%
                  inner_join(Data) %>%
                  select(-SpeciesGroupID,-VegPotID,-SpeciesID) %>%
                  select(PlacetteID,origTreeID,GrEspece,everything()))

            return(Data)

}
