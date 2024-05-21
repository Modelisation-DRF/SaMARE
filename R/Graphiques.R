#' Fonction qui prépare les graphiques à insérer dans l'interface utilisateur
#' cette function prend en parametre une sortie de la fonction SimulSaMARE

#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des
#'                    iterations du simulateur SaMARE. Typiquement un résultat retourné
#'                    par la fonction "SimulSaMARE".
#'@param Espece Un code de trois lettre majuscule présentant l'espèce pour laquelle on veut faire
#'              les graphiques. Le code d'espèce doit être celui d'un des groupes d'espèce de SaMARE
#'              ou le code "TOT" pour l'ensemble des espèces.
#'@Variable La variable pour laquelle le graphique d'évolution sera créé. Cette variable doit être une des
#'          variable de la sortie "Dendro-SaMARE" soit "Vol_HA", "ST_HA", "DQM","HDomM" ou "nbTi_HA"
#' @return  Retourne une liste de deux graphiques, le premier montrant l'évolution d'une variables
#'          dendrométrique et le second la distribution en diamètre au début et à la fin de la simulation
#'          moyen quadratique et la hauteur dominante.
#' @examples
#'


Graph<-function (SimulHtVol, Espece="TOT", Variable='ST_HA'){

Data<-SortieDendroSamare(SimulHtVol) %>%
      filter(GrEspece==Espece) %>%
      mutate(Yvar=NA)

Data <- Data %>% #Permet de retirer le Residuel=0 dans le cas où on a également un résiduel=1 pour la même année
  group_by(Placette,Annee) %>%
  slice_tail() %>%
  ungroup()


if (Variable=='Vol_HA'){
   Data$Yvar<-Data$Vol_HA
   Etiquette="Volume marchand (m3/ha)"
}

if (Variable=='ST_HA'){
  Data$Yvar<-Data$ST_HA
  Etiquette="Surface terrière marchande (m2/ha)"
}

if (Variable=='DQM'){
  Data$Yvar<-Data$DQM
  Etiquette="Diamètre quadratique moyen (cm)"
}

if (Variable=='HDomM'){
  Data$Yvar<-Data$HDomM
  Etiquette="Hauteur dominante (m)"
}

if (Variable=='nbTi_HA'){
  Data$Yvar<-Data$nbTi_HA
  Etiquette="Densité (nb/ha)"
}

if (Espece=="TOT"){Essence="Toutes essences"}
if (Espece=="BOJ"){Essence="Bouleau jaune"}
if (Espece=="ERR"){Essence="Érable rouge"}
if (Espece=="ERS"){Essence="Érable à sucre"}
if (Espece=="FEN"){Essence="Feuillus nobles"}
if (Espece=="FIN"){Essence="Feuillus intollérants"}
if (Espece=="EPX"){Essence="Épinettes"}
if (Espece=="SAB"){Essence="Sapin baumier"}
if (Espece=="RES"){Essence="Résineux"}
if (Espece=="HEG"){Essence="Hêtre à grandes feuilles"}
if (Espece=="AUT"){Essence="Autres essences"}

ymax<-max(Data$Yvar)

AnneeDep=min(Data$Annee)
AnneeFin=max(Data$Annee)

dernieres_valeurs <- Data %>%
                    group_by(Placette) %>%
                    slice(n()) %>%
                     ungroup()

GraphEvol<-Data%>%
             ggplot(aes(x=Annee,y=Yvar,group=Placette, label = Placette))+
             geom_line(aes(),show.legend=FALSE, lwd=1.25, colour="#1B9E77")+
             ylim(0,ymax+5)+
             xlab(bquote(bold("Année de la simulation")))+ ylab(paste(Etiquette))+
             scale_x_continuous(breaks = seq(AnneeDep, AnneeFin, by = 5))+
             theme_bw() +
             ggtitle(paste(Etiquette,"  ",Essence))+
             theme(
      strip.background = element_rect(fill = "white"),
      axis.title=element_text(size=14,face="bold"),
      axis.text.x = element_text(angle = 45,  hjust=1),
      strip.text.x = element_text(size = 12,face="bold"),
      plot.title = element_text(hjust = 0.5,size=14,face="bold"))+
     geom_text(data=dernieres_valeurs,aes(label = Placette), hjust = 1, vjust=-0.2,size=3)
  GraphEvol

NbPlac<-length(unique(SimulHtVol$Placette))

Sommaire<-Sommaire_Classes_DHP(SimulHtVol) %>%
          filter(Annee %in% c(AnneeDep,AnneeFin)) %>%
          filter(GrEspece==Espece) %>%
          mutate(DHP_cl=round(DHP_cl/5)*5) %>%
          group_by(Annee, DHP_cl) %>%
          summarise(NbHa=(sum(NbHa)/NbPlac),.groups="drop")


MaxDHP<-max(Sommaire$DHP_cl)

ClassesDHP<-data.frame("Annee"=c(rep(AnneeDep,13), rep(AnneeFin,13)),"DHP_cl"=rep(seq(10,70,by=5),2))

GraphDist<-Sommaire %>%
           full_join(ClassesDHP, relationship="many-to-many",by =join_by(Annee, DHP_cl)) %>%
           mutate(NbHa=ifelse(is.na(NbHa)==TRUE,0,NbHa),Annee=as.factor(Annee)) %>%
           ggplot(aes(x=DHP_cl, y=NbHa, fill=Annee))+
            geom_bar(position=position_dodge(preserve="single"), stat="identity", color="black", width=3)+
           ggtitle(paste("Distribution diamétrale","  ",Essence))+
          xlab(bquote(bold("Classe de DHP (cm)")))+ ylab("Nombre de tiges par hectare")+
          scale_x_continuous(breaks = seq(10, 70, by = 5))+
          scale_fill_manual(values=c("#D95F02" ,"#1B9E77"))+
          theme(strip.background = element_rect(fill = "white"),
          plot.title=element_text(size=12,face="bold",hjust=0.5),
          axis.title=element_text(size=12,face="bold"),
          axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10),
          strip.text = element_text(size = 10,face="bold"),
          legend.text= element_text(size = 12),
          legend.title= element_text(size = 12))+
          labs(fill="Année")+
          theme(legend.position="top")
GraphDist

Graphiques<-list(GraphEvol,GraphDist)

return(Graphiques)


}
