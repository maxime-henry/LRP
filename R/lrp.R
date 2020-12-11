#' Représentation graphique de la relevance
#' Représentation d'une couche caché
#' Les fléches correspondent à la distribution de 50% de la relevance
#' @param relevance une matrice contenant les valeurs de relevance des neurones
#'
#' @import ggplot2
#' @import tidyr
#' @import forcats
#' @import ggforce
#' @import dplyr
#' @return un graphique de couche cachées
#' @export



lrp<-function(relevance){
  relevance %>% ggplot(aes(y=as.numeric(names))) + geom_point(aes(x=V1-V1,color=-V1,size=6),stat='identity',alpha=0.7,show.legend = FALSE)+
  theme_minimal()+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y = element_blank())+
    guides(fill=FALSE)+scale_y_continuous(trans = "reverse")+
    geom_point(aes(x=0.8,y=(nrow(relevance)/2)),size=6)+xlim(-1,1)+
    annotate(geom='text',x=0.85,y=(nrow(relevance)/2)+1,label=paste('Relevance = ',round(sum(relevance$V1),1)))->legraph

  relevance %>% slice_max(V1,n=nrow(relevance))->certainesfleches
somme=j=0
while(somme < sum(relevance$V1)/2){
  j=j+1
  somme=somme+certainesfleches[j,2]
}
relevance %>% slice_max(V1,n=j)->certainesfleches

for(i in 1:nrow(relevance)){

  if(i %in% certainesfleches$names == TRUE){
    legraph<-legraph+geom_segment(aes_string(x = 0.7, y =(nrow(relevance)/2) , yend = i, xend = 0.05), arrow = arrow(length = unit(0.3, "cm")))
  }

}
legraph<-legraph+labs(title="Distribution de 50% de la relevance à travers une couche cachée"
                      )

return(legraph)
}
