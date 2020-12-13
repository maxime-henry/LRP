#' Représentation graphique de la relevance
#' Représentation d'une couche caché
#' Les fléches correspondent à la distribution de 50% de la relevance
#' @param relevance une matrice contenant les valeurs de relevance des neurones
#' @param resuh une matrice contenant la relevance pour chacune des variables
#'
#' @import ggplot2
#' @import tidyr
#' @import forcats
#' @import ggforce
#' @import dplyr
#' @import scales
#' @return un graphique de couche cachées
#' @export



lrp<-function(relevance,resuh){
  relevance %>% ggplot(aes(y=as.numeric(names))) + geom_point(aes(x=V1-V1,color=-V1,size=6),stat='identity',alpha=0.7,show.legend = FALSE)+
  theme_minimal()+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.x=element_blank(),axis.title.y = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.ticks.y =element_blank())+
    guides(fill=FALSE)+scale_y_continuous(trans = "reverse")+
    geom_point(aes(x=0.8,y=(nrow(relevance)/2)),size=6)+xlim(-1,1)+
    annotate(geom='text',x=0.85,y=(nrow(relevance)/2)+2.5,label=paste('Relevance = ',round(sum(relevance$V1),1)))->legraph

relevance %>% mutate(alpha=rescale(V1))->certainesfleches

for(i in 1:nrow(relevance)){
  legraph<-legraph+annotate(geom='text',x=0,y=i,label=i,size=3,color='white')
  if(i %in% certainesfleches$names == TRUE){
    legraph<-legraph+annotate(geom='segment',x = 0.7, y =(nrow(relevance)/2) , yend = i, xend = 0.05,alpha=certainesfleches[i,3],size=1, arrow = arrow(length = unit(0.3, "cm")))
  }
}
legraph<-legraph+labs(title="Distribution de la relevance a travers une couche cachee",subtitle = 'Representation des variables les plus importantes par neurones')



alph<-matrix(ncol=1,nrow=nrow(resuh1))

for(i in 1:nrow(resuh1)){
as.data.frame(resuh) %>% mutate(var=rownames(resuh)) %>% slice_max(order_by = resuh[,i],n=1) ->variablelocale
alph[i,1]=variablelocale[1,i]
}

alph[,1]<-rescale(alph[,1])

for(i in 1:nrow(resuh1)){
  as.data.frame(resuh) %>% mutate(var=rownames(resuh)) %>% slice_max(order_by = resuh[,i],n=1) ->variablelocale
legraph<-legraph+annotate(geom='segment',x = -0.1, y =i , yend = i, xend = -0.5,alpha=alph[i,1],size=1, arrow = arrow(length = unit(0.3, "cm")))
legraph<-legraph+annotate(geom='text',x=-0.8,y=i,label=variablelocale[1,'var'],alpha=alph[i,1])
}
return(legraph)
}



