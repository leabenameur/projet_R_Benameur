#' verif nooeuds
#'
#' @param g grille
#'
#' @returns verifie qu'il n'y a pas de noeuds en s'assurant que chaque coin recoit soit 0 arête soit 2 arêtes
#' @export
#'
#' @examples


check_noeuds <- function(g){
  for(i in 1:(g$n+1))for(j in 1:(g$m+1)){
    d<-0
    if(j<=g$m)d<-d+g$horizontal[i,j]
    if(j>1)d<-d+g$horizontal[i,j-1]
    if(i<=g$n)d<-d+g$vertical[i,j]
    if(i>1)d<-d+g$vertical[i-1,j]
    if(!(d%in%c(0,2))) return(FALSE)
  }
  TRUE
}
