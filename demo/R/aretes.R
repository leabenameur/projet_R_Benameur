#' ajout d'arêtes
#'
#' @param grid matrice de jeu
#' @param type arête horizontale ou verticale
#' @param i coordonnée abscisse
#' @param j coordonnée ordonnée
#'
#' @returns matrice des arêtes horizontales et matrice des arêtes verticales
#' @export
#'
#' @examples
#' ajout_arete(generate_slitherlink(6,6),"h",2,3)
#'


ajout_arete <- function(grid,type,i,j){
  if(type=="h") grid$horizontal[i,j]<-1-grid$horizontal[i,j]
  if(type=="v") grid$vertical[i,j]<-1-grid$vertical[i,j]
  grid
}



