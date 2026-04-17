#' Verification du nombre d'arêtes
#'
#' @param g grille
#'
#' @returns si on a le bon nombre d'arrêtes en fonction du chiffre dans la case
#' @export
#'
#' @examples

verif_arete <- function(g){
  for(i in 1:g$n)
    for(j in 1:g$m){
      s <- g$horizontal[i,j] + g$horizontal[i+1,j] +
        g$vertical[i,j] + g$vertical[i,j+1]
      if(s != g$clues[i,j]) return(FALSE)
    }
  TRUE
}
