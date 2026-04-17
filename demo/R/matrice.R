#' grille de jeu
#'
#' @param n nombre de lignes
#' @param m nombre de colonnes
#' @param max_attempts nombre max de boucles parcourues par la fonction pour essayer de créer une matrice correcte
#'
#' @returns liste de la grille de jeu, la matrice du nombre d'arêtes horizontales et la matrice du nombre d'arêtes verticales
#' @export
#'
#' @examples
#' generate_slitherlink(6,6)
#' generate_slitherlink(4,4)

generate_slitherlink <- function(n, m, max_attempts = 1000) {


  neighbors <- function(x, y) { #on liste les coordonnées des voisins d'un point de la grille
    list(
      c(x - 1, y),
      c(x + 1, y),
      c(x, y - 1),
      c(x, y + 1)
    )
  }


  is_valid <- function(x, y) { # on vérifie si un point existe (coordonnées entre 1 et n)
    x >= 1 && x <= n + 1 && y >= 1 && y <= m + 1
  }

  for (k in 1:max_attempts) {


    horizontal <- matrix(0, n + 1, m)     #matrice des arêtes horizontales
    vertical <- matrix(0, n, m + 1)     # verticales
    degre  <- matrix(0, n + 1, m + 1) # matrice du nombre d’arêtes touchant chaque sommet


    start <- c(sample(1:(n + 1), 1), sample(1:(m + 1), 1)) # point de départ pris aléatoirement
    point_actuel <- start

    path <- list(start)

    repeat {

      # voisins possibles du point actuel
      voisins_poss <- neighbors(point_actuel[1], point_actuel[2])

      # filtrer les voisins non existant ou déjà trop utilisés
      voisins_poss <- Filter(function(p) {
        is_valid(p[1], p[2]) && degre[p[1], p[2]] < 2
      }, voisins_poss)



      if (length(voisins_poss) == 0) break #on s'arrête s'il n'y a plus de voisins selectionnables


      prochain_pt <- voisins_poss[[sample(length(voisins_poss), 1)]] # on choisit un voisin parmi ceux possibles

      x1 <- point_actuel[1]
      y1 <- point_actuel[2]
      x2 <- prochain_pt[1]
      y2 <- prochain_pt[2]


      if (x1 == x2) {
        horizontal[x1, min(y1, y2)] <- 1
      } else {
        vertical[min(x1, x2), y1] <- 1
      }


      # on update la matrice des degrés
      degre[x1, y1] <- degre[x1, y1] + 1
      degre[x2, y2] <- degre[x2, y2] + 1


      point_actuel <- prochain_pt
      path[[length(path) + 1]] <- point_actuel



      ### ON S'ARRETE SI LA BOUCLE EST FERMÉE
      if (length(path) > 4 &&  all(point_actuel == start)) break
    }



    # vérifier que chaque sommet utilisé a un degré égal à 2
    used <- degre > 0
    if (any(degre[used] != 2)) next


    # on fait la grille finale à afficher
    grid <- matrix(0, n, m)

    for (i in 1:n) {
      for (j in 1:m) {
        grid[i, j] <-
          horizontal[i, j] +
          horizontal[i + 1, j] +
          vertical[i, j] +
          vertical[i, j + 1]
      }
    }

    return(list(
      grid = grid,
      h = horizontal,
      v = vertical
    ))
  }

  stop("fail")
}


