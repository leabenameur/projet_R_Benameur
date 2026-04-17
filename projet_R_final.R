library(shiny)
library(demo)

ui <- fluidPage(
  titlePanel("Slitherlink"),
  
  selectInput("niveau", "Niveau",
              choices = c("Débutant", "Moyen", "Expert"),
              selected = "Moyen"),
  
  actionButton("new","Nouvelle grille"),
  actionButton("reset","Reset"),
  actionButton("solution","Solution"),
  actionButton("check","Vérifier"),
  
  h3(textOutput("timer")),
  plotOutput("grid", click="plot_click"),
  h3(textOutput("result"))
)


server <- function(input, output, session){
  
  grid <- reactiveVal(NULL)
  solution <- reactiveVal(NULL)
  show_solution <- reactiveVal(FALSE)
  start_time <- reactiveVal(Sys.time())
  
  new_game <- function(){
    n <- switch(input$niveau,
                "Débutant" = 4,
                "Moyen" = 6,
                "Expert" = 8)
    
    grille <- generate_slitherlink(n, n)
    
    grid(list(
      n = n,
      m = n,
      horizontal = matrix(0, n+1, n),
      vertical = matrix(0, n, n+1),
      clues = grille$grid
    ))
    
    solution(grille)
    show_solution(FALSE)
    start_time(Sys.time()) 
  }
  
  observe({ new_game() }) 
  observeEvent(input$new, new_game)
  
  observeEvent(input$solution, {
    show_solution(!show_solution())
  })
  
  observeEvent(input$reset,{
    g <- grid()
    g$horizontal[,] <- 0
    g$vertical[,] <- 0
    grid(g)
    start_time(Sys.time())
  })
  
  
  output$timer <- renderText({
    invalidateLater(1000, session)
    elapsed <- as.numeric(difftime(Sys.time(), start_time(), units="secs")) 
    sprintf("Temps : %02d:%02d", floor(elapsed/60), floor(elapsed %% 60))
  })
  
  output$grid <- renderPlot({
    g <- grid()
    sol <- solution()
    
    plot(0,0,type="n",xlim=c(0,g$m),ylim=c(0,g$n),
         axes=FALSE,xlab="",ylab="")
    

    for(i in 0:g$n)
      for(j in 0:g$m)
        points(j, g$n-i, pch=16)
    
    
    #on affiche la solution 
    if(show_solution() && !is.null(sol)){
      for(i in 1:(g$n+1))
        for(j in 1:g$m)
          if(sol$h[i,j]==1)
            segments(j-1, g$n-(i-1), j, g$n-(i-1), col="green", lwd=3)
      
      for(i in 1:g$n)
        for(j in 1:(g$m+1))
          if(sol$v[i,j]==1)
            segments(j-1, g$n-(i-1), j-1, g$n-i, col="green", lwd=3)
    }
    
    # affiche les traits
    for(i in 1:(g$n+1))
      for(j in 1:g$m)
        if(g$horizontal[i,j]==1)
          segments(j-1, g$n-(i-1), j, g$n-(i-1), lwd=3)
    
    for(i in 1:g$n)
      for(j in 1:(g$m+1))
        if(g$vertical[i,j]==1)
          segments(j-1, g$n-(i-1), j-1, g$n-i, lwd=3)
    
    # affiche les chiffres dans les cases de la grille
    for(i in 1:g$n)
      for(j in 1:g$m)
        if(!is.na(g$clues[i,j]))
          text(j-0.5, g$n-(i-0.5), g$clues[i,j],
               col="blue", cex=1.5)
  })
  
  observeEvent(input$plot_click,{
    click <- input$plot_click
    g <- grid()
    
    x <- click$x
    y <- g$n - click$y #inversé parce que la grille est inversée
    
    dx <- abs(x - round(x))
    dy <- abs(y - round(y))
    
    if(dx < dy && dx < 0.15){
      g <- ajout_arete(g, "v", floor(y)+1, round(x)+1) 
    } else if(dy < dx && dy < 0.15){
      g <- ajout_arete(g, "h", round(y)+1, floor(x)+1)
    }
    
    grid(g)
  })
  
  observeEvent(input$check,{
    g <- grid()
    if(!verif_arete(g) || !check_noeuds(g)){
      output$result <- renderText("Pas bon, réessaie !")
    } else {
      output$result <- renderText("Bravo, c'est gagné !!!")
    }
  })
}

shinyApp(ui, server)
