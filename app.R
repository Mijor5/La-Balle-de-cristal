#Application Shiny afin de prédire le nombre de coups total qui sera effectué
#par un quatuor.

library(shiny)


# Définition de l'objet UI pour l'application.
ui <- fluidPage(

    # Changement de la couleur de l'arrière-plan
    tags$style(type="text/css", "body { background-color: #99FF66; }"),

    # Titre de l'application.
    titlePanel("La Balle de cristal"),

    # Section pour entrer le nombre de coups effectué à chaque trou.
    sidebarLayout(
        sidebarPanel(
            numericInput("t1", "Nombre de coups au trou 1",
                         0, min = 0, max = 17, 1),
            numericInput("t2", "Nombre de coups au trou 2",
                         0, min = 0, max = 17, 1),
            numericInput("t3", "Nombre de coups au trou 3",
                         0, min = 0, max = 17, 1),
            numericInput("t4", "Nombre de coups au trou 4",
                         0, min = 0, max = 17, 1),
            numericInput("t5", "Nombre de coups au trou 5",
                         0, min = 0, max = 17, 1),
            numericInput("t6", "Nombre de coups au trou 6",
                         0, min = 0, max = 17, 1),
            numericInput("t7", "Nombre de coups au trou 7",
                         0, min = 0, max = 17, 1),
            numericInput("t8", "Nombre de coups au trou 8",
                         0, min = 0, max = 17, 1),
            numericInput("t9", "Nombre de coups au trou 9",
                         0, min = 0, max = 17, 1),
            numericInput("t10", "Nombre de coups au trou 10",
                         0, min = 0, max = 17, 1),
            numericInput("t11", "Nombre de coups au trou 11",
                         0, min = 0, max = 17, 1),
            numericInput("t12", "Nombre de coups au trou 12",
                         0, min = 0, max = 17, 1),
            numericInput("t13", "Nombre de coups au trou 13",
                         0, min = 0, max = 17, 1),
            numericInput("t14", "Nombre de coups au trou 14",
                         0, min = 0, max = 17, 1),
            numericInput("t15", "Nombre de coups au trou 15",
                         0, min = 0, max = 17, 1),
            numericInput("t16", "Nombre de coups au trou 16",
                         0, min = 0, max = 17, 1),
            numericInput("t17", "Nombre de coups au trou 17",
                         0, min = 0, max = 17, 1),
            numericInput("t18", "Nombre de coups au trou 18",
                         0, min = 0, max = 17, 1),
        ),

#Affichage du tableau principal.
        mainPanel(
           h2(textOutput(outputId = "prediction"))
        )
    )
)

# Section où la prédiction est calculé.
server <- function(input, output) {

#Création du vecteur contenant le nombre de coups effectué à chacun des coups.
    Sit <- reactive(c(input$t1, input$t2, input$t3, input$t4, input$t5,
                          input$t6, input$t7, input$t8, input$t9, input$t10,
                          input$t11, input$t12, input$t13, input$t14, input$t15,
                          input$t16, input$t17, input$t18))



#Vecteur contenant la normale de chaque trou.
    wit <- c(4, 4, 4, 3, 5, 4, 4, 3, 4, 5, 4, 5, 3, 4, 3, 4, 3, 5)

#Création du vecteur permettant de savoir quels trous ont été fait.
    trouf <- reactive(as.numeric(Sit() > 0))

#Calcul du par déjà joué.
    WiS <- reactive(sum(trouf() * wit))

#Calcul du ratio entre le nombre de coups total joué et le par joué.
    Xiw <- reactive(sum(Sit()) / WiS())

#Variance intra, il s'agit de la moyenne, pour l'ensemble des quatuors, des
#variances entre les nombres de coups et le par de chaque trou pour un même
#quatuor. La valeur a été calculée à partir d'une base de données de 2000 rondes
#complètes.
    s2 <- 0.25731588649544324

#Variance inter, il s'agit de la variance entre les nombres de coups des
#quatuors. La valeur a été calculée à partir d'une base de données de 2000
#rondes complètes.
    atilde <- 0.006102055545287843

#Prime collective, il s'agit du ratio collectif du nombre de coups moyen fait
#par un quatuor divisé par la normale totale qui est de 71.
    m <- 1.1721830985915493

#Calcul du facteur de crédibilité, il s'agit du pourcentage que l'on va
#attribuer à l'expérience du quatuor.
    z <- reactive(WiS()/(WiS() + s2/atilde))

#Prédiction du nombre de coups pour les trous non joués.
    pred_bstraub <- reactive((z() * Xiw() + (1-z()) * m) * (71 - WiS()))

#Ajout des coups déjà faits à la prédiction pour les trous non joués.
    pred <- reactive(pred_bstraub() + sum(Sit()))

#Création de l'objet pour afficher la prédiction sur le tableau de bord.
    output$prediction <- renderText(paste("Le nombre de coups prédit  pour le
                                          quatuor est de :",
                                          ifelse(max(Sit()) == 0, round(m*71),
                                                 round(pred()))))
    }

#Lancement de l'application
shinyApp(ui = ui, server = server)
