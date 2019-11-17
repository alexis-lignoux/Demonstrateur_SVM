library(shiny)
library(e1071)
library(plotly)
library(shinythemes)

options(Encoding="UTF-8")
donnees=readRDS("templatePDF/creditcard.rds")
shinyUI(navbarPage(
    theme=shinytheme("flatly"),
    shiny::tags$header("Démonstrateur SVM",
                       style="color: white; font-weight: bold;",
                       tags$link(rel = "stylesheet", type = "text/css", href = "apparence.css")),
    tabPanel(
        tags$header("Avant-propos",style="color: white"),
        tags$p("Nous allons vous présenter au moyen de ce démonstrateur une méthode de classification des données appelée Machine à Vecteurs de Support.",class="paragraphe"),
        tags$p("Vous pouvez télécharger la notice de cette introduction aux SVM au format PDF :",style="font-size: 15px; font-style: italic"),
        downloadButton("dlPDF","Télécharger",class="btn-primary"),
        hr(),
        tags$p("Tout d'abord, définissons la notion de problème de classification.",class="paragraphe"),
        tags$p("Il s'agit tout simplement d'arriver à attribuer une des catégories de la variable réponse à un individu à partir d'un ensemble de prédicteurs. Par exemple,
               il peut s'agir de prédire l'espèce d'une fleur d'après des caractèristiques telles que la longueur des pétales, la largeur... Il peut
               également s'agir de prédire si un individu va faire défaut ou non suite à un octroi de crédit en tenant compte de son âge, son salaire, 
               sa catégorie socio-professionnelle etc...",class="paragraphe"),
        br(),
        br(),
        tags$p("L'idée générale",style="font-weight: bold; font-size: 20px; font-style: italic; text-align: center; color: #2C3E50"),
        br(),
        tags$p("Prenons un exemple simple de problème de classification :",class="paragraphe"),
        tags$p("Nous possèdons deux prédicteurs X et Y pour déterminer si un individu appartient à la classe A ou la classe B. 
                En voici une représentation graphique :",class="paragraphe"),
        plotOutput("plotintro"),
        tags$p("Nous introduisons maintenant un nouvel individu et nous voulons savoir à quelle classe il appartient.",class="paragraphe"),
        plotOutput("plotintro2"),
        tags$p("Une règle de décision simple serait de tracer une droite qui permet de séparer ces deux nuages de points. Nous attribuons à chaque individu la modalité 
               du sous espace auquel il appartient. Ainsi, cet individu appartient à la classe A.",class="paragraphe"),
        plotOutput("plotintro3"),
        br()
    ),
    tabPanel(
        tags$header("Échantillon linéairement séparable",style="color: white"),
        tags$p("Dans cette section, nous tâcherons de vous expliquer la méthode SVM.",class="paragraphe"),
        br(),
        tags$p("Les SVM sont une méthode de Machine Learning autrement dit d'apprentissage automatique. Ainsi, au fur et à mesure que le modèle s’entraine sur les données,
                il est de plus en plus efficace. En effet, on commence par faire tourner les SVM sur l’échantillon d’apprentissage (où on connait la valeur 
                de la variable cible), afin que l'algorithme adopte une règle de décision.", class="paragraphe"),
        tags$p("Dans le cas des SVM, nous voulons estimer l'hyperplan qui permet de séparer deux nuages de points. Cependant, il peut en exister une infinité. L'hyperplan séparateur 
               optimal est celui qui maximise la distance entre les points les plus proches et la frontière, cette distance est appelée marge. Intuitivement, 
               plus nos points sont éloignés de la frontière, plus ils ont de chances d'être bien classés. Les points les plus proches de la marge sont qualifiés de vecteurs de 
               support.",class="paragraphe"),
        tags$p("Ainsi, l’emplacement de l'hyperplan connu, le modèle est maintenant capable de déterminer l’appartenance de n’importe quel nouvel individu.",class="paragraphe"),
        br(),
        sidebarLayout(
            sidebarPanel(
                height=30,
                width = 3,
                tags$p("Choisissez ce que vous voulez afficher :"),
                radioButtons("bouton1","Affichage de l'hyperplan séparateur ?",choices=c("Oui","Non"),selected="Non"),
                conditionalPanel(
                    condition="input.bouton1=='Oui'",
                    radioButtons("bouton2","Affichage de la marge ?",choices=c("Oui","Non"),selected="Non"),
                    conditionalPanel(
                        condition="input.bouton2=='Oui'",
                        radioButtons("bouton3","Affichage des vecteurs supports ?",choices=c("Oui","Non"),selected="Non")
                    )
                )
            ),
            mainPanel(
                plotOutput("plot1")
            )
        )
    ),
    tabPanel(
        tags$header("Échantillon non-linéairement séparable",style="color: white"),
        tags$p("Précèdemment, nous avons vu le cas simple où les données étudiées peuvent être linéairement séparées.
               Cependant, il existe aussi des échantillons non-linéairement séparables.",class="paragraphe"),
        tags$p("Le soft margin",
               style="font-weight: bold; font-size: 20px; color: #2C3E50; text-decoration: underline"),
        tags$p("Comme nous le voyons avec l'exemple ci-dessous, il n'est parfois pas possible de tracer une droite permettant de diviser linéairement un nuage 
                de points:",class="paragraphe"),
        plotlyOutput("plot2"),
        tags$p("Dans cet exemple, nos données sont néanmoins \"presque\" linéairement séparables.",class="paragraphe"),
        tags$p("La première astuce pour faire face à cela est d'utiliser ce qu'on appelle le \"soft margin\". Son principe est simple. Il s'agit ici de permettre
               à quelques observations d'être \"mal classées\", c'est à dire d'être du mauvais côté de l'hyperplan séparateur, mais également d'être bien
               classées mais à l'intérieur de la marge. Le programme d'optimisation reste sensiblement le même, à l'exception que nous introduisons des
               \"slack variables\", des variables \"ressort\", qui permettent cette mauvaise classification.",class="paragraphe"),
        tags$p("Nous devons maximiser une marge où il est dorénavant possible d'avoir des individus mal classés, mais il reste à déterminer le nombre d'individus qu'il est judicieux de mal classer. Nous devons donc introduire dans notre programme de maximisation 
               un paramètre de pénalisation, de telle sorte qu'à chaque fois que nous autorisons un individu à être mal classé (attribution d'une variable ressort), 
               nous pénalisons la marge d'un coût, ce qui revient à pénaliser les erreurs de classification. Sans ce dernier, nous aurions alors une 
               marge infinie.",class="paragraphe"),
        tags$p("De ce fait, plus le paramètre de coût fixé par l'utilisateur est élevé, et moins la marge sera étendue. Et inversement, plus le paramètre de coût sera faible,
               et plus la marge sera étendue car nous permettons l'introduction de plus en plus de slack variables.",class="paragraphe"),
        tags$p("Le prochain exemple vous permet de faire varier le paramètre de coût. Vous pourrez ainsi voir l'impact de ce paramètre sur l'hyperplan
               séparateur, ainsi que sur la marge de cet hyperplan.",class="paragraphe"),
        sidebarPanel(
            "Faites varier le paramètre de coût :",
            sliderInput("cost","Paramètre de coût",min=0,max=0.5,step=0.001,value=0.5)
        ),
        mainPanel(
            plotOutput("plot3")
        ),
        tags$p("L'astuce KERNEL",
               style="font-weight: bold; font-size: 20px; color: #2C3E50; text-decoration: underline"),
        tags$p("Il y a cependant des cas de figure où l'utilisation de slack variables ne permet pas d'effectuer une bonne classification. En effet, dans 
               l'exemple précédent, les données étaient \"presque\" linéairement séparables. Or, il existe des cas où les données ne sont pas du tout 
               linéairement séparables, en voici un exemple :",class="paragraphe"),
        plotOutput("plot4"),
        tags$p("Comme nous pouvons le voir, nous ne disposons que d'une variable X, et ses valeurs sont donc situées sur
               une droite. Ainsi, nous sommes dans un espace à une dimension, l'hyperplan séparateur serait donc un point sur cette droite. Ce point
               permettrait de séparer en deux notre nuage de points. Or, nous voyons bien ici qu'obtenir une bonne séparation est impossible.
               Quel que soit l'endroit où nous plaçons notre point, la classification n'est jamais parfaite.",class="paragraphe"),
        tags$p("C'est ici que nous allons donc aborder l'astuce Kernel. Cette astuce consiste tout simplement à augmenter l'espace de représentation
               des données en procédant à des transformations des prédicteurs dont nous disposons. Reprenons notre exemple :",class="paragraphe"),
        tags$p("X est l'unique prédicteur de la classe, nous sommes donc dans un espace à une dimension. Nous décidons par exemple de créer
               un nouveau prédicteur à partir de X : X². Nous avons donc dès lors deux prédicteurs, X et X².",class="paragraphe"),
        tags$p("Représentons alors nos données dans cet espace à 2 dimensions :",class="paragraphe"),
        plotOutput("plot5"),
        tags$p("Étant dans un espace à 2 dimensions, l'hyperplan séparateur est une droite. Or, nous voyons que nous pouvons aisément tracer une droite 
               pour séparer nos deux groupes A et B. Nous obtenons, dans ce cas, une classification parfaite :",class="paragraphe"),
        plotOutput("plot6"),
        tags$p("Le principe du \"kernel trick\" revient en fait à :",tags$span("augmenter l'espace de réprésentation des données à partir de transformées des 
               prédicteurs initiaux.",style="font-style: italic"),"Pour ce faire, nous avons dans la pratique recourt à une fonction noyau qui va générer ce
               nouvel espace.",class="paragraphe"),
        tags$p("Voici un deuxième exemple avec deux prédicteurs, X et Y :",class="paragraphe"),
        plotlyOutput("plot7"),
        tags$p("Il n'existe aucune droite permettant une \"bonne\" classification de ces données.",class="paragraphe"),
        tags$p("Cependant, si nous créons une troisième variable à partir des deux précédentes : Z=(X²+Y²), nous pouvons transformer ce nuage de points dans
               un espace à 3 dimensions. Nous pouvons donc effectuer une classification parfaite en tracant le plan suivant :",class="paragraphe"),
        radioButtons("plan","Affichage de l'hyperplan séparateur ?",choices=c("Oui","Non"),selected = "Non"),
        plotlyOutput("plot9")
    ),
    tabPanel(
        tags$header("Les données",style="color: white"),
        tags$p("Considérons maintenant la table de données mise à notre disposition.",class="paragraphe"),
        br(),
        tags$p("Ces données ont été collectées dans le cadre d'une étude menée par Wordline and the Machine Learning Group et l'Université
               Libre de Bruxelles sur le big data et la détection de fraude. ",class="paragraphe"),
        br(),
        tableOutput("description1"),
        br(),
        tags$p("La variable de réponse est Class, elle prend la valeur 1 si la transaction est frauduleuse et 0 sinon.",class="paragraphe"),
        tags$p("Nous avons 30 variables explicatives dont les secondes passées entre chaque transactions et leur montant
               et 28 variables anonymes par soucis de confidentialité (V1-V28).",class="paragraphe"),
        br(),
        tableOutput("description2"),
        br(),
        tags$p("Le but de cette analyse est de pouvoir prédire si une transaction est frauduleuse.",class="paragraphe")
        
    ),
    tabPanel(
        tags$header("Traitement des données",style="color: white"),
        tags$p("Une première analyse des fréquences de notre variable de réponse (Fraude), ayant pour modalité 1 si l'individu a connu l'évènement
               et 0 sinon nous adresse le constat suivant : 99.83% des individus de la base de données n'ont pas
               connu l'évènement, et seulement 0.17% sont des fraudeurs :",class="paragraphe"),
        tags$div(plotOutput("plot_class"),style="text-align: center"),
        tags$p("Nous faisons donc face à une base de données avec des données déséquilibrées, avec un problème d'évènement rare.",class="paragraphe"),
        tags$p("La première étape consiste à partitionner notre échantillon en deux sous-échantillons : un premier d'apprentissage, et un second de test.
               30% des observations initiales serviront à construire l'échantillon test et les 70% restantes constitueront notre échantillon d'apprentissage.
               C'est sur ce dernier que nous allons fixer les hyperparamètres optimaux (gamma, cost...) et estimer notre modèle. Nous pouvons donc voir
               que la proportion d'évenement ou de non-évenement, dans ces deux échantillons est similaire.",class="paragraphe"),
        tags$div(plotOutput("plot_class2"),style="text-align:center"),
        tags$p("Faisant face à ce problème de représentabilité des individus fraudeurs, il nous faut procéder à un rééchantillonnage de notre ensemble
               d'apprentissage. Plusieurs solutions s'offrent à nous : l'over-sampling, l'under-sampling ou encore le smote. Nous choisissons
               dans notre cas d'utiliser l'undersampling, de sorte à créer un échantillon avec 80% de non fraudeurs et 20% de fraudeurs. Pour ce faire, nous
               faisons un tirage aléatoire de 1 420 individus sans remises dans les 199 010 de l'ensemble d'apprentissage qui sont non fraudeurs
               (4 x 355 = 1 420). On ajoute à ces 1 420 obervations les 355 individus fraudeurs.",class="paragraphe"),
        tags$div(plotOutput("plot_class3"),style="text-align: center")
    ),
    tabPanel(
        tags$header("Visualisation de notre échantillon",style="color: white"),
        tags$p("Pour une meilleure visualisation des données, nous vous proposons des graphiques 2D et 3D.",class="paragraphe"),
        br(),
        sidebarLayout(
            sidebarPanel(
                selectInput("Choix_var","Sélectionnez 2 ou 3 variables",choices=names(donnees[,-31]),selected="V1",multiple=TRUE)
            ),
            mainPanel(
                h4(textOutput("text")),
                plotlyOutput("plot10")
            )
        ),
        conditionalPanel(
            condition="output.choice==2||output.choice==3",
            tags$p("Nous voyons qu'une simple projection dans un espace à 2 ou 3 dimensions ne permet pas de faire une classification parfaite de nos
                   données.",class="paragraphe")
        )
    ),
    tabPanel(
        tags$header("Estimation",style="color: white"),
        sidebarLayout(
            sidebarPanel(
                conditionalPanel(
                    condition="output.gen1=='FALSE'",
                    tags$p("Étape n°1 : Construire les échantillons d'apprentissage et de test :",style="font-weight: bold"),
                    hr(),
                    numericInput("seed1","Seed",min=0,max=10,value=1),
                    sliderInput("prop1","Pourcentage d'individus dans l'échantillon d'apprentissage",min=30,max=90,step=1,value=70),
                    actionButton("generate1","Générer les échantillons",class="btn-primary")
                ),
                conditionalPanel(
                    condition="output.gen1=='TRUE'",
                    conditionalPanel(
                        condition="output.gen2=='FALSE'",
                        tags$p("Étape n°2 : Rééchantillonner notre échantillon d'apprentissage :",style="font-weight: bold"),
                        hr(),
                        numericInput("seed2","Seed",min=0,max=10,value=1),
                        sliderInput("prop2","Taux d'indivus fraudeurs après rééchantillonnage",min=5,max=95,step=1,value=20),
                        actionButton("generate2","Rééchantillonner",class="btn-primary")
                    ),
                    conditionalPanel(
                        condition="output.gen2=='TRUE'",
                        tags$p("Étape n°3 : Estimer notre modèle :",style="font-weight: bold"),
                        hr(),
                        sliderInput("cost2","Paramètre de coût",min=0,max=20,step=0.01,value = 10),
                        selectInput("kernel","Type de kernel",choices=c("Linéaire"="linear","Polynomial"="polynomial",
                                                                        "Sigmoïde"="sigmoid","Radial Basis"="radial"),selected = "linear"),
                        conditionalPanel(
                            condition="input.kernel!='linear'",
                            conditionalPanel(
                              condition="input.kernel=='polynomial'",
                              sliderInput("degre","Degré du Polynome",min=1,max=10,value=3)
                            ),
                            sliderInput("gamma","Paramètre d'ajustement",min=0,max=1,step=0.01,value=0)
                        ),
                        sliderInput("weight","Poids d'un individu fraudeur par rapport à un non fraudeur (ref=1)",min=0,max=50,step=0.1,value=1),
                        actionButton("estim","Estimer le modèle",class="btn-primary"),
                        conditionalPanel(
                            condition="output.est=='TRUE'",
                            hr(),
                            tags$p("Étape n°4 : Évaluer les capacités prédictives du modèle sur l'échantillon test :",style="font-weight: bold"),
                            actionButton("eval","Evaluer",class="btn-primary")
                        )
                    ),
                    hr(),
                    actionButton("reinit","Réinitialiser",class="btn-primary")
                )
            ),
            mainPanel(
                verbatimTextOutput("text3"),
                tableOutput("rendu4"),
                tableOutput("rendu5")
            )
        )
    ),
    tabPanel(
      tags$header("Optimisation",style="color: white"),
      tags$p("Nous déterminons par cross-validation les hyper-paramètres optimaux, soient ici : le type de kernel, le paramètre de coût, et gamma,
             le paramètre d'ajustement. Nous utilisons la méthode k-folds avec k=10. (On notera que la valeur du paramètre d'ajustement gamma n'a aucune importance
             lorsque le kernel retenu est linéaire.) Nous obtenons pour les hyper-paramètres les valeurs suivantes :",class="paragraphe"),
      tableOutput("rendu0"),
      tags$p("Ces paramètres sont optimaux dans le sens où notre critère est le taux d'erreur. En effet, nous n'imputons ici aucune fonction de coût qui nous permettrait
             de faire un arbitrage entre sensibilité et spécificité.",class="paragraphe"),
      tags$p("Nous estimons ensuite notre modèle sur notre ensemble d'apprentissage, puis nous évaluons les capacités prédictives de notre modèle sur notre échantillon test.
             Nous obtenons le taux d'erreur, la spécificité (le taux de non-fraudeurs correctement identifiés) et la sensibilité (le taux de fraudeurs correctement
             identifiés) suivants :",class="paragraphe"),
      verbatimTextOutput("rendu1"),
      tableOutput("rendu2"),
      tableOutput("rendu3")
    ),
    tabPanel(
        tags$header("Comparaison",style="color: white"),
        tags$p("Il est intéressant de comparer les résultats obtenus grâce aux SVM avec d'autres méthodes de Machine Learning.",class="paragraphe"),
        br(),
        sidebarPanel(
            radioButtons(inputId="test",label="Autres benchmarks",choices=c("Régression Logistique"="Reg","KNN"="KNN","Boosting"="Boosting","Random Forest"="Rf")),
            conditionalPanel(
                condition="output.comp=='FALSE'",
                hr(),
                tags$p("Cliquez ici pour lancer la comparaison :",style="font-weight: bold"),
                actionButton("compar","Lancer la comparaison",class="btn-primary")
            )
        ),
        mainPanel(
            conditionalPanel(
                condition="input.test=='Reg'",
                tags$p("La régression logistique est une méthode de Machine Learning typiquement utilisée lorsque la variable de réponse est qualitative. Son but est donc de relier la variable
                       dépendante à des prédicteurs (qui, quant à eux, peuvent être quantitatifs et/ou qualitatifs).",class="paragraphe")
            ),
            conditionalPanel(
                condition="input.test=='KNN'",
                tags$p("La méthode des K plus proches voisins (K Nearest Neighbors) est un des algorithmes les plus intuitif de Maching Learning.
                       C'est une méthode de classification supervisée qui consiste à classifier des points en fonction de leurs distances par rapport à
                       ses plus proches voisins dans l'échantillon d'apprentissage.",class="paragraphe"),
                tags$p("Il faut donc chercher comment trouver les points les plus similaires à celui du point mystère. Généralement, on mesure la distance
                       euclidienne entre le nouvel élément et chaque autre élément du dataset et on garde les k plus proches.",class="paragraphe")
            ),
            conditionalPanel(
                condition="input.test=='Boosting'",
                tags$p("La méthode Boosting consiste à utiliser une méthode itérative pour combiner plusieurs classifieurs faibles, avec une pondération proportionnelle à leur pouvoir prédictif. 
                        Les exemples mal classés lors d'une itération sont 'boostés' de sorte à avoir plus d'importance pour le classifieur faible choisi lors de l'itération suivante.",class="paragraphe")
            ),
            conditionalPanel(
                condition="input.test=='Rf'",
                tags$p("Une forêt aléatoire est formée par de multiples arbres de décisions estimés sur des échantillons bootstrapés. Plusieurs modèles faibles sont alors aggrégés afin de créer un modèle robuste.
                       C'est un algorithme de Machine Learning particulièrement efficace pour la prédiction.",class="paragraphe")
            ),
            br(),
            tableOutput('Matrixconfus')
        ),
        conditionalPanel(
            condition="output.comp=='TRUE'",
            tags$p("Afin de comparer au mieux les performances obtenues avec ces différents benchmarks, nous avons construit des courbes ROC. En ordonnée, nous avons 
                   le taux de vrais positifs, qui représente la proportion de fraudeurs correctement identifiés (la sensibilité) en fonction des faux positifs, proportion de non-fraudeurs incorrectement identifiés 
                   (1-spécificité). L'aire sous la courbe est appelée AUC, et plus elle se rapproche de 1, plus la performance du classificateur est bonne. L'indice de gini est compris entre 0 et 1, plus la valeur
                   tend vers 1 et meilleure est la discrimination.",class="paragraphe"),
            fluidRow(
                column(width=6,offset = 3,plotOutput('plot_ROC'))
            ),
            br(),
            tableOutput('comparaison_erreur'),
            tags$p("*Indice de Gini : Gini=2*AUC-1",style="text-align: center; font-style: italic; font-size: 15px")
        )
    ),
    tabPanel(
        tags$header("Pros & Cons",style="color: white"),
        tags$p("Les machines à vecteurs de support, en comparaison avec la régression logitique par exemple, possèdent un certain
               nombre d'avantages et d'inconvénients.",br(),
               "On notera que les capacités prédictives pour ces deux méthodes sont similaires dans notre exemple.",class="paragraphe"),
        br(),
        tags$p("Avantages :",style="font-weight: bold; font-size: 18px"),
        tags$p("- Capacité à faire des prévisions à partir de relativement peu d'observations",br(),
               "- Capacité à traiter des cas non linéaires (astuce kernel)",br(),
               "- Compréhension globale du modèle assez simple (tracer une droite pour séparer deux ensembles de points dans un espace à deux dimensions)",br(),
               "- Gestion simple de l'over-fitting avec les paramètres de coût et d'ajustement (c et gamma)",class="paragraphe",style="margin-left: 40px",br(),
               "- Capacité à gérer des problèmes de grande dimension (grand nombre de variables)",br(),
               "- Modèle très intéressant en terme de prévision pure"),br(),
        tags$p("Limites :",style="font-weight: bold; font-size: 18px"),
        tags$p("- Difficulté à interprêter le modèle, on ne sait pas quelles variables sont créées et utilisées : \"Boîte noire\"",br(),
               "- De ce fait : Impossibilité de faire de l'inférence statistique comme le permettrait la régression logistique",br(),
               class="paragraphe",style="margin-left: 40px"),
        br(),
        tags$p("En règle générale, la performance des SVM, comme toute autre méthode de Machine Learning voit ses performances prédictives affectées par les
              données considérées, raison pour laquelle il convient de tester plusieurs méthodes et de les comparer afin d'identifier celle qui génère les meilleures prédictions
              (sur l'échantillon test). Il convient également pour les SVM, tout comme par exemple le boosting ou le random forest, de fixer les hyper-paramètres
              de façon optimale. Un mauvais choix dans ces hyper-paramètres, comme vous avez sans doute pu le constater dans l'onglet \"Estimation\", peut conduire
              à un modèle très peu performant.",class="paragraphe")
    )
))