library(shiny)
library(shinydashboard)
library(qgraph)
library(igraph)
library(shinycssloaders)
library(knitr)

ui <- dashboardPage(
    skin = "green",
    # Header panel
    dashboardHeader(title = "Shiny Networks"),
    
    # Sidebar panel
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "overview", icon = icon("home")),
            menuItem("Simulate", tabName = "simulation", icon = icon("calculator"),
                     menuSubItem("Erdos-Renyi model", tabName = "model3"),
                     menuSubItem("Watts-Strogatz model", tabName = "model2"),
                     menuSubItem("Barabasi-Albert model", tabName = "model1"),
                     menuSubItem("Geometric random model", tabName = "model5"),
                     menuSubItem("Growing random model", tabName = "model6"),
                     menuSubItem("Forest fire model", tabName = "model4")),
            menuItem("Quiz",tabName = "quiz", icon = icon("question"))
        )
    ),
    
    # Body
    dashboardBody(
        tabItems(
            
            # Homepage
            tabItem(tabName = "overview",
                    fluidRow(
                        column(4, align = "bottom", 
                               box(
                                   title = "Barabasi-Albert", status = "success", solidHeader = TRUE,
                                   collapsible = FALSE,width = 10, 
                                   img(src='model1.jpeg', align = "center",height = 300, width = 300),
                                   class = "'right.Align"
                               )),
                        column(4, align = "bottom"
                        ),
                        column(4, align = "bottom",
                               box(
                                   title = "Forest fire", status = "success", solidHeader = TRUE,
                                   collapsible = FALSE,width = 10, 
                                   img(src='model4.jpeg', align = "center",height = 300, width = 300)
                               ))),
                    fluidRow(
                        column(4, align = "bottom",
                               box(
                                   title = "Geometric random graph", status = "success", solidHeader = TRUE,
                                   collapsible = FALSE,width = 10, 
                                   img(src='model5.jpeg', align = "center",height = 300, width = 300)
                               )),
                        column(4, align = "bottom"),
                        column(4, align = "bottom",
                               box(
                                   title = "Growing random graph", status = "success", solidHeader = TRUE,
                                   collapsible = FALSE,width = 10,
                                   img(src='model6.jpeg', align = "center",height = 300, width = 300)
                               ))
                    ),
                    fluidRow(
                        column(4, align = "bottom",
                               box(
                                   title = "Erdos-Renyi", status = "success", solidHeader = TRUE,
                                   collapsible = FALSE,width = 10,
                                   img(src='model3.jpeg', align = "center",height = 300, width = 300)
                               )),
                        column(4, align = "botom"),
                        column(4, align = "bottom",
                               box(
                                   title = "Watts-Strogatz", status = "success", solidHeader = TRUE,
                                   collapsible = FALSE,width = 10, 
                                   img(src='model2.jpeg', align = "center",height = 300, width = 300)
                               ))
                    )
            ),
            
            # Barabasi-Albert sampling
            tabItem(tabName = "model1",
                    fluidPage(
                        column(12, align = "center", titlePanel("Barabasi-Albert model")),
                        fluidRow(
                            sidebarPanel(
                                sliderInput(
                                    "n", 
                                    "Number of vertices",
                                    min = 10, max = 200, step = 1, value = 100, width = 400
                                ),
                                sliderInput(
                                    "power", 
                                    "Power",
                                    min = 0, max = 10, step = .25, value = 1, width = 400
                                ),
                                sliderInput(
                                    "m", 
                                    "Edges added per time step",
                                    min = 1, max = 10, step = 1, value = 1, width = 400
                                ),
                                checkboxInput(
                                    "directed1",
                                    "Directed graph"
                                ),
                                actionButton('sample1', 'Resample')
                            ),
                            mainPanel(
                                withSpinner(plotOutput("BAplot"),type = 6, color = "green")
                            )
                        )
                    )
            ),
            
            # Watts-Strogatz sampling
            tabItem(tabName = "model2",
                    column(12, align = "center", titlePanel("Watts-Strogatz model")),
                    fluidPage(
                        sidebarPanel(
                            # sliderInput(
                            #     "dimensions", "Number of dimensions",
                            #     min = 1, max = 3, step = 1, value = 1),
                            sliderInput(
                                "size", "Size",
                                min = 10, max = 200, step = 1, value = 100),
                            sliderInput(
                                "neighbourhood", "Neighborhood",
                                min = 0, max = 10, step = 1, value = 5),
                            sliderInput(
                                "probability",
                                "Rewiring probability",
                                min = 0, max = 1, step = 0.1, value = 0.05),
                            actionButton(inputId = 'sample2', 'Resample')
                            
                        ),
                        mainPanel(
                            withSpinner(plotOutput("WSplot"), type = 6, color = "green")
                        )
                    )),
            
            # Erdos-Renyi sampling
            tabItem(tabName = "model3",
                    column(12, align = "center", titlePanel("Erdos-Renyi model")),
                    fluidPage(
                        sidebarPanel(
                            sliderInput(
                                "vertices", "Number of vertices",
                                min = 10, max = 200, step = 1, value = 150),
                            sliderInput(
                                "edges", "Number of edges",
                                min = 400, max = 800, step = 1, value = 400),
                            checkboxInput(
                                "directed3",
                                "Directed graph"
                            ),
                            actionButton(inputId = 'sample3', 'Resample')
                        ),
                        mainPanel(
                            withSpinner(plotOutput("ERplot"), type = 6, color = "green")
                        )
                    )
            ),
            
            # Forest Fire sampling
            tabItem(tabName = "model4",
                    column(12, align = "center", titlePanel("Forest Fire model")),
                    fluidPage(
                        sidebarPanel(
                            sliderInput(
                                "nodes", "Number of nodes",
                                min = 10, max = 200, step = 1, value = 100),
                            sliderInput(
                                "probfor", "Burning forward probability",
                                min = 0, max = 1, step = 0.1, value = .5),
                            sliderInput(
                                "probback", "Burning backwards probability",
                                min = 0, max = 1, step = 0.1, value = .5),
                            # sliderInput(
                            #     "ambs", "Ambassador vertices",
                            #     min = 0, max = 200, step = 1, value = 100),
                            checkboxInput(
                                "directed4",
                                "Directed graph"
                            ),
                            actionButton('sample4', 'Resample')
                            
                        ),
                        mainPanel(
                            withSpinner(plotOutput("FFplot"), type = 6, color = "green")
                        )
                    )
            ),
            
            # Geometric random sampling
            tabItem(tabName = "model5",
                    fluidRow()
            ),
            
            # Growing random sampling
            tabItem(tabName = "model6",
                    column(12, align = "center", titlePanel("Growing Random model")),
                    fluidPage(
                        sidebarPanel(
                            sliderInput(
                                "vertices", "Number of vertices",
                                min = 10, max = 200, step = 1, value = 100),
                            sliderInput(
                                "edges", "Edges added in each time step",
                                min = 1, max = 10, step = 1, value = 1),
                            checkboxInput(
                                "directed6",
                                "Directed graph"
                            ),
                            checkboxInput(
                                "citation",
                                "Citation graph"
                            ),
                            actionButton('sample6', 'Resample')
                            
                        ),
                        mainPanel(
                            withSpinner(plotOutput("SGplot"),type = 6, color = "green")
                        )
                    )
            )
        )
    )
)

server <- function(input, output) { 
    
    ## Initialize Barabasi-Albert ##
    g1 <- igraph::sample_pa(n = 150, power = 1, m = 1, directed = FALSE)
    clusters1 <- igraph::spinglass.community(g1)$membership
    g1 <- igraph::get.adjacency(g1)
    output$BAplot <- renderPlot({qgraph::qgraph(g1, color = clusters1, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                border.color = "black", shape = "circle",label.cex = 1.5, label.color = "white",
                                                bg = "gray94", borders = FALSE)},
                                width = 950, 
                                height = 700)
    
    ## Resample when button is pressed ##
    observeEvent(input$sample1, {
        g <- igraph::sample_pa(n = input$n, power = input$power, m = input$m, directed = input$directed1)
        clusters1 <- igraph::spinglass.community(g)$membership
        g <- igraph::get.adjacency(g)
        output$BAplot <- renderPlot({qgraph::qgraph(g, color = clusters1, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                    border.color = "black", shape = "circle",label.cex = 1.5, label.color = "white",
                                                    bg = "gray94", borders = FALSE)},
                                    width = 950, 
                                    height = 700)
    })
    
    ## Initialize Watts-Strogatz ##
    g2 <- igraph::sample_smallworld(dim = 1, size = 100, nei = 5, p = .1)
    clusters2 <- igraph::spinglass.community(g2)$membership
    g2 <- igraph::get.adjacency(g2)
    output$WSplot <- renderPlot({qgraph::qgraph(g2, color = clusters2, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                border.color = "black", shape = "circle",label.cex = 1.5, label.color = "white", repulsion = 2,
                                                bg = "gray94", borders = FALSE)},
                                width = 950, 
                                height = 700)
    ## Resample when button is pressed ##
    observeEvent(input$sample2,
                 {
                     g <- igraph::sample_smallworld(dim = 1, size = input$size, nei = input$neighbourhood, p = input$probability)
                     clusters2 <- igraph::spinglass.community(g)$membership
                     g <- igraph::get.adjacency(g)
                     output$WSplot <- renderPlot({qgraph::qgraph(g, color = clusters2, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                                 border.color = "black", shape = "circle",label.cex = 1.5, label.color = "white", repulsion = 2,
                                                                 bg = "gray94", borders = FALSE)},
                                                 width = 950, 
                                                 height = 700)
                 })
    ## Initialize Erdos-Renyi ##
    g3 <- igraph::erdos.renyi.game(n = 150, p.or.m = 400, type = "gnm", directed = FALSE)
    # Some error handling
    p <- try({clusters3 <- igraph::spinglass.community(g3)$membership})
    if(class(p) == "try-error"){
        clusters3 <- "red" 
    }
    g3 <- igraph::get.adjacency(g3)
    output$ERplot <- renderPlot({qgraph::qgraph(g3, color = clusters3, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white",
                                                bg = "gray94", borders = FALSE)}, 
                                width = 950, 
                                height = 700)
    ## Resample when button is pressed ##
    observeEvent(input$sample3,
                 {
                     g3 <- igraph::erdos.renyi.game(n = input$vertices, p.or.m = input$edges, type = "gnm", directed = input$directed3)
                     p3 <- try({clusters3 <- spinglass.community(g3)$membership})
                     if(class(p3) == "try-error"){
                         clusters3 <- "red" 
                     }
                     g3 <- igraph::get.adjacency(g3)
                     output$ERplot <- renderPlot({qgraph::qgraph(g3, color = clusters3, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                                 border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white",
                                                                 bg = "gray94", borders = FALSE)}, 
                                                 width = 950, 
                                                 height = 700)
                 })
    
    ## Initialize Forest Fire model ##
    g4 <- igraph::sample_forestfire(nodes = 100, fw.prob=0.5, bw.factor=0.5,directed = FALSE)
    clusters4 <- spinglass.community(g4)$membership
    g4 <- igraph::get.adjacency(g4)
    output$FFplot <- renderPlot({qgraph::qgraph(g4, color = clusters4, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white",
                                                bg = "gray94", borders = FALSE)},
                                width = 950, 
                                height = 700
    )
    ## Resample when button is pressed ##
    observeEvent(input$sample4,
                 {
                     g4 <- igraph::sample_forestfire(nodes = input$nodes, fw.prob=input$probfor,bw.factor=input$probback,
                                                     #ambs=input$ambs, 
                                                     directed = input$directed4)
                     clusters4 <- spinglass.community(g4)$membership
                     g4 <- igraph::get.adjacency(g4)
                     output$FFplot <- renderPlot({qgraph::qgraph(g4, color = clusters4, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                                 border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white",
                                                                 bg = "gray94", borders = FALSE)},
                                                 width = 950, 
                                                 height = 700
                     )
                 })
    
    ## Initialize Growing random model ##
    g6 <- sample_growing(n = 100, m = 1, citation=FALSE, directed = FALSE)
    g6 <- igraph::get.adjacency(g6)
    output$SGplot <- renderPlot({qgraph::qgraph(g6, color = "black", edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white", layout = "spring",
                                                bg = "gray94", borders = FALSE)},
                                width = 950, 
                                height = 700
    )
    ## Resample when button is pressed ##
    observeEvent(input$sample6,
                 {
                     g6 <- sample_growing(n = input$vertices, m = input$edges, citation=input$citation, directed = input$directed6)
                     g6 <- igraph::get.adjacency(g6)
                     output$SGplot <- renderPlot({qgraph::qgraph(g6, color = "black", edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                                 border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white", layout = "spring",
                                                                 bg = "gray94", borders = FALSE)},
                                                 width = 950, 
                                                 height = 700
                     )
                 })
    
}

shinyApp(ui, server)