library(shiny)
library(shinydashboard)
library(qgraph)
library(igraph)
library(shinycssloaders)
library(knitr)

ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "Shiny Networks"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "overview", icon = icon("home")),
            menuItem("Simulate", tabName = "simulation", icon = icon("calculator"),
                     menuSubItem("Erdos-Renyi graph", tabName = "model3"),
                     menuSubItem("Watts-Strogatz graph", tabName = "model2"),
                     menuSubItem("Barabasi-Albert graph", tabName = "model1"),
                     menuSubItem("Geometric random graph", tabName = "model5"),
                     menuSubItem("Growing random graph", tabName = "model6"),
                     menuSubItem("Forest fire graph", tabName = "model4")),
            menuItem("Quiz",tabName = "quiz", icon = icon("question"))
        )
    ),
    dashboardBody(
        tabItems(
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
            tabItem(tabName = "model1",
                    fluidPage(
                        titlePanel("Barabasi-Albert graph"),
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
            tabItem(tabName = "model2",
                    titlePanel("Watts-Strogatz graph"),
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
            tabItem(tabName = "model3",
                    titlePanel("Erdos-Renyi graph"),
                    fluidPage(
                        sidebarPanel(
                            sliderInput(
                                "vertices", "Number of vertices",
                                min = 10, max = 200, step = 1, value = 100),
                            sliderInput(
                                "edges", "Number of edges",
                                min = 0, max = 500, step = 1, value = 250),
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
            tabItem(tabName = "model4",
                    fluidRow()
            ),
            tabItem(tabName = "model5",
                    fluidRow()
            ),
            tabItem(tabName = "model6",
                    fluidRow()
            )
        )
    )
)

server <- function(input, output) { 
    
    # Initialize the graph
    g <- igraph::sample_pa(n = 100, power = 1, m = 1, directed = FALSE)
    clusters <- spinglass.community(g)$membership
    g <- igraph::get.adjacency(g)
    output$BAplot <- renderPlot({qgraph::qgraph(g, color = clusters, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                border.color = "black", shape = "circle",label.cex = 1.5, label.color = "white",
                                                bg = "gray94")},
                                width = 950, 
                                height = 700)
    
    
    observeEvent(input$sample1, {
        g <- igraph::sample_pa(n = input$n, power = input$power, m = input$m, directed = input$directed1)
        clusters <- spinglass.community(g)$membership
        g <- igraph::get.adjacency(g)
        output$BAplot <- renderPlot({qgraph::qgraph(g, color = clusters, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                    border.color = "black", shape = "circle",label.cex = 1.5, label.color = "white",
                                                    bg = "gray94")},
                                    width = 950, 
                                    height = 700)
    })
 
    # Initialize the graph
    g <- sample_smallworld(dim = 1, size = 100, nei = 5, p = .1)
    clusters <- spinglass.community(g)$membership
    g <- get.adjacency(g)
    output$WSplot <- renderPlot({qgraph::qgraph(g, color = clusters, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                border.color = "black", shape = "circle",label.cex = 1.5, label.color = "white", repulsion = 2,
                                                bg = "gray94")},
                                width = 950, 
                                height = 700)
    observeEvent(input$sample2,
                 {
                     g <- sample_smallworld(dim = 1, size = input$size, nei = input$neighbourhood, p = input$probability)
                     clusters <- spinglass.community(g)$membership
                     g <- get.adjacency(g)
                     output$WSplot <- renderPlot({qgraph::qgraph(g, color = clusters, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                                 border.color = "black", shape = "circle",label.cex = 1.5, label.color = "white", repulsion = 2,
                                                                 bg = "gray94")},
                                                 width = 950, 
                                                 height = 700)
                 })
    
    # Initialize the graph
    g <- erdos.renyi.game(n = 100, p.or.m = 250, type = "gnm", directed = FALSE)
    p <- try({clusters <- spinglass.community(g)$membership})
    if(class(p) == "try-error"){
        clusters <- "red"           # Error color
    }
    g <- igraph::get.adjacency(g)
    output$ERplot <- renderPlot({qgraph::qgraph(g, color = clusters, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white",
                                                bg = "gray94")}, 
                                width = 950, 
                                height = 700)
    observeEvent(input$sample3,
                 {
                     g <- erdos.renyi.game(n = input$vertices, p.or.m = input$edges, type = "gnm", directed = input$directed3)
                     clusters <- spinglass.community(g)$membership
                     g <- igraph::get.adjacency(g)
                     output$ERplot <- renderPlot({qgraph::qgraph(g, color = clusters, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                                 border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white",
                                                                 bg = "gray94")}, 
                                                 width = 950, 
                                                 height = 700)
                 })
    
}

shinyApp(ui, server)