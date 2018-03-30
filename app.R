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
                     menuSubItem("Geometric Random model", tabName = "model5"),
                     menuSubItem("Growing Random model", tabName = "model6"),
                     menuSubItem("Forest Fire model", tabName = "model4")),
            menuItem("Upload Data", tabName = "upload", icon = icon("database")),
            menuItem("Info", tabName = "info", icon = icon("info")),
            menuItem("Quiz",tabName = "quiz", icon = icon("question"))
        )
    ),
    
    # Body
    dashboardBody(
        tabItems(
            
            # Homepage
            tabItem(tabName = "overview",
                    column(12, align = "center", titlePanel(HTML('<font size="10">Untangling the Growing Network Web</font>'))),
                    fluidRow(
                        column( 4, align = "bottom",
                                box(
                                    title = "Barabasi-Albert model", status = "success", solidHeader = TRUE,
                                    collapsible = FALSE,width = 10,
                                    img(src = 'model1.gif', height = 250, width = 350, align = "center")
                                )),
                        column( 4, align = "bottom",
                                box(
                                    title = "Erdos-Renyi model", status = "success", solidHeader = TRUE,
                                    collapsible = FALSE,width = 10,
                                    img(src = 'model3.gif', height = 250, width = 350, align = "center")
                                )),
                        column( 4, align = "bottom",
                                box(
                                    title = "Watts-Strogatz model", status = "success", solidHeader = TRUE,
                                    collapsible = FALSE,width = 10, 
                                    img(src = 'model2.gif', height = 250, width = 350, align = "center")
                                ))),
                    
                    fluidRow(
                        column( 4, align = "bottom",
                                box(
                                    title = "Geometric Random model", status = "success", solidHeader = TRUE,
                                    collapsible = FALSE,width = 10, 
                                    img(src = 'model5.gif', height = 250, width = 350, align = "center")
                                )),
                        column( 4, align = "bottom", 
                                box(
                                    title = "Growing Random model", status = "success", solidHeader = TRUE,
                                    collapsible = FALSE,width = 10,
                                    img(src = 'model6.gif', height = 250, width = 350, align = "center")
                                )),
                        column( 4, align = "bottom",
                                box(
                                    title = "Forest Fire model", status = "success", solidHeader = TRUE,
                                    collapsible = FALSE,width = 10, 
                                    img(src = 'model4.gif', height = 250, width = 350, align = "center")
                                    
                                ))
                    )
                    ),
            
            # Barabasi-Albert sampling
            tabItem(tabName = "model1",
                    fluidPage(
                        
                        tags$head(tags$style(".progress-bar{background-color:#00cc00;}")),
                        
                        fluidRow(
                            sidebarPanel(h1("Barabasi-Albert model"),
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
                    fluidPage(
                        sidebarPanel(
                            h1("Watts-Strogatz model"),
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
                    fluidPage(
                        sidebarPanel(
                            h1("Erdos-Renyi model"),
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
                    fluidPage(
                        sidebarPanel(
                            h1("Forest Fire model"),
                            sliderInput(
                                "nodes", "Number of nodes",
                                min = 10, max = 200, step = 1, value = 100),
                            sliderInput(
                                "probfor", "Burning forward probability",
                                min = 0, max = 1, step = 0.1, value = .5),
                            sliderInput(
                                "probback", "Burning backwards probability",
                                min = 0, max = 1, step = 0.1, value = .5),
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
                    fluidPage(
                        sidebarPanel(
                            h1("Geometric Random model"),
                            sliderInput(
                                "node", "Number of nodes",
                                min = 0, max = 200, step = 1, value = 100),
                            sliderInput(
                                "radius", "size of radius",
                                min = 0.2, max = 1, step = 0.01, value = 0.2),
                            checkboxInput(
                                "torus", "Use torus"),
                            actionButton('sample5', 'Resample')
                            
                        ),
                        mainPanel(
                            withSpinner(plotOutput("RGGplot"), type = 6, color = "green")
                        )
                    )
            ),
            
            # Growing random sampling
            tabItem(tabName = "model6",
                    fluidPage(
                        sidebarPanel(
                            h1("Growing Random model"),
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
                                "Citation graph: in each time step the added edges are originating from the new vertex"
                            ),
                            actionButton('sample6', 'Resample')
                            
                        ),
                        mainPanel(
                            withSpinner(plotOutput("SGplot"),type = 6, color = "green")
                        )
                    )
            ),
            tabItem(tabName = "upload",
                    fluidPage(
                        sidebarPanel(
                            h1("Upload Adjacency matrix"),
                            fileInput("data", label = "", width = "400px",
                                      buttonLabel = "Choose file",
                                      placeholder = "adjacencyMatrix.txt"),
                            actionButton("upl", label = "Upload")
                        ),
                        mainPanel(
                            plotOutput("graph"),
                            tableOutput("result")
                        )
                    )
            ),
            tabItem(tabName = "info",
                    column(12, align = "center", titlePanel("Information")),
                    fluidPage()
            ),
            tabItem(tabName = "quiz",
                    column(12, align = "center", titlePanel("Quiz")),
                    fluidPage()
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
        withProgress(message = 'Generating graph', value = 0, {
            g <- igraph::sample_pa(n = input$n, power = input$power, m = input$m, directed = input$directed1)
            incProgress(1/3)
            clusters1 <- igraph::spinglass.community(g)$membership
            incProgress(1/3)
            g <- igraph::get.adjacency(g)
            incProgress(1/3)
            output$BAplot <- renderPlot({qgraph::qgraph(g, color = clusters1, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                        border.color = "black", shape = "circle",label.cex = 1.5, label.color = "white",
                                                        bg = "gray94", borders = FALSE)},
                                        width = 950, 
                                        height = 700)
        })
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
                     withProgress(message = 'Generating graph', value = 0, {
                         g <- igraph::sample_smallworld(dim = 1, size = input$size, nei = input$neighbourhood, p = input$probability)
                         incProgress(1/3)
                         clusters2 <- igraph::spinglass.community(g)$membership
                         incProgress(1/3)
                         g <- igraph::get.adjacency(g)
                         incProgress(1/3)
                         output$WSplot <- renderPlot({qgraph::qgraph(g, color = clusters2, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                                     border.color = "black", shape = "circle",label.cex = 1.5, label.color = "white", repulsion = 2,
                                                                     bg = "gray94", borders = FALSE)},
                                                     width = 950, 
                                                     height = 700)
                     })
                 })
    ## Initialize Erdos-Renyi ##
    g3 <- igraph::erdos.renyi.game(n = 150, p.or.m = 400, type = "gnm", directed = FALSE)
    # Some error handling
    p <- try({clusters3 <- igraph::spinglass.community(g3)$membership})
    if(class(p) == "try-error"){
        clusters3 <- "black" 
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
                     withProgress(message = 'Generating graph', value = 0, {
                         g3 <- igraph::erdos.renyi.game(n = input$vertices, p.or.m = input$edges, type = "gnm", directed = input$directed3)
                         incProgress(1/3)
                         p3 <- try({clusters3 <- igraph::spinglass.community(g3)$membership})
                         if(class(p3) == "try-error"){
                             clusters3 <- "black" 
                         }
                         incProgress(1/3)
                         g3 <- igraph::get.adjacency(g3)
                         incProgress(1/3)
                         output$ERplot <- renderPlot({qgraph::qgraph(g3, color = clusters3, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                                     border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white",
                                                                     bg = "gray94", borders = FALSE)}, 
                                                     width = 950, 
                                                     height = 700)
                     })
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
                     withProgress(message = 'Generating graph', value = 0, {
                         g4 <- igraph::sample_forestfire(nodes = input$nodes, fw.prob=input$probfor,bw.factor=input$probback,
                                                         directed = input$directed4)
                         incProgress(1/3)
                         clusters4 <- spinglass.community(g4)$membership
                         incProgress(1/3)
                         g4 <- igraph::get.adjacency(g4)
                         incProgress(1/3)
                         output$FFplot <- renderPlot({qgraph::qgraph(g4, color = clusters4, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                                     border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white",
                                                                     bg = "gray94", borders = FALSE)},
                                                     width = 950, 
                                                     height = 700
                         )
                     })
                 })
    
    ## Initialize Geometric random model ##
    g5 <- sample_grg(nodes = 100, radius = 0.2, torus = FALSE)
    p5 <- try({clusters5 <- spinglass.community(g5)$membership})
    if (class(p5) == "try-error"){
        clusters5 <- "black"
    }
    g5 <- igraph::get.adjacency(g5)
    output$RGGplot <- renderPlot({qgraph::qgraph(g5, color = clusters5, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                 border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white", layout = "spring",
                                                 bg = "gray94", borders = FALSE)},
                                 width = 950, 
                                 height = 700
    )
    ## Resample when button is pressed ##
    observeEvent(input$sample5,
                 {
                     withProgress(message = 'Generating graph', value = 0, {
                         g5 <- sample_grg(nodes = input$node, radius = input$radius, torus = input$torus)
                         incProgress(1/3)
                         p5 <- try({clusters5 <- spinglass.community(g5)$membership})
                         if (class(p5) == "try-error"){
                             clusters5 <- "black"
                         }
                         incProgress(1/3)
                         g5 <- igraph::get.adjacency(g5)
                         incProgress(1/3)
                         output$RGGplot <- renderPlot({qgraph::qgraph(g5, color = clusters5, edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                                      border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white", layout = "spring",
                                                                      bg = "gray94", borders = FALSE)},
                                                      width = 950, 
                                                      height = 700
                         )
                     })
                 })
    
    ## Initialize Growing random model ##
    g6 <- sample_growing(n = 100, m = 1, citation=FALSE, directed = FALSE)
    g6 <- igraph::get.adjacency(g6)
    diag(g6) <- 0
    output$SGplot <- renderPlot({qgraph::qgraph(g6, color = "black", edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white", layout = "spring",
                                                bg = "gray94", borders = FALSE)},
                                width = 950, 
                                height = 700
    )
    ## Resample when button is pressed ##
    observeEvent(input$sample6,
                 {
                     withProgress(message = 'Generating graph', value = 0, {
                         g6 <- sample_growing(n = input$vertices, m = input$edges, citation=input$citation, directed = input$directed6)
                         incProgress(1/2)
                         g6 <- igraph::get.adjacency(g6)
                         diag(g6) <- 0
                         incProgress(1/2)
                         output$SGplot <- renderPlot({qgraph::qgraph(g6, color = "black", edge.color = "darkgrey", edge.width = .5, vsize = 5, 
                                                                     border.color = "black", shape = "circle", label.cex = 1.5, label.color = "white", layout = "spring",
                                                                     bg = "gray94", borders = FALSE)},
                                                     width = 950, 
                                                     height = 700
                         )
                     })
                 })
    
    observeEvent(input$upl, {
        withProgress(message = 'Generating graph', value = 0, {
            incProgress(1/4)
            file <- input$data
            if (is.null(file)) {
                # No file has been uploaded
                return(NULL)
            }
            datafile <- read.table(file$datapath)
            output$graph <- renderPlot({qgraph::qgraph(datafile, labels = TRUE, bg = "gray94")})
            
            incProgress(1/4)
            tab <- data.frame("Model" = c("Barabasi-Albert","Erdos-Renyi", "Watts-Strogatz", "Geometric Random", "Growing Random", "Forest Fire"), "LogLikelihood" = rep(NA,6))

            datafile <- as.matrix(datafile)
            # Extract degree distribution
            degree <- as.numeric(colSums(datafile))
            incProgress(1/4)

            # Compute the loglikelihoods
            erdos.renyi.logl <- sum(dbinom(degree, size = length(degree), prob = (1/length(degree)),log = TRUE))

            # Fill the table
            tab[2,2] <- erdos.renyi.logl

            # Order according to higher likelihood
            tab <- tab[order(-tab$LogLikelihood),]
            output$result <- renderTable(tab)
        })
    })
    
}

shinyApp(ui, server)