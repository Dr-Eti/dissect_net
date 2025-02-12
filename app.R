## Graph Subsetting using Shiny
## October 2019 revised Feb 2025 (fixes colour "shifting")

rm(list = ls())
gc()

## 00.0 required libraries                              ####
library(igraph)
library(networkD3)
library(shiny)
library(RColorBrewer)
library(jsonlite)                



## 00.1 random graph function                           ####
myRandGraph <- function(n_nodes, max_attr, edge_prob){
  test_graph <- igraph::sample_gnp(n = n_nodes, p = edge_prob, directed = TRUE)
  V(test_graph)$name <- letters[1:vcount(test_graph)]
  set.seed(25)
  E(test_graph)$weight <- floor(runif(ecount(test_graph), min=1, max=5))        # http://www.cookbook-r.com/Numbers/Generating_random_numbers/
  ## generate node attributes for use to subset the graph  
  V(test_graph)$attr1 <- floor(runif(vcount(test_graph), min = 1, max = max_attr))
  ## adjacency matrix
  test_adj <- as_adjacency_matrix(test_graph, attr="weight")
  ## generate D3 graph elements         
  nodes_D3 <- igraph_to_networkD3(test_graph, group = V(test_graph)$attr1, what = "nodes")
  edges_D3 <- igraph_to_networkD3(test_graph, group = V(test_graph)$attr1, what = "links")
  nodes_D3$id <- seq_len(nrow(nodes_D3)) - 1                                    # due to JS indexing convention
  nodes_D3$id_dynamic <- nodes_D3$id
  edges_D3$source_dynamic <- edges_D3$source
  edges_D3$target_dynamic <- edges_D3$target
  edges_D3$value <- E(test_graph)$weight/5
  ## copy nodes group properties to edges for filtering
  for (i in 1:nrow(edges_D3)){
    edges_D3[i,"source_node_group"] <- nodes_D3[which(nodes_D3$id == edges_D3[i,"source"]), "group"]
    edges_D3[i,"target_node_group"] <- nodes_D3[which(nodes_D3$id == edges_D3[i,"target"]), "group"]
  }
  ## rescale node size for visualisation
  min_scale <- 1
  max_scale <- 100
  visual_range <- max_scale - min_scale
  test_size <- degree(test_graph, mode="all")
  test_range <- max(test_size) - min(test_size)
  test_size_rescaled <- (((test_size - min(test_size)) * visual_range) +1) / test_range
  nodes_D3$size <- test_size_rescaled
  ##output
  list(test_graph = test_graph,
       test_adj = test_adj,
       nodes_D3 = nodes_D3,
       edges_D3 = edges_D3
       )
}

## 00.2 generate random graph with random attributes    ####
n_nodes <- 25
max_attr <- 4
edge_prob <- 2/10
rG <- myRandGraph(n_nodes, max_attr, edge_prob)
nodes_D3 <- rG$nodes_D3
edges_D3 <- rG$edges_D3


## 00.3 colours trick                                   ####
## some pre-processing to maintain the same colour for a given group even after filtering 
## Necessary because unfortunately JS maps the first element in the domain on the first colour in the palette and so on https://d3js.org/d3-scale/ordinal#scaleOrdinal)
color_scale <- RColorBrewer::brewer.pal((max_attr - 1), "Paired")
#color_scale <- c("red", "green", "blue", "orange")
color_idx <- unique(nodes_D3$group)
my_color_range <- as.data.frame(cbind(color_idx, color_scale))


##                                                          ####
## Shiny App                                                ####
## for network thread: https://github.com/christophergandrud/networkD3-shiny-example/blob/master/app.R
## more in general: https://shiny.rstudio.com/ see bottom example

## UI                                                       #### 
ui <- fluidPage(
  titlePanel("Interactive subsetting of a (random) D3 graph"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "checkGroup",
                         label = "Choose Category",
                         choices =  unique(nodes_D3$group),
                         selected = unique(nodes_D3$group)
                         ),
    
      verbatimTextOutput("value")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = 'Graph',
                 fluidRow(
                   column(width = 12, 
                          #h3('A random graph'),  
                          forceNetworkOutput(outputId = "force", height = "650px")
                   )
                 )
        ),
        tabPanel(title = 'Tabular view', 
                 tableOutput("test_table_2")
                 
        ),
        tabPanel(title = 'Tabular view 2',
                 tableOutput("test_table")
        )
      )
    )
  )
)


## Server ##### 
server <- function(input, output) {
  output$value <- renderText({input$checkGroup })                                                ## just for debug: display the groups selected by the user
  
  ## Interactive PARTITIONING
  ## Some resources on REACTIVE/dynamic filtering:
  ## --- https://stackoverflow.com/questions/46150358/subset-a-column-of-a-dataframe-stored-as-a-reactive-expression-eventreactive
  ## --- https://stackoverflow.com/questions/51153184/dynamic-filters-and-reactive-plot-in-shiny
  
  ## nodes subset
  ## -  MUST MAKE SURE THAT Source/Target REMAINS zero-indexed AFTER FILTERING. This is required in JavaScript and so your plot may not render
  nodes_subset <- reactive({
      req(input$checkGroup)
      selected <- input$checkGroup
      nodes_temp_subset <- nodes_D3[which(nodes_D3$group %in% selected),]
      nodes_temp_subset$id_dynamic = match(nodes_temp_subset$id_dynamic, unique(nodes_temp_subset$id_dynamic)) - 1                            # re-indexing thread: https://stackoverflow.com/questions/51158295/how-to-renumber-group-id-sequentially-in-r)
      nodes_temp_subset
  })
  
  ## arcs subset
  edges_subset <- reactive({
    req(input$checkGroup)
    selected <- input$checkGroup
    edges_temp_subset <- edges_D3[which(edges_D3$source_node_group %in% selected & edges_D3$target_node_group %in% selected), ]
    edges_temp_subset$source_dynamic <- nodes_subset()[match(edges_temp_subset$source, nodes_subset()$id), "id_dynamic"]
    edges_temp_subset$target_dynamic <- nodes_subset()[match(edges_temp_subset$target, nodes_subset()$id), "id_dynamic"]
    edges_temp_subset
  })
  
  ## show output in table
  output$test_table_2 <- renderTable(nodes_subset(), rownames = TRUE)
  output$test_table <- renderTable(edges_subset(), rownames = TRUE)
  
  
  ## prepare to pass color domain and range to D3
  color_JS <- reactive({
    req(input$checkGroup)
    color_domain_subset <- my_color_range[unique(nodes_subset()$group), "color_scale"]
    nodes_to_color <- my_color_range[unique(nodes_subset()$group), " color_idx"]
    color_scale_JS <- jsonlite::toJSON(as.character(color_domain_subset))                            # thread: https://stackoverflow.com/questions/36503315/convert-r-data-frame-to-javascript-array
    color_domain_JS <- jsonlite::toJSON(as.character(nodes_to_color)) 
    list(color_scale_JS = color_scale_JS,
         color_domain_JS = color_domain_JS)
  })
  
  
  ## D3 network
  output$force <- renderForceNetwork({
    req(input$checkGroup)
    forceNetwork(
                 Links = edges_subset(),
                 Nodes = nodes_subset(),
                 Source = "source_dynamic",
                 Target = "target_dynamic",
                 NodeID = "name", Nodesize = "size", 
                 Group = "group", 
                 Value = "value", 
                 arrows = TRUE, 
                 charge = -800,  
                 linkDistance = 20,
                 #colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),    # list of colour scales https://github.com/d3/d3-scale-chromatic/blob/master/README.md
                 colourScale = JS(paste0("d3.scaleOrdinal(", color_JS()$color_scale_JS,").domain(", color_JS()$color_domain_JS,");")),  
                 opacity = 1, 
                 opacityNoHover = TRUE, 
                 fontSize = 11, 
                 fontFamily = "calibri", 
                 zoom = TRUE, 
                 legend = TRUE)
  })
}

#### Run ####
shinyApp(ui = ui, server = server)
