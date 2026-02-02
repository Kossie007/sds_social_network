# Reproducibility
seed <- 20251224
set.seed(seed)

# Packages
req <- c("dplyr", "igraph", "visNetwork", "kableExtra", "this.path")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) {  
  install.packages(to_install, repos = "https://cloud.r-project.org")  
}
invisible(lapply(req, library, character.only = TRUE)) 

# Setting working directory to the current script location
setwd(this.path::here())
rm(req, to_install)


# Reading the data ----
raw_data <- read.csv("SDD network.csv", sep = ";", na.strings = c("", "NA"))
head(raw_data)


# Adjacency Matrix
# Logic: NA = unknown 
#        0 = no connection  
#        1 = weak 
#        2 = strong
adj_mat <- as.matrix(raw_data[,-1])
rownames(adj_mat) <- raw_data[[1]]
adj_mat[is.na(adj_mat)] <- 0
colnames(adj_mat) <- gsub("^X", "", colnames(adj_mat))
head(adj_mat)

# Whole class (respondents and non-respondents) network visualizations ----
## All connections (unweighted)

# If value >= 1 then 1 (edge), else 0
mat1 <- adj_mat
mat1[mat1 > 0] <- 1

g1 <- graph_from_adjacency_matrix(mat1, mode = "directed", diag = FALSE)

visIgraph(g1) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visNodes(color = list(background = "lightblue", border = "darkblue")) %>%
  visEdges(arrows = "to") %>%
  visPhysics(stabilization = TRUE) %>%
  visLayout(randomSeed = seed)


# Only strong connections (value = 2)
mat2 <- adj_mat
mat2[mat2 != 2] <- 0
mat2[mat2 == 2] <- 1 

g2 <- graph_from_adjacency_matrix(mat2, mode = "directed", diag = FALSE)

visIgraph(g2) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visNodes(shape = "dot", color = list(background = "tomato", border = "darkred")) %>%
  visEdges(arrows = "to", color = "red") %>%
  visPhysics(stabilization = TRUE) %>%
  visLayout(randomSeed = seed)


# Weighted connections (1 and 2)
g3 <- graph_from_adjacency_matrix(adj_mat, mode = "directed", weighted = TRUE, diag = FALSE)

# Preparing data for visNetwork

data_vis <- toVisNetworkData(g3)
data_vis$edges$width <- data_vis$edges$weight * 10 

visNetwork(
  nodes = data_vis$nodes, 
  edges = data_vis$edges, 
  main = list(text = "Weighted Network", style = "color:black;font-size:20px;"),  
  submain = list(text = "Only 1-s and 2-s as connections", style = "color:black;font-size:15px;")
) %>%
  visIgraphLayout(layout = "layout_with_fr", randomSeed = seed) %>% 
  visPhysics(enabled = FALSE) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visEdges(
    arrows = "to", 
    color = list(
      color = "rgba(0, 0, 0, 0.3)",       
      highlight = "rgba(255, 0, 0, 1)"   
    )
  ) %>%
  visNodes(
    color = list(
      background = "black", 
      border = "black", 
      highlight = "red"
    ),
    font = list(color = "black")
  )


## Analyzing the Strong Connections Graph (g2) ----
g2_metrics_all <- data.frame(
  Metric = c(
    # 1. Density: Ratio of actual edges to possible edges
    "Density",
    # 2. Reciprocity: Proportion of mutual connections
    "Reciprocity",
    # 3. Transitivity (Clustering Coefficient): Probability that adjacent nodes are connected
    "Global Clustering Coefficient",
    # 4. Average Path Length: Average number of steps between any two nodes
    "Average Path Length",
    # 5. Diameter: The longest shortest path in the network
    "Diameter",
    # 6. Components: Number of isolated subgraphs
    "Number of Isolated Components",
    # 7. Size of the largest component
    "Size of Largest Component",
    # 8. Average degree
    "Average Degree",
    # 9. Maximum indegree
    "Maximum In-Degree",
    # 10. Maximum outdegree
    "Maximum Out-Degree",
    # 11. Minimum indegree
    "Minimum In-Degree",
    # 12. Minimum outdegree
    "Minimum Out-Degree"
  ),
  Value = c(
    round(edge_density(g2), 4),
    round(reciprocity(g2), 4),
    round(transitivity(g2, type = "global"), 4),
    round(mean_distance(g2, directed = TRUE), 4),
    diameter(g2, directed = TRUE),
    components(g2)$no,
    max(components(g2)$csize),
    round(mean(degree(g2)), 4),
    max(degree(g2, mode = "in")),
    max(degree(g2, mode = "out")),
    min(degree(g2, mode = "in")),
    min(degree(g2, mode = "out"))
  )
)


kbl(g2_metrics_all, caption = "Summary Metrics for Strong Connections Network") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = F, 
                position = "center") %>%
  row_spec(0, background = "black", color = "white") %>% 
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, color = "black")


## Identifying Key Agents and Communities ---- 
# Degree centrality (Who has the most connections?)
hubs <- data.frame(
  ID = V(g2)$name,
  # In-degree: Who is most 'popular' (others marked them as 2)
  InDegree = degree(g2, mode = "in"),
  # Out-degree: Who is most 'active' (they marked others as 2)
  OutDegree = degree(g2, mode = "out"),
  # Betweenness: Who acts as a bridge between different groups?
  BridgeScore = round(betweenness(g2, directed = TRUE), 2)
)

# Sorting by BridgeScore to find the most critical connectors
hubs <- hubs[order(-hubs$BridgeScore), ]
print(head(hubs, 5))


# Community detection (Walktrap algorithm)
g2_comm <- cluster_walktrap(g2)
V(g2)$group <- g2_comm$membership

data_comm <- toVisNetworkData(g2)

visNetwork(data_comm$nodes, data_comm$edges, main = list(text = "Community Detection (g2)", style = "color:black;font-size:20px;")) %>%
  visIgraphLayout(layout = "layout_with_fr", randomSeed = seed) %>%
  visOptions(selectedBy = "group", highlightNearest = TRUE) %>%
  visPhysics(enabled = FALSE) %>%
  visEdges(arrows = "to")

# Respondent-only network visualization

# Identifying respondents (those who submitted data)
has_sent_data <- rowSums(adj_mat > 0, na.rm = TRUE) > 0
respondent_ids <- rownames(adj_mat)[has_sent_data]

# Cleaning subset matrix containing only those who responded
mat_r_base <- adj_mat[respondent_ids, respondent_ids, drop = FALSE]
head(mat_r_base)

## All connections of respondents (unweighted)

# --- g1_r: All connections (Binary) ---
mat1_r <- mat_r_base
mat1_r[mat1_r > 0] <- 1
g1_r <- graph_from_adjacency_matrix(mat1_r, mode = "directed", diag = FALSE)

visIgraph(g1_r) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visNodes(color = list(background = "lightblue", border = "darkblue")) %>%
  visEdges(arrows = "to") %>%
  visPhysics(stabilization = TRUE) %>%
  visLayout(randomSeed = seed)

## Strong (2) connections only

# --- g2_r: Strong connections only (Value = 2) ---
mat2_r <- mat_r_base
mat2_r[mat2_r != 2] <- 0
mat2_r[mat2_r == 2] <- 1
g2_r <- graph_from_adjacency_matrix(mat2_r, mode = "directed", diag = FALSE)

visIgraph(g2_r) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visNodes(shape = "dot", color = list(background = "tomato", border = "darkred")) %>%
  visEdges(arrows = "to", color = "red") %>%
  visPhysics(stabilization = TRUE) %>%
  visLayout(randomSeed = seed)


## Weighted connections (1-2)

# --- g3_r: Weighted connections ---
g3_r <- graph_from_adjacency_matrix(mat_r_base, mode = "directed", weighted = TRUE, diag = FALSE)

data_vis_r <- toVisNetworkData(g3_r)
data_vis_r$edges$width <- data_vis_r$edges$weight * 10 

# Weighted Network for Respondents (g3_r)
data_vis_r <- toVisNetworkData(g3_r)
# Scaling width for better visibility
data_vis_r$edges$width <- data_vis_r$edges$weight * 4

visNetwork(
  nodes = data_vis_r$nodes, 
  edges = data_vis_r$edges, 
  main = list(text = "Weighted Network of Respondents", style = "color:black;font-size:20px;"), 
  submain = list(text = "Thickness represents tie strength (1 vs 2)", style = "color:black;font-size:15px;")
) %>%
  visIgraphLayout(layout = "layout_with_fr", randomSeed = seed) %>% 
  visPhysics(enabled = FALSE) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visEdges(
    arrows = "to", 
    color = list(
      color = "rgba(0, 0, 0, 0.25)",     
      highlight = "rgba(255, 0, 0, 1)"  
    )
  ) %>%
  visNodes(
    color = list(
      background = "black", 
      border = "black", 
      highlight = "red"
    ),
    font = list(color = "black", size = 14)
  )

##  Analyzing the Strong Connections Graph of Respondents (g2)

g2_metrics <- data.frame(
  Metric = c(
    # 1. Density: Ratio of actual edges to possible edges
    "Density",
    # 2. Reciprocity: Proportion of mutual connections
    "Reciprocity",
    # 3. Transitivity (Clustering Coefficient): Probability that adjacent nodes are connected
    "Global Clustering Coefficient",
    # 4. Average Path Length: Average number of steps between any two nodes
    "Average Path Length",
    # 5. Diameter: The longest shortest path in the network
    "Diameter",
    # 6. Components: Number of isolated subgraphs
    "Number of Isolated Components",
    # 7. Size of the largest component
    "Size of Largest Component",
    # 8. Average degree
    "Average Degree",
    # 9. Maximum indegree
    "Maximum In-Degree",
    # 10. Maximum outdegree
    "Maximum Out-Degree",
    # 11. Minimum indegree
    "Minimum In-Degree",
    # 12. Minimum outdegree
    "Minimum Out-Degree"
  ),
  Value = c(
    round(edge_density(g2_r), 4),
    round(reciprocity(g2_r), 4),
    round(transitivity(g2_r, type = "global"), 4),
    round(mean_distance(g2_r, directed = TRUE), 4),
    diameter(g2_r, directed = TRUE),
    components(g2_r)$no,
    max(components(g2_r)$csize),
    round(mean(degree(g2_r)), 4),
    max(degree(g2_r, mode = "in")),
    max(degree(g2_r, mode = "out")),
    min(degree(g2_r, mode = "in")),
    min(degree(g2_r, mode = "out"))
  )
)

# Display the table
kbl(g2_metrics, caption = "Summary Metrics for Strong Connections Network (Respondents Only)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = F, 
                position = "center") %>%
  row_spec(0, background = "black", color = "white") %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, color = "black")


## Identifying Key Agents and Communities
# Degree centrality (Who has the most connections?)
hubs_r <- data.frame(
  ID = V(g2_r)$name,
  # In-degree: Who is most 'popular' (others marked them as 2)
  InDegree = degree(g2_r, mode = "in"),
  # Out-degree: Who is most 'active' (they marked others as 2)
  OutDegree = degree(g2_r, mode = "out"),
  # Betweenness: Who acts as a bridge between different groups?
  BridgeScore = round(betweenness(g2_r, directed = TRUE), 2)
)

# Sort by BridgeScore to find the most critical connectors
hubs_r <- hubs_r[order(-hubs_r$BridgeScore), ]
print(head(hubs_r, 5))
 
g2_comm_r <- cluster_walktrap(g2_r)
V(g2_r)$group <- g2_comm_r$membership

data_comm_r <- toVisNetworkData(g2_r)

visNetwork(data_comm_r$nodes, data_comm_r$edges, main = list(text = "Community Detection (g2)", style = "color:black;font-size:20px;")
) %>%
  visIgraphLayout(layout = "layout_with_fr", randomSeed = seed) %>%
  visOptions(selectedBy = "group", highlightNearest = TRUE) %>%
  visPhysics(enabled = FALSE) %>%
  visEdges(arrows = "to")
