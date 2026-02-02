# Reproducibility
seed <- 20251224
set.seed(seed)

# Packages: Ensuring all required libraries are installed and loaded ----
req <- c("dplyr", "igraph", "visNetwork", "this.path")
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

# Formatting as Adjacency Matrix
adj_mat <- as.matrix(raw_data[,-1])
rownames(adj_mat) <- raw_data[[1]]

# Logic:  NA = unknown 
#         0 = no connection 
#         1 = weak
#         2 = strong

# Graph 1: All connections (unweighted) ----
  # If value >= 1 then 1 (edge), else 0
mat1 <- adj_mat
mat1[is.na(mat1)] <- 0
mat1[mat1 > 0] <- 1

g1 <- graph_from_adjacency_matrix(mat1, mode = "directed", diag = FALSE)

visIgraph(g1) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visNodes(color = list(background = "lightblue", border = "darkblue")) %>%
  visEdges(arrows = "to") %>%
  visPhysics(stabilization = TRUE) %>%
  visLayout(randomSeed = seed)

# Graph 2: Only strong connections (value = 2) ----
mat2 <- adj_mat
mat2[is.na(mat2)] <- 0
mat2[mat2 != 2] <- 0
mat2[mat2 == 2] <- 1 

g2 <- graph_from_adjacency_matrix(mat2, mode = "directed", diag = FALSE)

visIgraph(g2) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visNodes(shape = "dot", color = list(background = "tomato", border = "darkred")) %>%
  visEdges(arrows = "to", color = "red") %>%
  visPhysics(stabilization = TRUE)

# Graph 3: Weighted connections (1 and 2) ----
mat3 <- adj_mat
mat3[is.na(mat3)] <- 0 

g3 <- graph_from_adjacency_matrix(mat3, mode = "directed", weighted = TRUE, diag = FALSE)

# Preparing data for visNetwork

data_vis <- toVisNetworkData(g3)
data_vis$edges$width <- data_vis$edges$weight * 10 

visNetwork(
  nodes = data_vis$nodes, 
  edges = data_vis$edges, 
  main = "Weighted Network", 
  submain = "Only 1-s and 2-s as connections"
) %>%
  # 1. Using a static igraph layout instead of live physics
  visIgraphLayout(layout = "layout_with_fr", randomSeed = seed) %>% 
  
  # 2. Disabling the physics engine entirely
  visPhysics(enabled = FALSE) %>% 
  
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visEdges(arrows = "to", color = list(color = "gray")) %>%
  visNodes(shadow = FALSE) %>% # Shadows also consume GPU/CPU resources
  visConfigure(enabled = FALSE) # Keeping configuration off saves resources

# Analyzing the Strong Connections Graph (g2) ----
# 1. Density: Ratio of actual edges to possible edges
density_g2 <- edge_density(g2)

# 2. Reciprocity: Proportion of mutual connections
reciprocity_g2 <- reciprocity(g2)

# 3. Transitivity (Clustering Coefficient): Probability that adjacent nodes are connected
clustering_g2 <- transitivity(g2, type = "global")

# 4. Average Path Length: Average number of steps between any two nodes
# Note: In a directed graph with disconnected components, this calculates the mean of existing paths
avg_path_g2 <- mean_distance(g2, directed = TRUE)

# 5. Diameter: The longest shortest path in the network
diameter_g2 <- diameter(g2, directed = TRUE)

# 6. Components: Number of isolated subgraphs
components_g2 <- components(g2)

cat("--- Strong Graph Measures ---\n")
cat("Density: ", round(density_g2, 4), "\n")
cat("Reciprocity: ", round(reciprocity_g2, 4), "\n")
cat("Global Clustering Coefficient: ", round(clustering_g2, 4), "\n")
cat("Average Path Length: ", round(avg_path_g2, 4), "\n")
cat("Diameter: ", diameter_g2, "\n")
cat("Number of isolated components: ", components_g2$no, "\n")


# Degree centrality (Who has the most connections?)
# In-degree: Who is most 'popular' (others marked them as 2)
# Out-degree: Who is most 'active' (they marked others as 2)
in_degree <- degree(g2, mode = "in")
out_degree <- degree(g2, mode = "out")

# Betweenness: Who acts as a bridge between different groups?
betweenness_g2 <- betweenness(g2, directed = TRUE)

# Table
hubs <- data.frame(
  ID = V(g2)$name,
  InDegree = in_degree,
  OutDegree = out_degree,
  BridgeScore = round(betweenness_g2, 2)
)

# Sorting by BridgeScore to find the most critical connectors ----
hubs <- hubs[order(-hubs$BridgeScore), ]
print(head(hubs, 5))


# Community detection (Walktrap algorithm) ----
cl <- cluster_walktrap(g2)
V(g2)$group <- cl$membership

# Converting to visNetwork data
data_comm <- toVisNetworkData(g2)

visNetwork(data_comm$nodes, data_comm$edges) %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visOptions(selectedBy = "group", highlightNearest = TRUE) %>%
  visPhysics(enabled = FALSE)
