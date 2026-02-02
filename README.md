# Network Analysis: Social Data Science Corvinus Masters
## Cohort Connectivity and Structural Analysis
### Project Overview
This repository contains the code and analysis for a network science project investigating the social structure of the Social Data Science (SDD) Corvinus Masters cohort. The project utilizes R to process survey data, visualize social ties, and calculate topological metrics.

The primary objective is to analyze the connectivity between students by differentiating between weak and strong ties. The analysis identifies key "hubs" (influencers), detects community clusters using the Walktrap algorithm, and visualizes the network topology using interactive graphs.

### Repository Structure
```
.
├── scripts/
│   ├── network_analysis.R      # Main R script for processing and visualization
│   └── SDS-network.html        # The output of the project with interactive visualizations
└── README.md                   # Project documentation
```

### Setup and Requirements
The analysis is written in R. The script automatically checks for and installs the necessary dependencies if they are missing.

### Required Libraries:
-   `dplyr`: Data manipulation.
-   `igraph`: Network analysis and metric calculation.
-   `visNetwork`: Interactive network visualization.
-   `this.path`: Relative file path handling.

### Data Description
The input file SDD `network.csv` is a comma-separated values file structured as an adjacency list or matrix of anonymized connections. The survey was filled out by SDS 2025/26 students (not full participation).
Rows: Source Node (Student ID)
Columns: Target Node (Student ID)
Values:
-   0: No tie
-   1: Weak tie
-   2: Strong tie

### Script Breakdown
The analysis is contained within a single execution script: network_analysis.R.

#### 1. Data Ingestion and Formatting
The script reads SDD `network.csv` and converts it into an adjacency matrix. It standardizes connection logic:
NA: Unknown/No Data (Converted to 0)

#### 2. Network Visualizations
The script generates three distinct interactive visualizations:

-   **Graph 1 (All Connections)**: An unweighted directed graph showing all ties (weak and strong). Nodes are rendered in light blue with visIgraph and a random layout.

-   **Graph 2 (Strong Connections)**: A filtered directed graph including only strong ties (value = 2). Nodes are rendered as red dots. This graph is used for the majority of the subsequent metric analysis.

-   **Graph 3 (Weighted Network)**: A weighted directed graph utilizing the `visNetwork` engine. Edge width is proportional to connection strength. Physics simulation is disabled in favor of a static Fruchterman-Reingold layout to optimize resource usage.

#### 3. Topological Analysis (Strong Connections)
The script calculates the following network measures based on Graph 2 (Strong connections only):

-   **Density**: The ratio of actual edges to possible edges.

-   **Reciprocity**: The proportion of mutual connections.

-   **Transitivity (Global Clustering)**: The probability that adjacent nodes are connected.

-   **Average Path Length**: The mean number of steps between any two nodes.

-   **Diameter**: The longest shortest path in the network.

-   **Components**: The count of isolated subgraphs.

#### 4. Centrality and Hub Detection
The script identifies critical nodes using three centrality measures:

-   **In-Degree**: Measure of "popularity" (how many students marked this node as a strong connection).

-   **Out-Degree**: Measure of "activity" (how many students this node marked as strong connections).

-   **Betweenness**: A bridge score indicating how often a node acts as a connector between different groups.

The output includes a table of the top 5 hubs sorted by Bridge Score.

#### 5. Community Detection
The analysis applies the Walktrap algorithm to the strong connection graph to identify subgroups. The final visualization colors nodes according to their detected community membership.

### License
MIT License (MIT): see the [License File](https://github.com/sensiolabs/GotenbergBundle/blob/1.x/LICENSE) for more details.

