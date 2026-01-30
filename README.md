# Network Analysis: Social Data Science Corvinus Masters
## Cohort Connectivity and Structural Analysis
### Project Overview
This repository contains the code and analysis for a network science project investigating the social structure of the Social Data Science (SDD) Corvinus Masters cohort. The project utilizes R to process survey data, visualize social ties, and calculate topological metrics.

The primary objective is to analyze the connectivity between students by differentiating between weak and strong ties. The analysis identifies key "hubs" (influencers), detects community clusters using the Walktrap algorithm, and visualizes the network topology using interactive graphs.

### Repository Structure
```
.
├── data/
│   └── SDD network.csv         # Raw adjacency data (Survey results)
├── scripts/
│   └── network_analysis.R      # Main R script for processing and visualization
└── README.md                   # Project documentation
```

### Setup and Requirements
The analysis is written in R. The script automatically checks for and installs the necessary dependencies if they are missing.

### Required Libraries:
-     dplyr: Data manipulation.
-     igraph: Network analysis and metric calculation.
-     visNetwork: Interactive network visualization.
-     this.path: Relative file path handling.
