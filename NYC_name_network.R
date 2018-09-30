#Benjamin Gordon
#May 5th, 2018
#Script to run a network analysis of New York baby names based on ethnicity and gender.
#Source: 
#NYC OpenData. (2018). Most Popular Baby Names by Sex and Mother's Ethnic Group, New York City (Version 1) [CSV]. Retrieved from https://catalog.data.gov/dataset/most-popular-baby-names-by-sex-and-mothers-ethnic-group-new-york-city-8c742

#Packages to install, currently commented out
#install.packages("network", dep = TRUE, type = "source")
#install.packages("ggplot2")
#install.packages("ergm")
#install.packages("igraph") 
#install.packages("statnet", dependencies = TRUE) 

#Libraries to load
library(network)
library(statnet)
library(ggplot2)
library(sna)
library(ergm)

# Read CSV into R from working directory
setwd("~/Documents/JuniorSpring/Data Visualization/NETWORK")
csv.df <- read.csv(file="Most_Popular_Baby_Names_by_Sex_and_Mother_s_Ethnic_Group__New_York_City.csv", header=TRUE, sep=",")
head(csv.df)

#Get rid of doubles by using unique and getting the max number of names
no_doubles.df <- unique(csv.df)
max_names <- which(no_doubles.df$Count == max(no_doubles.df$Count)) #find the max
no_doubles.df[max_names,] #print the max

#Get all the names and ethnicities lower cased to create consistency
no_doubles.df$Child.s.First.Name <- tolower(no_doubles.df$Child.s.First.Name)
no_doubles.df$Ethnicity <- tolower(no_doubles.df$Ethnicity)

#Loop through data frame and fix typos in the ethincity column
for(i in 1:nrow(no_doubles.df)){
  if(no_doubles.df$Ethnicity[i] == "asian and paci")
    no_doubles.df$Ethnicity[i] <- "asian and pacific islander"
  if(no_doubles.df$Ethnicity[i] == "black non hisp")
    no_doubles.df$Ethnicity[i] <- "black non hispanic"
  if(no_doubles.df$Ethnicity[i] == "white non hisp")
    no_doubles.df$Ethnicity[i] <- "white non hispanic"
}

#Make new data frame and take out the year and rank columns
names_no_year <- no_doubles.df
names_no_year$Year.of.Birth <- NULL
names_no_year$Rank <- NULL

#Sort the data frame first by ethnicity, then by name
attach(names_no_year)
names_no_year <- names_no_year[order(Ethnicity),]
attach(names_no_year)
names_no_year <- names_no_year[order(Child.s.First.Name),]

#Create a current index, curr item and empty data frame before looping through csv
curr <- names_no_year[1,]
index <- 1
aggregate_names <- data.frame()

#Loop through the file without doubles and aggregate equal ethnicities and names
for(i in 2:nrow(names_no_year)){
  ethnicity_name <- curr$Child.s.First.Name == names_no_year[i,]$Child.s.First.Name && curr$Ethnicity == names_no_year[i,]$Ethnicity
  
  #if names and ethnicites are both equal, add counts together
  if(ethnicity_name){
      curr$Count <- curr$Count + names_no_year[i,]$Count
  }
  #if name or ethnicity are not equal, then add curr to the data frame and index forward 
  else {
    for(j in 1:ncol(names_no_year)){
      aggregate_names[index,j] <- curr[j] #add curr to data frame
    }
    curr <- names_no_year[i,] #set new curr
    index <- index + 1 #increase index
  }
}

#Sort the aggregate names before assigning id's
attach(aggregate_names)
aggregate_names <- aggregate_names[order(Child.s.First.Name),]
aggregate_names$id <- NULL

#Assign id's to each row so we can enter in as a node list
for(i in 1:nrow(aggregate_names)){
  aggregate_names$id[i] <- i;
}

#Now create 4 separate data frames by each ethnicity
hispanic.v <- which(aggregate_names$Ethnicity == "hispanic")
white.v <- which(aggregate_names$Ethnicity == "white non hispanic")
asian.v <- which(aggregate_names$Ethnicity == "asian and pacific islander")
black.v <- which(aggregate_names$Ethnicity == "black non hispanic")
hispanic.df <- aggregate_names[hispanic.v,]
white.df <- aggregate_names[white.v,]
asian.df <- aggregate_names[asian.v,]
black.df <- aggregate_names[black.v,]

#now do the same thing but for ethnicities and find the aggregate of names without race
aggregate_names_no_ethnicity <- aggregate_names

#Sort by name
attach(aggregate_names_no_ethnicity)
aggregate_names_no_ethnicity <- aggregate_names_no_ethnicity[order(Child.s.First.Name),]

#Create a current index, curr item and empty data frame before looping through file
curr <- aggregate_names_no_ethnicity[1,]
index <- 1
name_counts <- data.frame()

#Loop through the file without doubles and aggregate equal names
for(i in 2:nrow(aggregate_names_no_ethnicity)){
  #if names are both equal, add counts together
  if(curr$Child.s.First.Name == aggregate_names_no_ethnicity[i,]$Child.s.First.Name){
    curr$Count <- curr$Count + aggregate_names_no_ethnicity[i,]$Count
  }
  #if name or ethnicity are not equal, then add curr to the data frame and index forward 
  else {
    for(j in 1:ncol(aggregate_names_no_ethnicity)){
      name_counts[index,j] <- curr[j] #add curr to data frame
    }
    curr <- aggregate_names_no_ethnicity[i,] #set new curr
    index <- index + 1 #increase index
  }
}

#Drop ethnicity column from the name counts
name_counts$Ethnicity <- NULL

#Create network variable with ethnicity as the vertex
#net <- as.network(2689, directed = FALSE, loops = FALSE, matrix.type = "list")
#network.vertex.names(net) <- aggregate_names$Child.s.First.Name
#set.vertex.attribute(net,"Ethnicity", aggregate_names$Ethnicity)

#Create a matrix for each race function to be run
create_edge_matrix_one_ethnicity <- function(names.df){
  edges <- as.vector(names.df$id)
  out.matrix <- matrix(nrow = nrow(names.df), ncol = nrow(names.df))
  colnames(out.matrix) <- edges
  rownames(out.matrix) <- edges
  
  #loop through and initialize matrix items
  for(i in 1:nrow(names.df)){
    for(j in 1:nrow(names.df)){
      if(i!=j){
        out.matrix[i,j] <- 0
      }
    }
  }
  return (out.matrix)
}

#Create matrices by using create_edge_matrix_one_ethnicity
white.matrix <- create_edge_matrix_one_ethnicity(white.df)
hispanic.matrix <- create_edge_matrix_one_ethnicity(hispanic.df)
black.matrix <- create_edge_matrix_one_ethnicity(black.df)
asian.matrix <- create_edge_matrix_one_ethnicity(asian.df)

#Create new data frame that holds edges
aggregate_names_with_edges <- aggregate_names

#Initialize list as 0's. 0's will be no edge, 1 is an edge.
net_length <- nrow(aggregate_names_with_edges)
aggregate_names_with_edges$edge1 <- 0
aggregate_names_with_edges$edge2 <- 0
aggregate_names_with_edges$edge3 <- 0
curr_next_4 <- aggregate_names_with_edges[2:4,]

#Get edges in the list by checking common names by ethnicity
for(i in 1:(nrow(aggregate_names_with_edges)-4)){
  curr <- aggregate_names_with_edges[i,]
  index <- 1
  curr_next_4 <- aggregate_names_with_edges[i:(i+3),]
  for(j in 1:4){
    if(curr$Child.s.First.Name==aggregate_names_with_edges[i+j,]$Child.s.First.Name){
      if(index == 1){
        aggregate_names_with_edges[i,]$edge1 <- i+j
        index <- index + 1
      }
      else if(index == 2){
        aggregate_names_with_edges[i,]$edge2 <- i+j
        index <- index + 1
      }
      else if(index == 3){
        aggregate_names_with_edges[i,]$edge3 <- i+j
        index <- index + 1
      }
    }    
  }
}

#Create an edge matrix based on common names
edges <- aggregate_names_with_edges
edges$Gender <- NULL
edges$Child.s.First.Name <- NULL
edges$Ethnicity <- NULL
edges$Gender <- NULL
edges$Count <- NULL
edges_matrix <- as.matrix(edges)

#Now add name_matrix and initialize all as 0's
name_matrix <- matrix(nrow = net_length, ncol = net_length)
for(i in 1:net_length){
  for(j in 1:net_length){
    name_matrix[i,j] <- 0
  }
}

#for(i in 1:net_length){
#  if(edges[i,]$edge1 > 0){
#    name_matrix[i,edges[i,]$edge1] <- 1
#    name_matrix[edges[i,]$edge1,i] <- 1
#    if(edges[i,]$edge2 > 0){
#      name_matrix[i,edges[i,]$edge2] <- 1
#      name_matrix[edges[i,]$edge2,i] <- 1
#      if(edges[i,]$edge3 > 0){
#        name_matrix[i,edges[i,]$edge3] <- 1
#        name_matrix[edges[i,]$edge3,i] <- 1
#      }
#    }
#  }
#}
for(i in 1:net_length){
      if(edges[i,]$edge3 > 0){
        name_matrix[i,edges[i,]$edge3] <- 1
        name_matrix[edges[i,]$edge3,i] <- 1
        name_matrix[i,edges[i,]$edge2] <- 1
        name_matrix[edges[i,]$edge2,i] <- 1
        name_matrix[i,edges[i,]$edge1] <- 1
        name_matrix[edges[i,]$edge1,i] <- 1
      }
}

#Create the common name network by combining the edge lists into each category
common_name_network <- as.network(name_matrix, directed = TRUE, loops = FALSE, matrix.type = "adjacency")
aggregate_names_with_edges$Gender <- as.character(aggregate_names_with_edges$Gender)
set.vertex.attribute(common_name_network,"Gender", aggregate_names_with_edges$Gender)
aggregate_names_with_edges$Child.s.First.Name <- as.character(aggregate_names_with_edges$Child.s.First.Name)
set.vertex.attribute(common_name_network,"Name", aggregate_names_with_edges$Child.s.First.Name)
aggregate_names_with_edges$Ethnicity <- as.character(aggregate_names_with_edges$Ethnicity)
set.vertex.attribute(common_name_network,"Ethnicity", aggregate_names_with_edges$Ethnicity)
set.vertex.attribute(common_name_network,"Count", aggregate_names_with_edges$Count)

#Function that creates network plot and data frame.
#Takes in a minimum count and a maximum count.
#Ex. the names being plotted must have at least a count of 500 and a maximum of 1500 names.
plot_network_limit_count_all_ethnicities <- function(new.df, count, top){
  df <- new.df
  df <- df[which(df$Count>count),]
  df <- df[which(df$Count< top),]
  length <- nrow(df)
  
  df$Rank <- 0
  attach(df)
  df <- df[order(-Count),]
  for(i in 1:length){
    df[i,]$Rank <- i
  }
  
  attach(df)
  df <- df[order(Child.s.First.Name),]
  df$edge1 <- 0
  df$edge2 <- 0
  df$edge3 <- 0
  curr_next_4 <- df[2:4,]
  
  #Get edges in the list
  for(i in 1:(length-4)){
    curr <- df[i,]
    index <- 1
    curr_next_4 <- df[i:(i+3),]
    for(j in 1:4){
      if(curr$Child.s.First.Name==df[i+j,]$Child.s.First.Name){
        if(index == 1){
          df[i,]$edge1 <- i+j
          index <- index + 1
        }
        else if(index == 2){
          df[i,]$edge2 <- i+j
          index <- index + 1
        }
        else if(index == 3){
          df[i,]$edge3 <- i+j
          index <- index + 1
        }
      }    
    }
  }
  
  edges <- df
  edges$Gender <- NULL
  edges$Child.s.First.Name <- NULL
  edges$Ethnicity <- NULL
  edges$Gender <- NULL
  edges$Count <- NULL
  edges_matrix <- as.matrix(edges)
  
  
  name_matrix <- matrix(nrow = length, ncol = length)
  
  for(i in 1:length){
    for(j in 1:length){
      name_matrix[i,j] <- 0
    }
  }
  
for(i in 1:length){
  if(edges[i,]$edge1 > 0){
    name_matrix[i,edges[i,]$edge1] <- 1
    name_matrix[edges[i,]$edge1,i] <- 1
    if(edges[i,]$edge2 > 0){
      name_matrix[i,edges[i,]$edge2] <- 1
      name_matrix[edges[i,]$edge2,i] <- 1
      if(edges[i,]$edge3 > 0){
        name_matrix[i,edges[i,]$edge3] <- 1
        name_matrix[edges[i,]$edge3,i] <- 1
      }
    }
  }
}
  
  name_network <- as.network(name_matrix, directed = TRUE, loops = FALSE, matrix.type = "adjacency")
  
  df$Gender <- as.character(df$Gender)
  set.vertex.attribute(name_network,"Gender",df$Gender)
  
  df$Child.s.First.Name <- as.character(df$Child.s.First.Name)
  set.vertex.attribute(name_network,"Name", df$Child.s.First.Name)
  
  df$Ethnicity <- as.character(df$Ethnicity)
  set.vertex.attribute(name_network,"Ethnicity", df$Ethnicity)
  
  node_colors <- rep("",length)
  for(i in 1:length){
    if(get.node.attr(name_network,"Gender")[i] == "FEMALE"){
      if(get.node.attr(name_network, "Ethnicity")[i] == "hispanic")
        node_colors[i] <- "maroon"
      else if(get.node.attr(name_network, "Ethnicity")[i] == "white non hispanic")
        node_colors[i] <- "firebrick3"
      else if(get.node.attr(name_network, "Ethnicity")[i] == "black")
        node_colors[i] <- "red"
      else if(get.node.attr(name_network, "Ethnicity")[i] == "asian and pacific islander")
        node_colors[i] <- "deeppink"
      else
        node_colors[i] <- "hotpink"
    }else{
      if(get.node.attr(name_network, "Ethnicity")[i] == "hispanic")
        node_colors[i] <- "light blue"
      else if(get.node.attr(name_network, "Ethnicity")[i] == "white non hispanic")
        node_colors[i] <- "dodgerblue4"
      else if(get.node.attr(name_network, "Ethnicity")[i] == "black")
        node_colors[i] <- "cyan"
      else if(get.node.attr(name_network, "Ethnicity")[i] == "asian and pacific islander")
        node_colors[i] <- "aquamarine"
      else
        node_colors[i] <- "blue"
    }
  }
  
  plot.network(name_network,  
               vertex.col = node_colors,
               vertex.cex = (df$Count)/450, 
               usearrows = FALSE,
               label = paste(df$Child.s.First.Name, df$Rank),
               label.cex = (df$Count)/1500,
               label.col = 'black',
               label.pos = 1,
               vertices.last = TRUE,
               edge.col = 'black',
               bg = 'grey',
               main = "Top Baby Names in NYC by Ethnicity and Gender (2011-2014)",
               sub = "Name and rank in label, nodes are proportional by count of names"
               #,new = FALSE
  )
  #Save plot as a jpeg
  
  output <- paste("network","min",count,"max", top, sep = "_", collapse = NULL)
  output <- paste(output, ".jpg", sep = "", collapse = NULL)
  dev.copy(jpeg,filename=output);
  dev.off ();
  
  return (df)
}

#Create the data frame and plot, saving the plot to a file.
df <- plot_network_limit_count_all_ethnicities(aggregate_names_with_edges, 500, 1500)
