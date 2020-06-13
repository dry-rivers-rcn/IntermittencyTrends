### Code for PCA in Fig. 1, Hammond et al. "Assessing spatial patterns and drivers of non-perennial flow in the continguous U.S." ###
### Edited by M.C. Mims, 2020.06.13 ###

# libraries
require(vegan)

# Set working directory (MCM laptop)
setwd("D:/Meryl - Professional/Projects/Active Projects/NSF_IR-RCN/Hammond paper")
source('colorRampPaletteAlpha.R', encoding = 'UTF-8')

# Read in John's dataset

  flow_vars <- read.delim("D:/Meryl - Professional/Projects/Active Projects/NSF_IR-RCN/Hammond paper/mean_annual_metrics_060320.txt")

# Subset to three vars identified by M. Zimmer and J. Hammond: annualfractionnoflow, firstnoflowcaly, p2z_mean

  flow_vars <- na.omit(flow_vars) #remove any rows with NAs
  target_vars <- (flow_vars[,2:4]) #remove any rows with NAs
  
# Explore data and evaluate need for transformation

  hist(log(target_vars$annualfractionnoflow)) #log transform due to right skew
  hist(target_vars$zeroflowfirst) #do not transform
  hist(log(target_vars$p2z)) #log transform due to high degree of right skew

# Transform appropriate variables, set column names

  log_AFNF <- log(target_vars$annualfractionnoflow)
  FirstnF <- target_vars$zeroflowfirst
  log_P2Z <- log(target_vars$p2z)
  
  trans_vars <- as.data.frame(cbind(log_AFNF, FirstnF, log_P2Z)) #creat new dataframe with 2 of 3 vars log transformed
  colnames(trans_vars) <- colnames(target_vars) #set column names for new dataframe

# Perform PCA
  
  PCA1 <- prcomp(trans_vars, scale = T) #scaled variables for unit variance
  summary(PCA1) #PCA output summary with proportion of variance
  evs <- as.data.frame(PCA1$sdev^2)
  loads <- as.data.frame(PCA1$rotation) #loadings
  loads
  scores <- as.data.frame(PCA1$x) # principal components (scores)

# Combine scores with Gage IDs for plotting

  Gage_scores <- as.data.frame(cbind(flow_vars[,1], scores))
  colnames(Gage_scores) <- c("gage_ID", "PC1", "PC2", "PC3")
  
#Calculate centroids for each group
  Gage_scores$region <- flow_vars$Aggregated_region
  pca.centroids <- aggregate(Gage_scores[,2:4], list(Type=Gage_scores$region), mean)
  
    
# Plot outputs
  plot(Gage_scores$PC1, Gage_scores$PC2) #exploratory
  
  # Set regional color scheme (matching map in first part of Fig. 1)
    region.col <- as.character(flow_vars$Aggregated_region)
    region.col <- sub("Eastern Forests", "springgreen4", region.col)
    region.col <- sub("Mediterranean California", "yellow", region.col)
    region.col <- sub("North Great Plains", "deepskyblue4", region.col)
    region.col <- sub("South Great Plains", "darkgoldenrod", region.col)
    region.col <- sub("Western Desert", "chocolate2", region.col)
    region.col <- sub("Western Mountains", "deepskyblue1", region.col)
    region.col_alph <- addalpha(region.col, 0.2)
    region.cent <- c("springgreen4", "yellow","deepskyblue4", "darkgoldenrod", "chocolate2", "deepskyblue1")
    region.cent_alph <- addalpha(region.cent, 0.8)

  # Set symbol type for Reference/Non-reference Gages
    gage.sym <- as.character(flow_vars$Class)
    gage.sym <- sub("Non-ref", 21, gage.sym)
    gage.sym <- sub("Ref", 22, gage.sym)
    gage.sym <- as.numeric(gage.sym)
    
    
  #Export plot
    
  png("Hammondetal_PCA_2020.06.13.png", width=8.0, height=8.0, units="in", res=300)
  
  par(mar=c(5,6,1,1), mgp=c(4,1,0))
  plot(Gage_scores$PC1, Gage_scores$PC2, type="n",  xlim=c(-4,4),ylim=c(-3.5,3), xaxt="n", main="",
       yaxt="n", xlab="PC1 (74.6%)", ylab="PC2 (14.7%)", cex.lab=1.5) #manually update % variation explained
  axis(2, at=c(-4.5,-3.0,-1.5,0,1.5,3.0), cex.axis=1.5, las=1.5)
  axis(1, at=c(-3.0,-1.5,0,1.5,3.0), cex.axis=1.5, las=1.5)
  points(Gage_scores$PC1, Gage_scores$PC2, cex=2, pch=gage.sym, col="grey60", bg=region.col_alph) #individual points
  points(pca.centroids$PC1, pca.centroids$PC2, cex=3.5, pch=21, col="grey20", bg=region.cent_alph) #centroids
  
  #Regional legend
  legcolors <- c("springgreen4","yellow","deepskyblue4","darkgoldenrod", "chocolate2", "deepskyblue1")
  legcolors_alph <- addalpha(legcolors, 0.4)
  legpch <- c(21, 21, 21, 21, 21, 21)
  leglabs <- c("Eastern Forests", "Mediterranean California", "North Great Plains", "South Great Plains", "Western Desert", "Western Mountains")
  legend("bottomright", leglabs, pch = legpch, pt.cex=2, col = "grey20", pt.bg=legcolors_alph, bty = "n")
  
  #Gage legend
  g_legcolor <- "grey80"
  g_legpch <- c(22,21)
  g_leglab <- c("Reference Gage", "Non-reference Gage")
  legend("topleft", g_leglab, pch = g_legpch, pt.cex=2, col = "grey20", pt.bg=g_legcolor, bty = "n")
  
  
  arrows(0,0,3*loads[1,1], 3*loads[1,2], lwd=3) #annualfractionnoflow
  arrows(0,0,3*loads[2,1], 3*loads[2,2], lwd=3) #firstnoflowcaly
  arrows(0,0,3*loads[3,1], 3*loads[3,2], lwd=3) #p2z_mean

  
  labels_x <- c(1.9,-1.5,-0.8) #adjusted manually for the plot
  labels_y <- c(-1,2,-2.5) #adjusted manually for the plot
  text(labels_x, labels_y, c("No flow fraction", "First no flow","Peak to zero"), cex=1.5) #labels requested by J.Hammond
  
  
  dev.off()

  
  #perMANOVA
  adonis(trans_vars~Aggregated_region+Class, data=flow_vars, permutations=1000, method="gower")

  #betadisper for regions
  trans_vars.d <- vegdist(trans_vars, "gower")
  vars.bd <- betadisper(trans_vars.d, flow_vars$Aggregated_region)
  anova(vars.bd)
  permutest(vars.bd, pairwise=TRUE)
  
  #betadisper for gage type
  trans_vars.d <- vegdist(trans_vars, "gower")
  vars.bd <- betadisper(trans_vars.d, flow_vars$Class)
  anova(vars.bd)
  permutest(vars.bd, pairwise=TRUE)
  
  
  #citations
  citation("vegan")
  