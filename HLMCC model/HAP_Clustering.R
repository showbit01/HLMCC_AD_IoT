library(caret) 
library(apcluster)
library(ggplot2)
library(cluster)
library(factoextra)
library(fossil)
library(fpc)
library(clues)


#--- load the datasets ---
rm(list=ls())
setwd("../Desktop/HLMCC/Datasets/original_Dataset/")
satellite_data <- read.csv("satellite-unsupervised-ad.csv", head = F)
str(satellite_data)

#------ Normalization  --------
normalized_satellite_data <- satellite_data
preprocessParams<-preProcess(satellite_data[,1:36], method="range" )
normalized_satellite_data <- predict(preprocessParams, satellite_data[,1:36])
head(normalized_satellite_data )
normalized_satellite_data[is.na(normalized_satellite_data)]<-0


#------ Visualization  --------
tiff("../Desktop/HLMCC/Plots/LandsatSatellite.tiff", units="in", width=8, height=6, res=600)
Label = satellite_data$V37
satellite_plot <- ggplot(satellite_data , aes(x=V1, y=V2  , color=Label) ) + geom_point() +  theme(text = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size=5)))
satellite_plot  + scale_color_manual(labels = c("Normal", "Anomaly"), breaks = c("n", "o"),
                                     values=c("#56B4E9", "red")) + ggtitle("Landsat Satellite") 
dev.off()

#------------ Hierarchical Affinity propagation-------------
sim <- negDistMat(normalized_satellite_data, r=1)
##run affinity propagation
apres <- apcluster(sim,  details=TRUE)
## show details of clustering results
show(apres)
## employ agglomerative clustering to join clusters
aggres <- aggExCluster(sim, apres)
## show information
show(aggres)

#----- label 
Label_HAP= labels(cutree(aggres,2)  , type ="enum" ) ## "1" = "anomaly" class And "2" = "normal"
table( Pred = Label_HAP, True = satellite_data$V37)

# --------- silhouette 
sil <- silhouette(Label_HAP, dist(normalized_satellite_data))
tiff("../Desktop/HLMCC/Plots/HAP_sil.tiff", units="in", width=8, height=6, res=600)
fviz_silhouette(sil) + theme(text = element_text(size=30))
dev.off()
table(Label_HAP)

#-------- adjustedRand 
labels_true <- as.integer(satellite_data$V37)
labels_pred_HAP <- Label_HAP
adjustedRand(labels_true,labels_pred_HAP , randMethod = "HA")

#------ Visualization for HAP 
tiff("../Desktop/HLMCC/Plots/satelliteHAP.tiff", units="in", width=8, height=6, res=600)
Label_HAP <- factor(Label_HAP)
HAP_plot <- ggplot(satellite_data , aes(x=V1, y=V2  , color=Label_HAP)) + geom_point() +  theme(text = element_text(size=30)) + guides(color = guide_legend(override.aes = list(size=5)))
HAP_plot  + scale_color_manual(labels = c( "Normal",   "Anomaly" ), breaks = c("2", "1"),
                               values=c(  "red", "#56B4E9" )) + ggtitle("Landsat Satellite - HAP")
dev.off()

