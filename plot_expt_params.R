library(pastecs)
library(caret)
library(ggplot2)
library(stringr)

exp_Data <- read.csv("/Volumes/PhD/BijelData/Bijel_Data_Cleaner_ToRead.csv", na.strings = "?")
exp_Data$Sample.Number <- as.character(exp_Data$Sample.Number)
rownames(exp_Data) <- exp_Data$Sample.Number

exp_plot_Data <- data.frame(Sample=exp_Data$Sample.Number, HMDS.g=exp_Data$HMDS..g, Nitromethane.m.f=exp_Data$Nitromethane.mass.fraction, Bijel=exp_Data$Bijel)
exp_plot_Data

batch_num <- as.numeric(str_extract(exp_plot_Data$Sample, "[0-9]+"))

sample_num <- str_extract(exp_plot_Data$Sample, "[aA-zZ]+")
sample_num <- nchar(sample_num)


exp_plot_Data$Batch.Number <- batch_num
exp_plot_Data$Sample.Number <- sample_num

index1=ifelse(exp_plot_Data$Sample.Number==1, TRUE, FALSE)
index2=ifelse(exp_plot_Data$Sample.Number==2, TRUE, FALSE)
dat1 <- exp_plot_Data[index1,]
dat2 <- exp_plot_Data[index2,]

png("experiment_params.png", width=1600, height=800)
plot(dat1$HMDS.g, dat1$Nitromethane.m.f, col=dat1$Bijel, pch=3, xlab="HMDS/g", ylab="Nitromethane mass fraction", cex=2, , cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)
points(dat2$HMDS.g, dat2$Nitromethane.m.f, col=dat2$Bijel, pch=4, cex=2)
legend("bottomright", legend=c("Sample 1", "Sample 2", "Bijel", "Non-bijel"), col=c(rep("gray",2),"black", "red"), pch=c(3,4,8,8), ncol=2, cex=2)
dev.off()
