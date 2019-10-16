
rm(list = ls())


library(plyr)
library(ggplot2)

library(RColorBrewer)

mrb.names <- list('Pacific Northwest','Pacific Coast','Southwest',
                  
                  'Missouri River', 'Midwest','Northeast','Southeast',
                  
                  'South Central')

cols <- brewer.pal(length(mrb.names), 'Dark2')

ylabel = expression('Cummulative Storage, '*log[10]*'(['*m^{3}*'/yr])')
xlabel = "Stream Order"

par(mfrow=c(1,1),cex.lab = 1.3,cex.axis = 1, cex.main = 1,mar=c(3,4.2,2,1), mgp = c(1.8,0.5,0))



plot(1:10,1:10, type = 'n', ylim = c(3,11), xlim = c(.8,9.5),
     
     xlab = xlabel, ylab = ylabel, xaxt="n", yaxt="n")
axis(1, at=1:10,labels=1:10)
axis(2, at=2:11,labels=2:11)
abline(h = 2:11, v = 1:10, col = 'lightgrey')

de <- data.frame(xx = 1:10)

for(my.mrb in 1:length(mrb.names)){
  
  
  
  VolsData <- readRDS(paste("data/",mrb.names[[my.mrb]],"_data_Year.rds", sep = ''))
  
  data <- VolsData$data
  so <- data$so
  
  logVol_up <- data$logVol_up_m3_yr
  logVol_down <- data$logVol_down_m3_yr
  deltaVol <- 10^logVol_down - 10^logVol_up

  volTot <- c()

  for(my.so in 1:max(so)){
    my.so
    toUse <- which(so == my.so)
    volTot <- c(volTot,sum(deltaVol[toUse], na.rm = T))

  }

  points(1:max(so),log10(volTot), col = cols[my.mrb], pch = 16, cex=1.8,lwd=2)
  
  lines(1:max(so),log10(volTot), col = cols[my.mrb], pch = 16, cex=2,lwd=2)
  Data_name_a <- toString(my.mrb)
  Data_name_b <- paste0("vol_")
  Data_name <- paste0(Data_name_b, Data_name_a)
  
   d <- data.frame(volTot)
   d <- rename(d, c("volTot"=Data_name))   
   de <- append(de,d)
   
}

legend(.8,7, legend=c("PNW", "PC", "SW", "MR", "MW", "NE", "SE", "SC"),
       
       col=cols, lwd=2, lty=1, cex=0.8)

#############################################

V_1 <- data.frame(de$vol_1)
V_1[9,] <- NA
V_2 <- data.frame(de$vol_2)
V_2[8:9,] <- NA
V_3 <- data.frame(de$vol_3)
V_4 <- data.frame(de$vol_4)
V_5 <- data.frame(de$vol_5)
V_5[9,] <- NA
V_6 <- data.frame(de$vol_6)
V_6[7:9,] <- NA
V_7 <- data.frame(de$vol_7)
V_7[8:9,] <- NA
V_8 <- data.frame(de$vol_8)
V_8[8:9,] <- NA

V_1[,2] <- seq(1,9)
V_2[,2] <- seq(1,9)
V_3[,2] <- seq(1,9)
V_4[,2] <- seq(1,9)
V_5[,2] <- seq(1,9)
V_6[,2] <- seq(1,9)
V_7[,2] <- seq(1,9)
V_8[,2] <- seq(1,9)
V <- merge(V_1,V_2)
V <- merge(V,V_3)
V <- merge(V,V_4)
V <- merge(V,V_5)
V <- merge(V,V_6)
V <- merge(V,V_7)
V <- merge(V,V_8)

###############
V <- V[1:8,]
p <- ggplot(V)

{
  mrb.names.nospace <- list('PNW','PC','SW','MR', 'MW','NE','SE','SC')
  MRBcol <- c("forestgreen","royalblue4","darkorange","darkorange","royalblue4","royalblue4",
              "royalblue4","forestgreen")
  Pnt_type <- c(1,10,1,0,7,0,1,0)
  Pnt_type <- c(0,1,2,5,6,7,9,10)
  Ln_type <- c(1,10,1,0,7,0,1,0)
  Ln_color <- c("forestgreen","royalblue4","darkorange","darkorange","royalblue4","royalblue4",
                "royalblue4","forestgreen")
  Plot_MRBs_Col <- data.frame(MRB = 1:8,MRBcol,Pnt_type = Pnt_type,Ln_type = Ln_type,Ln_color)
}


for (i in 1:8) {
  MRBcol_plot <- toString(Plot_MRBs_Col[i,2])
  MRB_Pnt_type_plot <- (Plot_MRBs_Col[i,3])
  MRB_Ln_type_plot <- (Plot_MRBs_Col[i,4])
  MRB_Ln_color_plot <- toString(Plot_MRBs_Col[i,5])
  
  yname_med <- paste0("de.vol_",i)
  
  p <- p + geom_line(aes_string(y=yname_med, x="V2"),color=MRB_Ln_color_plot, na.rm = TRUE)
  p <- p + geom_point(aes_string(y=yname_med, x="V2"),shape=MRB_Pnt_type_plot,size=1, na.rm = TRUE)
}
p

p <- p + theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank())
p <- p + scale_x_continuous(breaks = 1:8)
p <- p + theme(axis.text=element_text(size=10))  
p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.background = element_blank())
p <- p + scale_y_log10()
p
p <- p + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank())
p <- p + theme(axis.title.y.left=element_blank(),
               axis.text.y.left =element_blank())
p

results_file_nm <- paste("Plot_")
results_file_nm <- paste(results_file_nm,sep="_")
results_file_nm <- paste(results_file_nm,".pdf",sep="")
ggsave(results_file_nm, plot = p , width = 6, height = 5, units = "cm")

