
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
#########################################################################
# FLoodplain Innundation time per event

rm(list=ls())
jj=8 # number of MRBs
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
dnames <- data.frame(MRBn = 1:8,MRbnames = c("Pacific Northwest","Pacific Coast","Southwest","Missouri River","Midwest","Northeast","Southeast","South Central"))
}

Data_test <- data.frame(med = 1:11, q25 = 1:11, q75 = 1:11, SO = 1:11)
d <- data.frame(SO = 1:11)
for (i in 1:jj) {
  
  {
    Data_name_a <- toString(dnames$MRbnames[i])
    Data_name_b <- paste0("_summaryVols_Event.rds")
    Data_name <- paste0("./data/",Data_name_a, Data_name_b)
  }
  
  Data_d <- readRDS(Data_name)
  Data_d <- list2env(Data_d)
  Data_d <- data.frame(Data_d$data)
  L <- length(Data_d)
  Data_test <- data.frame(med=Data_d$logDV_L_50_m2_yr)
  Data_test$q25 <- Data_d$logDV_L_25_m2_yr
  Data_test$q75 <- Data_d$logDV_L_75_m2_yr
  Data_test$SO <- Data_d$so

  names(Data_test)[1]<-paste("med",i,sep="_")
  names(Data_test)[2]<-paste("q25",i,sep="_")
  names(Data_test)[3]<-paste("q75",i,sep="_")
  
  Data_test <- Data_test[1:8,]
  
  d <- merge(d, Data_test, by="SO",all=T)
}

d <- d[1:7,]
p <- ggplot(d)
for (i in 1:jj) {
  MRBcol_plot <- toString(Plot_MRBs_Col[i,2])
  MRB_Pnt_type_plot <- (Plot_MRBs_Col[i,3])
  MRB_Ln_type_plot <- (Plot_MRBs_Col[i,4])
  MRB_Ln_color_plot <- toString(Plot_MRBs_Col[i,5])
  
  yname_med <- paste0("med_",i)
  yname_q25 <- paste0("q25_",i)
  yname_q75 <- paste0("q75_",i)
  xname <- paste0("SO")

  p <- p + geom_ribbon(aes_string(ymin=yname_q25, ymax=yname_q75, x=xname), fill = MRB_Ln_color_plot,show.legend=FALSE, alpha = 0.3)
} 

for (i in 1:jj) {
  MRBcol_plot <- toString(Plot_MRBs_Col[i,2])
  MRB_Pnt_type_plot <- (Plot_MRBs_Col[i,3])
  MRB_Ln_type_plot <- (Plot_MRBs_Col[i,4])
  MRB_Ln_color_plot <- toString(Plot_MRBs_Col[i,5])
  
  yname_med <- paste0("med_",i)
  yname_q25 <- paste0("q25_",i)
  yname_q75 <- paste0("q75_",i)
  
  p <- p + geom_line(aes_string(y=yname_med, x="SO"),color=MRB_Ln_color_plot)
  p <- p + geom_point(aes_string(y=yname_med, x="SO"),shape=MRB_Pnt_type_plot,size=1)
} 

p <- p + theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank())
p <- p + scale_x_continuous(breaks = 1:8)
p <- p + theme(axis.text=element_text(size=10))  
p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.background = element_blank()) + labs(y = "V / VTyear")
p <- p + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank())

# Save plot
results_file_nm <- paste("Plot_")
results_file_nm <- paste(results_file_nm,Data_name_b,sep="_")
results_file_nm <- paste(results_file_nm,".pdf",sep="")
ggsave(results_file_nm, plot = p , width = 6, height = 5, units = "cm",useDingbats=FALSE)
