#this script contains code to create the water qualtiy figures for the manuscript


library(tidyverse)
library(cowplot)
library(ggh4x)
library(magick)






# Import Data -------------------------------------------------------------


LIMSP_Provisional_Data_Tidy <-read_csv("LIMSP_Provisional_Data_Tidy.csv")



# Summarize Data ---------------------------------------------------------------

Data_summary <- LIMSP_Provisional_Data_Tidy %>%
mutate(Date=as.Date(COLLECT_DATE)) %>%  
filter(MATRIX=="SW",TEST_NAME %in% c("SRP","PP","TPO4","DOP","TN","NH4","NOX"),Date <"2025-05-01",COLLECT_METHOD=="GP") %>%  #filter to analytes of interest
group_by(Treatment,TEST_NAME,Date,`Burn Day`) %>%                                                                            #group by date
summarise(n(),Mean=mean(VALUE*1000,na.rm=T),SD=sd(VALUE*1000,na.rm=T))                                                       #summarize by treatment and date


# WQ Printed Figure (TPO4 only)  ---------------------------------------------

shared_plot_components<- list(scale_fill_manual(values = c("grey85","grey50","grey70","grey35")), scale_shape_manual(values=c(21,22,23,24)),scale_color_manual(values = c("grey85","grey50","grey70","grey35")),
                              geom_line(aes(linetype = Treatment),linewidth=.75),geom_point(size=3.5,color="black"),theme_bw(base_size = 16))


Pre_burn_TP <-ggplot(filter(Data_summary,Date< "2025-04-01", TEST_NAME =="TPO4" ),aes(Date,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
geom_errorbar(aes(Date, ymax=Mean+SD, ymin=Mean-SD),width=5)+
annotate(geom = "text", x = as.Date("2025-04-01"), y = 300, label = "Burn",angle = 90,size=4) +
geom_vline(aes(xintercept =as.Date("2025-04-07")),linetype=2,linewidth=1)+
annotate(geom = "text", x = as.Date("2024-11-10"), y = 580, label = "A",size=5)+ #do not label plot for poster figure
shared_plot_components+  
ylab(expression(TP~(mu~g/L)))+labs(x="Pre-Burn")+
coord_cartesian(ylim = c(-10,600),expand=F,xlim = as.Date(c("2024-11-01","2025-04-12")))+
scale_x_date(date_labels = "%b",date_breaks = "1 month")+
scale_y_continuous(breaks = seq(0,700,100))+theme(legend.position = "none")


#combined Pre-burn and post burn. Need separate plots because of the different time scales

#post burn plot with days from burn on x-axis
Post_burn_TP <- ggplot(filter(Data_summary ,Date> "2025-04-09",Date<"2025-05-01",TEST_NAME=="TPO4"),aes(`Burn Day`,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
geom_errorbarh(aes(y=14,xmax=11, xmin=6),linewidth=1,height=16,linetype="dotted")+
geom_errorbarh(aes(y=14,xmax=6, xmin=0),linewidth=1,height=16)+
geom_errorbar(aes(`Burn Day`, ymax=Mean+SD, ymin=Mean-SD),linewidth=1)+
annotate(geom = "text", x = 11, y = 580, label = "B",size=5)+  #Use for publication fig. Do not use for poster fig
annotate(geom = "text", x = 3, y = 2, label = "Duration",size=4)+
shared_plot_components+
ylab(expression(TPO4~(mu~g/L)))+labs(x="Days from Burn")+
coord_cartesian(ylim = c(-10,600),expand=F,xlim = c(-1,12))+scale_x_continuous(breaks = seq(0,12,2))+
scale_y_continuous(breaks = seq(0,600,100))+
theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y.left = element_blank(),legend.position = "none")

#combine pre and post burn plots 
TP_plot <-plot_grid(Pre_burn_TP, Post_burn_TP,rel_widths =c(2, 3))

#create separate legend to be used in cowplot 
legend_plot <- ggplot(Data_summary,aes(Date ,Mean,fill=Treatment,shape=Treatment))+
shared_plot_components+
theme_bw()+theme(legend.position="bottom",text = element_text(size = 16))

#extract legend
legend <-get_plot_component(legend_plot, 'guide-box-bottom', return_all = TRUE)

TP_plot_with_legend <- plot_grid(TP_plot , legend , labels = "", rel_heights = c(10, 1),ncol=1)

ggsave(plot = TP_plot_with_legend ,filename="./TP Pre- and Post-burn.jpeg",width =10, height =6, units = "in")  #publication size



# P and N forms plots -----------------------------------------------------

#pre-burn plot

Pre_burn_PP <- ggplot(filter(Data_summary,TEST_NAME=="PP",Date< "2025-04-01"),aes(Date,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
annotate(geom = "text", x = as.Date("2025-03-30"), y = 300, label = "Burn",angle = 90,size=4) +
geom_errorbar(aes(Date, ymax=Mean+SD, ymin=Mean-SD),width=4)+
geom_vline(aes(xintercept =as.Date("2025-04-07")),linetype=2,linewidth=1)+
annotate(geom = "text", x = as.Date("2024-11-10"), y = 582, label = "A",size=5)+
ylab(expression(PP~(mu~g/L)))+labs(x="")+shared_plot_components+  
coord_cartesian(ylim = c(-50,600),expand=F,xlim = as.Date(c("2024-11-01","2025-04-12")))+
scale_x_date(date_labels = "%b",date_breaks = "1 month")+
scale_y_continuous(breaks = seq(0,600,100))+
theme(legend.position = "none")

#PP post-burn
Post_burn_PP<- ggplot(filter(Data_summary,Date> "2025-04-09",Date<"2025-05-01",TEST_NAME=="PP"),aes(`Burn Day`,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
geom_errorbar(aes(`Burn Day`, ymax=Mean+SD, ymin=Mean-SD),linewidth=1)+
geom_line()+annotate(geom = "text", x = 11, y = 582, label = "B",size=5)+
annotate(geom = "text", x = 2, y = -35, label = "Duration",size=4)+
geom_errorbarh(aes(y=-20,xmax=4, xmin=1),linewidth=1,height=25,linetype="dotted")+
geom_errorbarh(aes(y=-20,xmax=1, xmin=0),linewidth=1,height=25)+
ylab(expression(TPO4~(mu~g/L)))+labs(x="")+shared_plot_components+  
coord_cartesian(ylim = c(-50,600),expand=F,xlim = c(-1,12))+scale_x_continuous(breaks = seq(0,12,2))+
scale_y_continuous(breaks = seq(0,600,100))+
theme(legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y.left = element_blank())


#pre-burn plot
Pre_burn_SRP <- ggplot(filter(Data_summary,Date< "2025-04-09",TEST_NAME=="SRP"),aes(Date,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
annotate(geom = "text", x = as.Date("2025-03-30"), y = 40, label = "Burn",angle = 90,size=4) +
geom_errorbar(aes(Date, ymax=Mean+SD, ymin=Mean-SD),width=4)+
geom_vline(aes(xintercept =as.Date("2025-04-07")),linetype=2,linewidth=1)+
annotate(geom = "text", x = as.Date("2024-11-10"), y = 77, label = "C",size=5)+
ylab(expression(SRP~(mu~g/L)))+labs(x="")+shared_plot_components+  
coord_cartesian(ylim = c(-10,80),expand=F,xlim = as.Date(c("2024-11-01","2025-04-12")))+
scale_x_date(date_labels = "%b",date_breaks = "1 month")+
scale_y_continuous(breaks = seq(0,80,20))+
theme(legend.position = "none")

#SRP post-burn
Post_burn_SRP<- ggplot(filter(Data_summary,Date> "2025-04-09",Date<"2025-05-01",TEST_NAME=="SRP"),aes(`Burn Day`,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
geom_errorbar(aes(`Burn Day`, ymax=Mean+SD, ymin=Mean-SD),linewidth=1)+
geom_line()+annotate(geom = "text", x = 11, y = 77, label = "D",size=5)+annotate(geom = "text", x = 2, y = -7, label = "Duration",size=4)+
geom_errorbarh(aes(y=-2,xmax=6, xmin=4),linewidth=1,height=6,linetype="dotted")+
geom_errorbarh(aes(y=-2,xmax=4, xmin=0),linewidth=1,height=6)+
labs(x="")+shared_plot_components+  
coord_cartesian(ylim = c(-10,80),expand=F,xlim = c(-1,12))+scale_x_continuous(breaks = seq(0,12,2))+
scale_y_continuous(breaks = seq(0,80,20))+
theme(legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y.left = element_blank())


#pre-burn plot
Pre_burn_DOP <- ggplot(filter(Data_summary,Date< "2025-04-09",TEST_NAME=="DOP"),aes(Date,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
annotate(geom = "text", x = as.Date("2025-03-30"), y = 30, label = "Burn",angle = 90,size=4) +
geom_errorbar(aes(Date, ymax=Mean+SD, ymin=Mean-SD),width=4)+
geom_vline(aes(xintercept =as.Date("2025-04-07")),linetype=2,linewidth=1)+
annotate(geom = "text", x = as.Date("2024-11-10"), y = 58, label = "E",size=5)+labs(x="Pre-Burn")+
ylab(expression(DOP~(mu~g/L)))+shared_plot_components+  
coord_cartesian(ylim = c(0,60),expand=F,xlim = as.Date(c("2024-11-01","2025-04-12")))+
scale_x_date(date_labels = "%b",date_breaks = "1 month")+
scale_y_continuous(breaks = seq(0,60,10))+
theme(legend.position = "none")

#post-burn
Post_burn_DOP<- ggplot(filter(Data_summary,Date> "2025-04-09",Date<"2025-05-01",TEST_NAME=="DOP"),aes(`Burn Day`,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
geom_errorbar(aes(`Burn Day`, ymax=Mean+SD, ymin=Mean-SD),linewidth=1)+
geom_line()+annotate(geom = "text", x = 11, y = 58, label = "F",size=5)+annotate(geom = "text", x = .7, y = 1.5, label = "Duration",size=4)+
geom_errorbarh(aes(y=5,xmax=4, xmin=1),linewidth=1,height=5,linetype="dotted")+
geom_errorbarh(aes(y=5,xmax=1, xmin=0),linewidth=1,height=5)+
shared_plot_components+  labs(x="Days from Burn")+
coord_cartesian(ylim = c(0,60),expand=F,xlim = c(-1,12))+scale_x_continuous(breaks = seq(0,12,2))+
scale_y_continuous(breaks = seq(0,60,10))+
theme(legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y.left = element_blank())

#pre-burn plot
Pre_burn_TN <- ggplot(filter(Data_summary,TEST_NAME=="TN",Date< "2025-04-09"),aes(Date,Mean/1000,color=Treatment,fill=Treatment,shape=Treatment))+
annotate(geom = "text", x = as.Date("2025-03-30"), y = 2, label = "Burn",angle = 90,size=4) +
geom_errorbar(aes(Date, ymax=Mean+SD, ymin=Mean-SD),width=4)+
geom_vline(aes(xintercept =as.Date("2025-04-07")),linetype=2,linewidth=1)+
annotate(geom = "text", x = as.Date("2024-11-10"), y = 3.4, label = "G",size=5)+
ylab(expression(TN~(mg/L)))+labs(x="")+shared_plot_components+  
coord_cartesian(ylim = c(0,3.5),expand=F,xlim = as.Date(c("2024-11-01","2025-04-12")))+
scale_x_date(date_labels = "%b",date_breaks = "1 month")+
scale_y_continuous(breaks = seq(0,3.5,.5))+
theme(legend.position = "none")

#post-burn
Post_burn_TN<- ggplot(filter(Data_summary,Date> "2025-04-09",Date<"2025-05-01",TEST_NAME=="TN"),aes(`Burn Day`,Mean/1000,color=Treatment,fill=Treatment,shape=Treatment))+
geom_errorbar(aes(`Burn Day`, ymax=(Mean+SD)/1000, ymin=(Mean-SD)/1000),linewidth=1)+
geom_line()+annotate(geom = "text", x = 11, y = 3.4, label = "H",size=5)+annotate(geom = "text", x = 2, y = .25, label = "Duration",size=4)+
geom_errorbarh(aes(y=.5,xmax=1, xmin=0),linewidth=1,height=0.25,linetype="dotted")+
geom_errorbarh(aes(y=.5,xmax=0, xmin=0),linewidth=1,height=0.25)+
labs(x="")+shared_plot_components+  
coord_cartesian(ylim = c(0,3.5),expand=F,xlim = c(-1,12))+scale_x_continuous(breaks = seq(0,12,2))+
scale_y_continuous(breaks = seq(0,3.5,10))+
theme(legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y.left = element_blank())

#pre-burn plot
Pre_burn_NOX <- ggplot(filter(Data_summary,TEST_NAME=="NOX",Date< "2025-04-09"),aes(Date,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
annotate(geom = "text", x = as.Date("2025-03-30"), y = 12, label = "Burn",angle = 90,size=4) +
geom_errorbar(aes(Date, ymax=Mean+SD, ymin=Mean-SD),width=4)+
geom_vline(aes(xintercept =as.Date("2025-04-07")),linetype=2,linewidth=1)+
annotate(geom = "text", x = as.Date("2024-11-10"), y = 14.5, label = "K",size=5)+labs(x="Pre-Burn")+
ylab(expression(NOx~(mu~g/L)))+shared_plot_components+  
coord_cartesian(ylim = c(0,15),expand=F,xlim = as.Date(c("2024-11-01","2025-04-12")))+
scale_x_date(date_labels = "%b",date_breaks = "1 month")+
scale_y_continuous(breaks = seq(0,15,3))+
theme(legend.position = "none")

#post-burn
Post_burn_NOX<- ggplot(filter(Data_summary,Date> "2025-04-09",Date<"2025-05-01",TEST_NAME=="NOX"),aes(`Burn Day`,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
geom_errorbar(aes(`Burn Day`, ymax=Mean+SD, ymin=Mean-SD),linewidth=1)+
geom_line()+annotate(geom = "text", x = 11, y = 14.5, label = "L",size=5)+annotate(geom = "text", x = 2, y = 1, label = "Duration",size=4)+
geom_errorbarh(aes(y=2.5,xmax=1, xmin=0),linewidth=1,height=1.5,linetype="dotted")+
geom_errorbarh(aes(y=2.5,xmax=0, xmin=0),linewidth=1,height=1.5)+
labs(x="")+shared_plot_components+  labs(x="Days from Burn")+
coord_cartesian(ylim = c(0,15),expand=F,xlim = c(-1,12))+scale_x_continuous(breaks = seq(0,12,2))+
scale_y_continuous(breaks = seq(0,15,3))+
theme(legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y.left = element_blank())

#pre-burn plot
Pre_burn_NH4 <- ggplot(filter(Data_summary,TEST_NAME=="NH4",Date< "2025-04-09"),aes(Date,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
annotate(geom = "text", x = as.Date("2025-03-30"), y = 125, label = "Burn",angle = 90,size=4) +
geom_errorbar(aes(Date, ymax=Mean+SD, ymin=Mean-SD),width=4)+
geom_vline(aes(xintercept =as.Date("2025-04-07")),linetype=2,linewidth=1)+
annotate(geom = "text", x = as.Date("2024-11-10"), y = 240, label = "I",size=5)+
ylab(expression(NH4~(mu~g/L)))+labs(x="")+shared_plot_components+  
coord_cartesian(ylim = c(-10,250),expand=F,xlim = as.Date(c("2024-11-01","2025-04-12")))+
scale_x_date(date_labels = "%b",date_breaks = "1 month")+
scale_y_continuous(breaks = seq(0,250,25))+
theme(legend.position = "none")

#post-burn
Post_burn_NH4<- ggplot(filter(Data_summary,Date> "2025-04-09",Date<"2025-05-01",TEST_NAME=="NH4"),aes(`Burn Day`,Mean,color=Treatment,fill=Treatment,shape=Treatment))+
geom_errorbar(aes(`Burn Day`, ymax=Mean+SD, ymin=Mean-SD),linewidth=1)+
geom_errorbarh(aes(y=5,xmax=1, xmin=0),linewidth=1,height=10,linetype="dotted")+
geom_errorbarh(aes(y=5,xmax=0, xmin=0),linewidth=1,height=10)+
geom_line()+annotate(geom = "text", x = 11, y = 240, label = "J",size=5)+annotate(geom = "text", x = 2, y = -5, label = "Duration",size=3.5)+
labs(x="")+shared_plot_components+  
coord_cartesian(ylim = c(-10,250),expand=F,xlim = c(-1,12))+scale_x_continuous(breaks = seq(0,12,2))+
scale_y_continuous(breaks = seq(0,250,25))+
theme(legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y.left = element_blank())

#Combine plots into single plot

Pre_burn_plot_width <-2.7  #set relative width of pre-burn plot
Post_burn_plot_width <-3   #set relative width of post-burn plot

PP_plot  <- plot_grid(Pre_burn_PP, Post_burn_PP,rel_widths =c( Pre_burn_plot_width,Post_burn_plot_width))
SRP_plot  <- plot_grid(Pre_burn_SRP, Post_burn_SRP,rel_widths =c( Pre_burn_plot_width,Post_burn_plot_width))
DOP_plot  <- plot_grid(Pre_burn_DOP, Post_burn_DOP,rel_widths =c( Pre_burn_plot_width,Post_burn_plot_width))
TN_plot  <- plot_grid(Pre_burn_TN, Post_burn_TN,rel_widths =c( Pre_burn_plot_width,Post_burn_plot_width))
NOX_plot  <- plot_grid(Pre_burn_NOX, Post_burn_NOX,rel_widths=c( Pre_burn_plot_width,Post_burn_plot_width))
NH4_plot  <- plot_grid(Pre_burn_NH4, Post_burn_NH4,rel_widths = c( Pre_burn_plot_width,Post_burn_plot_width))

Nutrient_plot1 <- plot_grid(PP_plot, TN_plot, SRP_plot,NH4_plot,DOP_plot,NOX_plot ,labels = NULL, ncol = 2,align = "hv",axis = "rlbt",rel_heights = c(1,1,1))

#create separate legend to be used in cowplot 
legend_plot <- ggplot(Data_summary,aes(Date ,Mean,fill=Treatment,shape=Treatment))+
shared_plot_components+
theme_bw()+theme(legend.position="bottom",text = element_text(size = 18))

#extract legend
legend <-get_plot_component(legend_plot, 'guide-box-bottom', return_all = TRUE)

N_P_forms_plot_with_legend <- plot_grid(Nutrient_plot1 , legend , labels = "", rel_heights = c(13, 1),ncol=1)

ggsave(plot = N_P_forms_plot_with_legend  ,filename="./Nutrient_plot means with SD.jpeg",width =12, height =14, units = "in")


