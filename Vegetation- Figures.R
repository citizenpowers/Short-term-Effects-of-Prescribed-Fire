#this script created the vegetation figures in the manuscript


library(tidyverse)
library(cowplot)
library(ggh4x)
library(magick)



# Import Data -------------------------------------------------------------

#Vegetation data
Veg_and_biomass_tidy <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Vegetation/Veg_and_biomass_tidys.csv")
Vegetation_Surveys_tidy_scaled <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Vegetation/Vegetation_Surveys_tidy_scaled.csv")
Supervised_Classification_tidy_scaled <- read_csv("//ad.sfwmd.gov/dfsroot/userdata/mpowers/Desktop/Fire_Study/Data/Vegetation/Supervised_Classification_tidy_scaled.csv") 



shared_plot_components<- list(scale_fill_manual(values = c("grey85","grey50","grey70","grey35")), scale_shape_manual(values=c(21,22,23,24)),scale_color_manual(values = c("grey85","grey50","grey70","grey35")),
                              geom_line(aes(linetype = Treatment),linewidth=.75),geom_point(size=3.5,color="black"),theme_bw())


# Biomass live dead cattail grouped together ------------------------------
total_biomass_mod_data <- Veg_and_biomass_tidy %>%
filter(Date==as.Date("2025-02-25") | Date==as.Date("2025-04-15")) %>%
filter(Species %in% c("Dead Cattail and Litter","Live Cattail")) %>%  
mutate(Burned=case_when(str_detect(Treatment,"Burn") & Phase =="Post_burn" ~"Yes",.default = "No")) %>%
mutate(Herbicide=case_when(str_detect(Treatment,"Herb") ~"Yes",.default = "No")) %>%
mutate(Weight=if_else(Weight==0 | is.na(Weight),.1,Weight)) %>% #Since GLM can't use 0 values substitute 0.1 for 0
group_by(Date,Treatment,Site,Rep,Burned,Herbicide,Phase) %>%
summarise(n(),Weight=sum(Weight))

total_biomass_mod_data_stats <- total_biomass_mod_data %>%
group_by(Treatment,Site,Phase,Rep) %>%
summarise(n(),Weight=sum(Weight,na.rm=T)) %>%
group_by(,Treatment,Phase) %>% 
summarise(`Average weight`=mean(Weight,na.rm=T),n())


#create LM and GLM for dead cattail and litter
total_biomass_dead_litter_glm_date <- glm(`Weight` ~Burned+Herbicide+Date, family = "Gamma"(link='log'),data = total_biomass_mod_data) #GLM 

#function to predict model in response scale 
predict_glm <- function(glm) {
ilink <-family(glm)$linkinv                                               #Extract the  link function (Exp or Gaussian depending on the model)
df.predict <- cbind(glm$data, dplyr::rename(data.frame(predict(glm,type = "response")),"Prediction"=1))   #Predict fit using prediction function in response scale
df.predict <-bind_cols(df.predict, setNames(as_tibble(predict(glm, glm$data, se.fit = TRUE)[1:2]), c('fit_link','se_link'))) #predict fit and se in link scale
df.predict <- mutate(df.predict, fit_resp  = ilink(fit_link), right_upr = ilink(fit_link + (2 * se_link)), right_lwr = ilink(fit_link - (2 * se_link)))  #Use inverse link function(exp) to transform link scale to response scale
return(df.predict)
}

#Joins prediction data from different models 
total_biomass_dead_litter_mod_estimates <- mutate(predict_glm(total_biomass_dead_litter_glm_date),Model="GLM Date") %>%  
mutate(Phase=ifelse(Phase=="Pre_burn","Pre-burn","Post-burn"))  %>%
mutate(Phase=factor(Phase,c("Pre-burn","Post-burn")))   

#Biomass plot
ggplot(filter(total_biomass_dead_litter_mod_estimates,Model=="GLM Date")  ,aes(Phase,Prediction,fill=Phase,))+geom_col(position="dodge",color="grey20")+
geom_errorbar(aes(x=Phase,ymax=right_upr, ymin=right_lwr,linetype = Phase),linewidth=.5,width=.8,position = position_dodge(width = 0.9))+
theme_bw(base_size = 16)+facet_wrap(~Treatment,nrow = 1)+scale_fill_manual(values = c("grey85","grey50"))+coord_cartesian(ylim = c(0,3500))+scale_y_continuous(expand = c(0, 0))+
ylab(expression(Biomass~(g/m^2)))+xlab("Total Biomass")+theme(,axis.text.x = element_blank())

ggsave(plot =last_plot() ,filename="./Biomass_GLM_plot.jpeg",width =12, height =6, units = "in")

# Quadrat cover surveyshowing model estimates as column plus/minus 95% CI -------------

#Cover data
cover_mod_data <- Vegetation_Surveys_tidy_scaled %>%
filter(Date==as.Date("2025-02-25") | Date==as.Date("2025-04-15")) %>%  
filter(Species %in% c("Dead Cattail","Litter","Live Cattail","Open Water")) %>%  
mutate(`Species new name`=if_else(Species %in% c("Dead Cattail","Litter"),"Dead Cattail and Litter",Species)) %>%   #combine litter and dead cattail into single category 
group_by(Treatment,Rep,Date,Phase,`Species new name`) %>%
summarise(Scaled_Cover=sum(Scaled_Cover))  %>%
mutate(Burned=case_when(str_detect(Treatment,"Burn") & Phase =="Post_burn" ~"Yes",.default = "No")) %>%
mutate(Herbicide=case_when(str_detect(Treatment,"Herb") ~"Yes",.default = "No")) %>%
mutate(Scaled_Cover=if_else(Scaled_Cover==0 | is.na(Scaled_Cover),.1,Scaled_Cover)) %>%  #Since GLM can't use 0 values substitute 0.1 for 0
rename(Species="Species new name")

#create LM and GLM for dead cattail 
cover_dead_cat_glm_date <- glm(`Scaled_Cover` ~Burned+Herbicide+Date, family = "Gamma"(link='log'),data = filter(cover_mod_data,Species=="Dead Cattail and Litter")) #GLM 

cover_dead_cat_mod_estimates <- mutate(predict_glm(cover_dead_cat_glm_date),Model="GLM Date") %>%
mutate(Phase=ifelse(Phase=="Pre_burn","Pre-burn","Post-burn"))  %>%
mutate(Phase=factor(Phase,c("Pre-burn","Post-burn")))   

#Cover Figure Dead Cattail
cover_dead_cat_plot <- ggplot(filter(cover_dead_cat_mod_estimates, Model=="GLM Date")  ,aes(Species,Prediction,fill=Phase))+geom_col(position="dodge",color="grey20")+
geom_errorbar(aes(x=Species,ymax=right_upr, ymin=right_lwr,linetype = Phase),linewidth=.5,width=.8,position = position_dodge(width = 0.9))+
theme_bw(base_size = 16)+facet_wrap(~Treatment,nrow = 1)+scale_fill_manual(values = c("grey85","grey50"))+coord_cartesian(ylim = c(0,100))+scale_y_continuous(expand = c(0, 0))+
ylab("Quadrat Survey Cover (%)")+xlab("Dead Cattail and Litter")+theme(legend.position = "none",axis.text.x = element_blank())

#create LM and GLM for live cattail 
cover_live_cat_glm_date <- glm(`Scaled_Cover` ~Burned+Herbicide+Date, family = "Gamma"(link='log'),data = filter(cover_mod_data,Species=="Live Cattail")) #GLM 

cover_live_cat_mod_estimates <- mutate(predict_glm(cover_live_cat_glm_date),Model="GLM Date") %>%
mutate(Phase=ifelse(Phase=="Pre_burn","Pre-burn","Post-burn"))  %>%
mutate(Phase=factor(Phase,c("Pre-burn","Post-burn")))   

#Cover Figure Live Cattail
cover_live_cat_plot <- ggplot(filter(cover_live_cat_mod_estimates,Model=="GLM Date")  ,aes(Species,Prediction,fill=Phase))+geom_col(position="dodge",color="grey20")+
geom_errorbar(aes(x=Species,ymax=right_upr, ymin=right_lwr,linetype = Phase),linewidth=.5,width=.8,position = position_dodge(width = 0.9))+
theme_bw(base_size = 16)+facet_wrap(~Treatment,nrow = 1)+scale_fill_manual(values = c("grey85","grey50"))+coord_cartesian(ylim = c(0,100))+scale_y_continuous(expand = c(0, 0))+
xlab("Live Cattail")+theme(legend.position = "none",axis.text.x = element_blank(),axis.text.y = element_blank())+ylab("")

#create LM and GLM for open water
cover_open_water_glm_date <- glm(`Scaled_Cover` ~Burned+Herbicide+Date, family = "Gamma"(link='log'),data = filter(cover_mod_data,Species=="Open Water")) #GLM 

cover_open_water_mod_estimates <- mutate(predict_glm(cover_open_water_glm_date ),Model="GLM Date") %>%
mutate(Phase=ifelse(Phase=="Pre_burn","Pre-burn","Post-burn"))  %>%
mutate(Phase=factor(Phase,c("Pre-burn","Post-burn")))   

#Cover Figure for open water (Manuscript)
cover_open_water_plot <- ggplot(filter(cover_open_water_mod_estimates,Model=="GLM Date" )  ,aes(Species,Prediction,fill=Phase))+geom_col(position="dodge",color="grey20")+
geom_errorbar(aes(x=Species,ymax=right_upr, ymin=right_lwr,linetype = Phase),linewidth=.5,width=.8,position = position_dodge(width = 0.9))+
theme_bw(base_size = 16)+facet_wrap(~Treatment,nrow = 1)+scale_fill_manual(values = c("grey85","grey50"))+coord_cartesian(ylim = c(0,100))+scale_y_continuous(expand = c(0, 0))+
xlab("Open Water")+theme(legend.position = "none",axis.text.x = element_blank(),axis.text.y = element_blank())+ylab("")

# Supervised classification survey showing model estimates as column plus/minus 95% CI----------------------------------------

#Cover as measured by supervised classification
super_cover_mod_data <- Supervised_Classification_tidy_scaled %>%
mutate(Treatment=if_else(Treatment=="Burn Herbicide","Burn/Herbicide",Treatment)) %>%
mutate(Treatment=factor(Treatment,c("Burn","Burn/Herbicide","Herbicide","Untreated"))) %>%
filter(Date=="2025-04-03" | Date== "2025-04-15" ) %>%  
mutate(Phase=ifelse(Date<"2025-04-10 00:00:00","Pre-burn","Post-burn")) %>%
mutate(Class=if_else(Class=="Water","Open Water",Class)) %>%  
mutate(Class=if_else(Class=="Other Veg","Other Vegetation",Class)) %>%    
filter(Class %in% c("Dead Cattail","Other Vegetation","Live Cattail","Open Water")) %>%  
mutate(Burned=case_when(str_detect(Treatment,"Burn") & Phase =="Post-burn" ~"Yes",.default = "No")) %>%
mutate(Herbicide=case_when(str_detect(Treatment,"Herb") ~"Yes",.default = "No")) %>%
mutate(`Scaled Cover`=if_else(`Scaled Cover`==0 | is.na(`Scaled Cover`),.1,`Scaled Cover`))  #Since GLM can't use 0 values substitute 0.1 for 0

#create LM and GLM for dead cattail 
super_cover_dead_cat_glm_date <- glm(`Scaled Cover` ~Burned+Herbicide+Date, family = "Gamma"(link='log'),data = filter(super_cover_mod_data,Class=="Dead Cattail")) #GLM 

super_cover_dead_cat_mod_estimates <- mutate(predict_glm(super_cover_dead_cat_glm_date),Model="GLM Date") %>%
mutate(Phase=factor(Phase,c("Pre-burn","Post-burn")))   

#Supervised cover for dead cattail (Manuscript)
Super_cover_dead_cat_plot <- ggplot(filter(super_cover_dead_cat_mod_estimates,Model=="GLM Date" )  ,aes(Class,Prediction,fill=Phase))+geom_col(position="dodge",color="grey20")+
geom_errorbar(aes(x=Class,ymax=right_upr, ymin=right_lwr,linetype = Phase),linewidth=.5,width=.8,position = position_dodge(width = 0.9))+
theme_bw(base_size = 16)+facet_wrap(~Treatment,nrow = 1)+scale_fill_manual(values = c("grey85","grey50"))+coord_cartesian(ylim = c(0,100))+scale_y_continuous(expand = c(0, 0))+
ylab("Supervised Classification Cover (%)")+xlab("Dead Cattail and Litter")+theme(legend.position = "none",axis.text.x = element_blank())


#create LM and GLM for live cattail 
super_cover_live_cat_glm_date <- glm(`Scaled Cover` ~Burned+Herbicide+Date, family = "Gamma"(link='log'),data = filter(super_cover_mod_data,Class=="Live Cattail")) #GLM 

super_cover_live_cat_mod_estimates <- mutate(predict_glm(super_cover_live_cat_glm_date),Model="GLM Date") %>%
mutate(Phase=factor(Phase,c("Pre-burn","Post-burn")))   

#Supervised cover for live cattail (Manuscript)
Super_cover_live_cat_plot <- ggplot(filter(super_cover_live_cat_mod_estimates,Model=="GLM Date" )  ,aes(Class,Prediction,fill=Phase))+geom_col(position="dodge",color="grey20")+
geom_errorbar(aes(x=Class,ymax=right_upr, ymin=right_lwr,linetype = Phase),linewidth=.5,width=.8,position = position_dodge(width = 0.9))+
theme_bw(base_size = 16)+facet_wrap(~Treatment,nrow = 1)+scale_fill_manual(values = c("grey85","grey50"))+coord_cartesian(ylim = c(0,100))+scale_y_continuous(expand = c(0, 0))+
ylab("")+xlab("Live Cattail")+theme(legend.position = "none",axis.text.x = element_blank(),axis.text.y = element_blank())

#create LM and GLM for open water 
super_cover_open_water_glm_date <- glm(`Scaled Cover` ~Burned+Herbicide+Date, family = "Gamma"(link='log'),data = filter(super_cover_mod_data,Class=="Open Water")) #GLM 

super_cover_open_water_mod_estimates <- mutate(predict_glm(super_cover_open_water_glm_date ),Model="GLM Date") %>%
  mutate(Phase=factor(Phase,c("Pre-burn","Post-burn")))   

#Supervised cover for open water (Manuscript)
Super_cover_open_water_plot <- ggplot(filter(super_cover_open_water_mod_estimates,Model=="GLM Date" ),aes(Class,Prediction,fill=Phase))+geom_col(position="dodge",color="grey20")+
geom_errorbar(aes(x=Class,ymax=right_upr, ymin=right_lwr,linetype = Phase),linewidth=.5,width=.8,position = position_dodge(width = 0.9))+
theme_bw(base_size = 16)+facet_wrap(~Treatment,nrow = 1)+scale_fill_manual(values = c("grey85","grey50"))+coord_cartesian(ylim = c(0,100))+scale_y_continuous(expand = c(0, 0))+
ylab("")+xlab("Open Water")+theme(legend.position = "none",axis.text.x = element_blank(),axis.text.y = element_blank())


# Combine cover plots into single figure ----------------------------------

#Combine plots for manuscript figure
cover_plot <- cowplot::plot_grid(cover_dead_cat_plot,cover_live_cat_plot,cover_open_water_plot,nrow = 1,labels=c("A","B","C"))

super_cover <- cowplot::plot_grid(Super_cover_dead_cat_plot,Super_cover_live_cat_plot,Super_cover_open_water_plot ,nrow = 1,labels=c("D","E","F"))

#create separate legend to be used in cowplot 
legend_plot <- ggplot(filter(super_cover_open_water_mod_estimates,Model=="GLM Date" ),aes(Class,Prediction,fill=Phase))+
scale_fill_manual(values = c("grey85","grey50"))+
geom_boxplot(aes(fill=`Phase`))+theme_bw(base_size = 24)+theme(legend.position="bottom")

#extract legend
legend_veg <-get_plot_component(legend_plot, 'guide-box-bottom', return_all = TRUE)

#combine plots into figure
#Plot elements 
veg_plot_1 <- plot_grid(cover_plot,super_cover,ncol=1)

veg_plot <- plot_grid(veg_plot_1 , legend_veg,  rel_heights = c(10, 1),ncol=1)

ggsave(plot =veg_plot ,filename="./Vegetation_cover_GLM_plot.jpeg",width =16.5, height =19, units = "in")
