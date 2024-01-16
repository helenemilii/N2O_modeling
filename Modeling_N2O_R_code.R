
#########################

#R code to model temporal variation of nitrous oxide (N2O) using Random forest with conditional inference trees 

#author: Helena Rautakoski (helena.rautakoski@fmi.fi)
#data: ~4 years of daily mean N2O fluxes measured with automatic chamber in Lettosuo drained peatland forest (Finland)

#########################

#load libraries
library(forcats)
library(ggplot2)
library(dplyr)
library(cowplot)
library(Metrics)
library(party)
library(randomForest)
library(UBL)
library(moreparty)

#########
#1. download data from zenodo

#download data to your computer
#data can be found from the same Github repository. File name Example_data.csv

#read in data
data<-read.csv("C:/Documents/Example_data.csv")

#########
#2. prepare data for modeling

#date as posixct
data$date<-as.POSIXct(strptime(data$date, "%Y-%m-%d", tz="UTC"))

#all days within the study period
dates<-seq.POSIXt(data$date[1], data$date[nrow(data)], by="1 day")

#create index column (row numbers)
data$ind<-seq(1:nrow(data))

#store training period data and take the fourth year of data for evaluation
trainingperiod<-data %>% filter(date <= dates[365+365+365]) #leave fourth year out from training period
evaluation_fourthtear<-data %>% filter(date > dates[365+365+365]) #store the fourth year as evaluation data

#separate training period to final training data (70% of training period data) and evaluation within the training period (30%): 
#take randomly 70% as training, and 30% as evaluation data
set.seed (500)
indexes<- sample(trainingperiod$ind, round(0.7*nrow(trainingperiod),0))
training <- trainingperiod[trainingperiod$ind %in% indexes,]
evaluation_within <- trainingperiod[!(trainingperiod$ind %in% indexes),]

#plot
ggplot(data=NULL, aes(x=date, y=n2o))+geom_point(data=training)+geom_point(data=evaluation_within, color="red")+geom_point(data=evaluation_fourthtear, color="blue")+theme_minimal()


#make classes more balanced with SMOGN
#=undersampling or common and oversampling of rare flux values
training_final <- SMOGNRegress(n2o ~ ., training[,-c(1)], C.perc = list(0.7,2), k=5, dist="Euclidean")

#before vs after SMOGN
hist(training$n2o)
hist(training_final$n2o)

#datasets for modeling are now ready
#training data: training period of three years, 70% randomly chosen and class imbalance decreased with SMOGN 
  #dataframe: training_final
#evaluation within training period: 30% of training period data randomly chosen
  #dataframe: evaluation_within
#evaluation outside training period: fourth year of data 
  #dataframe: evaluation_fourthyear


#######
#3. train random forest with conditional inference trees

#train and use chosen ntree (500) and default mtry (mtry = explanatory variables / 3, rounded down)
set.seed(500)
mod<-cforest(n2o ~., data=training_final[,-c(58)], control = cforest_unbiased(mtry=18, ntree=500))


######
#4. variable importance (VI)

#function to calculate VIs and store them in dataframe
VI_func<-function(model){
  vi <- varimp(model,conditional = T)
  vi_<-as.data.frame(vi)
  vi_$column<-rownames(vi_)
  vi_<-vi_[order(-vi_[,1]),]
  
  return(vi_)
}

#calculate VIs using the function
variable_importance<-VI_func(mod) #takes several minutes
#interpretation: the variable with the highest VI value is the most important variable explaining temporal variation of N2O

#plot VIs
variable_importance %>% mutate(column = fct_reorder(column, desc(vi))) %>% ggplot(aes(x=column, y=vi))+geom_point()+theme(axis.text.x = element_text(angle = 90), panel.background = element_rect(fill="white"), panel.grid = element_line(color="grey90"))+ylab("Conditional permutation importance")+xlab("Explanatory variable")

#total VIs (unlagged + lagged VI)
variable_importance<- variable_importance[order(variable_importance$column), ]
total_vi<-data.frame(variable=c("moist20", "moist7","precip","t_5cm","t_air","t_surf","wtl"),vi=c(sum(variable_importance$v[1:8]),sum(variable_importance$v[9:16]),sum(variable_importance$v[17:24]),sum(variable_importance$v[25:32]),sum(variable_importance$v[33:40]),sum(variable_importance$v[41:48]),sum(variable_importance$v[49:56])))

#plot total VIs
total_vi %>%  mutate(variable = fct_reorder(variable, desc(vi))) %>% ggplot(aes(variable, vi))+geom_point()+theme(axis.text.x = element_text(angle = 0), panel.background = element_rect(fill="white"), panel.grid = element_line(color="grey90"))+ylab("Conditional permutation importance")+xlab("Variable (unlagged + lagged)")


######
#5. accumulated local effects

#calculate ALE values
aledata <- GetAleData(mod) #takes several minutes
#interpretation: cat = values of an environmental variable, value = ALE values 

#store ALE values of each variable in separate dataframe
m10<-aledata %>% filter(var=="moist7")
m10_1<-aledata %>% filter(var=="moist7_1")
m10_2<-aledata %>% filter(var=="moist7_2")
m10_3<-aledata %>% filter(var=="moist7_3")
m10_4<-aledata %>% filter(var=="moist7_4")
m10_5<-aledata %>% filter(var=="moist7_5")
m10_6<-aledata %>% filter(var=="moist7_6")
m10_7<-aledata %>% filter(var=="moist7_7")

m20<-aledata %>% filter(var=="moist20")
m20_1<-aledata %>% filter(var=="moist20_1")
m20_2<-aledata %>% filter(var=="moist20_2")
m20_3<-aledata %>% filter(var=="moist20_3")
m20_4<-aledata %>% filter(var=="moist20_4")
m20_5<-aledata %>% filter(var=="moist20_5")
m20_6<-aledata %>% filter(var=="moist20_6")
m20_7<-aledata %>% filter(var=="moist20_7")

wtl<-aledata %>% filter(var=="wtl")
wtl_1<-aledata %>% filter(var=="wtl_1")
wtl_2<-aledata %>% filter(var=="wtl_2")
wtl_3<-aledata %>% filter(var=="wtl_3")
wtl_4<-aledata %>% filter(var=="wtl_4")
wtl_5<-aledata %>% filter(var=="wtl_5")
wtl_6<-aledata %>% filter(var=="wtl_6")
wtl_7<-aledata %>% filter(var=="wtl_7")

precip<-aledata %>% filter(var=="precip")
precip_1<-aledata %>% filter(var=="precip_1")
precip_2<-aledata %>% filter(var=="precip_2")
precip_3<-aledata %>% filter(var=="precip_3")
precip_4<-aledata %>% filter(var=="precip_4")
precip_5<-aledata %>% filter(var=="precip_5")
precip_6<-aledata %>% filter(var=="precip_6")
precip_7<-aledata %>% filter(var=="precip_7")

t_air<-aledata %>% filter(var=="t_air")
t_air_1<-aledata %>% filter(var=="t_air_1")
t_air_2<-aledata %>% filter(var=="t_air_2")
t_air_3<-aledata %>% filter(var=="t_air_3")
t_air_4<-aledata %>% filter(var=="t_air_4")
t_air_5<-aledata %>% filter(var=="t_air_5")
t_air_6<-aledata %>% filter(var=="t_air_6")
t_air_7<-aledata %>% filter(var=="t_air_7")

t_5<-aledata %>% filter(var=="t_5")
t_5_1<-aledata %>% filter(var=="t_5_1")
t_5_2<-aledata %>% filter(var=="t_5_2")
t_5_3<-aledata %>% filter(var=="t_5_3")
t_5_4<-aledata %>% filter(var=="t_5_4")
t_5_5<-aledata %>% filter(var=="t_5_5")
t_5_6<-aledata %>% filter(var=="t_5_6")
t_5_7<-aledata %>% filter(var=="t_5_7")

t_surf<-aledata %>% filter(var=="t_surface")
t_surf_1<-aledata %>% filter(var=="t_surface_1")
t_surf_2<-aledata %>% filter(var=="t_surface_2")
t_surf_3<-aledata %>% filter(var=="t_surface_3")
t_surf_4<-aledata %>% filter(var=="t_surface_4")
t_surf_5<-aledata %>% filter(var=="t_surface_5")
t_surf_6<-aledata %>% filter(var=="t_surface_6")
t_surf_7<-aledata %>% filter(var=="t_surface_7")


#plot ALEs: one plot for each environmental variable, lags included
(moist10plot<-ggplot(data=NULL, aes(x=as.numeric(cat), y=value))+geom_line(data=m10_7, linewidth=1,color="#DBE9F5" )+geom_line(data=m10_6, size=1, color="#AFC6D9")+geom_line(data=m10_5, size=1, color="#83A3BE")+geom_line(data=m10_4, size=1, color="#5880A2")+geom_line(data=m10_3, size=1, color="#2C5D87")+geom_line(data=m10_2, size=1, color="#003A6B")+geom_line(data=m10_1, size=1, color="gray24")+geom_line(data=m10, size=1.3, color="#000000")+theme_minimal()+xlab(expression(Soil~moisture~7~cm~(m^3~m^3)))+ylab("ALE value")+theme(text=element_text(size=7)) +
  theme(panel.background = element_rect(fill="white", color="black", linetype = "solid"),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_line(color="gray90"), panel.grid.major.x = element_line(color="gray90"),panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black"), axis.text = element_text(color="black", size=12),axis.title = element_text(size=12, color="black") ,legend.position = "none",plot.margin = ggplot2::margin(t=20,r=2,b=2,l=5)))

(moist20plot<-ggplot(data=NULL, aes(x=as.numeric(cat), y=value))+geom_line(data=m20_7, linewidth=1,color="#DBE9F5" )+geom_line(data=m20_6, size=1, color="#AFC6D9")+geom_line(data=m20_5, size=1, color="#83A3BE")+geom_line(data=m20_4, size=1, color="#5880A2")+geom_line(data=m20_3, size=1, color="#2C5D87")+geom_line(data=m20_2, size=1, color="#003A6B")+geom_line(data=m20_1, size=1, color="gray24")+geom_line(data=m20, size=1.3, color="#000000")+theme_minimal()+xlab(expression(Soil~moisture~20~cm~(m^3~m^3)))+ylab("ALE value")+theme(text=element_text(size=7)) +
  theme(panel.background = element_rect(fill="white", color="black", linetype = "solid"),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_line(color="gray90"), panel.grid.major.x = element_line(color="gray90"),panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black"), axis.text = element_text(color="black", size=12),axis.title = element_text(size=12, color="black") ,legend.position = "none",plot.margin = ggplot2::margin(t=20,r=2,b=2,l=5)))

(wtlplot<-ggplot(data=NULL, aes(x=as.numeric(cat), y=value))+geom_line(data=wtl_7, linewidth=1,color="#DBE9F5" )+geom_line(data=wtl_6, size=1, color="#AFC6D9")+geom_line(data=wtl_5, size=1, color="#83A3BE")+geom_line(data=wtl_4, size=1, color="#5880A2")+geom_line(data=wtl_3, size=1, color="#2C5D87")+geom_line(data=wtl_2, size=1, color="#003A6B")+geom_line(data=wtl_1, size=1, color="gray24")+geom_line(data=wtl, size=1.3, color="#000000")+theme_minimal()+xlab("WTL (cm)")+ylab("ALE value")+theme(text=element_text(size=7)) +
  theme(panel.background = element_rect(fill="white", color="black", linetype = "solid"),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_line(color="gray90"), panel.grid.major.x = element_line(color="gray90"),panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black"), axis.text = element_text(color="black", size=12),axis.title = element_text(size=12, color="black") ,legend.position = "none",plot.margin = ggplot2::margin(t=20,r=2,b=2,l=5)))

(precipplot<-ggplot(data=NULL, aes(x=as.numeric(cat), y=value))+geom_line(data=precip_7, linewidth=1,color="#DBE9F5" )+geom_line(data=precip_6, size=1, color="#AFC6D9")+geom_line(data=precip_5, size=1, color="#83A3BE")+geom_line(data=precip_4, size=1, color="#5880A2")+geom_line(data=precip_3, size=1, color="#2C5D87")+geom_line(data=precip_2, size=1, color="#003A6B")+geom_line(data=precip_1, size=1, color="gray24")+geom_line(data=precip, size=1.3, color="#000000")+theme_minimal()+xlab("Precipitation (mm)")+ylab("ALE value")+theme(text=element_text(size=7))+
  theme(panel.background = element_rect(fill="white", color="black", linetype = "solid"),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_line(color="gray90"), panel.grid.major.x = element_line(color="gray90"),panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black"), axis.text = element_text(color="black", size=12),axis.title = element_text(size=12, color="black") ,legend.position = "none",plot.margin = ggplot2::margin(t=20,r=2,b=2,l=5)))

(t_airplot<-ggplot(data=NULL, aes(x=as.numeric(cat), y=value))+geom_line(data=t_air_7, linewidth=1,color="#DBE9F5" )+geom_line(data=t_air_6, size=1, color="#AFC6D9")+geom_line(data=t_air_5, size=1, color="#83A3BE")+geom_line(data=t_air_4, size=1, color="#5880A2")+geom_line(data=t_air_3, size=1, color="#2C5D87")+geom_line(data=t_air_2, size=1, color="#003A6B")+geom_line(data=t_air_1, size=1, color="gray24")+geom_line(data=t_air, size=1.3, color="#000000")+theme_minimal()+xlab("Air temp. (°C)")+ylab("ALE value")+theme(text=element_text(size=7)) +
  theme(panel.background = element_rect(fill="white", color="black", linetype = "solid"),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_line(color="gray90"), panel.grid.major.x = element_line(color="gray90"),panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black"), axis.text = element_text(color="black", size=12),axis.title = element_text(size=12, color="black") ,legend.position = "none",plot.margin = ggplot2::margin(t=2,r=2,b=2,l=5)))

(t_surfplot<-ggplot(data=NULL, aes(x=as.numeric(cat), y=value))+geom_line(data=t_surf_7, linewidth=1,color="#DBE9F5" )+geom_line(data=t_surf_6, size=1, color="#AFC6D9")+geom_line(data=t_surf_5, size=1, color="#83A3BE")+geom_line(data=t_surf_4, size=1, color="#5880A2")+geom_line(data=t_surf_3, size=1, color="#2C5D87")+geom_line(data=t_surf_2, size=1, color="#003A6B")+geom_line(data=t_surf_1, size=1, color="gray24")+geom_line(data=t_surf, size=1.3, color="#000000")+theme_minimal()+xlab("Soil surface temp. (°C)")+ylab("ALE value")+theme(text=element_text(size=7))+
  theme(panel.background = element_rect(fill="white", color="black", linetype = "solid"),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_line(color="gray90"), panel.grid.major.x = element_line(color="gray90"),panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black"), axis.text = element_text(color="black", size=12),axis.title = element_text(size=12, color="black") ,legend.position = "none",plot.margin = ggplot2::margin(t=2,r=2,b=2,l=5)))

(t_5plot<-ggplot(data=NULL, aes(x=as.numeric(cat), y=value))+geom_line(data=t_5_7, linewidth=1,color="#DBE9F5" )+geom_line(data=t_5_6, size=1, color="#AFC6D9")+geom_line(data=t_5_5, size=1, color="#83A3BE")+geom_line(data=t_5_4, size=1, color="#5880A2")+geom_line(data=t_5_3, size=1, color="#2C5D87")+geom_line(data=t_5_2, size=1, color="#003A6B")+geom_line(data=t_5_1, size=1, color="gray24")+geom_line(data=t_5, size=1.3, color="#000000")+theme_minimal()+xlab("Soil 5 cm temp. (°C)")+ylab("ALE value")+theme(text=element_text(size=7)) +
  theme(panel.background = element_rect(fill="white", color="black", linetype = "solid"),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_line(color="gray90"), panel.grid.major.x = element_line(color="gray90"),panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black"), axis.text = element_text(color="black", size=12),axis.title = element_text(size=12, color="black") ,legend.position = "none",plot.margin = ggplot2::margin(t=2,r=2,b=2,l=5)))

#make legend
for_legend<-ggplot()+ylim(2,10)+xlim(1,10)+
  geom_segment(aes(y=9.5, yend=9.5 ,x=1,xend=2.2),color="#000000", size=1.5)+annotate("text",x=4.32,y=9.6,label=c("No lag"),size=3.6)+
  geom_segment(aes(y=8.5, yend=8.5 ,x=1,xend=2.2),color="gray24", size=1)+annotate("text",x=4.75,y=8.6,label=c("1 day lag"),size=3.6)+
  geom_segment(aes(y=7.5, yend=7.5 ,x=1,xend=2.2),color="#003A6B", size=1)+annotate("text",x=5,y=7.6,label=c("2 days lag"),size=3.6)+
  geom_segment(aes(y=6.5, yend=6.5 ,x=1,xend=2.2),color="#2C5D87", size=1)+annotate("text",x=5,y=6.6,label=c("3 days lag"),size=3.6)+
  geom_segment(aes(y=5.5, yend=5.5 ,x=1,xend=2.2),color="#5880A2", size=1)+annotate("text",x=5,y=5.6,label=c("4 days lag"),size=3.6)+
  geom_segment(aes(y=4.5, yend=4.5 ,x=1,xend=2.2),color="#83A3BE", size=1)+annotate("text",x=5,y=4.6,label=c("5 days lag"),size=3.6)+
  geom_segment(aes(y=3.5, yend=3.5 ,x=1,xend=2.2),color="#AFC6D9", size=1)+annotate("text",x=5,y=3.6,label=c("6 days lag"),size=3.6)+
  geom_segment(aes(y=2.5, yend=2.5 ,x=1,xend=2.2),color="#DBE9F5", size=1)+annotate("text",x=5,y=2.6,label=c("7 days lag"),size=3.6)+
  theme(panel.background = element_rect(fill="white", color="white", linetype = "solid"),axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

#combine plots
plot_grid(moist10plot,moist20plot,wtlplot, precipplot, t_ambbplot, t_surfplot, t_5plot, for_legend, ncol = 4, nrow=2, align="hv", labels = c("a)","b)","c)","d)","e)","f)","g)"),label_fontface = "plain", label_size = 12, vjust = 2.8, hjust=-0.8)
#interpretation: ALE value (y-axis) zero is the mean predicted N2O flux. ALE values larger than that mean that predicted flux is larger. Negative values mean smaller predicted N2O flux than the mean prediction.

######
#6. model evaluation

#make OOB prediction
preds<-predict(mod,type="response" ,OOB=T)

#OOB RMSE
rmse(actual=training_final$n2o, predicted=preds)

#OOB R2
cor(preds, training_final$n2o)^2

##make prediction to within-training-period evaluation data
evaluation_within$preds<-predict(mod, newdata=evaluation_within, type="response")

#evaluation within training period RMSE
rmse(actual=evaluation_within$n2o, predicted=evaluation_within$preds)

#evaluation within training period R2
cor(evaluation_within$preds, evaluation_within$n2o)^2

##make prediction to outside-training-period evaluation data
evaluation_fourthtear$preds<-predict(mod, newdata=evaluation_fourthtear, type="response")

#evaluation outside training period RMSE
rmse(actual=evaluation_fourthtear$n2o, predicted=evaluation_fourthtear$preds)

#evaluation within training period R2
cor(evaluation_fourthtear$preds, evaluation_fourthtear$n2o)^2

#plot predicted vs measured
plot(training_final$n2o, preds)
plot(evaluation_within$n2o, preds2)
plot(evaluation_fourthtear$n2o, preds3)

#plot in time series
x<-data
names(x)[2]<-"preds"
ggplot(data=NULL, aes(x=date, y=preds))+geom_point(data=x)+geom_point(data=evaluation_within, color="tomato")+geom_point(data=evaluation_fourthtear, color="tomato4")+ylab("predicted/measured flux")


