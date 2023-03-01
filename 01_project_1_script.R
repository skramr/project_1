#Project 1 script

# LIBRARIES ----
library(tidymodels)
library(vip)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(GGally)
library(tidyr)
library(generics)
library(rsample)
library(parsnip)
library(recipes)
library(workflows)
#load data


#overview of data
pine_beetles_dat <-readxl::read_excel("~/Downloads/data_science_2/project_1/Data_1993.xlsx")

#variable names
# what is response? infest_serv1 and 2 are the same description...



#Infest_sever1- Infestation severity nearest to response tree
#Ind_DeadDist-Indicator if nearest brood tree is within 50m effective distance
#DeadDist-Minimum linear distance to nearest brood tree
#SDI_20th-Stand Density Index @ 1/20th acre neighborhood surrounding response 
#Neigh_SDI_1/4th- Basal area total summed for all trees within 1/4th acre neighborhood of response tree
#BA_20th-Basal Area @ 1/20th acre neighborhood surrounding respons
#Neigh_1/4th-Basal area total summed for all trees within 1/4th acre neighborhood of response 
#BA_Infest_1.5-Indicator of any infested trees within 1.5 acre neighborhood of response tree
#IND_BA_Infest_20th-binary  indicator for if a response tree has any infested trees within neighboorhood
#BA_Infest_1/4th-Indicator of any infested trees within 1/4th acre neighborhood of response tree


pine_beetles_dat %>% ggplot(aes(x=Infest_Serv1, y=Neigh_1)) + geom_smooth()

#Logistic regression
Ind_DeadDist ~ BA_Infest_1/4th + Ind_DeadDist+ tree_diam



#map out the trees
library(ggmap)
library(sf)




# getting the map
#easting is lat
p1 <- st_as_sf(pine_beetles_dat, coords = c("Easting", "Northing"), crs = "EPSG:3857")
p2 <- st_transform(p1, crs= "EPSG:4326")

mapgilbert <- get_map(location = c(lon = mean(pine_beetles_dat$Northing), lat = mean(pine_beetles_dat$Easting)), zoom = 4,
                      maptype = "satellite", scale = 2)

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

pine_beetles_dat %>% 
  mutate(spUTM = map(pine_beetles_dat, ~ 
                       SpatialPoints(df[c('Easting', 'Northing')], proj4string = CRS(.x))))
pine_beetles_dat %>% ggplot(aes(x= Easting, y= Northing)) + geom_point(aes(color=factor(Response)))





#graph of killed trees over dead distance
pine_beetles_dat$Response<- as.factor(pine_beetles_dat$Response)
pine_beetles_dat  %>% 
  ggplot( aes(x=DeadDist, fill=Response)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

# pine_beetles_dat  %>% 
ggplot( aes(x=`Neigh_1/2th`, y= 'Neigh_1/4th', fill=Response)) +
  geom_point() +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


# neigh .25-1.5 faceted w/ bins

#set up cut off values
breaks <- c(0,10,20,30, 40, 50, 60, 70, 80, 90, 100)
#create buckets
tags <- c("[0-10)","[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)","[60-70)", "[70-80)","[80-90)", "[90-100)")
# bucketing values into bins
group_tags <- cut(pine_beetles_dat$`Neigh_1/4th`, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)

summary(group_tags)


neigh_area_.25_groups <- factor(group_tags, 
                           levels = labels,
                           ordered = TRUE)




#Bar Plot--Response Tree infection to Neighboorhood Infection
pine_beetles_dat %>% dplyr::select(IND_BA_Infest_20th:IND_BA_Infest_1.5, DeadDist, Response) %>% #get dsm scores in long format
  tidyr::pivot_longer(IND_BA_Infest_20th:IND_BA_Infest_1,
                      names_to="neighboorhood_tree_radius",
                      values_to = "infection_response")%>%   group_by(neighboorhood_tree_radius, infection_response, Response)  %>%
  summarise(N=n())  %>% #sum of n to find out the occurences of infection at each infected tree radius
mutate(neighboorhood_tree_radius= factor(neighboorhood_tree_radius),
       N= as.numeric(N),
       infection_response= recode_factor(infection_response, "0"= "No tree in neighboorhood infected",
                            "1"= "Tree has been infected in neighboorhood"))  %>%
  filter(infection_response != "NA") %>% ungroup()    %>%
  ggplot(aes(x=fct_reorder(neighboorhood_tree_radius, N), y=N, fill=Response))+ geom_bar(position="dodge", stat="identity")+ 
  facet_wrap(infection_response~., scales="free_y", ncol=1)+ labs(x= "Neighboorhood Size (acres)") +
  scale_x_discrete(labels=c("IND_BA_Infest_20th"="All sizes", "IND_BA_Infest_1/4th"= ".25", "IND_BA_Infest_1/2th"=".5", "IND_BA_Infest_1"= "1", "IND_BA_Infest_1.5"="1.5")) + theme_bw()
  

#Bins of infested trees within 1.5 acre
breaks <- c(0,10,20,30, 40, 50, 60, 70, 80, 90, 110)
#create buckets
tags <- c("[0-10)","[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)","[60-70)", "[70-80)","[80-90)", "[90-110)")
# bucketing values into bins
pine_beetles_dat<- pine_beetles_dat %>% drop_na()

group_tags <- cut(pine_beetles_dat$BA_Infest_1.5, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags) 

summary(group_tags)


factor(group_tags, 
       levels = tags,
       ordered = TRUE)


ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) + 
  geom_bar(fill="bisque",color="white",alpha=0.7) + 
  stat_count(geom="text", aes(label=sprintf("%.4f",..count../length(group_tags))), vjust=-0.5) +
  labs(x='Infested trees per 1.5 acre') +
  theme_minimal() 

pine_beetles_dat %>% 
  group_by(Response)  %>% 
  ggplot(aes(x = Infest_Serv1, y = DeadDist,color = Response)) +
  geom_point()

ggplot(aes(x=BA_Infest_1.5, y=SDI_20th),data=pine_beetles_dat)+
  geom_jitter(alpha=1/20)+
  ylim(0,50)

ggplot(aes(x=age,y=friend_count),data=pf)+
  geom_point(alpha=1/20,
             position=position_jitter(h=0),
             color='red')+
  xlim(13,90)+
  coord_trans(y = "sqrt")+
  geom_line(stat='summary',fun.y=mean)+
  geom_line(stat='summary',fun.y=median,linetype=2,color='blue')+
  geom_line(stat='summary',fun.y=quantile,fun.args=list(probs=0.9),color='blue')

  #test corplot
  test<- pine_beetles_dat %>% select(DeadDist:Neigh_1.5) %>% cor()
corrplot::corrplot(test)
  
  
#DT Table
  
DT::datatable(pine_beetles_dat, options = list(
    bPaginate = FALSE
  ))
  
  filter(value != "NA") %>% ungroup()  %>% mutate(Sex = recode_factor(Sex,
                                                                      "1" = "Male", 
                                                                      "0" = "Female"))  %>%
  ggplot(aes(x=fct_reorder(dsm_dx, N), y=N, fill=Sex))

heatmap(as.matrix(pine_beetles_dat), Rowv = NA, Colv = NA)   



#new graph

#create different tiers of infestation severity
#look at the mean and scores that are more than 2 SD away are severe
data_long <- pine_beetles_dat %>% select(`Neigh_1/4th`:`Neigh_1.5`) %>% gather()

data_long<- data_long%>% group_by(key)%>%
  summarise(mean= mean(value), sd= sd(value), max = max(value),min = min(value)) 

ggplot(aes(x=key , y=mean), color=Response)            
            
ggplot(pine_beetles_dat, aes(x = Infest_Serv1, y = DeadDist, group=Response))+ geom_boxplot()
#mild, moderate, severe


# Hexagonal binning
ggplot(pine_beetles_dat, aes(TreeDiam, DeadDist)) +
  geom_hex(bins = 20, color = "white")+
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07")+
  theme_minimal()


### logistic regression


pine_beetles_dat<- pine_beetles_dat %>%
  mutate(Response = as.factor(Response)) #specify outcome type what type of variable is it


set.seed(123)


#tidy model framework

# MODELING ----
model_fit_glm <- logistic_reg() %>% #specify type of model you want
  set_engine("glm") %>% #set engine, what technique do we want to estimate the parameters
  fit(Response ~ Infest_Serv1 + TreeDiam+ BA_Infest_1.5 , data = training(splits)) # predict the year and use the training data (grab the training data from splits)

model_fit_glm

model_fit_glm_2<- logistic_reg() %>% 
  set_engine("glm")
# * Create Workflow ----
pine_wflow <- 
  workflow() %>% 
  add_model(model_fit_glm_2) %>% 
  add_recipe(pine_rec)

pine_wflow


pine_fit <- 
  pine_wflow %>% 
  fit(data = pine_beetles_dat)


# * Create Recipe ----
## specify variable relationships
## specify (training) data
## feature engineer
## process recipe on data
pine_rec <- pine_beetles_dat %>% 
  recipe(Response ~ TreeDiam + Infest_Serv1 + BA_Infest_1.5 + DeadDist ) %>% 
  step_sqrt(.) %>% 
  step_corr(.) %>% 
 prep()

# View feature engineered data
pine_rec %>% 
  prep() %>% 
  juice()

# * Create Model GLM
glm_mod <- 
  logistic_reg() %>% 
  set_engine("glm")


# * Create Workflow ----
pine_wflow <- 
  workflow() %>% 
  add_model(glm_mod) %>% 
  add_recipe(pine_rec)

pine_wflow


pine_fit <- 
  pine_wflow %>% 
  fit(data = pine_beetles_dat)


pine_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

pine_fit %>% 
  extract_fit_parsnip() %>% 
  glance()

pine_fit %>% 
  extract_fit_parsnip() %>% 
  parnsip::check_model()

pine_fit %>% 
  extract_preprocessor()





# PREDICTION ----
#doesnt work
prediction_class_test <- predict(pine_fit, new_data = testing(splits), type = "class") #tells you what the predicted class is

prediction_prob_test  <- predict(pine_fit, new_data = testing(splits), type = "prob") #tells you the probability of predicting each class

results_tbl <- bind_cols( #Combine two predictions and the testing data
  prediction_class_test,
  prediction_prob_test,
  testing(splits)
)

#error?
pine_fit$fit %>%
  vip(                      #supply fit from the model
    num_features = 4,
    geom         = "point",
    aesthetics   = list(
      size     = 4,
      color    = "#18bc9c"
    )
  ) +
  theme_minimal(base_size = 18) +
  labs(title = "Logistic Regression: Feature Importance")


