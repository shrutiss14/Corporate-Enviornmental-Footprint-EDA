library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(scales)

data<-read.csv("C:\\Users\\sharm\\OneDrive\\Desktop\\Visu Project\\final_raw_sample_0_percent.csv")

data <- data %>%
  rename("Company Name"="Company.Name",
         "Exiobase industry category" = "Industry..Exiobase.",
         "Total Environmental Intensity in % (Revenue)" = "Total.Environmental.Intensity..Revenue.",
         "Total Environmental Intensity in % (Operating Income)" = "Total.Environmental.Intensity..Operating.Income.",
         "Total Environmental Cost" = "Total.Environmental.Cost",
         "Working Capacity" ="Working.Capacity",
         "Fish Production Capacity" = "Fish.Production.Capacity",                                     
         "Crop Production Capacity" = "Crop.Production.Capacity",                                     
         "Meat Production Capacity" = "Meat.Production.Capacity",                                  
         "Abiotic Resources" = "Abiotic.Resources",
         "Water Production Capacity" = "Water.production.capacity..Drinking.water...Irrigation.Water.",
         "Wood Production Capacity" = "Wood.Production.Capacity",
         "% Imputed" = "X..Imputed")

data <- data %>%
  mutate(across(c("Total Environmental Intensity in % (Revenue)",
                  "Total Environmental Intensity in % (Operating Income)",
                  "% Imputed"), 
                ~ as.numeric(gsub("%", "", .))))

data <- data %>%
  mutate(across(c("Total Environmental Cost",                             
                  "Working Capacity",                                     
                  "Fish Production Capacity",                             
                  "Crop Production Capacity",                             
                  "Meat Production Capacity",                             
                  "Biodiversity",                                         
                  "Abiotic Resources",                                    
                  "Water Production Capacity",                            
                  "Wood Production Capacity"),
                ~as.numeric(gsub("\\((.*?)\\)", "-\\1", gsub(",", "", .)))))

data <- na.omit(data)

ggplot(data, aes(x = `Total Environmental Intensity in % (Revenue)`)) +
  geom_histogram(binwidth = 0.5,fill="lightblue",color="black",alpha=0.7) +
  labs(title = "Distribution of Total Environmental Intensity (Revenue)",
       x = "Intensity (% of Revenue)", y = "Frequency") +
  xlim(-20, 5) +theme_minimal()+
  theme(axis.title.x = element_text(margin = margin(t = 10)),  
        axis.title.y = element_text(margin = margin(r = 10)))

ggplot(data, aes(x = `Total Environmental Intensity in % (Operating Income)`)) +
  geom_histogram(binwidth = 0.5,fill = "seagreen1", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Total Environmental Intensity (Operating Income)",
       x = "Intensity", y = "Frequency") +
  xlim(-50, 5) +theme_minimal()+
  theme(axis.title.x = element_text(margin = margin(t = 10)),  
        axis.title.y = element_text(margin = margin(r = 10)))

ggplot(data, aes(x = `Year`, y = `Total Environmental Cost`)) +
  geom_line(stat = "summary", fun = "mean",color="slateblue4") +
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Average Environmental Cost Over Time",
       x = "Year", y = "Average Environmental Cost")+theme_minimal()+
  theme(axis.title.x = element_text(margin = margin(t = 10)),  
        axis.title.y = element_text(margin = margin(r = 10)))

ggplot(data, aes(x = `Total Environmental Intensity in % (Revenue)`, 
                 y = `Total Environmental Cost`,
                 size = `Total Environmental Intensity in % (Operating Income)`)) +
  geom_point(alpha = 0.7,shape=21,fill="lightpink",color="hotpink3",stroke=0.4) +
  labs(title = "Environmental Cost vs. Total Environmental Intensities", 
       x = "Total Environmental Intensity (% of Revenue)", 
       y = "Total Environmental Cost") +
  theme_minimal() +
  scale_size_continuous(name = "Intensity (% of Operating Income)",labels=label_number())+
  scale_y_continuous(labels = label_number()) +
  scale_x_continuous(labels = label_number())+
  theme(axis.title.x = element_text(margin = margin(t = 10)),  
        axis.title.y = element_text(margin = margin(r = 10)))
