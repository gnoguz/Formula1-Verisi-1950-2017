##OGUZHAN#GUNDUZ##
##gnoguz@outlook.com##



library(dplyr)
library(readr)
library(magrittr)
library(DT)
library(ggplot2)
library(plotly)
library(naniar)
library(rgl)
library(lubridate)
library(mice)
library(UpSetR)
library(stringr)


F1data <- read.csv("F1AllData.csv")
head(F1data)

datatable(F1data, options = list(scrollX=T))



summary(F1data)


#1)SEZONLARDAKÝ PÝLOT SAYILARI

one_seoson_pilot_number <- F1data %>% group_by(season, full.name) %>%
  summarise(number_of_pilot = n())%>%  
  group_by(season)%>% 
  summarise(number_of_pilot = n())

one_seoson_pilot_number <- as.data.frame(one_seoson_pilot_number)
datatable(one_seoson_pilot_number)


ggplot(data = one_seoson_pilot_number)+
  geom_col(aes(x=season, 
               y = number_of_pilot, ),fill = "blue",  size=5)+
  labs(x = "Yýllar", 
       y = "Her Yýldaki Pilot Sayýsý",
       title = "Yýllara Göre Pilot Sayýlarý")+
  theme_bw()


datatable(one_seoson_pilot_number, options = list(scrollX=T))






#2)SEZONLARDAKÝ PÝST SAYILARI

one_seoson_circuit_number <- F1data %>% group_by(season, circuitName) %>%
  summarise(number_of_circuit = n())%>%  
  group_by(season)%>% 
  summarise(number_of_circuit = n())

one_seoson_circuit_number

ggplot(data = one_seoson_circuit_number)+
  geom_col(aes(x=season, 
               y =number_of_circuit ),fill = "red",  size=5)+
  labs(x = "Yýllar", 
       y = "Her Yýldaki Pist Sayýsý",
       title = "Yýllara Göre Pist Sayýlarý")+
  theme_bw()

datatable(one_seoson_circuit_number, options = list(scrollX=T))






#3)2000-2017 YILLARI ARASINDAKÝ YARIÞAN TAKIMLARIN ÜLKE DAÐILIMLARI 

F1data %>%filter(season>2000) %>%  select(Constructor.name, Constructor.nationality) %>%
  ggplot()+
  geom_point(aes(x= Constructor.nationality, 
                 y = Constructor.name), color = "blue", size = 5)+
  labs(x = "Takýmýn Ülkesi", 
       y = "Takýmýn Ýsmi",
       title = "Takýmlarýn Ülkelere Göre DAðýlýmý")+
  theme_bw()






#4)HER SEZONUN ÞAMPÝYON PÝLOTUNUN HESAPLANMASI

sampiyonlar<-F1data %>% select(season,full.name,results.grid, points) %>%
  group_by(full.name, season) %>%  summarise(total = sum(points)) %>%
  group_by(season) %>% 
  top_n(1) %>% 
  arrange(season)

datatable(sampiyonlar)

  ggplot(data = sampiyonlar)+
  geom_point(aes(x = total, 
                 y = full.name, 
                 color = season),  size=3)+
  scale_colour_gradient(low = "blue", high = "red", na.value = NA)+
  labs(x = "Toplam Puan", 
       y = "Pilotlar",
       title = "Þampiyon Olan Pilotlarýn Toplam Puanlarý")+
  theme_bw()




#5)PÝLOTLARIN YARIÞTIKLARI YAÞLAR


#"-" ile ayrýlan tarihlerin düzeltilmesi

for (i in 1:length(F1data$dateOfBirth)-1){
  f1split <- strsplit(F1data$dateOfBirth[i], split = "")
  f1split <- unlist(f1split)
  
  if("-" %in% f1split){
    
    f1split <- strsplit(F1data$dateOfBirth[i], split = "-")
    f1split <- unlist(f1split)
    f1split_1 <- f1split[1] 
    f1split_2 <- f1split[2]
    f1split_3 <- f1split[3]
    
    f1split[1]<- f1split_2
    f1split[2]<- f1split_3
    f1split[3]<- f1split_1
    
    f1splitx <- paste(f1split[1],f1split[2],f1split[3])
    f1splitx <- gsub(pattern = " ", "/", f1splitx)
    F1data$dateOfBirth[i] <-f1splitx
  }
  
}


F1data$dateOfBirth


# Gün,ay ve yýlýn ayrýlmasý

F1data$race_day <- str_split(F1data$date,"/",simplify = T)[,1]
F1data$race_month <- str_split(F1data$date,"/",simplify = T)[,2]
F1data$race_year <- str_split(F1data$date,"/",simplify = T)[,3]

F1data$pilot_day <- str_split(F1data$dateOfBirth, "/",simplify =T)[,1]
F1data$pilot_month <- str_split(F1data$dateOfBirth, "/",simplify =T)[,2]
F1data$pilot_year <- str_split(F1data$dateOfBirth, "/",simplify =T)[,3]


F1data %<>% relocate(race_day, .after=date)
F1data %<>% relocate(race_month, .after=race_day)
F1data %<>% relocate(race_year, .after=race_month)

F1data %<>% relocate(pilot_day, .after=dateOfBirth)
F1data %<>% relocate(pilot_month, .after=pilot_day)
F1data %<>% relocate(pilot_year, .after=pilot_month)

datatable(F1data, options = list(scrollX=T))

F1data$race_year <- as.integer(F1data$race_year)
F1data$pilot_year<- as.integer( F1data$pilot_year)



for(i in 1:length(F1data$dateOfBirth)-1){
  
  
  
  F1data$pilot_age[i] = (F1data$race_year[i] - F1data$pilot_year[i])
  
  
} 

F1data %<>% relocate(pilot_age, .after=pilot_year)

datatable(F1data, options = list(scrollX=T))






#6)HER BÝR PÝLOTUN NE KADAR YARIÞ KAZANDIÐI

#Fonksiyon
Lead_pilot <- function(x){
  
  
  x<- toupper(x)
  counter <- 0
  
  
  F1data$familyName <- toupper(F1data$familyName)
  F1data$familyName
  
  if(!is.character(x)){
    stop('Bu Fonksiyon Sadece Pilot Ýsimleri Ýle Çalýþmaktadýr\n',
         ' classes:\n',
         'x: ', class(x))
  }
  
  
  if( x %in% F1data$familyName){
    
  }else{
    stop('Aradýðýnýz Kiþi Pilotlar Arasýnda Yok')
    
  }
  
  
  for (i in 1:length(F1data$familyName)-1) {
    
    if (x %in% F1data$familyName[i]){
      
      if (F1data$results.grid[i] == 1){
        
        counter<-counter+1
        
      }
    }
  }
  
wins<- counter
wins
}
#test
Lead_pilot("SCHUMACHER")


#Pilot Ýsimlerinin ve Kazandýklarý Yarýþ Sayýlarýnýn Birleþtirilmesi
pilots<-  F1data %>%  select(familyName, results.grid) %>%  group_by(familyName) %>% 
  summarise(race_number_for_each_pilot = n()) 

pilot_name <- pilots$familyName

wins <- sapply(pilot_name, Lead_pilot)

wins <- as.data.frame(wins, colnames(wins))

wins<- wins$wins

wins <- data.frame(pilot_name,wins)
datatable(wins)


#Görselleþtirme
wins %>% filter(wins > 5) %>%
  ggplot()+
  geom_col(aes(x=wins, 
                 y = pilot_name, 
                 fill = wins) ,size=2 )+
  scale_fill_gradient(low = "blue", high = "red", na.value = NA)+
  labs(x = "Kazandýklarý Yarýþ Sayýsý", 
       y = "Pilotlarýn Ýsmi",
       title = "5'ten Fazla Yarýþ Kazana Pilotlarýn Kazandýklarý Yarýþ Sayýlarý")+
  theme_bw()




#7)PÝLOTLARIN YARIÞ BAÞINA ALDIKLARI PUANLAR

#katýldýklarý yarýþlarýn hesaplanmasý için fonksiyon
Races_pilot <- function(x){
  
  
  x<- toupper(x)
  c <- 0
  
  F1data$familyName <- toupper(F1data$familyName)
  F1data$familyName
  
  for (i in 1:length(F1data$familyName)-1) {
    
    if (x %in% F1data$familyName[i]){
      
      c<-c+1
      
    }
  }
  
  races <-c
  races
  
}
#test
Races_pilot("SCHUMACHER")



pilots<-  F1data %>%  select(familyName, position) %>%  group_by(familyName) %>% 
  summarise(race_number_for_each_pilot = n()) 

pilot_name <- pilots$familyName

races <- sapply(pilot_name, Races_pilot)

races <- as.data.frame(races, colnames(races))

races<- races$races

races <- data.frame(pilot_name,races)
races



total_points<- F1data %>% select(familyName,position, points) %>%
  group_by(familyName) %>%  summarise(total_points = sum(points)) 
total_points


yaris_basi_puan<- right_join(total_points,races, by = c("familyName"="pilot_name"))

datatable(yaris_basi_puan)


yaris_basi_puan<-yaris_basi_puan %>% mutate(yaris_basi_puan = total_points/races) %>% 
  arrange(desc(yaris_basi_puan))

datatable(yaris_basi_puan)


yaris_basi_puan %>% filter(yaris_basi_puan>2) %>% 
  ggplot()+
  geom_point(aes(x= yaris_basi_puan, 
                 y = familyName), color = "red", size = 3)+
  labs(x = "Yarýþ Baþýna Puan", 
       y = "Pilotlar",
       title = "Her Bir Pilotun Yarýþ Baþýna Aldýðý Puan")+
  theme_bw()







