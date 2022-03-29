###############################
#Estimating Completions and Permissions 2011-2021
###############################
#
# Dr Paul Kilgarriff
#

rm(list = ls())
library(csodata)
library(stringr)
library(dplyr)

#permissions
cso_perm <- cso_get_data("BHQ12",wide_format="long")
#rename - quarter column
colnames(cso_perm)[2] <- "quarter"

#keep 2010 onwards - drop using row index - keep only planning units
cso_perm <- cso_perm[ which(cso_perm$Statistic=="Units for which Permission Granted"),]
cso_perm <- cso_perm[-c(1:29584),]
cso_perm$Statistic <- as.character(cso_perm$Statistic)
cso_perm$Type.of.Dwelling <- as.character(cso_perm$Type.of.Dwelling)
cso_perm$Region.and.County <- as.character(cso_perm$Region.and.County)

#drop provinces and NUTS regions
cso_perm <- cso_perm[ which(
                                cso_perm$Region.and.County!='Dublin'&
                                cso_perm$Region.and.County!='Eastern and Midland'&
                                cso_perm$Region.and.County!='Mid-East'&
                                cso_perm$Region.and.County!='Midland'&
                                cso_perm$Region.and.County!='South-West'&
                                cso_perm$Region.and.County!='South-East'&
                                cso_perm$Region.and.County!='Mid-West'&
                                cso_perm$Region.and.County!='Southern'&
                                cso_perm$Region.and.County!='West'&
                                cso_perm$Region.and.County!='Border'&
                                cso_perm$Region.and.County!='Northern and Western'),]
#rename local authorities
cso_perm$Region.and.County <- as.character(cso_perm$Region.and.County)
cso_perm$Region.and.County[cso_perm$Region.and.County == "Cork"] <- "Cork County"
cso_perm$Region.and.County[cso_perm$Region.and.County == "Galway"] <- "Galway County"
cso_perm$Region.and.County[cso_perm$Region.and.County == "Dun Laoghaire - Rathdown"] <- "DLR"

#replace NA values
cso_perm[is.na(cso_perm)] <- 0

#drop variable
cso_perm$Statistic <- NULL

#replace non-numeric characters
cso_perm$value <- gsub("-", "0", cso_perm$value)
cso_perm$value <- as.numeric(cso_perm$value)

#sum by groups
cso_perm <- cso_perm %>% 
  group_by(quarter,Region.and.County,Type.of.Dwelling) %>% 
  summarise(value = sum(value))

#permissions pre-2018
cso_perm_old <- cso_get_data("BHQ02",wide_format="long")
#rename quarter
colnames(cso_perm_old)[2] <- "quarter"

#reformat variables
cso_perm_old$Statistic <- as.character(cso_perm_old$Statistic)
cso_perm_old$Type.of.Dwelling <- as.character(cso_perm_old$Type.of.Dwelling)
cso_perm_old$Region.and.County <- as.character(cso_perm_old$Region.and.County)

#subset keep observations
cso_perm_old <- cso_perm_old[ which(cso_perm_old$Statistic=="Units for which Permission Granted"),]

#rename variables - merge north and south tipperary etc.
cso_perm_old$Region.and.County[cso_perm_old$Region.and.County == "Cork"] <- "Cork County"
cso_perm_old$Region.and.County[cso_perm_old$Region.and.County == "Galway"] <- "Galway County"
cso_perm_old$Region.and.County[cso_perm_old$Region.and.County == "Dun Laoghaire-Rathdown"] <- "DLR"
cso_perm_old$Region.and.County[cso_perm_old$Region.and.County == "South Dublin Co. Co."] <- "South Dublin"
cso_perm_old$Region.and.County[cso_perm_old$Region.and.County == "South Tipperary"] <- "Tipperary"
cso_perm_old$Region.and.County[cso_perm_old$Region.and.County == "North Tipperary"] <- "Tipperary"
cso_perm_old$Region.and.County[cso_perm_old$Region.and.County == "Limerick City"] <- "Limerick"
cso_perm_old$Region.and.County[cso_perm_old$Region.and.County == "Waterford City"] <- "Waterford"

#drop observations pre 2010 using row index
cso_perm_old <- cso_perm_old[-c(1:6480),]
cso_perm_old[is.na(cso_perm_old)] <- 0

#drop provinces and NUTS regions
cso_perm_old <- cso_perm_old[ which(
                              cso_perm_old$Region.and.County!='Dublin'&
                              cso_perm_old$Region.and.County!='Eastern and Midland'&
                              cso_perm_old$Region.and.County!='Mid-East'&
                              cso_perm_old$Region.and.County!='Midland'&
                              cso_perm_old$Region.and.County!='South-West'&
                              cso_perm_old$Region.and.County!='South-East'&
                              cso_perm_old$Region.and.County!='Mid-West'&
                              cso_perm_old$Region.and.County!='Southern'&
                              cso_perm_old$Region.and.County!='West'&
                              cso_perm_old$Region.and.County!='Border'&
                                cso_perm_old$Region.and.County!='Border, Midland and Western'&
                                cso_perm_old$Region.and.County!='Southern and Eastern'&
                              cso_perm_old$Region.and.County!='Northern and Western'),]

#sum by groups
cso_perm_old <- cso_perm_old %>% 
  group_by(quarter,Region.and.County,Type.of.Dwelling) %>% 
  summarise(value = sum(value))

#replace NA values
cso_perm_old[is.na(cso_perm_old)] <- 0

#rbind - planning permissions 2010-2017 and 2018-2021
cso_perm <- rbind(cso_perm,cso_perm_old)

#replace values and rename columns
cso_perm$Type.of.Dwelling[cso_perm$Type.of.Dwelling == "Multi development houses"] <- "Scheme house"
cso_perm$Type.of.Dwelling[cso_perm$Type.of.Dwelling == "One off houses"] <- "Single house"
cso_perm$Type.of.Dwelling[cso_perm$Type.of.Dwelling == "Private flats/apartments"] <- "Apartment"
colnames(cso_perm)[3] <- "dwelling"
colnames(cso_perm)[2] <- "county"
colnames(cso_perm)[4] <- "perm_units"

cso_perm$perm_units <- gsub("-", "0", cso_perm$perm_units)
cso_perm$perm_units <- as.numeric(cso_perm$perm_units)
cso_perm <- cso_perm[order(cso_perm$quarter),]
cso_perm[is.na(cso_perm)] <- 0

#permissions complete

#completions
cso_comp <- cso_get_data("NDQ06",wide_format="long")
#rename variables and fix local authority names
cso_comp$Local.Authority <- as.character(cso_comp$Local.Authority)
cso_comp$Local.Authority[cso_comp$Local.Authority == "Ireland"] <- "State County Council"
cso_comp$Local.Authority<-str_sub(cso_comp$Local.Authority,end=-16)
cso_comp$Type.of.House <- as.character(cso_comp$Type.of.House)

#fix names - rename
cso_comp$Local.Authority[cso_comp$Local.Authority == "South Dublin Co. Co."] <- "South Dublin"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Galway"] <- "Galway County"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Galw"] <- "Galway City"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Tipperary N.R."] <- "Tipperary"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Tipperary S.R."] <- "Tipperary"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Limeri"] <- "Limerick"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Waterfo"] <- "Waterford"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Cork"] <- "Cork County"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Co"] <- "Cork City"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Dun Laoire/Rathdown"] <- "DLR"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Dún Laoghaire Rathdown"] <- "DLR"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Dubl"] <- "Dublin City"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Co"] <- "Cork City"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Limerick City &"] <- "Limerick"
cso_comp$Local.Authority[cso_comp$Local.Authority == "Waterford City &"] <- "Waterford"

#replace NA values with 0
cso_comp$quarter <- as.character(cso_comp$Quarter)
cso_comp$Quarter <- NULL
cso_comp[is.na(cso_comp)] <- 0
  
#sum by group
cso_comp <- cso_comp %>% 
  group_by(quarter,Local.Authority,Type.of.House) %>% 
  summarise(value = sum(value))

#create houses type of dwelling by adding scheme and single houses
df1 <- cso_comp
df1 <- df1[ which(df1$Type.of.House!="Apartment"),]
df1$Type.of.House = "Houses"
df1 <- df1 %>% 
  group_by(quarter,Local.Authority,Type.of.House) %>% 
  summarise(value = sum(value))

#rbind houses to other house completion data
cso_comp <- rbind(cso_comp,df1)
colnames(cso_comp)[2] <- "county"
colnames(cso_comp)[3] <- "dwelling"
colnames(cso_comp)[4] <- "comp_units"

#drop dataframes
rm("df1","cso_perm_old")

#formate variable
cso_perm$quarter <- as.character(cso_perm$quarter)

#completions complete.


#merge together - permissions and completions
cso_comp_perm <- merge(cso_comp,cso_perm,by=c("quarter","county","dwelling"),all.x=T,all.y=T)
cso_comp_perm$perm_units <- gsub("-", "0", cso_comp_perm$perm_units)
cso_comp_perm$perm_units <- as.numeric(cso_comp_perm$perm_units)
cso_comp_perm[is.na(cso_comp_perm)] <- 0

#difference new permissions and completions
cso_comp_perm$diff <- cso_comp_perm$perm_units - cso_comp_perm$comp_units

#order by quarter before cumulative sum
cso_comp_perm <- cso_comp_perm[order(cso_comp_perm$quarter),]

#cumulative sum
df1 <- cso_comp_perm %>% 
  group_by(county,dwelling) %>% 
  mutate(csum = cumsum(diff))

#save output as CSV file
write.csv(df1,"D:/Glenveagh/data/Alternative_NPF/comple_planning_10_21_v6.csv", row.names = FALSE)














#creating graphs with ggplot2

#add colours to dataframe
df1$colour_code = ""
df1$colour_code[df1$dwelling == "Apartment"]<- "blue"
df1$colour_code[df1$dwelling == "Scheme house"]<- "red"
df1$colour_code[df1$dwelling == "Single house"]<- "brown"
df1$colour_code[df1$dwelling == "Houses"]<- "black"

library(ggthemr)
#create graphs by local authority
ctys <- unique(df1$county)

setwd("D:/Glenveagh/output/outstanding_units/")

for(i in ctys) {
  print(i) 
  df_sub <- df1[ which(df1$county==i),] 
  df_sub <- df_sub[ which(df_sub$dwelling!="Houses"),] 
  
#Make plot
ggthemr("dust") 

graph_title <- as.character(paste(i," - Cumulative dwelling units", sep=""))
file_save <- as.character(paste("csum_",i,".png", sep=""))

ggplot(df_sub, aes(x = year, y = csum, group = dwelling, colour=dwelling)) +
  geom_line(size = 1) +
  scale_colour_manual(name="Dwelling type:",values=setNames(df_sub$colour_code, df_sub$dwelling))+
  geom_hline(yintercept = 0, size = 1,alpha=0.5, colour="#333333") +
  #scale_y_continuous(trans='log',breaks = seq(0, 1, by = .10))+
  labs(title=graph_title,
       subtitle = "Cumulative sum of new planning permission units less units completed during year",
       caption = "")+
  labs(x = "Year", y = "Cumulative sum of units")

ggsave(file_save, dpi = 500)

}
