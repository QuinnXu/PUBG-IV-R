library(jpeg)
library(ggplot2)
library(plyr)
library(dplyr)
library(gtable)
library(grid)
library(gridExtra)
library(scales)

##############################################################################################################
# LOAD DATA
##############################################################################################################

### load map image ###
img <- readJPEG("pubg-miramar.jpg",native=F)
g <- rasterGrob(img, interpolate = TRUE)
img_ <- readJPEG("pubg-erangel.jpg",native=F)
g_ <- rasterGrob(img_, interpolate = TRUE)

### load data ###
pubg.deaths <- read.csv("all_deaths.csv", header=T, sep=",")
pubg.deaths <- mutate(pubg.deaths,distance=sqrt(((victim_x-killer_x)/100)^2+((victim_y-killer_y)/100)^2))
head(pubg.deaths)

pubg.stats <- read.csv("all_stats.csv", header=T, sep=",")
head(pubg.deaths)


##############################################################################################################
# DATA MANIPULATION
##############################################################################################################

### deaths location ###
# map-miramar
miramar <- subset(pubg.deaths,map_id == 'MIRAMAR')
miramar <- miramar[,c("time_event","victim_x","victim_y")]
# map-erangel             
erangel <- subset(pubg.deaths,map_id == 'ERANGEL')
erangel <- erangel[,c("time_event","victim_x","victim_y")]

### death description data manipulation ###
### AR,SR,SG,LMG,Shotguns,Pistols,Coldsteel,Hitbycar,Grenade,Bluezone,Accident ###

description <- pubg.deaths[,c("description","time_event","distance")]

# Assault Rifles: AKM,AUG,Groza,M16A4,M416,SCAR-L
AR <- subset(description, description == "AKM" | 
               description == "AUG" | description == "Groza" |
               description == "M16A4" | description == "M416" |
               description == "SCAR-L")
AR <- mutate(AR, type = 'AR')

# Sniper Rifles: AWM,Kar98k,M24,Mini 14,Mk14,SKS,VSS,Win94
SR <- subset(description, description == "AWM" | 
               description == "Kar98k" | description == "M24" |
               description == "Mini 14" | description == "Mk14" |
               description == "SKS" | description == "VSS" |
               description == "Win94")
SR <- mutate(SR, type = 'SR')

# Submachine Guns: Micro UZI,Tommy Gun,UMP9,Vector
SG <- subset(description, description == "Micro UZI" | 
               description == "Tommy Gun" | description == "UMP9" |
               description == "Vector")
SG <- mutate(SG, type = 'SG')

# Light Machine Gun: DP-28,M249
LMG <- subset(description, description == "DP-28" | 
               description == "M249")
LMG <- mutate(LMG, type = 'LMG')

# Shotguns: S12K,S1897,S686,Sawed-off
Shotguns <- subset(description, description == "S12K" | 
               description == "S1897" | description == "S686" |
               description == "Sawed-off")
Shotguns <- mutate(Shotguns, type = 'Shotguns')

# Pistols: P18C,P1911,P92,R1895,R45
Pistols <- subset(description, description == "P18C" | 
               description == "P1911" | description == "P92" |
               description == "R1895" | description == "R45")
Pistols <- mutate(Pistols, type = 'Pistols')

# Cold Steel: Crowbar,Machete,Pan,Sickle,Crossbow,Punch
Coldsteel <- subset(description, description == "Crowbar" | 
                     description == "Machete" | description == "Pan" |
                     description == "Sickle" | description == "Crossbow" |
                     description == "Punch")
Coldsteel <- mutate(Coldsteel, type = 'Coldsteel')

# Hit by car: Hit by Car,Buggy,Dacia,Motorbike,Pickup Truck,Uaz
Hitbycar <- subset(description, description == "Hit by Car" | 
                      description == "Buggy" | description == "Pan" |
                      description == "Dacia" | description == "Motorbike" |
                      description == "Pickup Truck" | description == "Uaz")
Hitbycar <- mutate(Hitbycar, type = 'Hitbycar')

# Grenade
Grenade <- subset(description, description == "Grenade")
Grenade <- mutate(Grenade, type = 'Coldsteel')

# Bluezone
Bluezone <- subset(description, description == "Bluezone")
Bluezone <- mutate(Bluezone, type = 'Bluezone')

# Accident: Drown,death.ProjMolotov_DamageField_C,RedZone,Falling
Accident <- subset(description, description == "death.ProjMolotov_DamageField_C" | 
                     description == "RedZone" | description == "Falling")
Accident <- mutate(Accident, type = 'Accident')

### types analysis ###
description <- rbind(AR,SR,SG,LMG,Shotguns,Pistols,Coldsteel,Hitbycar,Grenade,Bluezone,Accident)

#frequency by descriptions
frequency <- table(description[c("description")])
frequency <- data.frame(frequency)
colnames(frequency)[1] <- "description"
order.all = frequency[order(frequency[,2],decreasing=T),]
order.top = order.all[0:10,]
type=c('Assault Rifles','Assault Rifles','Assault Rifles','Assault Rifles','Submachine Guns','Bluezone','Sniper Rifles','Sniper Rifles','Shotogun','Sniper Rifles')
order.top = cbind(order.top,type)
order.top$description <- factor(order.top$description, levels=unique(order.top$description))

#frequency by types
type.frequency <- table(description[c("type")])
type.frequency <- data.frame(type.frequency)
colnames(type.frequency)[1] <- "type"
factor=c('Non-Combat','Combat','Non-Combat','Combat','Combat','Combat','Combat','Combat','Combat','Combat')
type.frequency = cbind(type.frequency,factor)
type.frequency = type.frequency[order(type.frequency[,2],decreasing=T),]


### players analysis ###
stats <- pubg.stats[,c("rank","kills","damage_dealt","walk_distance","ride_distance")]
stats <- mutate(stats, total_distance = walk_distance+ride_distance)

##############################################################################################################
# BASIC ANALYSIS AND VISUALIZATION
##############################################################################################################

message("Q1:Where does the battle occurrs? And which areas are intensive?")
p_miramar = ggplot(miramar,aes(x=victim_x, y=-victim_y,colour = time_event)) + 
      annotation_custom(g, xmin=0, xmax=800000, ymin=-800000, ymax=0) +
      geom_point(size=0.2) + theme_bw() + 
      theme(panel.grid=element_blank(), 
            panel.border=element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank())+
            xlim(0,800000) + ylim(-800000,0)

p_erangel = ggplot(erangel,aes(x=victim_x, y=-victim_y,colour = time_event)) + 
      annotation_custom(g_, xmin=0, xmax=800000, ymin=-800000, ymax=0) +
      geom_point(size=0.2) + theme_bw() + 
      theme(panel.grid=element_blank(), 
            panel.border=element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank())+
            xlim(0,800000) + ylim(-800000,0)
grid.arrange(p_erangel,p_miramar,ncol=2)

message("Q2:How does player die in the match? What kind of the weapons complete most part of kills?")
ggplot(type.frequency,aes(x=type, y=Freq , fill=factor )) + geom_bar(position = "dodge", stat = "identity",width = 0.7) +
geom_text(aes(label = Freq), vjust = -0.5, colour = "black", position = position_dodge(.9), size = 2)

ggplot(order.top,aes(x=order.top$description, y=Freq,fill=type)) + geom_bar(position = "dodge", stat = "identity",width = 0.87) +
geom_text(aes(label = Freq), vjust = -0.5, colour = "black", position = position_dodge(.9), size = 2)


message("Q3:What's the characteristic of different types weapon?")

type_res = ddply(description, c("type"), summarize ,meantime=mean(time_event, 2), meandistance=mean(distance,2))
type_res <- type_res[-c(1,3,4,5),]
ggplot(type_res,aes(x=meandistance, y=meantime, fill=type)) + geom_point(shape=21)+geom_text(aes(label=type),colour='white', size=2,vjust = -0.5)+theme_dark()

p_AR = ggplot(AR,aes(x=time_event,y=distance,colour=description))+geom_point(shape=19,size=1.5)+ylim(0,1000)+theme_dark()
p_SR = ggplot(SR,aes(x=time_event,y=distance,colour=description))+geom_point(shape=19,size=1.5)+ylim(0,1000)+theme_dark()
p_Shotguns = ggplot(Shotguns,aes(x=time_event,y=distance,colour=description))+geom_point(shape=19,size=1.5)+ylim(0,1000)+theme_dark()
p_SG = ggplot(SG,aes(x=time_event,y=distance,colour=description))+geom_point(shape=19,size=1.5)+ylim(0,1000)+theme_dark()
grid.arrange(p_AR,p_SR,p_SG,p_Shotguns,ncol=2)

message("Q4:What's the relationship between rank and kills/damages and movedistance")

stats_distance = ddply(stats, c("rank"), summarize, meanwalk=mean(walk_distance, 2), meandrive=mean(ride_distance, 2),meandistance=mean(total_distance, 2))
meanwalk <- stats_distance[,c('rank','meanwalk')]
colnames(meanwalk)[2] <- "distance"
meanwalk <- mutate(meanwalk,type='meanwalk')
meandrive <- stats_distance[,c('rank','meandrive')]
colnames(meandrive)[2] <- "distance"
meandrive <- mutate(meandrive,type='meandrive')
meandistance <- stats_distance[,c('rank','meandistance')]
colnames(meandistance)[2] <- "distance"
meandistance <- mutate(meandistance,type='meandistance')
stats_distance = rbind(meanwalk,meandrive)

ggplot(stats_distance,aes(x=rank,y=distance,fill=type))+geom_area()

stats_kills = ddply(stats, c("rank"), summarize, meankills=mean(kills, 2), meandamage=mean(damage_dealt, 2))
p_kills = ggplot(stats_kills,aes(x=rank,y=meankills))+geom_line(size=1,color = hue_pal()(4)[2])+theme_bw()
p_damage = ggplot(stats_kills,aes(x=rank,y=meandamage))+geom_line(size=1,color = hue_pal()(4)[3])+theme_bw()%+replace%
  theme(panel.background = element_rect(fill = NA))

g1 <- ggplot_gtable(ggplot_build(p_kills))
g2 <- ggplot_gtable(ggplot_build(p_damage))
pp <- c(subset(g1$layout,name=="panel",se=t:r))
g <- gtable_add_grob(g1,g2$grobs[[which(g2$layout$name =="panel")]],pp$t,
                     pp$l,pp$b,pp$l)
# axix
ia <- which(g2$layout$name=="axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x -unit(1,"npc") +unit(0,"cm")
g <- gtable_add_cols(g,g2$widths[g2$layout[ia,]$l],length(g$widths) -1)
g <- gtable_add_grob(g,ax,pp$t,length(g$widths)-1,pp$b)
ga <- g2$grobs[[ia]]
g <- gtable_add_cols(g,g2$widths[g2$layout[ia,]$l],length(g$widths)-1)
g <- gtable_add_grob(g,ga,pp$t,length(g$widths)-1,pp$b)

grid.draw(g)
message("green:meankills,cyan:meandamage")

