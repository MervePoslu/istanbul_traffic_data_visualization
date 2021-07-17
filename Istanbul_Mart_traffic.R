install.packages("ggplot2")
install.packages("Lattice")
install.packages("highcharter")
install.packages("RColorBrewer")
install.packages("plotly")
install.packages("sunburstR")
install.packages("RGL")
install.packages("dygraphs")
install.packages("dplyr") # alternative installation of the %>%
install.packages("corrplot")
install.packages("devtools")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("MASS")
install.packages("psych")
install.packages("car")

library(dplyr)
library(corrplot)
library(MASS)
library(psych)
library(car)
library(emmeans)
library(interactions)
library(corrplot)
library(ggplot2)
library(tidyverse)
library(dplyr)    # alternatively, this also loads %>%


library(readr)
data10 <- read.csv("~/Downloads/data10.txt", sep="")
View(data10)
describe(data10)

install.packages("zoo")
library(tidyverse) # data manipulation
library(plotly) # interactive visualizations
library(janitor) # data manipulation
library(stringr) # character class data manipulation
library(treemap) # tree map visualization
library(igraph)
library(gridExtra)
library(ggraph)
library(highcharter) 
library(lubridate)
library(stringr)
library(dplyr)
library(xts)


data10 %>%
  count(DATE_TIME) %>%
  hchart('area', hcaes(x = "DATE_TIME", y = "n")) %>%
  hc_colors("#fb4901") %>%
  hc_add_theme(hc_theme_ffx()) %>%
  hc_title(text = "Distribution of Date")

data10 %>%
  count(NUMBER_OF_VEHICLES) %>%
  hchart('area', hcaes(x = "NUMBER_OF_VEHICLES", y = "n")) %>%
  hc_colors("#fb4901") %>%
  hc_add_theme(hc_theme_ffx()) %>%
  hc_title(text = "Distribution of vehicles")

library(ggplot2)
library(dplyr)
library(hrbrthemes) #necessary for theme_ipsum

data10$hiz_seviye <- as.factor(ifelse(data10$AVERAGE_SPEED>=mean(data10$AVERAGE_SPEED), 'hiz_seviye') "Ort.Üstü Hız","Ort. Altı Hız"))
# categorical speed histogram
graph1 <- data10 %>%
  ggplot( aes(x=NUMBER_OF_VEHICLES, fill=)) +
  geom_histogram( alpha=0.9, binwidth = 10) +
  theme_ipsum()
graph1

#categorical time 
library(lubridate)
data10$Hour <- hour(data10$DATE_TIME)

library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(ggrepel)

library(datetime)

data10$HOUR <- format(as.POSIXct(data10$DATE_TIME,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")

data10$DAY <- format(as.POSIXct(data10$DATE_TIME,format="%Y-%m-%d %H:%M:%S"),"%d")

# Categoric for is hour day or night
data10$day_night <- ifelse( data10$Hour<6 | 19<data10$Hour, "Night","Day" )

# Categoric for is hour curfew  or not curfew
data10$ban <- ifelse( data10$Hour<5 | 21<data10$Hour, "Curfew","Not_Curfew" )

data10$cat <- NULL

options(repr.plot.width=16, repr.plot.height=8)

data10 %>%  
  ggplot(aes(x = DAY, fill = ban)) +
  geom_bar(stat = "count", position = "dodge") +
  geom_text(aes(label=..count..), stat="count", position=position_dodge(0.8), vjust=-0.2, size=4) +
  scale_fill_manual(values = c(colorsPuYe[6], colorsPuYe[2])) +
  theme_classic() + 
  my_theme +
  labs(x = "Day", y = "Frequency", subtitle = " Günün sokağa çıkma yasağı olan saatlerine göre kayıt sayıları", caption = "Yasaklı Günler")

# categorical ban histogram
graphban <- data10 %>%
  ggplot( aes(x=NUMBER_OF_VEHICLES, fill=ban)) +
  geom_histogram( alpha=0.9, binwidth = 10) +
  theme_ipsum()
graphban

#Relation Graphs
install.packages("GGally")
library(plotly)
install.packages("ggcorrplot")
library(ggcorrplot)
ggplotly(ggcorr(data10[5:8], geom = "circle", nbreaks = 5,label = TRUE))

day_hour <- data10[, c("DAY", "Hour")] %>% group_by(DAY, Hour) %>% summarise(Count = n())

day_hour <- as.data.frame(day_hour)

ggplot(day_hour, aes(Hour, DAY, fill = Count)) + geom_tile(color = "white", size = 0.1) + 
  scale_fill_viridis(name="Recording Count") + coord_equal() + labs(title=" Hour of the day") 

la_long <- data10[, c("LONGITUDE","LATITUDE")] %>% group_by(LONGITUDE, LATITUDE) %>% summarise(Count = n())

la_long <- as.data.frame(la_long)

ggplot(la_long, aes(LONGITUDE, LATITUDE, fill = Count)) + geom_tile(color = "white", size = 0.1) + 
  scale_fill_viridis(name="Recording Count") + coord_equal() + labs(title=" Latitude & Longitude") 

library(leafletR)
library(leaflet)

colorsReBu <- c("#922B21", "#EE865D", "#DDCD5E", "#59BEC4", "#048B9F", "#114676")
colorsPuYe <- c("#5C374C", "#985277", "#CE6A85", "#FFCF6A", "#FFB742", "#E9692C")
colorsEarth <- c("#DA4511", "#FFBD00", "#6A953F", "#9A6233", "#D3AE7C", "#307CA1")
colorsRainbow <- c("#FF8EC8", "#FFDF51", "#46DBDF", "#FF8F46", "#42BAB7", "#DB0000")
colorsPastels <- c("#FA6E4F", "#F2CF59", "#FB8E7E", "#C5D7C0", "#8EC9BB", "#F8CA9D")


p1 <- data10 %>% 
  head(70) %>% 
  ggplot(aes(x = MAXIMUM_SPEED, y = Hour)) +
  geom_point(shape = 1, size = 5, stroke = 1.3, color = colorsEarth[1]) +
  theme_test() + 
  theme(axis.text = element_blank()) +
  labs(x = "max_speed", y = "hour", title = "Shape1")
p1

p2 <- data10 %>% 
  head(70) %>% 
  ggplot(aes(x = MINIMUM_SPEED, y = Hour)) +
  geom_point(shape = 2, size = 5, stroke = 1.3, color = colorsEarth[2]) +
  theme_test() + 
  theme(axis.text = element_blank()) +
  labs(x = "min_speed", y = "hour", title = "Shape2")
p2

p3 <- data10 %>% 
  head(70) %>% 
  ggplot(aes(x = AVERAGE_SPEED, y = NUMBER_OF_VEHICLES)) +
  geom_point(shape = 8, size = 5, stroke = 1.3, color = colorsEarth[6]) +
  theme_test() + 
  theme(axis.text = element_blank()) +
  labs(x = "avg_speed", y = "number_of_vehixles", title = "Shape3")
p3

options(repr.plot.width=16, repr.plot.height=8)
grid.arrange(p1, p2, p3, ncol = 3)

options(repr.plot.width=16, repr.plot.height=8)

my_theme <- theme(
  text = element_text(color = "grey35"),
  plot.title = element_text(size = 25, face = "bold"),
  axis.title = element_text(size = 20),
  axis.text = element_text(size = 15),
  axis.line = element_line(size = 1.2, color = "grey35"),
  legend.box.background = element_rect(color = "grey75", size = 1),
  legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5),
  legend.title = element_text(face = "bold", size = 15),
  legend.text = element_text(size=13))


p4 <- data10 %>% 
ggplot(aes(x = DAY)) +
  geom_line(aes(y = NUMBER_OF_VEHICLES), size = 1.5, color = colorsReBu[5]) +
  labs(x = "Day", y = "Number of vehicles", title = "Basic Line Plot", subtitle = "Line Plot", caption = "Dataset10") +
  theme_light() +
  my_theme +
  theme(panel.border = element_rect(color = "grey35")) +
  scale_y_continuous(breaks = seq(0, 2000000, 200000)) +
  geom_smooth(aes(y = NUMBER_OF_VEHICLES), color = colorsReBu[2], se = F, span = 0.4)
p4

options(repr.plot.width=16, repr.plot.height=8)

data10 %>% 
  group_by(DAY) %>% 
  summarise(n = n()) %>% 
ggplot(aes(x = reorder(DAY, n), y = n)) +
  geom_bar(stat = "identity", aes(fill = n)) +
  coord_flip() +
  geom_label(aes(label = n), size = 6) +
  scale_fill_gradient(low=colorsPuYe[3], high=colorsPuYe[1], guide = "none") +
  geom_hline(yintercept = 30, color = colorsPuYe[4], alpha = 0.6, size = 3) +
  geom_hline(yintercept = 60, color = colorsPuYe[5], alpha = 0.6, size = 3) +
  geom_hline(yintercept = 90, color = colorsPuYe[6], alpha = 0.6, size = 3) +
  theme_test() + 
  my_theme +
  labs(x = "DAY", y = "Frequency", title = "Simple Barplot", subtitle = "Bar Plot")

data10 %>% 
  group_by(Hour) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = order(Hour, n), y = n)) +
  geom_bar(stat = "identity", aes(fill = n)) +
  coord_flip() +
  geom_label(aes(label = n), size = 5) +
  scale_fill_gradient(low=colorsPuYe[3], high=colorsPuYe[1], guide = "none") +
  geom_hline(yintercept = 30, color = colorsPuYe[4], alpha = 0.6, size = 3) +
  geom_hline(yintercept = 60, color = colorsPuYe[5], alpha = 0.6, size = 3) +
  geom_hline(yintercept = 90, color = colorsPuYe[6], alpha = 0.6, size = 3) +
  theme_test() + 
  my_theme +
  labs(x = "Hour", y = "Frequency", title = "Simple Barplot", subtitle = "Bar Plot")

colorsPastels <- c("#FA6E4F", "#F2CF59", "#FB8E7E", "#C5D7C0", "#8EC9BB", "#F8CA9D")

p5 <- data10 %>% 
  ggplot(aes(x = AVERAGE_SPEED)) +
  geom_histogram(color = "grey30", fill = colorsPastels[4], binwidth = 10) +
  theme_test() +
  labs(title = "Histogram") +
  theme(axis.text = element_blank())
p5

## MAPPING
library(sp) # Konumsal (Spatial) veri için
tr <- readRDS("/Users/merve.poslu/Downloads/ililce.rds") #reading a spatial data

plot(tr)

view(tr)

istanbul=tr%>%subset(NAME_1=="Istanbul")
plot(istanbul)

istdata<-fortify(tr)
head(tr@data) 
head(istdata) 

ggplot(istdata) + geom_polygon(aes(x = long, y = lat,group = group),color = "white",fill = "red") +
  theme_void() + coord_fixed()

tr@data$id<-rownames(tr@data)
tr@data$id<-as.numeric(tr@data$id)
istdata$id<-as.numeric(istdata$id)
istdata$id<-istdata$id-1
match <- left_join(istdata, tr@data, by = "id")
unique(match$HASC_2)

istanbul_for <- filter(match, grepl("TR.IB", HASC_2))
head(istanbul_for)

view(istanbul_for)

final_map <- left_join(istanbul_for, tr, by = "NAME_2")


options(scipen=10000)
p2<-ggplot(final_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=NUMBER_OF_VEHICLES),color = "black")+
  labs(x="",y="")+ theme_bw()+
  coord_fixed()


##İnteractive-3D graphs

plot_ly(
  # (1) data; (2) # Assign X, Y, and Z variables (put '~' before each variable).
  data = data10, x = ~LONGITUDE, y = ~LATITUDE, z = ~GEOHASH,
  color = ~DAY,  # Separate variable by color. Put '~' before variable.
  type = "scatter3d",  # Makes a 3D scatterplot.
  mode = "markers"  # Use markers. 
) %>%  
  layout(scene = list(xaxis = list(title = 'Boylam'), # Assign x, y, & z axes names. 
                      yaxis = list(title = 'Enlem'),
                      zaxis = list(title = 'Geohash')))


# Create ggplot
p6 <- ggplot(
  # (1) set data; (2) specify x & y variables; (3) set what variable to separate by color. 
  data = data10, mapping = aes(x = ~DAY, y = ~Hour, color = NUMBER_OF_VEHICLES)) +
  geom_point() +  # Specifies that we want a scatter plot. 
  geom_smooth() + # Add standard error bar line.
  scale_color_brewer(palette = 'Accent') +  # Choose color for Species levels.
  theme_classic() + # Set theme.
  theme(plot.background = element_rect(fill = "grey97")) + # Background color.
  labs(title = 'Latitude and Longitude ', 
       x = 'Longitude', y = 'Latitude')  # Title & axis names. 

# Make plot interactive
ggplotly(p6)  
p6


pie_chrt <- data10 %>% count(GEOHASH)

# Pick colors for pie chart
color <- RColorBrewer::brewer.pal(3, 'Accent') # Palette & number of colors to grab

# Create pie chart
plot_ly(data = pie_chrt, # Set data
        labels = ~GEOHASH, # Specify variable to divide pie chart by
        marker = list(colors = color), # Set color
        type = 'pie') %>%  # Make pie chart
  layout(title = 'Pie chart of geohash ', # Set title
         paper_bgcolor='#F5F5F5') # Background color 


# Create ggplot
saatler <- ggplot(
  # (1) set data; (2) specify x & y variables; (3) set what variable to separate by color. 
  data = data10, mapping = aes(x = MAXIMUM_SPEED, y = MINIMUM_SPEED, color = day_night)) +
  geom_point() +  # Specifies that we want a scatter plot. 
  geom_smooth() + # Add standard error bar line.
  scale_color_brewer(palette = 'Accent') +  # Choose color for Species levels.
  theme_classic() + # Set theme.
  theme(plot.background = element_rect(fill = "grey97"))  # Background color.)  # Title & axis names. 

# Make plot interactive
ggplotly(saatler)  
# (For a non-interactive version run object 'day'.)

konum <- ggplot(
  # (1) set data; (2) specify x & y variables; (3) set what variable to separate by color. 
  data = data10, mapping = aes(x = LONGITUDE, y = LATITUDE, color = day_night)) +
  geom_point() +  # Specifies that we want a scatter plot. 
  geom_smooth() + # Add standard error bar line.
  scale_color_brewer(palette = 'Accent') +  # Choose color for Species levels.
  theme_classic() + # Set theme.
  theme(plot.background = element_rect(fill = "grey97"))  # Background color.)  # Title & axis names. 

# Make plot interactive
ggplotly(konum)  
# (For a non-interactive version run object 'KONUM'.)



