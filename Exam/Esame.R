dev.off()

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library("mxmaps")

confirmedraw <- read_csv("confirmedraw.csv")
population_MX <- read_csv("population_MX.csv")
time_series_confirmed_MX <- read_csv("time_series_confirmed_MX.csv")
confirmedraw <- confirmedraw[, -1]
confirmedraw <- subset(confirmedraw, Estado!="AGUASCALIENTES" & Estado!="BAJA CALIFORNIA" & Estado!="BAJA CALIFORNIA SUR" & Estado!="COAHUILA" & Estado!="DURANGO" & Estado!="GUANAJUATO" & Estado!="JALISCO" & Estado!="NAYARIT" & Estado!="NUEVO LEÓN" & Estado!="QUERETARO" & Estado!="SAN LUIS POTOSÍ" & Estado!="SINALOA" & Estado!="SONORA" & Estado!="CHIHUAHUA" & Estado!="TAMAULIPAS" & Estado!="ZACATECAS" & Estado!="NACIONAL" & Estado != "COLIMA" & Estado != "MICHOACÁN" & Estado != "MÉXICO" & Estado != "HIDALGO" & Estado != "TLAXCALA")
population_MX <- subset(population_MX, Estado!="AGUASCALIENTES" & Estado!="BAJA CALIFORNIA" & Estado!="BAJA CALIFORNIA SUR" & Estado!="COAHUILA" & Estado!="DURANGO" & Estado!="GUANAJUATO" & Estado!="JALISCO" & Estado!="NAYARIT" & Estado!="NUEVO LEÓN" & Estado!="QUERETARO" & Estado!="SAN LUIS POTOSÍ" & Estado!="SINALOA" & Estado!="SONORA" & Estado!="CHIHUAHUA" & Estado!="TAMAULIPAS" & Estado!="ZACATECAS" & Estado!="NACIONAL" & Estado != "COLIMA" & Estado != "MICHOACÁN" & Estado != "MÉXICO" & Estado != "HIDALGO" & Estado != "TLAXCALA")
time_series_confirmed_MX <- subset(time_series_confirmed_MX, Estado!="AGUASCALIENTES" & Estado!="BAJA CALIFORNIA" & Estado!="BAJA CALIFORNIA SUR" & Estado!="COAHUILA" & Estado!="DURANGO" & Estado!="GUANAJUATO" & Estado!="JALISCO" & Estado!="NAYARIT" & Estado!="NUEVO LEÓN" & Estado!="QUERETARO" & Estado!="SAN LUIS POTOSÍ" & Estado!="SINALOA" & Estado!="SONORA" & Estado!="CHIHUAHUA" & Estado!="TAMAULIPAS" & Estado!="ZACATECAS" & Estado!="NACIONAL" & Estado != "COLIMA" & Estado != "MICHOACÁN" & Estado != "MÉXICO" & Estado != "HIDALGO" & Estado != "TLAXCALA")
names(confirmedraw) <- c("State","Sex","Age", "Date")
names(population_MX) <- c("State", "Population")
colnames(time_series_confirmed_MX)[1] <- "State"

# plot time series for region
df <- time_series_confirmed_MX
df <- melt(df)
df$variable <- as.POSIXct(df$variable, format = "%m/%d/%Y")
time_series_plot <- ggplot(data=df, 
            aes(x=variable, y=value, group=State, colour=State)) +
  geom_point(size=1) + 
  xlab("") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
time_series_plot + ggtitle("Positive cases vs Date") +
  ylab("Number of positive cases") + xlab("Date") + theme_minimal()

# plot trend of average age
df2 <- confirmedraw
df2$Date <- as.POSIXct(df2$Date, format = "%d/%m/%Y")
Avg_Age <- aggregate(df2$Age, by=list(df2$Date, df2$State), mean)
names(Avg_Age) <- c("Date", "State", "Avg_Age")
age_trend_plot <- ggplot(data=Avg_Age, aes(x=Date, y=Avg_Age, group=State, colour = State)) +
  geom_point(size=1) + xlab("") + theme(axis.text.x=element_text(angle=60, hjust=1)) + geom_hline(yintercept=29.2, linetype="dashed") + ggtitle("Average age of positive cases vs Date") +
  ylab("Average age of positive cases") + xlab("Date")
age_trend_plot + theme_minimal()

# istogram with total at 18/04
maxl <- data.frame(time_series_confirmed_MX$State, time_series_confirmed_MX$`4/18/20`)
colnames(maxl) = c("State" , "Tot")
maxl <- maxl[order(-maxl$Tot),]
ggplot(maxl,  
       aes(x=Tot, y=State)) +
  geom_bar(stat="identity",
           fill = "red") +
  geom_text(aes(label=Tot), vjust=0.5, hjust=-0.1, 
            color="black", size=3) +
  guides(fill=guide_legend(title="Sex")) +
  labs(title = "Confirmed cases of CoViD19",
       y = "State", x = "Cases") +
  theme_minimal()

# istogram 
try <- subset(confirmedraw,select = -c(`Date`))
try <- try %>%
  group_by(try$State,
           try$Sex) %>%
  summarize(count = n())
ggplot(try,
       aes(x=count, y=try$`try$State`,
           fill=try$`try$Sex`)) +
  geom_bar(stat="identity",
           position=position_dodge()) +
  geom_text(aes(label=try$count), vjust=0.5, hjust=-0.1, 
            color="black", position=position_dodge(1), size=3) +
  guides(fill=guide_legend(title="Sex")) +
  labs(title = "Confirmed cases of CoViD19", subtitle = "Divided by Sex",
       y = "State", x = "Cases") +
  theme_minimal()

# pie chart
slices <- c(maxl$Tot)
lbls <- maxl$State
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels=lbls, 
    cex=1, col=rainbow(length(lbls)),
    main="Pie Chart of Confirmed cases")  
pie3D(slices, radius=0.9,  labels=lbls,
      labelcex=0.75, explode=0.1, 
      col=rainbow(length(lbls)),
      main="Pie3D Chart of Confirmed cases")  

# maps
timeseries <- read_csv("time_series_confirmed_MX.csv")
timeseries <- timeseries[-33,]

df_mxstate <- df_mxstate[order(df_mxstate$state_name),]
df_mxstate$value <- df_mxstate$pop
mxstate_choropleth(df_mxstate, 
                   num_colors = 5,
                   zoom = subset(df_mxstate, region %in% c("04","07","27", "09", "12", "17", "20", "21", "23", "30", "31"))$region,
                   title = "Distribution of the population by state")

df_mxstate[,13] <- timeseries[,53]
mxstate_choropleth(df_mxstate,
                   num_colors = 5,
                   zoom = subset(df_mxstate, region %in% c("04","07","27", "09", "12", "17", "20", "21", "23", "30", "31"))$region,
                   title = "Confirmed Cases by State")