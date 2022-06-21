dev.off()

library(readr)
library(ggplot2)
library(plotrix)
library(dplyr)
library(reshape2)
library("mxmaps")
library(scales)
library(caret)
library(rstanarm)
library(ciTools)
library(gridExtra)


population <- read_csv("population_MX.csv")
names(population) <- c("State", "Population")
totalpopulation <-  sum(population$Population)
totalpopulation

confirmedcases <- read_csv("casos_confirmados.csv")
confirmedcases <- confirmedcases[, -1]
confirmedcases <- confirmedcases[, -5]
confirmedcases$State <- gsub('baja california sur', 'BAJA CALIFORNIA SUR', confirmedcases$State)
confirmedcases$State <- gsub('sinaloa', 'SINALOA', confirmedcases$State)
confirmedcases$State <- gsub('distrito federal', 'CIUDAD DE MÉXICO', confirmedcases$State)
confirmedcases$State <- gsub('méxico', 'MÉXICO', confirmedcases$State)
confirmedcases$State <- gsub('nuevo león', 'NUEVO LEÓN', confirmedcases$State)
confirmedcases$State <- gsub('jalisco', 'JALISCO', confirmedcases$State)
confirmedcases$State <- gsub('tabasco', 'TABASCO', confirmedcases$State)
confirmedcases$State <- gsub('guanajuato', 'GUANAJUATO', confirmedcases$State)
confirmedcases$State <- gsub('yucatán', 'YUCATÁN', confirmedcases$State)
confirmedcases$State <- gsub('chihuahua', 'CHIHUAHUA', confirmedcases$State)
confirmedcases$State <- gsub('baja california', 'BAJA CALIFORNIA', confirmedcases$State)
confirmedcases$State <- gsub('sonora', 'SONORA', confirmedcases$State)
confirmedcases$State <- gsub('veracruz de ignacio de la llave', 'VERACRUZ', confirmedcases$State)
confirmedcases$State <- gsub('quintana roo', 'QUINTANA ROO', confirmedcases$State)
confirmedcases$State <- gsub('michoacán', 'MICHOACÁN', confirmedcases$State)
confirmedcases$State <- gsub('querétaro', 'QUERETARO', confirmedcases$State)
confirmedcases$State <- gsub('puebla', 'PUEBLA', confirmedcases$State)
confirmedcases$State <- gsub('hidalgo', 'HIDALGO', confirmedcases$State)
confirmedcases$State <- gsub('chiapas', 'CHIAPAS', confirmedcases$State)
confirmedcases$State <- gsub('tamaulipas', 'TAMAULIPAS', confirmedcases$State)
confirmedcases$State <- gsub('aguascalientes', 'AGUASCALIENTES', confirmedcases$State)
confirmedcases$State <- gsub('campeche', 'CAMPECHE', confirmedcases$State)
confirmedcases$State <- gsub('coahuila', 'COAHUILA', confirmedcases$State)
confirmedcases$State <- gsub('colima', 'COLIMA', confirmedcases$State)
confirmedcases$State <- gsub('durango', 'DURANGO', confirmedcases$State)
confirmedcases$State <- gsub('guerrero', 'GUERRERO', confirmedcases$State)
confirmedcases$State <- gsub('morelos', 'MORELOS', confirmedcases$State)
confirmedcases$State <- gsub('nayarit', 'NAYARIT', confirmedcases$State)
confirmedcases$State <- gsub('oaxaca', 'OAXACA', confirmedcases$State)
confirmedcases$State <- gsub('san luis potosí', 'SAN LUIS POTOSÍ', confirmedcases$State)
confirmedcases$State <- gsub('tlaxcala', 'TLAXCALA', confirmedcases$State)
confirmedcases$State <- gsub('zacatecas', 'ZACATECAS', confirmedcases$State)

population <- subset(population, State!="AGUASCALIENTES" & State!="BAJA CALIFORNIA" & State!="BAJA CALIFORNIA SUR" & State!="COAHUILA" & State!="DURANGO" & State!="GUANAJUATO" & State!="JALISCO" & State!="NAYARIT" & State!="NUEVO LEÓN" & State!="QUERETARO" & State!="SAN LUIS POTOSÍ" & State!="SINALOA" & State!="SONORA" & State!="CHIHUAHUA" & State!="TAMAULIPAS" & State!="ZACATECAS" & State!="NACIONAL" & State != "COLIMA" & State != "MICHOACÁN" & State != "MÉXICO" & State != "HIDALGO" & State != "TLAXCALA")

#set new population values 2018
population$Population <- c(952300, 5463300, 8781300, 3629000, 1994100, 4091400, 6388100, 1722600, 2460900, 8236700, 2207200)

#set area values of the states (km^2)
areas <- population
names(areas) <- c("State", "Area")
areas$Area <- c(57507, 73311, 1495, 63596, 4879, 93757, 34306, 44705, 24731, 71826, 39524)

#population density
density <- population
names(density) <- c("State", "Density")
density$Density <- (population$Population)/areas$Area

confirmedcases <- subset(confirmedcases, State!="AGUASCALIENTES" & State!="BAJA CALIFORNIA" & State!="BAJA CALIFORNIA SUR" & State!="COAHUILA" & State!="DURANGO" & State!="GUANAJUATO" & State!="JALISCO" & State!="NAYARIT" & State!="NUEVO LEÓN" & State!="QUERETARO" & State!="SAN LUIS POTOSÍ" & State!="SINALOA" & State!="SONORA" & State!="CHIHUAHUA" & State!="TAMAULIPAS" & State!="ZACATECAS" & State!="NACIONAL" & State != "COLIMA" & State != "MICHOACÁN" & State != "MÉXICO" & State != "HIDALGO" & State != "TLAXCALA")
confirmedcases <- subset(confirmedcases, Date!="2020-07-15" & Date!="2020-07-16")
confirmedcases <- full_join(confirmedcases, density)
head(confirmedcases, n=15)

# time series
copy <- confirmedcases
copy <- copy %>%
  group_by(copy$Date,
           copy$State) %>%
  summarize(count = n())
colnames(copy) = c("Date", "State", "DailyConfirmed")

# plot trend of average age
df2 <- confirmedcases
df2$Date <- as.POSIXct(df2$Date, format = "%d/%m/%Y")
Avg_Age <- aggregate(df2$Age, by=list(df2$Date, df2$State), mean)
names(Avg_Age) <- c("Date", "State", "Avg_Age")
age_trend_plot <- ggplot(data=Avg_Age, aes(x=Date, y=Avg_Age, group=State, colour = State)) +
  geom_point(size=1) + xlab("") + theme(axis.text.x=element_text(angle=60, hjust=1)) + geom_hline(yintercept=29.2, linetype="dashed") + ggtitle("Average age of positive cases vs Date") +
  ylab("Average age of positive cases") + xlab("Date")
age_trend_plot + theme_minimal() + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

# histogram 
try <- subset(confirmedcases,select = -c(`Date`))
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
            color="black", position=position_dodge(1), size=4.5) +
  guides(fill=guide_legend(title="Sex")) +
  labs(title = "Confirmed cases of CoViD19", subtitle = "Divided by Sex",
       y = "State", x = "Cases") +
  theme_minimal() + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=20), strip.text = element_text(30), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

# plot daily cases
df <- copy
p <- ggplot(data=df, 
            aes(x=Date, y=DailyConfirmed, group=State, colour=State)) +
  geom_line(size=1) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p + ggtitle("Daily positive cases vs Days") +
  ylab("Number of positive cases") +
  xlab("Date") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

# drop rows after a certain value
values <- vector()
for (i in seq(1, nrow(copy), 1))
  values[i] = as.character(copy[i, 1])
copy <- cbind(copy, values)
colnames(copy) = c("Date", "State", "TotalCases", "Values")

# drop rows after a certain value
df13 <- copy
values <- vector()
for (i in seq(1, nrow(copy), 1))
  values[i] = as.character(copy[i, 1])
df13 <- cbind(copy, values)
colnames(df13) = c("Date", "State", "DailyConfirmed", "Values")
df13 <- df13[!(df13$Values < 18383), ] 
df13 <- df13[!(df13$Values > 18407), ]



df101 <- df13[(df13$State == "CAMPECHE"),]
df102 <- df13[(df13$State == "CHIAPAS"),]
df103 <- df13[(df13$State == "CIUDAD DE MÉXICO"),]
df104 <- df13[(df13$State == "GUERRERO"),]
df105 <- df13[(df13$State == "MORELOS"),]
df106 <- df13[(df13$State == "OAXACA"),]
df107 <- df13[(df13$State == "PUEBLA"),]
df108 <- df13[(df13$State == "QUINTANA ROO"),]
df109 <- df13[(df13$State == "TABASCO"),]
df110 <- df13[(df13$State == "VERACRUZ"),]
df111 <- df13[(df13$State == "YUCATÁN"),]
states <- c("CAMPECHE", "CHIAPAS", "CIUDAD DE MÉXICO", "GUERRERO", "MORELOS", "OAXACA", "PUEBLA", "QUINTANA ROO", "TABASCO", "VERACRUZ", "YUCATÁN")
colors <- hue_pal()(length(states))
grid.arrange(
  ggplot(data=df13, 
         aes(x=Date, y=DailyConfirmed, group=State, colour=State)) +
    geom_line(size=1) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+ 
    theme(legend.position = "none") +
    ggtitle("Daily positive cases vs Days") +
    ylab("Number of positive cases") +
    xlab("Date"),
  ggplot(data=df101, 
         aes(x=Date, y=DailyConfirmed)) +
    geom_line(size=1, color=colors[1]) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+
    theme(legend.position = "none") +
    ggtitle(states[1]) +
    ylab("Number of positive cases") +
    xlab("Date"),
  ggplot(data=df102, 
         aes(x=Date, y=DailyConfirmed)) +
    geom_line(size=1, color=colors[2]) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+
    theme(legend.position = "none") +
    ggtitle(states[2]) +
    ylab("Number of positive cases") +
    xlab("Date"),
  ggplot(data=df103, 
         aes(x=Date, y=DailyConfirmed)) +
    geom_line(size=1, color=colors[3]) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+
    theme(legend.position = "none") +
    ggtitle(states[3]) +
    ylab("Number of positive cases") +
    xlab("Date"),
  ggplot(data=df104, 
         aes(x=Date, y=DailyConfirmed)) +
    geom_line(size=1, color=colors[4]) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+
    theme(legend.position = "none") +
    ggtitle(states[4]) +
    ylab("Number of positive cases") +
    xlab("Date"),
  ggplot(data=df105, 
         aes(x=Date, y=DailyConfirmed)) +
    geom_line(size=1, color=colors[5]) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+
    theme(legend.position = "none") +
    ggtitle(states[5]) +
    ylab("Number of positive cases") +
    xlab("Date"),
  ggplot(data=df106, 
         aes(x=Date, y=DailyConfirmed)) +
    geom_line(size=1, color=colors[6]) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+
    theme(legend.position = "none") +
    ggtitle(states[6]) +
    ylab("Number of positive cases") +
    xlab("Date"),
  ggplot(data=df107, 
         aes(x=Date, y=DailyConfirmed)) +
    geom_line(size=1, color=colors[7]) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+
    theme(legend.position = "none") +
    ggtitle(states[7]) +
    ylab("Number of positive cases") +
    xlab("Date"),
  ggplot(data=df108, 
         aes(x=Date, y=DailyConfirmed)) +
    geom_line(size=1, color=colors[8]) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+
    theme(legend.position = "none") +
    ggtitle(states[8]) +
    ylab("Number of positive cases") +
    xlab("Date"),
  ggplot(data=df109, 
         aes(x=Date, y=DailyConfirmed)) +
    geom_line(size=1, color=colors[9]) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+
    theme(legend.position = "none") +
    ggtitle(states[9]) +
    ylab("Number of positive cases") +
    xlab("Date"),
  ggplot(data=df110, 
         aes(x=Date, y=DailyConfirmed)) +
    geom_line(size=1, color=colors[10]) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+
    theme(legend.position = "none") +
    ggtitle(states[10]) +
    ylab("Number of positive cases") +
    xlab("Date"),
  ggplot(data=df111, 
         aes(x=Date, y=DailyConfirmed)) +
    geom_line(size=1, color=colors[11]) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=0, hjust=0.5))+
    theme(legend.position = "none") +
    ggtitle(states[11]) +
    ylab("Number of positive cases") +
    xlab("Date")
)

copy2 <- copy[with(copy, order(copy$State, copy$Values)), ]
check <- ""
for (j in seq(1, nrow(copy2), 1))
  if (check == copy2[j, 2]) {
    copy2[j, 3] <- copy2[j, 3] + copy2[j-1, 3]
  } else {
    check <- copy2[j, 2]
  } 

df <- copy
ts <- (seq.POSIXt(as.POSIXct("2020-02-27",'%m/%d/%y'), as.POSIXct("2020-07-14",'%m/%d/%y'), by="day"))
ts <- rep(ts, times=1)
l <- length(ts)
states <- c(rep("CAMPECHE", times = l), rep("CHIAPAS", times = l), rep("CIUDAD DE MÉXICO", times = l), rep("GUERRERO", times = l), rep("MORELOS", times = l), rep("OAXACA", times = l), rep("PUEBLA", times = l), rep("QUINTANA ROO", times = l), rep("TABASCO", times = l), rep("VERACRUZ", times = l), rep("YUCATÁN", times = l))
changed <- data.frame(Date=ts, State=states)
data_with_missing_times <- full_join(changed, df)
data_with_missing_times[is.na(data_with_missing_times)] <- 0
dailycases <- data_with_missing_times[,-4]
colnames(dailycases) = c("Date", "State", "DailyConfirmed")


for (i in 0:10) {
  k <- (1+139*i)
  l <- 139*(i+1) - 1
  for (j in k:l) {
    data_with_missing_times[j+1,3] <- data_with_missing_times[j,3]+data_with_missing_times[j+1,3]
  }
}

df <- copy2
p <- ggplot(data=df, 
            aes(x=Date, y=TotalCases, group=State, colour=State)) +
  geom_point(size=1) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p + ggtitle("Total positive cases vs Days") +
  ylab("Number of positive cases") +
  xlab("Date") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

p <- ggplot(data=data_with_missing_times, 
            aes(x=Date, y=TotalCases, group=State, colour=State)) +
  geom_point(size=1) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p + ggtitle("Total positive cases vs Days") +
  ylab("Number of positive cases") +
  xlab("Date") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

maxl <- subset(data_with_missing_times, Values=="18457")

maxl <- maxl[order(-maxl$TotalCases),]
ggplot(maxl,  
       aes(x=TotalCases, y=State)) +
  geom_bar(stat="identity",
           fill = "red") +
  geom_text(aes(label=TotalCases), vjust=0.5, hjust=-0.1, 
            color="black", size=6.5) +
  guides(fill=guide_legend(title="Sex")) +
  labs(title = "Confirmed cases of CoViD19",
       y = "State", x = "Cases") +
  theme_minimal() + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

# pie chart
slices <- c(maxl$TotalCases)
lbls <- maxl$State
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
par(
  # Change font size
  cex.main=3, cex.lab=1.7, cex.sub=1.2, cex.axis=2)
pie(slices, labels=lbls, 
    cex=1.5, col=rainbow(length(lbls)),
    main="Pie Chart of Confirmed cases")  
pie3D(slices, radius=0.9,  labels=lbls,
      labelcex=1.55, explode=0.1, 
      col=rainbow(length(lbls)),
      main="Pie3D Chart of Confirmed cases")  

# maps

df_mxstate <- subset(df_mxstate, region %in% c("04","07","27", "09", "12", "17", "20", "21", "23", "30", "31"))
df_mxstate <- df_mxstate[order(df_mxstate$state_name),]
df_mxstate$value <- population$Population
mxstate_choropleth(df_mxstate, 
                   num_colors = 1,
                   zoom = subset(df_mxstate, region %in% c("04","07","27", "09", "12", "17", "20", "21", "23", "30", "31"))$region,
                   title = "Distribution of the population by state")

maxl <- maxl[order(maxl$State),]
df_mxstate2 <- df_mxstate
df_mxstate2[,13] <- maxl[,3]
mxstate_choropleth(df_mxstate2,
                   num_colors = 1,
                   zoom = subset(df_mxstate2, region %in% c("04","07","27", "09", "12", "17", "20", "21", "23", "30", "31"))$region,
                   title = "Confirmed Cases by State")


seba <- copy
ts <- (seq.POSIXt(as.POSIXct("2020-02-27",'%m/%d/%y'), as.POSIXct("2020-07-14",'%m/%d/%y'), by="day"))
ts <- rep(ts, times=1)
l <- length(ts)
states <- c(rep("CAMPECHE", times = l), rep("CHIAPAS", times = l), rep("CIUDAD DE MÉXICO", times = l), rep("GUERRERO", times = l), rep("MORELOS", times = l), rep("OAXACA", times = l), rep("PUEBLA", times = l), rep("QUINTANA ROO", times = l), rep("TABASCO", times = l), rep("VERACRUZ", times = l), rep("YUCATÁN", times = l))
changed <- data.frame(Date=ts, State=states)

## Dataset total cases
dataset <- data_with_missing_times[,-4]
dataset <- full_join(dataset, density)
dataset <- full_join(dataset, Avg_Age)
dataset[is.na(dataset)] <- 0

#considering average sex per day per state
tempconfirmedcases <- confirmedcases[,-5]
tempconfirmedcases$Sex[tempconfirmedcases$Sex == "FEMENINO"] <- 0
tempconfirmedcases$Sex[tempconfirmedcases$Sex == "MASCULINO"] <- 1
tempconfirmedcases$Sex <- as.numeric(tempconfirmedcases$Sex)
Avg_Sex <- aggregate(tempconfirmedcases$Sex, by=list(tempconfirmedcases$Date, tempconfirmedcases$State), mean)
names(Avg_Sex) <- c("Date", "State", "Avg_Sex")
dataset <- full_join(dataset, Avg_Sex)
dataset[is.na(dataset)] <- 0


## Dataset daily cases
dataset2 <- dailycases
dataset2 <- full_join(dataset2, density)
dataset2 <- full_join(dataset2, population)
dataset2 <- full_join(dataset2, Avg_Age)
dataset2[is.na(dataset2)] <- 0

dataset2 <- full_join(dataset2, Avg_Sex)
dataset2[is.na(dataset2)] <- 0

startdate <- as.Date("27/02/2020","%d/%m/%Y")
dataset2$Days  <- difftime(dataset2$Date,startdate ,units="days")
dataset2$Days <- dataset2$Days +1
dataset2$State <- as.factor(dataset2$State)
dataset2$Days <- as.numeric(dataset2$Days)

#TOT MODEL
dataset2 <- dataset2[order(dataset2$Days),]
set.seed(101)
train <- dataset2[1:round(0.9*nrow(dataset2)), ]
test  <- dataset2[round(0.9*nrow(dataset2)+1):nrow(dataset2), ]
qpglm <- glm(DailyConfirmed ~ Days + Avg_Age + Avg_Sex + I(Days^2) + State, data = train, family = quasipoisson(link = "log"))
summary(qpglm)
plot(train$Days, train$DailyConfirmed, main = "Poisson regression")
results <- as.data.frame(qpglm$fitted.values)
results$index <- as.numeric(row.names(results))
results <- results[order(results$index),]
results$index <- results$index
colnames(results) = c("fitted.values", "index")
lines(results$index, as.vector(results$fitted.values), col ="red")
varImp(qpglm, scale = FALSE)

regions <- population$State

dataset2 <- ciTools::add_pi(tb = dataset2,
                            qpglm, names=c("qp_lwr", "qp_upr"), alpha=0.05)
dataset2 <- rename(dataset2, pred_qp = pred)
myplots_qp <- list()  # new empty list

for (state in 1:length(regions)) {
  statewise_qp <- dataset2[dataset2$State==regions[state],]
  myplots_qp[[state]] <- ggplot(statewise_qp, aes(x = Date, y = DailyConfirmed)) +
    theme(plot.title = element_text(hjust = 0.5))+
    geom_point(size = 1) +
    geom_line(aes(y = pred_qp), size = 1, color = "maroon") +
    geom_ribbon(data=statewise_qp[which(statewise_qp$Date<"2020-07-01"),],
                aes(ymin = qp_lwr, ymax = qp_upr), fill =
                  "royalblue1", alpha = 0.3) +
    geom_ribbon(data=statewise_qp[which(statewise_qp$Date>"2020-06-30"),],
                aes(ymin = qp_lwr, ymax = qp_upr), fill =
                  "red", alpha = 0.3) +
    labs(title=regions[state]) +
    xlab("Days") + ylab("Daily positives")
  
}

g<-do.call(grid.arrange, c(myplots_qp, ncol = 4, nrow=3,   top = "Quasi-Poisson: predictions and P.I.s"))

ggsave("qp_ppi.png", plot=g, dpi = 300, width = 20, height = 20, units = "cm")





#SINGLE MODELS

regions <- population$State
myplots_qp <- list()  # new empty list

for (state in 1:length(regions)) {
  
  dati <- dataset2[dataset2$State==regions[state],]
  train <- dati[1:round(0.9*nrow(dati)), ]
  test  <- dati[round(0.9*nrow(dati)+1):nrow(dati), ]
  
  train <- train[order(train$Days),]
  
  qpglm <- glm(DailyConfirmed ~ Days + Avg_Age + Avg_Sex + I(Days^2), data = train, family = quasipoisson(link = "log"))
  summary(qpglm)
  plot(train$Days, train$DailyConfirmed, main = "Poisson regression")
  lines(train$Days, as.vector(qpglm$fitted.values), col ="red")
  varImp(qpglm, scale = FALSE)
  
  dati <- ciTools::add_pi(tb = dati,
                              qpglm, names=c("qp_lwr", "qp_upr"), alpha=0.05)
  dati <- rename(dati, pred_qp = pred)
  
  statewise_qp <- dati[dati$State==regions[state],]
  myplots_qp[[state]] <- ggplot(statewise_qp, aes(x = Date, y = DailyConfirmed)) +
    theme(plot.title = element_text(hjust = 0.5))+
    geom_point(size = 1) +
    geom_line(aes(y = pred_qp), size = 1, color = "maroon") +
    geom_ribbon(data=statewise_qp[which(statewise_qp$Date<"2020-07-01"),],
                aes(ymin = qp_lwr, ymax = qp_upr), fill =
                  "royalblue1", alpha = 0.3) +
    geom_ribbon(data=statewise_qp[which(statewise_qp$Date>"2020-06-30"),],
                aes(ymin = qp_lwr, ymax = qp_upr), fill =
                  "red", alpha = 0.3) +
    labs(title=regions[state]) +
    xlab("Days") + ylab("Daily positives")
}

g<-do.call(grid.arrange, c(myplots_qp, ncol = 4, nrow=3,   top = "Quasi-Poisson: predictions and P.I.s"))

ggsave("qp_ppi.png", plot=g, dpi = 300, width = 20, height = 20, units = "cm")