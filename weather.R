library(ggplot2)

weather <- read.csv("~/Desktop/NY1_weather.csv")
weather$Date <- as.Date(weather$Date, format = "%d-%b-%y")

bbi <- element_text(face="bold.italic", color="black")

#actual weather graph
ggplot(data=weather, aes(x=Date)) + 
  geom_line(aes(y=High, col="High"), size=1.5) + 
  geom_line(aes(y=Low, col="Low"), size=1.5) +
  labs(title="NYC Summer 2016: Highs and Lows", 
       y="Temperature", x="Date") +
  theme (title=bbi) + 
  scale_colour_manual("", breaks=c("High", "Low"), 
                      values=c("red", "blue"))

#NY1 comp graph
#change numbers from 1 to 7 to get full week
ggplot(data=weather, aes(x=Date)) + 
  geom_line(aes(y=High, col="Actual High"), size=1.5) +
  geom_line(aes(y=H7, col="Forecasted High"), size=0.7, linetype=3) +
  labs(title="NYC Weather vs. NY1 Forecast, 7 days out", 
       y="Temperature", x="Date") +
  theme (title=bbi) + 
  scale_colour_manual("", breaks=c("Actual High", "Forecasted High"), 
                      values=c("red", "black")) +
  annotate("text", x=as.Date("2016-07-15"), y =65, 
           label=paste("mean error =\n", 
                       round(mean(abs(weather$High - weather$H7), 
                                  na.rm=T),2), "degrees"), size=4)

#JLT comp graph
#change numbers from 1 to 7 to get full week
ggplot(data=weather, aes(x=Date)) + 
  geom_line(aes(y=High, col="Actual High"), size=1.5) +
  geom_line(aes(y=X7DAH, col="JLT High"), size=0.7, linetype=3) +
  labs(title="NYC Weather vs. 'Just Like Today,' 7 days out", 
       y="Temperature", x="Date") +
  theme (title=bbi) + 
  scale_colour_manual("", breaks=c("Actual High", "JLT High"), 
                      values=c("red", "black")) +
  annotate("text", x=as.Date("2016-07-15"), y =65, 
           label=paste("mean error =\n", 
                       round(mean(abs(weather$High - weather$X7DAH), 
                                  na.rm=T),2), "degrees"), size=4)

#creating grid of histograms
NY_1 <- ggplot(weather, aes(x=H1-High)) + 
  geom_histogram(binwidth=1, fill=I("firebrick4"), col=I("black")) +
  xlab("Error") + ylab("Frequency") + 
  labs(title="NY1 1 day out") +
  theme(legend.position='none', title=bbi) + 
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_continuous(limits = c(0, 15))

JLT_1 <- ggplot(weather, aes(x=X1DAH-High)) + 
  geom_histogram(binwidth=1, fill=I("firebrick4"), col=I("black")) +
  xlab("Error") + ylab("Frequency") + 
  labs(title="JLT 1 day out") +
  theme(legend.position='none', title=bbi)+ 
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_continuous(limits = c(0, 15))

NY_3 <- ggplot(weather, aes(x=H3-High)) + 
  geom_histogram(binwidth=1, fill=I("firebrick4"), col=I("black")) +
  xlab("Error") + ylab("Frequency") + 
  labs(title="NY1 3 days out") +
  theme(legend.position='none', title=bbi) + 
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_continuous(limits = c(0, 15))

JLT_3 <- ggplot(weather, aes(x=X3DAH-High)) + 
  geom_histogram(binwidth=1, fill=I("firebrick4"), col=I("black")) +
  xlab("Error") + ylab("Frequency") + 
  labs(title="JLT 3 days out") +
  theme(legend.position='none', title=bbi) + 
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_continuous(limits = c(0, 15))

NY_5 <- ggplot(weather, aes(x=H5-High)) + 
  geom_histogram(binwidth=1, fill=I("firebrick4"), col=I("black")) +
  xlab("Error") + ylab("Frequency") + 
  labs(title="NY1 5 days out") +
  theme(legend.position='none', title=bbi) + 
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_continuous(limits = c(0, 15))

JLT_5 <- ggplot(weather, aes(x=X5DAH-High)) + 
  geom_histogram(binwidth=1, fill=I("firebrick4"), col=I("black")) +
  xlab("Error") + ylab("Frequency") + 
  labs(title="JLT 5 days out") +
  theme(legend.position='none', title=bbi) + 
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_continuous(limits = c(0, 15))

grid.arrange(NY_1, JLT_1, NY_3, JLT_3, NY_5, JLT_5, ncol=2)