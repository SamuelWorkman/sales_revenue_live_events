options(scipen = 12)
library(truncnorm)
library(moments)
library(ggplot2)
library(ggExtra)
library(ggthemes)
library(showtext)
extrafont::loadfonts(quiet = TRUE)

wvublue <- "#002855"
wvugold <- "#EAAA00"

events <- seq(1,2, by = .01)
eventsdraw <- sample(events, 10000, replace = TRUE)

book <- rbeta(10000, 8, 3)

tick <- rtruncnorm(10000,a=35,b=65,mean=50,sd=10)

bev <- rtruncnorm(10000,a=15,b=35,mean=25,sd=5)

events <- eventsdraw*52

patrons <- 1200*book

spons <- rep(135000,10000)

bevrev <- patrons*events*bev
ticrev <- patrons*events*tick

rev <- bevrev+ticrev+spons

hist(rev, breaks = 100)

sales <- cbind(rev,events,patrons,bevrev,ticrev,spons)
sales <- data.frame(sales)


ggplot(sales, aes(rev)) +
  #geom_histogram(bins = 100, colour = "#E69F00", fill = "#56B4E9") +
  geom_histogram(bins = 100, colour = wvublue, fill = wvugold) +
  scale_x_continuous(breaks = c(2000000, 4000000, 6000000, 8000000, 10000000), labels = c("2MM", "4MM", "6MM", "8MM", "10MM")) +
  xlab("Annual Sales $") +
  ylab("Simulations") +
  theme_tufte() +
  theme(
    axis.text.x=element_text(family = "News Gothic MT", size=8),
    axis.text.y=element_text(family = "News Gothic MT", size=8),
    axis.title.x=element_text(family = "News Gothic MT", size=11),
    axis.title.y=element_text(family = "News Gothic MT", size=11))
ggsave("sales_sims.png", height = 5, width = 5)

summary(sales$rev)
skewness(sales$rev)
kurtosis(sales$rev)
