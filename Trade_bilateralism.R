

##*********Bilateral Trade Agreements******************************##
##*****************************************************************##
##*****************************************************************##



library(tidyverse)
library(readxl)
library(lubridate)

multilateral_tradeRounds <- read_xlsx(file.choose())

trade_bilateralism <- read_xls(file.choose())

trade_bilateralism
colnames(trade_bilateralism)

trade_bilateralism_small <- trade_bilateralism %>%
                          select(`RTA ID`,`RTA Name`,"Coverage","Type","Notification", "Status",
                                  "Date of Signature (G)", "RTA Composition", "Region", "Cross-regional")

trade_bilateralism_small$`Date of Signature (G)` <- as.Date(trade_bilateralism_small$`Date of Signature (G)`)
## Only filtering bilateral trade agreements already in force.
trade_bilateralism_small <- filter(trade_bilateralism_small, Status == "In Force", str_detect(`RTA Composition`, pattern = "Bilateral"))

trade_bilateralism_small <- trade_bilateralism_small%>% arrange(`Date of Signature (G)`)

trade_bilateralism_small$col <- 1

trade_bilateralism_small <- mutate(trade_bilateralism_small, cum_btas = cumsum(col))


ggplot(data =trade_bilateralism_small, mapping = aes(x = `Date of Signature (G)`, y = cum_btas))+
  geom_smooth(method = "gam", color = "blue")+
  geom_point(size = 3.5)+
  theme_bw()+
  labs(
    title = "Cumulative Bilateral Trade Agreements across time",
    x = "Date of Signature",
    y = "Cumulative bilateral trade agreements",
    caption = "Data source: WTO") +
  theme(plot.title = element_text(hjust = 0.50, face="bold",size=18))

## UN membership by year is acquired from the following weblink https://www.un.org/en/about-us/growth-in-un-membership
    
trade_bilateral_data <- data.frame(year = c(1973, 1977, 1983, 1990, 1994, 1999, 2006, 2011, 2022),
                                 Bta  = c(2, 4, 7,10, 29, 63, 136,205,297),
                        Un_membership = c(135, 149, 158, 159, 185,188, 192, 193, 193))


trade_bilateral_data <- mutate(trade_bilateral_data, total_dyads = choose(Un_membership, 2),
                             bilateral_fraction = round(Bta/total_dyads,3))

write_csv(trade_bilateral_data, "bilateral_fraction_dyad.csv")


ggplot()+
  geom_point(data = trade_bilateral_data, mapping = aes(x = year, y = bilateral_fraction), size = 5)+
  geom_line(data = trade_bilateral_data, mapping = aes(x = year, y = bilateral_fraction),color = "blue", size = 1)+
  geom_point(data = multilateral_tradeRounds, mapping = aes(x = Year, y = Fraction_dyad), size = 5)+
  geom_line(data = multilateral_tradeRounds, mapping = aes(x = Year, y = Fraction_dyad), color = "red", size = 1)+
  geom_text(data = multilateral_tradeRounds,x = 1985, y = 0.60, size = 3.5, label = "Fraction of dyads engaged in multilateralism\n shown in red color line")+
  geom_text(data = trade_bilateral_data,x = 2000, y = 0.10, size = 3.5, label = "Fraction of dyads engaged in bilateralism\n shown in blue color line")+
  theme_bw()+
  labs(
    title = "Fraction of dyads engaged in multilateralism and bilateralism",
    x = "Year",
    y = "Fraction of dyads",
    caption = "Data source: WTO & UN") +
  theme(plot.title = element_text(hjust = 0.50, face="bold",size=18),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
ggsave("dyad_fraction.png")
        



