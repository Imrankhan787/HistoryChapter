


library(tidyverse)
library(readxl)
library(lubridate)


trade_gdp_ratio <- read_xls(file.choose(), skip = 3)
colnames(trade_gdp_ratio)

world_trade_gdpRatio <- filter(trade_gdp_ratio, `Country Name` == "World" )

world_trade_gdpRatio <- select(world_trade_gdpRatio, -c("Country Name", "Country Code","Indicator Name","Indicator Code"))


world_trade_gdpRatio <- pivot_longer(world_trade_gdpRatio, cols = everything(), names_to = "Year", values_to = "global_trade_gdp_ratio" )

world_trade_gdpRatio <- filter(world_trade_gdpRatio, !is.na(global_trade_gdp_ratio))
world_trade_gdpRatio$Year <- as.numeric(world_trade_gdpRatio$Year)


ggplot(data = world_trade_gdpRatio, mapping = aes(x = Year, y = global_trade_gdp_ratio) ) +
  geom_point(size = 3.5)+
  geom_line(color = "blue", size = 1.5)+
  theme_bw()+
  labs(
    title = "Global trade to GDP ratio",
    x = "Year",
    y = "Global trade to GDP ratio",
    caption = "Data source: World Bank") +
  theme(plot.title = element_text(hjust = 0.50, face="bold",size=18))
ggsave("globalTradeGdpRatio.png")
