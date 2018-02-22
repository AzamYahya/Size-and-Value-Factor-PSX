library(tidyverse)
pak_ratio <- readRDS("Z:/quant/quantamental/pakistan/Model/pak_ratio.rda")

weights <- readRDS("Z:/quant/quantamental/pakistan/Model/weights.rda")


pak_ratio <-
  left_join(pak_ratio, weights, by = "script") %>% select(-market_cap)

ratios <- pak_ratio[,c(2,24, 27:66)]



pak_ratio <-
  pak_ratio %>%
  mutate(market_capitalization = price * `Outstanding Shares`) %>% 
  filter(!`Total Equity` <0 ) %>% 
  mutate(book_to_market = `Total Equity`/market_capitalization)

pak_ratio <-
    pak_ratio %>%
  filter(!market_capitalization  <= 0) %>%
  mutate(ln_market_capitalization = log(market_capitalization))


pak_ratio$size <-
  case_when(
    pak_ratio$ln_market_capitalization < quantile(pak_ratio$ln_market_capitalization, .3) ~ "small",
    pak_ratio$ln_market_capitalization > quantile(pak_ratio$ln_market_capitalization, .7) ~ "large",
    TRUE ~ "medium"
  )


###Returns plot with respect to size portfolio


pak_ratio %>% group_by(size, date) %>% 
  summarise(avg_return = mean(return_T1, na.rm = T) * 100) %>%
  ggplot(aes(as.numeric(date), avg_return)) +
  geom_bar(aes(fill = size), stat = "identity") + geom_smooth() +
  facet_grid(size ~., scale = "free_y") + theme_bw() +
  labs(x  = "", y = "Average Returns(%)", title = "Average Returns of Size Portfolio")

library(ggthemes)
####cumulative returns

pak_ratio %>% group_by(size, date) %>% 
  summarise(avg_return = mean(return_T1, na.rm = T) * 100) %>%
  mutate(compound = cumsum(avg_return)) %>%  
  ggplot(aes(x =as.numeric(date), y = compound, color = size)) +   
  geom_line(size  = 2)+geom_hline(aes(yintercept  = 0))+ theme_economist()+
  labs(y = "Cumulative Returns(%)",x = "",
   title = "Cumulative Returns of Size Portfolios")+
  theme(legend.title = element_blank())



#####SMB  and HML ratio


pak_ratio <-
  pak_ratio %>%
  mutate(market_capitalization = price * `Outstanding Shares`) %>% 
  filter(!`Total Equity` <0 ) %>% 
  mutate(book_to_market = `Total Equity`/market_capitalization)


# pak_ratio <- pak_ratio %>% 
#   group_by(date) %>% 
#   summarise(var.p70 = quantile(book_to_market, probs = .7, na.rm = T),
#             var.p30 = quantile(book_to_market, probs = .3, na.rm = T),
#             size.Med = quantile(ln_market_capitalization, probs = .5, na.rm = T)) %>% 
#   left_join(pak_ratio, ., by = "date")


###apply the function as describes
#https://stackoverflow.com/questions/42854632/r-function-with-expression-as-parameter-for-dplyr-summarise

var_portfolio <- function(df, ...){ 
  df %>% group_by(date) %>% summarise_(.dots = lazyeval::lazy_dots(...)) 
}

# 
# var_portfolio(pak_ratio, p70 = quantile(book_to_market, .7),
#               p30 = quantile(book_to_market, .3), 
#               size.Med = quantile(ln_market_capitalization, .5))
###Define breakpoints
pak_ratio <-  
  var_portfolio(pak_ratio, p70 = quantile(book_to_market, .7),
                p30 = quantile(book_to_market, .3), 
                size.Med = quantile(ln_market_capitalization, .5)) %>% 
  left_join(pak_ratio, . , by = "date")

###sort portfolio
pak_ratio <- pak_ratio %>% mutate(size = ifelse(ln_market_capitalization < size.Med,
                                   "small", "big"),
                     var = ifelse(book_to_market < p30, "low",
                            ifelse(book_to_market > p70, "high", "neutral")),
                     Port = paste(size, var, sep = "."))

##calculate weights by date


pak_ratio <-
  pak_ratio %>% group_by(date) %>%
  mutate(weig = ln_market_capitalization /
  sum(ln_market_capitalization))

###calculate value weighted returns
Ret<- pak_ratio %>%   group_by(date, Port) %>%
  summarise(avg_return = weighted.mean(return_T1,weig ,na.rm = T)) %>%  
  spread(Port, avg_return) %>% 
  mutate(Small = (small.high + small.neutral + small.low)/3,
         Big = (big.high + big.neutral + big.low)/3,
         SMB = Small -  Big,
         High = (small.high + big.high)/2,
         Low = (small.low + big.low)/2,
         HML = High - Low)


###plot SMB and HML

Ret %>%  select(date, SMB, HML) %>% gather(factor, returns, -date) %>%
  group_by(factor) %>% 
  mutate(compound = cumsum(returns)*100) %>%  ungroup() %>% 
  select(date, compound, factor) %>% ungroup() %>% 
  ggplot(aes(x =as.numeric(date), y = compound, color = factor)) +   
  geom_line(size = 2)+geom_hline(aes(yintercept  = 0))+ theme_economist()+
  labs(y = "Cumulative Returns(%)",
       title = "Cumulative Returns of Fama & French Factors",
       x = "date")+ theme(legend.title = element_blank())


###average returns for value portfolio



Ret %>% select(date, High, Low) %>% gather(factor, value , -date) %>% 
  mutate(value = value *100) %>% ggplot(aes(date, value)) +
  geom_bar(aes(fill = factor), stat = "identity") + geom_smooth() +
  facet_grid(factor ~ .) + theme_bw() + theme(legend.position = "none") +
  labs(x = "", y = "Average Returns(%)", title = "Average Returns for Value Portfolio")

###Cumulative returns for value portfolio

Ret %>% select(date, High, Low) %>% gather(factor, value, -date) %>% 
  mutate(value = value *100) %>%
   mutate(factor =  if_else(factor == "High",
  "High Book-to-Market",
  "Low Book-to-Market")) %>% 
  group_by(factor) %>%
  mutate(value = cumsum(value)) %>% 
  ggplot(aes(date, value))+ 
  geom_line(aes(colour = factor), size = 2)+theme_economist()+
  theme(legend.title = element_blank())+
  labs(title = "Cumulative Returns of Value Premium Portfolio",
       y = "Cumulative Returns(%)", x = "")
  
