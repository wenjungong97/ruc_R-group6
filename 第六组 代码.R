#安装所需要的包
install.packages("tidyverse")
install.packages("lubridate")
install.packages("readxl")
install.packages("highcharter")
install.packages("tidyquant")
install.packages("timetk")
install.packages("tibbletime")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("scales")

#加载包
library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)

#创建对象
symbols <- c("SZ","AG", "BG", "HS","ZXB")

#选择上证综合指数，上证综合A股指数，上证综合B股指数，沪深300，中小板指，2012年12月31日至2017年12月31日的日收盘价格，数据来源：国泰安数据库

#数据保存至Excel文件，并提取数据，并新增一列日期，格式为年月日
#将数据转换成时间序列格式数据
prices <-
  read_excel("mydata.xlsx",
             col_types = c("text", "numeric",
                           "numeric", "numeric",
                           "numeric", "numeric")) %>%
  mutate(date = ymd(date)) %>%
  tk_xts(date_var = date)

#将日度数据转为月度数据，并至每月最后一天
prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = FALSE)
head(prices_monthly, 3)

#计算数据的收益率-第一种方法
asset_returns_xts <-
  Return.calculate(prices_monthly,
                   method = "log") %>%
  na.omit()
head(asset_returns_xts, 3)

#计算数据的收益率-第二种方法
asset_returns_dplyr_byhand <-
  prices %>%
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  data.frame(date = index(.)) %>%
  # 移除指数因为指数转换成了行名
  remove_rownames() %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  mutate(returns = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  spread(asset, returns)%>%
  select(date, symbols)

#删除空缺值，即第一行
asset_returns_dplyr_byhand <-
  asset_returns_dplyr_byhand %>%
  na.omit()

#计算数据的收益率-第三种方法
asset_returns_tq_builtin <-
  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               period = "monthly",
               type = "log") %>%
  spread(asset, monthly.returns)%>%
  select(date, symbols) %>%
  slice(-1)

#计算数据的收益率-第四种方法
asset_returns_tbltime <-
  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  as_tbl_time(index = date) %>%
  as_period(period = "month",
            side = "end") %>%
  gather(asset, returns, -date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               type = "log") %>%
  spread(asset, monthly.returns) %>%
  select(date, symbols) %>%
  slice(-1)

#将数据整理为3列
asset_returns_long <-
  asset_returns_dplyr_byhand %>%
  gather(asset, returns, -date) %>%
  group_by(asset)

#画出收益率曲线-折线图
highchart(type = "stock") %>%
  hc_title(text = "Monthly Log Returns") %>%
  hc_add_series(asset_returns_xts[, symbols[1]],
                name = symbols[1]) %>%
  hc_add_series(asset_returns_xts[, symbols[2]],
                name = symbols[2]) %>%
  hc_add_series(asset_returns_xts[, symbols[3]],
                name = symbols[3]) %>%
  hc_add_series(asset_returns_xts[, symbols[4]],
                name = symbols[4]) %>%
  hc_add_series(asset_returns_xts[, symbols[5]],
                name = symbols[5]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) 

#画出收益率曲线-直方图
hchart(hc_hist, color = "cornflowerblue") %>%
  hc_title(text =
             paste(symbols[1],
                   "Log Returns Distribution",
                   sep = " ")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)

#创建权重向量数据
w <- c(0.25,0.25,0.20,0.20,0.10)
tibble(w, symbols)#创建简单的数据集将权重和资产组合进行匹配，检查匹配情况

portfolio_returns_xts_rebalanced_monthly <-#计算加权投资组合每月的收益率
  Return.portfolio(asset_returns_xts,#提取资产组合收益率数据
                   weights = w,#进行加权
                   rebalance_on = "months") %>% #每个月对权重进行重新调节，使权重每月都固定
  `colnames<-`("returns")#将列变量名称改为“returns”

head(portfolio_returns_xts_rebalanced_monthly, 3)

#使用tidyverse计算资产组合月收益率
asset_returns_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5])) %>%
  head(3)
portfolio_returns_dplyr_byhand <-
  asset_returns_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5]),
         weighted_returns = returns * weights) %>%
  group_by(date) %>%
  summarise(returns = sum(weighted_returns))
head(portfolio_returns_dplyr_byhand, 3)


#使用tidyquant包实现资产组合月平衡收益率的计算
portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")

#画出资产组合月收益率的散点图
portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = date, y = returns)) +
  geom_point(colour = "cornflowerblue")+
  xlab("date") +
  ylab("monthly return") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Returns Scatter") +
  scale_x_date(breaks = pretty_breaks(n=6))
#画出资产组合月收益直方图
portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = .005,
                 fill = "cornflowerblue",
                 color = "cornflowerblue") +
  ggtitle("Portfolio Returns Distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

#画出资产组合的月收益率时间序列折线图
highchart(type = "stock") %>%
  hc_title(text = "Portfolio Monthly Returns") %>%
  hc_add_series(portfolio_returns_xts_rebalanced_monthly$returns,
                name = "Rebalanced Monthly",
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

#画出单个资产的分布直方图与资产组合进行比较
asset_returns_long %>%
  ggplot(aes(x = returns,
             fill = asset)) +
  geom_histogram(alpha = 0.15,
                 binwidth = .01) +
  geom_histogram(data = portfolio_returns_tq_rebalanced_monthly,
                 fill = "cornflowerblue",
                 binwidth = .01) +
  ggtitle("Portfolio and Asset Monthly Returns") +
  theme_update(plot.title = element_text(hjust = 0.5))
#画出资产组合收益率的密度分布图和直方图
portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = .01,
                 colour = "cornflowerblue",
                 fill = "cornflowerblue") +
  geom_density(alpha = 1, color = "red") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Histogram and Density")

#用kurtosis()函数计算其峰度
kurt_xts <-
  kurtosis(portfolio_returns_xts_rebalanced_monthly$returns)

#利用kurtosis的计算公式和直接用kurtosis(）函数相比较，发现计算结果是一致的
kurt_tidy <-
  portfolio_returns_tq_rebalanced_monthly %>%
  summarise(
    kurt_builtin = kurtosis(returns),
    kurt_byhand =
      ((sum((returns - mean(returns))^4)/
          length(returns))/
         ((sum((returns - mean(returns))^2)/
             length(returns))^2)) - 3) %>%
  select(kurt_builtin, kurt_byhand)
#新增一列
kurt_tidy %>%
  mutate(xts = kurt_xts)
#画出收益率的密度分布图
portfolio_density_plot <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  ggplot(aes(x = returns)) +
  stat_density(geom = "line", alpha = 1, colour = "cornflowerblue")
portfolio_density_plot

#将峰度可视化，使用ggplot将投资组合f峰度密度函数展示成图形，并将高于和低于收益率均值两个标准差的尾部用粉色阴影标示
sd_pos <-
  mean + (2* sd(portfolio_returns_tq_rebalanced_monthly$returns))
sd_neg <-
  mean - (2* sd(portfolio_returns_tq_rebalanced_monthly$returns))
sd_pos_shaded_area <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x > sd_pos )
sd_neg_shaded_area <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x < sd_neg)
portfolio_density_plot +
  geom_area(data = sd_pos_shaded_area,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) +
  geom_area(data = sd_neg_shaded_area,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))


#将上述图形美化，增加更多图形元素，更加标准化
#增加加入均值和中位数统计变量，将均值用红色虚线标示，将中位数用黑色虚线标示
mean <- mean(portfolio_returns_tq_rebalanced_monthly$returns)

mean_line_data <- 
  ggplot_build(portfolio_density_plot)$data[[1]] %>% 
  filter(x <= mean)

median<- median(portfolio_returns_tq_rebalanced_monthly$returns)

median_line_data <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x <= median)

portfolio_density_plot +
  geom_area(data = sd_pos_shaded_area,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) +
  geom_area(data = sd_neg_shaded_area,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) +
  geom_segment(data = shaded_area_data,
               aes(x = mean,
                   y = 0,
                   xend = mean,
                   yend = density),
               color = "red",
               linetype = "dotted") +
  
  annotate(geom = "text",
           x = mean,
           y = 5,
           label = "mean",
           color = "red",
           fontface = "plain",
           angle = 90,
           alpha = .8,
           vjust = -1.75) +
  geom_segment(data = median_line_data,
               aes(x = median,
                   y = 0,
                   xend = median,
                   yend = density),
               color = "black",
               linetype = "dotted") +
  annotate(geom = "text",
           x = median,
           y = 5,
           label = "median",
           fontface = "plain",
           angle = 90,
           alpha = .8,
           vjust = 1.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))

#将资产组合的峰度与单个资产的峰度进行比较，X轴为资产，y轴为峰度
asset_returns_long %>%
  summarize(kurt_assets = kurtosis(returns)) %>%
  add_row(asset = "Portfolio",
          kurt_assets = kurt_tidy$kurt_byhand) %>%
  ggplot(aes(x = asset,
             y = kurt_assets,
             colour = asset)) +
  geom_point() +
  geom_text(
    aes(x = "Portfolio",
        y =
          kurt_tidy$kurt_byhand + .06),
    label = "Portfolio",
    color = "cornflowerblue") +
  labs(y = "kurtosis")

#计算时间序列中的滚动峰度，并删除缺失值
window <- 24
rolling_kurt_xts <-
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = kurtosis,
            width = window) %>%
  na.omit()

#使用rollify()函数，将任何函数转换为自身的版本
kurt_roll_24 <-
  rollify(kurtosis,
          window = window)
roll_kurt_tibbletime <-
  portfolio_returns_tq_rebalanced_monthly %>%
  as_tbl_time(index = date) %>%
  mutate(kurt = kurt_roll_24(returns)) %>%
  select(-returns) %>%
  na.omit()

#使用tq_mutate() 函数
rolling_kurt_tq <-
  portfolio_returns_tq_rebalanced_monthly %>%
  tq_mutate(select = returns,
            mutate_fun = rollapply,
            width = window,
            FUN = kurtosis,
            col_rename = "tq") %>%
  select(-returns) %>%
  na.omit()
#将滚动峰值可视化
rolling_kurt_tq %>%
  mutate(xts = coredata(rolling_kurt_xts),
         tbltime = roll_kurt_tibbletime$kurt) %>%
  mutate_if(is.numeric, funs(round(.,3))) %>%
  tail(3)

#使用 highcharter包来可视化滚动峰值
highchart(type = "stock") %>%
  hc_title(text = "Rolling 24-Month kurtosis") %>%
  hc_add_series(rolling_kurt_xts,
                name = "Rolling 24-Month kurtosis",
                color = "cornflowerblue") %>%
  hc_yAxis(title = list(text = "kurtosis"),
           opposite = FALSE) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE)

#使用ggplot包使图更加完善
rolling_kurt_tq %>%
  ggplot(aes(x = date, y = tq)) +
  geom_line(color = "cornflowerblue") +
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  ggtitle("Rolling 24-Month Kurtosis") +
  labs(y = "rolling kurtosis") +
  theme_update(plot.title = element_text(hjust = 0.5))
