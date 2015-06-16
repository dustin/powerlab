# Reading the data
powerlab <- read.csv("~/Desktop/log.csv", as.is=T)#
powerlab$ts <- strptime(powerlab$ts, "%Y-%m-%dT%H:%M:%S-07:00")#
powerlab$field <- factor(powerlab$field)#
powerlab.nums <- powerlab#
powerlab.nums$value <- as.numeric(powerlab.nums$value)

## Example plot
# ggplot(powerlab.nums[powerlab.nums$field == 'voltage' & powerlab.nums$cell < 4 & powerlab.nums$value > 0,], aes(ts, value)) + geom_line(alpha=0.2) + theme_bw() + facet_wrap(~cell) + stat_smooth() + labs(y='V')
