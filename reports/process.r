# Reading the data
powerlab <- read.csv("~/Desktop/log.csv", as.is=T)
powerlab$ts <- strptime(powerlab$ts, "%Y-%m-%dT%H:%M:%S-07:00")
powerlab$field <- factor(powerlab$field)
powerlab.nums <- powerlab
powerlab.nums$value <- as.numeric(powerlab.nums$value)
powerlab.nums$cell <- factor(powerlab.nums$cell)

## Example plot
# ggplot(powerlab.nums[powerlab.nums$field == 'voltage' & powerlab.nums$cell < 4 & powerlab.nums$value > 0,], aes(ts, value)) + geom_line(alpha=0.2) + theme_bw() + facet_wrap(~cell) + stat_smooth() + labs(y='V')


## Showing IR over charge cycles
# ggplot(rbind(powerlab.nums[powerlab.nums$field == 'ir' & powerlab.nums$value > 0,], powerlab.nums[powerlab.nums$field == 'fast_amps',]), aes(ts, value, color=cell)) + geom_line(alpha=0.2) + theme_bw() + stat_smooth() + labs(y='mΩ vs. Amps', x='') + facet_wrap(~field, nrow=2, scales='free_y') + guides(color=F)
