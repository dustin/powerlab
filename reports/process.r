# Reading the data
powerlab <- read.csv("~/Desktop/log.csv", as.is=T)
powerlab$ts <- strptime(powerlab$ts, "%Y-%m-%dT%H:%M:%S-07:00")
powerlab$field <- factor(powerlab$field)
powerlab.nums <- powerlab
powerlab.nums$value <- as.numeric(powerlab.nums$value)
powerlab.nums$cell <- factor(powerlab.nums$cell)

## Example plot
# ggplot(powerlab.nums[powerlab.nums$field == 'voltage' & powerlab.nums$value > 0,], aes(ts, value)) + geom_line(alpha=0.2) + theme_bw() + facet_wrap(~cell) + stat_smooth() + labs(y='V')


## Showing IR over charge cycles
# ggplot(rbind(powerlab.nums[powerlab.nums$field == 'ir' & powerlab.nums$value > 0,], powerlab.nums[powerlab.nums$field == 'fast_amps',]), aes(ts, value, color=cell)) + geom_line(alpha=0.2) + theme_bw() + stat_smooth() + labs(y='mΩ vs. Amps', x='') + facet_wrap(~field, nrow=2, scales='free_y') + guides(color=F)

## Show current over time (for those 1C tests): e.g. https://usercontent.irccloud-cdn.com/file/oxpchTHx/1c.png
# ggplot(powerlab.nums[powerlab.nums$field %in% c('avg_amps')  & powerlab.nums$value > 0,], aes(ts, value)) + geom_line(alpha=0.2) + theme_bw() + stat_smooth() + labs(y='A', x='') + geom_hline(y=1.3, color='#339933') +  annotate("text", label="Ideal 1C", x=as.POSIXct('2015-08-05 23:22'), y=1.35, size=7, colour = "#339933")
