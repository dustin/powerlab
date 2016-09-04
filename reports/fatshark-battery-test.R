powerlab <- read.csv("~/Desktop/fatshark.csv", header=T, as.is=T)

powerlab$ts <- strptime(powerlab$ts, "%Y-%m-%dT%H:%M:%S-07:00")
powerlab$field <- factor(powerlab$field)
powerlab.nums <- powerlab[powerlab$cell %in% c(0, 1, 2),]
powerlab.nums$value <- as.numeric(powerlab.nums$value)
powerlab.nums$cell <- factor(powerlab.nums$cell)
powerlab.nums <- powerlab.nums[powerlab.nums$value > 0 | !(powerlab.nums$field %in% c('voltage', 'ir')),]

ggplot(powerlab.nums[powerlab.nums$field %in% c('voltage', 'ir', 'avg_amps'),], aes(ts, value, color=cell, group=cell)) +
    geom_line() + theme_bw() + labs(x='', y='') +
    facet_wrap(~field, nrow=3, scales='free_y') + ggtitle('Crappy Battery Cycle Test') + guides(color=F)
