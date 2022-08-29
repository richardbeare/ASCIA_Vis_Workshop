library(tidyverse)

x <- 2*pi/100 * (1:100)
traces <- tibble(T1 = sin(x), 
                 T2 = cos(x),
                 T3 = 0.3*T1 + rnorm(length(x), sd = 0.05),
                 T4 = 0.5*T2 + rnorm(length(x), mean=0.2, sd=0.1)
                 )

traces
matplot(traces)

matplot(traces, type='l')

matplot(x=x, y=traces, type='l')


traces.ggplot <- bind_cols(x=x, traces)

traces.long <- pivot_longer(traces.ggplot, cols = starts_with("T"), values_to = "y", names_to = "Group")
ggplot(traces.long, aes(x=x, y=y)) + geom_point()

ggplot(traces.long, aes(x=x, y=y)) + geom_line(aes(group=Group))

ggplot(traces.long, aes(x=x, y=y)) + geom_point(aes(colour=Group))

otherdata <- tibble(Group=c("T1", "T2", "T3", "T4"), Experiment=c("A", "A", "B", "B"), temperature = c(10, 20, 30, 40))
traces.long <- left_join(traces.long, otherdata)

traces.long


ggplot(traces.long, aes(x=x, y=y)) + geom_point()

ggplot(traces.long, aes(x=x, y=y)) + geom_point(aes(size=x))
ggplot(traces.long, aes(x=x, y=y)) + geom_point(aes(size=y))
ggplot(traces.long, aes(x=x, y=y)) + geom_point(aes(size=temperature, colour=Group))

ggplot(traces.long, aes(x=x, y=y)) + geom_text(aes(label=Group, colour=temperature))

