
#install.package(c("tidyverse", "devtools", "lemon"))

library(tidyverse)

mycoltypes <- rep("text", 18)
mycoltypes
mycoltypes[2] <- "numeric"
mycoltypes[c(2, 5, 10, 12:15, 18)] <- "numeric"
mycoltypes[11] <- "date"
leapdata <- readxl::read_excel("leap_sim_data.xlsx")

#leapdata <- readxl::read_excel("simdata.xlsx", col_types = mycoltypes, .name_repair = "universal")

leapdata <- readxl::read_excel("leap_sim_data.xlsx", col_types = mycoltypes, .name_repair = "universal")
colnames(leapdata)
# View(head(leapdata))

# make the names nicer
colnames(leapdata)
leapdata <- rename(leapdata, 
                   Stratum = Stratum..char.,
                   Treatment.Group = Treatment.Group..Char.,
                   Age.months = Age..months.,
                   Sex = Sex..char.,
                   ITT = Intent.to.treat..ITT..Sample,
                   Peanut.specific.IgE = Peanut.specific.IgE..log10.,
                   Peanut.specific.IgG4.Result = Peanut.specific.IgG4.Result..log10.,
                   Peanut.specific.IgG.Result = Peanut.specific.IgG.Result..log10.,
                   IgG4.IgE.Ratio = IgG4.IgE.Ratio..Log10.,
                   Overall.V60.Outcome = Overall.V60.Outcome..OFC...Indeterm..,
                   Peanut.Wheal = Peanut.Wheal..mm.,
                   Did.subject.meet.all.PP.Definitions = Did.subject.meet.all.PP.Definitions.)

leapdata <- mutate(leapdata, 
                   Stratum = factor(Stratum),
                   Treatment.Group = factor(Treatment.Group, 
                                            levels=c("Peanut Avoidance", "Peanut Consumption")),
                   Treatment.Group2 = ifelse(Treatment.Group == "Peanut Avoidance", "Avoidance Group", "Consumption Group"),
                   Treatment.Group2 = factor(Treatment.Group2),
                   Sex = factor(Sex),
                   ITT = factor(ITT, levels=c("No", "Yes")),
                   Did.subject.meet.all.PP.Definitions = factor(Did.subject.meet.all.PP.Definitions),
                   Primary.Ethnicity = factor(Primary.Ethnicity, 
                                              levels=c("White", "Asian", "Black", "Chinese, Middle Eastern, or Other Ethnic Group", "Mixed", "Missing")),
                   Allergic = Overall.V60.Outcome == "FAIL OFC")



# Create a new column, replacing the -1 with Age

leapdata <- mutate(leapdata, Visit.Age = ifelse(Visit == -1, Age.months, Visit))

ggplot(data=leapdata, aes(x = Visit.Age, y = IgG4.IgE.Ratio)) + geom_point()


ggplot(data=leapdata, aes(x = Visit.Date, y = IgG4.IgE.Ratio)) + geom_point()

ggplot(data=leapdata, aes(x = Visit.Date, y = IgG4.IgE.Ratio, group = Participant.ID)) + 
  geom_point() +
  geom_line() + 
  facet_grid(Treatment.Group ~ Sex)


leapfilt <- filter(leapdata, ITT == "Yes", Visit == 60)
# easiest way is to do the summary - separately for each treatment group
# Normally geom_bar deals with counts, or creates proportions of total.
# This is not what we want here.
G1 <- summarise(group_by(leapfilt, Stratum, Treatment.Group), Proportion = 100 * sum(Overall.V60.Outcome == "FAIL OFC")/n())
# explain grouping 
G1 <- summarise(group_by(leapfilt, Stratum, Treatment.Group), 
                Proportion = 100 * sum(Overall.V60.Outcome == "FAIL OFC")/n(),
                .groups = "drop")

G2 <- summarise(group_by(leapfilt, Treatment.Group), Proportion = 100 * sum(Overall.V60.Outcome == "FAIL OFC")/n())
## Add a stratum column, make sure it is a factor
G2 <- mutate(G2, Stratum = "Combined")
G2 <- mutate(G2, Stratum = factor("Combined"))
GG <- bind_rows(G1, G2)
#GG <- rename(GG, Treatment.Group = Treatment.Group2)
levels(GG$Stratum)
# we use geom_col because we aren't interested in counts
ggplot(GG, aes(x=Treatment.Group)) + geom_col(aes(y=Proportion)) + facet_wrap(~Stratum)

# add text
ggplot(GG, aes(x=Treatment.Group)) + 
  geom_col(aes(y=Proportion)) + 
  geom_text(aes(y = Proportion + 1, label=paste(round(Proportion,2), "%"))) +
  facet_wrap(~Stratum)

# Or add the text as a new column
GG <- mutate(GG, annotation = paste(round(Proportion, 1), "%"))
ggplot(GG, aes(x=Treatment.Group)) + 
  geom_col(aes(y=Proportion)) + 
  geom_text(aes(y = Proportion + 1, label=annotation)) +
  facet_wrap(~Stratum)


# colours - note that it starts including legends
ggplot(GG, aes(x=Treatment.Group)) + 
  geom_col(aes(y=Proportion, colour = Treatment.Group)) + 
  geom_text(aes(y = Proportion + 1, label=annotation)) +
  facet_wrap(~Stratum)

# moving the mapping around
ggplot(GG, aes(x=Treatment.Group, colour = Treatment.Group)) + 
  geom_col(aes(y=Proportion)) + 
  geom_text(aes(y = Proportion + 1, label=annotation)) +
  facet_wrap(~Stratum)


ggplot(GG, aes(x=Treatment.Group)) + 
  geom_col(aes(y=Proportion, colour = Treatment.Group, fill = Treatment.Group)) + 
  geom_text(aes(y = Proportion + 1, label=annotation)) +
  facet_wrap(~Stratum) +
  scale_fill_manual( values=c("Peanut Avoidance" = "dark grey", "Peanut Consumption" = "dark green")) +
  scale_colour_manual( values=c("Peanut Avoidance" = "dark grey", "Peanut Consumption" = "dark green"))
  #scale_fill_manual( values=c("Avoidance Group" = "dark grey", "Consumption Group" = "dark green")) +
  #scale_colour_manual( values=c("Avoidance Group" = "dark grey", "Consumption Group" = "dark green"))

nejmcolourstuff <- list(scale_fill_manual( values=c("Peanut Avoidance" = "dark grey", "Peanut Consumption" = "dark green")),
                        scale_colour_manual( values=c("Peanut Avoidance" = "dark grey", "Peanut Consumption" = "dark green"))
)


ggplot(GG, aes(x=Treatment.Group)) + 
  geom_col(aes(y=Proportion, colour = Treatment.Group, fill = Treatment.Group)) + 
  geom_text(aes(y = Proportion + 1, label=annotation)) +
  facet_wrap(~Stratum) +
  nejmcolourstuff


## At this point we go back and add treatment group 2, with new names, then run those examples again
# Then add this theme stuff


nejmcolourstuff <- list(
  scale_fill_manual( values=c("Avoidance Group" = "gray40", "Consumption Group" = "dark green")),
  scale_colour_manual( values=c("Avoidance Group" = "gray40", "Consumption Group" = "dark green")),
  theme(legend.position = 'none', panel.grid = element_blank(), panel.background = element_rect(fill="white", colour="gray"))
)

ggplot(GG, aes(x=Treatment.Group)) + 
  geom_col(aes(y=Proportion, colour = Treatment.Group, fill = Treatment.Group)) + 
  geom_text(aes(y = Proportion + 1, label=annotation)) +
  facet_wrap(~Stratum) +
  nejmcolourstuff

ggplot(GG, aes(x=Treatment.Group)) + 
  geom_col(aes(y=Proportion, colour = Treatment.Group, fill = Treatment.Group)) + 
  geom_text(aes(y = Proportion + 1 , label=annotation)) +
  facet_wrap(~Stratum) +
  nejmcolourstuff


# Per protocol analysis - Fig 2B - total = 589
# point is to emphasise the filtering steps.
leapfig2b <- filter(leapdata, Did.subject.meet.all.PP.Definitions == "Yes", Visit == 60)
length(unique(leapfig2b$Participant.ID))
# Now we do our summary
# 
G1 <- summarise(group_by(leapfig2b, Stratum, Treatment.Group2), 
                Proportion = 100 * sum(Overall.V60.Outcome == "FAIL OFC")/n(),
                .groups = "drop")
G2 <- summarise(group_by(leapfig2b, Treatment.Group2), Proportion = 100 * sum(Overall.V60.Outcome == "FAIL OFC")/n())
## Add a stratum column, make sure it is a factor
G2 <- mutate(G2, Stratum = factor("Combined"))
GGb <- bind_rows(G1, G2)
GGb <- rename(GGb, Treatment.Group = Treatment.Group2)
GGb <- mutate(GGb, annotation = paste(round(Proportion, 1), "%"))


# Let them do the next plot

ggplot(GGb, aes(x=Treatment.Group)) + 
  geom_col(aes(y=Proportion, colour = Treatment.Group, fill = Treatment.Group)) + 
  geom_text(aes(y = Proportion + 1 , label=annotation)) +
  facet_wrap(~Stratum) +
  nejmcolourstuff
######

###########
# Fig 2C - skip this if we're going slowly
# We need to carry out worst case imputation
# Those in avoidance group with missing data are assumed to be OFC PASS
# Those in consumption group with missing data are assumed to be OFC FAIL
leapfig2c_complete <- filter(leapdata, !is.na(Overall.V60.Outcome))
leapfig2c_missing <- filter(leapdata, is.na(Overall.V60.Outcome))
leapfig2c_missing <- mutate(leapfig2c_missing, 
                            Overall.V60.Outcome = ifelse(Treatment.Group == "Peanut Avoidance", "PASS OFC", "FAIL OFC")
)
length(unique(leapfig2c_complete$Participant.ID))
length(unique(leapfig2c_missing$Participant.ID))


leapfig2c <- bind_rows(leapfig2c_complete, leapfig2c_missing)
leapfig2c <- filter(leapfig2c, Visit == -1)

G1 <- summarise(group_by(leapfig2c, Stratum, Treatment.Group2), 
                Proportion = 100 * sum(Overall.V60.Outcome == "FAIL OFC")/n(),
                .groups = "drop")
G2 <- summarise(group_by(leapfig2c, Treatment.Group2), Proportion = 100 * sum(Overall.V60.Outcome == "FAIL OFC")/n())
## Add a stratum column, make sure it is a factor
G2 <- mutate(G2, Stratum = factor("Combined"))
GGc <- bind_rows(G1, G2)
GGc <- rename(GGc, Treatment.Group = Treatment.Group2)
GGc <- mutate(GGc, annotation = paste(round(Proportion, 1), "%"))


ggplot(GGc, aes(x=Treatment.Group)) + 
  geom_col(aes(y=Proportion, colour = Treatment.Group, fill = Treatment.Group)) + 
  geom_text(aes(y = Proportion + 1 , label=annotation)) +
  facet_wrap(~Stratum) +
  nejmcolourstuff

###########
## Figure 3 a
## This is a little bit tricky - stick to doing each row

## Points - this is an example of combining raw and summarized data.

# set up the x axis
leapdata_fig3 <- mutate(leapdata, VisitCategory = factor(Visit, labels=c("4 to < 11", "12", "30", "60")))
leapdata_fig3 <- filter(leapdata_fig3, Did.subject.meet.all.PP.Definitions=="Yes")
# some missing data - people without measures at some timepoints, I guess
ggplot(leapdata_fig3, aes(x=VisitCategory, y=Peanut.Wheal)) +
  #geom_violin(fill="grey50", colour=NA) +
  geom_boxplot() +
  geom_point(aes(colour=Allergic), shape=1) +
  facet_wrap(~Treatment.Group)

ggplot(leapdata_fig3, aes(x=VisitCategory, y=Peanut.Wheal)) +
  geom_violin(fill="grey50", colour=NA, bw=1) +
  geom_point(aes(colour=Allergic), shape=1, position=position_jitter(width=0.1)) +
  geom_line(aes(colour=Allergic, group=Participant.ID), alpha=0.5) +
  facet_wrap(~Treatment.Group)

## trajectories with peanut allergy only
allergiccolor <- 'red'
nejmcolourstuff2 <- list(
  scale_color_manual(values=c('darkgreen', allergiccolor))
)
ggplot(leapdata_fig3, aes(x=VisitCategory, y=Peanut.Wheal)) +
  geom_violin(fill="grey50", colour=NA, bw=1) +
  geom_point(aes(colour=Allergic), shape=1, position=position_jitter(width=0.1)) +
  geom_line(data=filter(leapdata_fig3, Allergic), aes(group=Participant.ID), alpha=0.5, color = allergiccolor) +
  facet_wrap(~Treatment.Group) +
  nejmcolourstuff2

#install.packages("lemon")

ggplot(leapdata_fig3, aes(x=VisitCategory, y=Peanut.Wheal)) +
  geom_violin(fill="grey50", colour=NA, bw=1) +
  lemon::geom_pointpath(aes(colour=Allergic, group=Participant.ID), position=position_jitter(width=0.1), distance=NA) +
  facet_wrap(~Treatment.Group) +
  nejmcolourstuff2

W <- 0.1
ggplot(leapdata_fig3, aes(x=VisitCategory, y=Peanut.Wheal)) +
  geom_violin(fill="grey50", colour=NA, bw=1) +
  geom_point(data=filter(leapdata_fig3, !Allergic), colour="dark green", position=position_jitter(width=W)) +
  lemon::geom_pointpath(data=filter(leapdata_fig3, Allergic), colour=allergiccolor, aes(group=Participant.ID), 
                        position=position_jitter(width=W), distance=NA) +
  facet_wrap(~Treatment.Group) +
  nejmcolourstuff2

ggplot(leapdata_fig3, aes(x=VisitCategory, y=Peanut.Wheal)) +
  geom_violin(fill="grey50", colour=NA, bw=1) +
  geom_point(data=filter(leapdata_fig3, !Allergic), colour="dark green", position=position_jitter(width=W)) +
  lemon::geom_pointpath(data=filter(leapdata_fig3, Allergic), colour=allergiccolor, aes(group=Participant.ID), 
                        position=position_jitter(width=W), distance=NA) +
  geom_smooth(data=filter(leapdata_fig3, Allergic), method="lm", colour='black') +
  facet_wrap(~Treatment.Group) +
  nejmcolourstuff2

# alternatively, it might just be a mean at each timepoint, connected together
# Should have looked at the following plots. This is definitely the case
ggplot(leapdata_fig3, aes(x=VisitCategory, y=Peanut.Wheal)) +
  geom_violin(fill="grey50", colour=NA, bw=1) +
  geom_point(data=filter(leapdata_fig3, !Allergic), colour="dark green", position=position_jitter(width=W)) +
  lemon::geom_pointpath(data=filter(leapdata_fig3, Allergic), colour=allergiccolor, aes(group=Participant.ID), 
                        position=position_jitter(width=W), distance=NA) +
  stat_summary(geom="line", fun="mean", aes(group=1), size=2) +
  facet_wrap(~Treatment.Group) +
  nejmcolourstuff2

#######
ggplot(leapdata_fig3, aes(x=VisitCategory, y=Peanut.specific.IgG.Result)) +
  geom_violin(fill="grey50", colour=NA) +
  geom_point(data=filter(leapdata_fig3, !Allergic), colour="dark green", position=position_jitter(width=W)) +
  lemon::geom_pointpath(data=filter(leapdata_fig3, Allergic), colour=allergiccolor, aes(group=Participant.ID), 
                        position=position_jitter(width=W), distance=NA) +
  stat_summary(geom="line", fun="mean", aes(group=1), size=2) +
  facet_wrap(~Treatment.Group) +
  nejmcolourstuff2
#
