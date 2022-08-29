library("tidyverse")

# 
leap <- readxl::read_xlsx("leap_sim_data.xlsx")

mycoltypes <- rep("text", 18)
mycoltypes[11] <- "date"
mycoltypes[c(2, 5, 10, 12:15, 18)] <- "numeric"

leap <- readxl::read_xlsx("leap_sim_data.xlsx", .name_repair = "universal")
leap <- readxl::read_xlsx("leap_sim_data.xlsx", col_types = mycoltypes, .name_repair = "universal")

leap <- rename(leap, 
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



leap <- mutate(leap, 
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

leap <- mutate(leap, Visit.Age = ifelse(Visit == -1, Age.months, Visit))

ggplot(leap, aes(x= Visit.Date, y = IgG4.IgE.Ratio)) +
  geom_point()


ggplot(leap, aes(x= Visit.Age, y = IgG4.IgE.Ratio)) +
  geom_point(aes(colour=Sex))


ggplot(leap, aes(x= Visit.Age, y = IgG4.IgE.Ratio)) +
  geom_point(aes(colour=Peanut.Wheal))



ggplot(leap, aes(x= Visit.Age, y = IgG4.IgE.Ratio)) +
  geom_point(aes(colour="red"))

ggplot(leap, aes(x= Visit.Age, y = IgG4.IgE.Ratio)) +
  geom_point(aes(colour=Sex)) + 
  geom_line(aes(group= Participant.ID), alpha=0.1)


ggplot(leap, aes(x= Visit.Age, y = IgG4.IgE.Ratio)) +
  geom_point( aes(colour=Sex)) + 
  geom_line(aes(group= Participant.ID), alpha=0.1) +
  facet_wrap(~Treatment.Group)


ggplot(leap, aes(x= Visit.Age, y = IgG4.IgE.Ratio)) +
  geom_point( aes(colour=Sex)) + 
  geom_line(aes(group= Participant.ID), alpha=0.1) +
  facet_grid(Sex~Treatment.Group)


ggplot(leap, aes(x= Visit.Date, y = IgG4.IgE.Ratio)) +
  geom_point( aes(colour=Sex)) + 
  geom_line(aes(group= Participant.ID), alpha=0.1) +
  facet_grid(Sex~Treatment.Group) +
  xlab("Date of assessment")


############################

leapfilt <- filter(leap, ITT == "Yes", Visit == 60)
G1 <- summarise(group_by(leapfilt, Stratum, Treatment.Group), 
                Proportion = 100 * sum(Overall.V60.Outcome == "FAIL OFC")/n())




G1 <- leapfilt %>% 
  group_by(Stratum, Treatment.Group) %>%   
  summarise(Proportion = 100 * sum(Overall.V60.Outcome == "FAIL OFC")/n(), .groups = "drop")
  
G2 <- summarise(group_by(leapfilt, Treatment.Group), 
                Proportion = 100 * sum(Overall.V60.Outcome == "FAIL OFC")/n())

G2 <- mutate(G2, Stratum = "Combined")
G2 <- mutate(G2, Stratum = factor("Combined"))
GG <- bind_rows(G1, G2)

ggplot(GG, aes(x=Treatment.Group, colour=Treatment.Group)) + 
  geom_col(aes(y=Proportion, fill=Treatment.Group)) +
  geom_text(aes(y = Proportion + 1, label=paste(round(Proportion, 1), "%", sep=""))) +
  facet_wrap(~Stratum) +
  scale_fill_manual( values=c("Peanut Avoidance" = "dark grey", 
                              "Peanut Consumption" = "dark green")) +
  scale_colour_manual( values=c("Peanut Avoidance" = "dark grey", 
                                "Peanut Consumption" = "dark green")) 


nejmcolourstuff <- list(scale_fill_manual( values=c("Peanut Avoidance" = "dark grey", "Peanut Consumption" = "dark green")),
                        scale_colour_manual( values=c("Peanut Avoidance" = "dark grey", "Peanut Consumption" = "dark green")),
                        theme(legend.position = "none", 
                              panel.grid = element_blank(),
                              panel.background = 
                                element_rect(fill="white", colour="gray"))
                        )
ggplot(GG, aes(x=Treatment.Group, colour=Treatment.Group)) + 
  geom_col(aes(y=Proportion, fill=Treatment.Group)) +
  geom_text(aes(y = Proportion + 1, label=paste(round(Proportion, 1), "%", sep=""))) +
  facet_wrap(~Stratum) +
  nejmcolourstuff

leapdata_fig3 <- mutate(leap, 
                        VisitCategory = 
                          factor(Visit, labels=c("4 to < 11", "12", "30", "60")))
leapdata_fig3 <- filter(leapdata_fig3,
                        Did.subject.meet.all.PP.Definitions=="Yes")

leapallergic <- filter(leapdata_fig3, Allergic)

ggplot(leapdata_fig3, aes(x=VisitCategory, y=Peanut.Wheal)) +
  geom_violin(fill="grey50", colour=NA) +
  #geom_boxplot() +
  geom_point(aes(colour=Allergic), shape=1, position=position_jitter(width=0.1)) +
  geom_line(data = leapallergic, aes(colour=Allergic, group=Participant.ID), alpha=0.25) +
  facet_wrap(~Treatment.Group)

library(lemon)

ggplot(leapdata_fig3, aes(x=VisitCategory, y=Peanut.Wheal)) +
  geom_violin(fill="grey50", colour=NA) +
  #geom_boxplot() +
  #geom_point(aes(colour=Allergic), shape=1, position=position_jitter(width=0.1)) +
  #geom_line(data = leapallergic, aes(colour=Allergic, group=Participant.ID), alpha=0.25) +
  geom_pointpath(aes(colour=Allergic, group=Participant.ID), 
                 position=position_jitter(width=0.1), distance=NA, alpha=0.1) +
  stat_summary(geom="line", fun="mean", aes(group=1), size=2) +
  facet_wrap(~Treatment.Group)
