##############EXERCISE 2: BUTTERFLY HOSTS, ADULT MASS AND GROWTH RATE###################3
#BIOS15, ELIZABETH CAMERON, 14/11/2026 TURN IN DATE

#code copied over from another script that was used for the rest of exercise 2

#load project libraries, install.packages("") first if not already installed
library(tidyverse) #ggplot
library(emmeans) #get estimated means easier
library(dbplyr)

################################################################################
###OVERARCHING INVESTIGATION: how do the different larval host plant types
#and maternal host plant types (Barbarea and Berteroa) affect butterfly
#adult weight and larval growth rate?

#load data and get summary statistics, make sure wd is set to where data is
bf<-read_csv("butterflies.csv")

#summary stats/parameters for adultweight
summary(bf$AdultWeight)

#summary stats/parameters for growth rate
summary(bf$GrowthRate)

#want summary stats within the different treatment groups, so grouping them
#adultweight summary:
summary_df_adw <- bf %>% #piping allows the dataset to be mentioned just once
  group_by(MaternalHost, LarvalHost) %>%
  summarize(
    mean_wt = mean(AdultWeight, na.rm = TRUE),
    median_wt = median(AdultWeight, na.rm = TRUE),
    quantile1 = quantile(AdultWeight, 0.25, na.rm=TRUE),
    quantile3 = quantile(AdultWeight, 0.75, na.rm=TRUE),
    IQR = abs(quantile1-quantile3),
    sd_wt = sd(AdultWeight, na.rm = TRUE),
    total_count = n()
  )
summary_df_adw

#growth rate summary:
summary_df_gr <- bf %>% 
  group_by(MaternalHost, LarvalHost) %>%
  summarize(
    mean_wt = mean(GrowthRate, na.rm = TRUE),
    median_wt = median(GrowthRate, na.rm = TRUE),
    quantile1 = quantile(GrowthRate, 0.25, na.rm=TRUE),
    quantile3 = quantile(GrowthRate, 0.75, na.rm=TRUE),
    IQR = abs(quantile1-quantile3),
    sd_wt = sd(GrowthRate, na.rm = TRUE),
    total_count = n()
  )
summary_df_gr


################################################################################
#1st question: how do larval host type and maternal host type (and their interaction)
#effect adult weight?
#two way ANOVA: lm(response~predictor1*predictor2, data=)

#Creating an interaction plot
bf$MaternalHost<-factor(bf$MaternalHost, levels=c("Barbarea","Berteroa"))
bf$LarvalHost<-factor(bf$LarvalHost, levels=c("Barbarea","Berteroa"))


sumdat_adw<- bf %>%
  group_by(MaternalHost, LarvalHost) %>% #grouping and then..
  summarise(mean=mean(AdultWeight, na.rm=TRUE),#getting means and se for each group
            se   = sd(AdultWeight, na.rm = TRUE) /
              sqrt(sum(!is.na(AdultWeight))),
            .groups = "drop")

pd <- position_dodge(width = 0.06)   # shifts overlapping elements slightly


ggplot(sumdat_adw, aes(x = LarvalHost, y = mean, #uses summary data of means as base of plot, 
                   #works bc still connected to bf data set using piping
                   group = MaternalHost,
                   shape = MaternalHost,
                   linetype = MaternalHost)) +
  geom_line(position = pd, linewidth=1) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = .1, linetype = 1, position = pd, linewidth=.5) +
  geom_point(position = pd, fill = "cadetblue", size = 5) +
  scale_shape_manual(values = c(21, 16)) + #specific point shapes
  scale_linetype_manual(values = c(2,1)) +
  labs(x = "Larval host",
       y = "Adult Mass (mg)",
       shape = "Maternal host",
       linetype = "Maternal host"
  ) +
  theme_classic() +
  theme(text=element_text(size=14, face="bold",
                          family="sans"), 
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.85))


#saving adult weight interaction plot
ggsave("Adult_Weight_MLhosts.jpg")

#looking at spread of data using boxplots w ind data points
bf$x0 <- as.numeric(bf$LarvalHost) + c(-0.25, 0.25)[as.numeric(bf$MaternalHost)]

ggplot(bf, aes(colour = MaternalHost, shape = MaternalHost)) +
  # raw points, to the left of each box
  geom_point(aes(x = x0 - 0.05, y = AdultWeight),
             alpha = 0.6, size = 1.5,
             position = position_jitter(width = 0.03, height = 0, seed = 1)) +
  # boxplots, to the right of the points
  geom_boxplot(aes(x = x0 + 0.1, y = AdultWeight,
                   group = interaction(MaternalHost, LarvalHost)),
               width = 0.15, fill = NA, outlier.shape = NA) +
  scale_x_continuous(breaks = 1:2, labels = levels(bf$LarvalHost),
                     limits = c(0.5, 2.5)) +
  scale_shape_manual(values = c(21, 16)) +
  scale_colour_manual(values = c("cadetblue", "black")) +
  labs(x = "Larval host",
       y = "Adult Weight (mg)",
       colour = "Maternal host",
       shape = "Maternal host") +
  theme_classic() +
  theme(text=element_text(face="bold"),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.10))

#saving boxplot for adultweight and hosts
ggsave("Adult_Weight_MLhosts_Boxplot.jpg")

#running two-way anova:
adwanova<-lm(AdultWeight~LarvalHost*MaternalHost, data=bf)
anova(adwanova) #when df is 1, sum sq and mean sq is the same 
summary(adwanova)
#intercept/reference comparing to: larvalBA and maternal BA, calc by hand first
#Calculating estimated means for each combination of modalities 
#estimated mean for larvalhostba and maternalhostba= 65.3549
#estimated mean for larvalhostba and maternalhostbe= 65.3549+1.3695=66.7244
#estimated mean for larvalhostbe and maternalba= 65.3549 + -11.8513= 53.5036
#estimated mean for larvalhostbe and maternalhostbe= 50.3769

bf$LarvalHost <- relevel(factor(bf$LarvalHost), ref = "Berteroa") #both int as berteroa
bf$MaternalHost <- relevel(factor(bf$MaternalHost), ref = "Berteroa")

adwanova<-lm(AdultWeight~LarvalHost*MaternalHost, data=bf)
summary(adwanova)

bf$LarvalHost <- relevel(factor(bf$LarvalHost), ref = "Barbarea") #int as Lbarb and Mbert
bf$MaternalHost <- relevel(factor(bf$MaternalHost), ref = "Berteroa")
summary(adwanova)

adwanova<-lm(AdultWeight~LarvalHost*MaternalHost, data=bf)
summary(adwanova)

#shows that switching to larval bert in either m host led to decrease in adult mass
#if did reverse, Lbert to Lbarb, contrast results would be the same but positive

#quicker way to do this: emmeans and pairs function
emmeans(adwanova, ~ LarvalHost*MaternalHost)
emadw<-emmeans(adwanova, ~ LarvalHost*MaternalHost)
#provides estimated means and CIs, handcalc correct!
pairs(emadw, infer=c(TRUE, TRUE))

#checking residuals-should be roughly normal in order to justify linear based model
par(mfrow=c(2,2)) #see all plots at once
plot(adwanova) #normal, follow qqplot line and res vs fitted line straight across

effectsize::eta_squared(aov(AdultWeight~LarvalHost*MaternalHost, data=bf), partial = FALSE)
#obtain effect sizes and 95% CI 
#larval host explains most of the variation

################################################################################
#2nd question: how do larval and maternal hosts and their interaction effect
#larval growth rates?

#repeating same steps, but changing response variable to GrowthRate
bf$MaternalHost<-factor(bf$MaternalHost, levels=c("Barbarea","Berteroa"))
bf$LarvalHost<-factor(bf$LarvalHost, levels=c("Barbarea","Berteroa"))

sumdat<- bf %>%
  group_by(MaternalHost, LarvalHost) %>% #grouping and then..
  summarise(mean=mean(GrowthRate, na.rm=TRUE),#getting means and se for each group
            se   = sd(GrowthRate, na.rm = TRUE) /
              sqrt(sum(!is.na(GrowthRate))),
            .groups = "drop")

pd <- position_dodge(width = 0.06)   # shifts overlapping elements slightly

par(mfrow=c(1,2))
ggplot(sumdat, aes(x = LarvalHost, y = mean, #uses summary data of means as base of plot, 
                   #works bc still connected to bf data set using piping
                   group = MaternalHost,
                   shape = MaternalHost,
                   linetype = MaternalHost)) +
  geom_line(position = pd, linewidth=1) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = .1, linetype = 1, position = pd, linewidth=.5) +
  geom_point(position = pd, fill = "sienna", size = 3) +
  scale_shape_manual(values = c(21, 16)) + #specific point shapes
  scale_linetype_manual(values = c(2,1)) +
  labs(x = "Larval host", 
       y = "Growth Rate ((final mg-initial mg)/larval development time (days))",
       shape = "Maternal host",
       linetype = "Maternal host")+
  theme_classic(base_size = 12) +
  theme(text=element_text(face="bold",
                          family="sans"), 
        axis.title.y.left = element_text(size=10),
        legend.position = "inside",
        legend.position.inside = c(0.65, 0.85))


#saving growth rate mean estimate plot/interaction plot
ggsave("Growth_Rate_MLhosts.jpg")


bf$x0 <- as.numeric(bf$LarvalHost) + c(-0.25, 0.25)[as.numeric(bf$MaternalHost)]

ggplot(bf, aes(colour = MaternalHost, shape = MaternalHost)) +
  # raw points, to the left of each box
  geom_point(aes(x = x0 - 0.05, y = GrowthRate),
             alpha = 0.6, size = 1.5,
             position = position_jitter(width = 0.03, height = 0, seed = 1)) +
  # boxplots, to the right of the points
  geom_boxplot(aes(x = x0 + 0.1, y = GrowthRate,
                   group = interaction(MaternalHost, LarvalHost)),
               width = 0.15, fill = NA, outlier.shape = NA) +
  scale_x_continuous(breaks = 1:2, labels = levels(bf$LarvalHost),
                     limits = c(0.5, 2.5)) +
  scale_shape_manual(values = c(21, 16)) +
  scale_colour_manual(values = c("sienna", "black")) +
  labs(x = "Larval host",
       y = "Growth Rate ((final mg-initial mg)/larval development time (days))",
       colour = "Maternal host",
       shape = "Maternal host") +
  theme_classic() +
  theme(text=element_text(face="bold"),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.9))

#saving boxplot for adultweight and hosts
ggsave("Growth_Rate_MLhosts_Boxplot.jpg")

#running ananova comparing effect of larval and maternal host on growth rate
granova<-lm(GrowthRate~LarvalHost*MaternalHost, data=bf)
granova1<-anova(granova) #when df is 1, sum sq and mean sq is the same 
summary(granova)
#intercept/reference comparing to: larvalBA and maternal BA

emmeans(granova, ~ LarvalHost*MaternalHost)
emm<-emmeans(granova, ~ LarvalHost*MaternalHost)
#getting estimated means and CIs for all contrasts at same time
pairs(emm, infer=c(TRUE, TRUE))

#checking residuals
par(mfrow=c(2,2)) #see all plots at once
plot(granova) #normal, follow qqplot line and res vs fitted line straight across



