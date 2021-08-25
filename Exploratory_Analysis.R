# Exploratory analysis for stats anxiety paper


# libraries
library(tidyverse)
library(gridExtra)
library(ggpubr)

# Load in data
survey_data <- read.csv("survey_data.csv")
dim(survey_data)

# Tidy data
survey_data$Statistics <- as.factor(survey_data$Statistics)
survey_data$Mathematics <- as.factor(survey_data$Mathematics)
survey_data$Nationality <- as.factor(survey_data$Nationality)
survey_data$Course <- as.factor(survey_data$Course)
survey_data$Specialist <- as.factor(survey_data$Specialist)
survey_data$Voluntary <- as.factor(survey_data$Voluntary)
survey_data$Gender <- as.factor(survey_data$Gender)

survey_data$Nationality <- factor(survey_data$Nationality,levels=levels(survey_data$Nationality)[c(1,4,2,3)],ordered=F)
survey_data$Course <- factor(survey_data$Course,levels=rev(levels(survey_data$Course)),ordered=F)
survey_data$Specialist <- factor(survey_data$Specialist,levels=rev(levels(survey_data$Specialist)),ordered=F)
survey_data$Voluntary <- factor(survey_data$Voluntary,levels=rev(levels(survey_data$Voluntary)),ordered=F)
survey_data$Year.of.Study <- factor(survey_data$Year.of.Study, ordered=F, 
                                    levels=c("Year 1", "Year 2", "Year 3", "Year 4", "Masters year"))



# Subsetted data for voluntary & non-specialist cases
survey_volun <- survey_data[survey_data$Voluntary=="Voluntary",]
survey_volun$Voluntary <- droplevels(survey_volun$Voluntary)
survey_spec <- survey_data[survey_data$Specialist=="Non-Specialist",]
survey_spec$Specialist <- droplevels(survey_spec$Specialist)
survey_performance <- survey_data[survey_data$Course!="Course 3",]
survey_performance$Course <- droplevels(survey_performance$Course)


# Initial plots #

# Set up generic plot commands 

# Set colour-blind friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##        ##
# Figure 1 #
##        ##

# Figure 1(a)
# prepare a special xlab with the number of obs for each group
my_xlab1a <- paste(levels(survey_data$Specialist),"\n(N=",table(survey_data$Specialist),")",sep="")
f1a <- ggplot(data=survey_data, aes(x=Specialist,y=SA.mean,fill=Specialist)) + 
  ylab("Overall anxiety score") + xlab("")+
  geom_boxplot(varwidth = TRUE, alpha=0.6)+
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab1a) + 
  scale_fill_manual(values=cbPalette) + 
  ggtitle("(a)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Figure 1(b)
my_xlab1b <- paste(levels(survey_data$Voluntary),"\n(N=",table(survey_data$Voluntary),")",sep="")
f1b <- ggplot(data=survey_data, aes(x=Voluntary, y=SA.mean,fill=Voluntary)) + 
  ylab("Overall anxiety score") + xlab("")+
  geom_boxplot(varwidth = TRUE, alpha=0.6)+
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab1b)+ 
  scale_fill_manual(values=cbPalette)+ 
  ggtitle("(b)") + 
  theme(plot.title = element_text(hjust = 0.5))


# Figure 1(c)
my_xlab1c <- paste(levels(survey_volun$Specialist),"\n(N=",table(survey_volun$Specialist),")",sep="")
f1c <- ggplot(data=survey_volun, aes(x=Specialist,y=SA.mean,fill=Specialist)) + 
  ylab("Overall anxiety score") + xlab("")+
  geom_boxplot(varwidth = TRUE, alpha=0.6)+
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab1c)+ 
  scale_fill_manual(values=cbPalette)+ 
  ggtitle("(c)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Figure 1(d)
my_xlab1d <- paste(levels(survey_spec$Voluntary),"\n(N=",table(survey_spec$Voluntary),")",sep="")
f1d <- ggplot(data=survey_spec, aes(x=Voluntary, y=SA.mean,fill=Voluntary)) + 
  ylab("Overall anxiety score") + xlab("")+
  geom_boxplot(varwidth = TRUE, alpha=0.6)+
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab1d)+ 
  scale_fill_manual(values=cbPalette)+ 
  ggtitle("(d)") + 
  theme(plot.title = element_text(hjust = 0.5))


# Arrange fig 1 plots in grid
grid.arrange(f1a,f1b,f1c,f1d,nrow=2,ncol=2)

##        ##
# Figure 2 #
##        ##

# Figure 2(a)
my_xlab2a <- paste(levels(survey_data$Gender),"\n(N=",table(survey_data$Gender),")",sep="")
f2a <- survey_data %>% 
  drop_na(Gender) %>%
  ggplot(aes(x=Gender, y=SA.mean,fill=Gender)) + 
  ylab("Overall anxiety score") + xlab("")+
  geom_boxplot(varwidth = TRUE, alpha=0.6)+
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab2a)+ 
  scale_fill_manual(values=cbPalette)+ 
  ggtitle("(a)") + 
  theme(plot.title = element_text(hjust = 0.5))


# Figure 2(b)
my_xlab2b <- paste(levels(survey_data$Nationality),"\n(N=",table(survey_data$Nationality),")",sep="")
f2b <- survey_data %>% 
  drop_na(Nationality) %>%
  ggplot(aes(x=Nationality, y=SA.mean,fill=Nationality)) + 
  ylab("Overall anxiety score") + xlab("")+
  geom_boxplot(varwidth = TRUE, alpha=0.6)+
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab2b)+ 
  scale_fill_manual(values=cbPalette)+ 
  ggtitle("(b)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Figure 2(c)
f2c <- ggplot(survey_data,aes(x=SE.mean,y=SA.mean)) + geom_point() +
  ylab("Overall anxiety score") + xlab("Self efficacy score") + 
  geom_smooth(method="lm") +   ggtitle("(c)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Arrange Fig 2 plots in grid
ggarrange(ggarrange(f2a,f2b,nrow=2),f2c,ncol=2)

##        ##
# Figure 3 #   STUDENT PERFORMANCE
##        ##

## THIS PLOT IS DROPPED
# Figure 3(a)  # PROBABLY DROP THIS PLOT
#my_xlab <- paste(levels(survey_performance$Course),"\n(N=",table(survey_performance$Course),")",sep="")
#f3a <- survey_performance %>% 
# drop_na(Course) %>%
#drop_na(Exam) %>%
#ggplot(aes(x=Course, y=Exam)) + 
#ylab("Student Performance") + xlab("")+
#geom_boxplot(varwidth = TRUE, alpha=0.2)+
#theme(legend.position="none") +
#scale_x_discrete(labels=my_xlab)


# NEED TO SEPARATE THE COURSES IN THE PLOTS BELOW HERE

# Figure 3(a)
my_xlab3a <- paste(levels(survey_performance$Gender),"\n(N=",table(survey_performance$Gender),")",sep="")
f3a <- survey_performance %>% 
  drop_na(Gender) %>%
  drop_na(Exam) %>%
  ggplot(aes(x=Course, y=Exam,fill=Gender)) + 
  ylab("Student Performance") + xlab("")+
  geom_boxplot(varwidth = FALSE, alpha=0.6)+
  scale_fill_manual(values=cbPalette)+ 
  ggtitle("(a)") + 
  theme(legend.position="top") +
  theme(plot.title = element_text(hjust = 0.5))+
  #  theme(legend.title = element_blank())+
  NULL   # identity() to end %>%



# Figure 3(b)
my_xlab3b <- paste(levels(survey_performance$Nationality),"\n(N=",table(survey_performance$Nationality),")",sep="")
f3b <- survey_performance %>% 
  drop_na(Nationality) %>%
  drop_na(Exam) %>%
  ggplot(aes(x=Course, y=Exam,fill=Nationality)) + 
  ylab("Student Performance") + xlab("")+
  geom_boxplot(varwidth = FALSE, alpha=0.6)+
  scale_fill_manual(values=cbPalette)+ 
  ggtitle("(b)") + 
  theme(legend.position="top") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  #  theme(legend.text = element_text(size=5))+
  #  theme(legend.title = element_blank())+
  #  theme(legend.key.size = unit(0.5, 'cm'))+
  #coord_flip()+
  NULL   # identity() to end %>%

# Figure 3(c)
f3c <- ggplot(survey_performance,aes(y=Exam,x=SA.mean,fill=Course,color=Course,shape=Course)) + geom_point() +
  xlab("Overall anxiety score") + ylab("Student performance") + 
  geom_smooth(method="lm")+ 
  ggtitle("(c)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values=cbPalette) + 
  scale_color_manual(values=cbPalette) + 
  theme(legend.position="top") +
  #  theme(legend.title = element_blank())+
  NULL   # identity() to end %>%



# Figure 3(d)
f3d <- ggplot(survey_performance,aes(y=Exam,x=SE.mean,fill=Course,shape=Course,color=Course)) + geom_point(aes(fill=Course)) +
  xlab("Self efficacy score") + ylab("Student performance") + 
  geom_smooth(method="lm") + 
  ggtitle("(d)") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=cbPalette) + 
  scale_color_manual(values=cbPalette)


grid.arrange(f3a,f3b,f3c,f3d,nrow=2,ncol=2)

# Alt arrange Fig 3 plots in grid
grid.arrange(grid.arrange(f3a,f3b,nrow=2),f3c,ncol=2)

grid.arrange(grid.arrange(f3a,f3b,ncol=2),f3c,nrow=2)

grid.arrange(f3a,f3b,f3c,nrow=3)


my_theme <- theme(axis.text=element_text(size=8),
                  #        axis.text.x = element_text(size = 14),
                  axis.title.x = element_text(size = 8),
                  axis.title.y = element_text(size = 8))


# Figure 3(a1&2)
my_xlab <- paste(levels(survey_performance$Gender[survey_performance$Course=="Course 1"]),"\n(N=",table(survey_performance$Gender[survey_performance$Course=="Course 1"]),")",sep="")
f3a1 <- survey_data %>%
  filter(Course=="Course 1") %>%
  drop_na(Gender) %>%
  ggplot(aes(x=Gender, y=Exam,fill=Gender)) + 
  ylab("Student Performance") + xlab("Course 1")+
  geom_boxplot(varwidth = TRUE, alpha=0.6)+
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab)+ 
  scale_fill_manual(values=cbPalette)+ 
  ggtitle("(a)") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=12))+
  my_theme + ylim(0,100)

my_xlab <- paste(levels(survey_performance$Gender[survey_performance$Course=="Course 2"]),"\n(N=",table(survey_performance$Gender[survey_performance$Course=="Course 2"]),")",sep="")
f3a2 <- survey_data %>%
  filter(Course=="Course 2") %>%
  drop_na(Gender) %>%
  ggplot(aes(x=Gender, y=Exam,fill=Gender)) + 
  ylab("Student Performance") + xlab("Course 2")+
  geom_boxplot(varwidth = TRUE, alpha=0.6)+
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab)+ 
  scale_fill_manual(values=cbPalette)+ 
  ggtitle("(b)") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=12))+
  my_theme + ylim(0,100)


# Figure 3(b1&2)
my_xlab <- paste(levels(survey_performance$Nationality[survey_performance$Course=="Course 1"]),"\n(N=",table(survey_performance$Nationality[survey_performance$Course=="Course 1"]),")",sep="")
f3b1 <- survey_data %>%
  filter(Course=="Course 1") %>%
  drop_na(Nationality) %>%
  ggplot(aes(x=Nationality, y=Exam,fill=Nationality)) + 
  ylab("Student Performance") + xlab("Course 1")+
  geom_boxplot(varwidth = TRUE, alpha=0.6)+
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab)+ 
  scale_fill_manual(values=cbPalette)+ 
  ggtitle("(c)") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=12))+
  my_theme + ylim(0,100)

my_xlab <- paste(levels(survey_performance$Nationality[survey_performance$Course=="Course 2"]),"\n(N=",table(survey_performance$Nationality[survey_performance$Course=="Course 2"]),")",sep="")
f3b2 <- survey_data %>%
  filter(Course=="Course 2") %>%
  drop_na(Nationality) %>%
  ggplot(aes(x=Nationality, y=Exam,fill=Nationality)) + 
  ylab("Student Performance") + xlab("Course 2")+
  geom_boxplot(varwidth = TRUE, alpha=0.6)+
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab)+ 
  scale_fill_manual(values=cbPalette)+ 
  ggtitle("(d)") + 
  theme(plot.title = element_text(hjust = 0.5))+
  my_theme + ylim(0,100)

# Figure 3(e)
f3e <- ggplot(survey_performance,aes(y=Exam,x=SA.mean,fill=Course,color=Course,shape=Course)) + geom_point() +
  xlab("Overall anxiety score") + ylab("Student performance") + 
  geom_smooth(method="lm")+ 
  ggtitle("(e)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values=cbPalette) + 
  scale_color_manual(values=cbPalette) + 
  theme(legend.position="bottom") +
  theme(legend.title = element_blank())+
  NULL   # identity() to end %>%

# Alt arrange Fig 3 plots in grid
grid.arrange(grid.arrange(grid.arrange(f3a1,f3a2,ncol=2),grid.arrange(f3b1,f3b2,ncol=2),nrow=2),f3c,ncol=2)

grid.arrange(grid.arrange(grid.arrange(f3a1,f3a2,ncol=2),grid.arrange(f3b1,f3b2,ncol=2),nrow=2),f3e,ncol=1)

