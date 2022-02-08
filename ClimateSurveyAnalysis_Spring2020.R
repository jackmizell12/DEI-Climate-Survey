
##### Code for the University of Arizona Graduate Student Climate Survey, 2020 #####


#checks and installs necessary libraries to run scripts, can be a little finicky, but will at least tell you what you do not have installed.

list.of.packages <- c("wesanderson", "tidyverse","patchwork","ggridges")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


#call necessary libraries

#this is just a color pallette I preferred
library(wesanderson)

#these are all actually necessary
library(tidyverse)
library(patchwork)
library(ggridges)


# !!! Need to change this to the desired file path where the csv is housed on your computer or on the osf ! ! !
data<- read_csv("C:/Users/jackm/Desktop/Climate Survey Report/Climate_Survey_Draft_2020_Anon.csv")


# basic plotting logic I used for all graphs, there are more elegant ways to do it, but this functions. 
data$Q1<-data$Q40_1

QuestionText<-data$Q1[1] 
QuestionText
scale1<-c("Definitely yes","Probably yes","Unsure","Probably not","Definitely not")
  

CovA<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "During the COVID-19 pandemic... \n - my timeline for completing the program has been affected") +
  scale_x_discrete(limits=scale1) +
  scale_y_continuous(breaks=seq(0,30,1)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))


CovA

data$Q1<-data$Q40_3

head(data$Q1)

###

CovB<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "During the COVID-19 pandemic... \n - I have been satisfied with the overall department response \n to the pandemic") +
  scale_x_discrete(limits=scale1) +
  scale_y_continuous(breaks=seq(0,30,1)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

CovB

###

data$Q1<-data$Q40_4

head(data$Q1)



CovC<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "During the COVID-19 pandemic... \n - I have felt adequately supported by my mentor") +
  scale_x_discrete(limits=scale1) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

CovC

###

data$Q1<-data$Q40_2

head(data$Q1)


CovD<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "During the COVID-19 pandemic... \n my mentor has given me flexibility during this time") +
  scale_x_discrete(limits=scale1) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

CovD

### Funding Stuff ###

data$Q1<-data$Q40_5

head(data$Q1)


CovE<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "During the COVID-19 pandemic... \n I have felt adequately supported by the department") +
  scale_x_discrete(limits=scale1) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

CovE

#########  Funding

scale2<-c("Yes","No")

data$Q1<-data$Q3

head(data$Q1)


FundA<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Do you feel that the current graduate student stipend \n is enough to live comfortably?") +
  scale_x_discrete(limits=scale2) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 2))

FundA

data$Q1<-data$Q4

head(data$Q1)


FundB<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Have you had to borrow money or receive other assistance \n in addition to your stipend during your time \n as a graduate student in this department?") +
  scale_x_discrete(limits=scale2) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 2))

FundB


FundA|FundB


#######3

FundA

data$Q1<-data$Q4

head(data$Q1)


FundB<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Have you had to borrow money or receive other assistance \n in addition to your stipend during your time \n as a graduate student in this department?") +
  scale_x_discrete(limits=scale2) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 2))


###### TA #######

Scale3<-c("Extremely satisfied", "Somewhat satisfied", "Neither satisfied nor dissatisfied", "Somewhat dissatisfied", "Extremely dissatisfied")

data$Q1<-data$Q6_1

head(data$Q1)


taA<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "How would you rate your experience as a TA \n in terms of Workload") +
  scale_x_discrete(limits=Scale3) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  xlab("Level of Satisfaction") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

taA

###
data$Q1<-data$Q6_2

head(data$Q1)


taB<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "How would you rate your experience as a TA \n in terms of Treatment by Professors") +
  scale_x_discrete(limits=Scale3) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  xlab("Level of Satisfaction") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

taB

####

data$Q1<-data$Q6_3

head(data$Q1)


taC<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "How would you rate your experience as a TA \n in terms of Your own growth as an instructor") +
  scale_x_discrete(limits=Scale3) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  xlab("Level of Satisfaction") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

taC

#### Discrimination
scale4<-c("Yes","Unsure","No")


data$Q1<-data$Q11_1

head(data$Q1)


DisA<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Age") +
  scale_x_discrete(limits=scale4) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3))

DisA

###
data$Q1<-data$Q11_2

head(data$Q1)


DisB<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Race") +
  scale_x_discrete(limits=scale4) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3))

DisB

#####
data$Q1<-data$Q11_3

head(data$Q1)


DisC<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Gender") +
  scale_x_discrete(limits=scale4) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3))

DisC


####
data$Q1<-data$Q11_5

head(data$Q1)


DisE<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Sexual Orientation") +
  scale_x_discrete(limits=scale4) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3))

DisE

####

data$Q1<-data$Q11_6

head(data$Q1)


DisF<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Religius Views") +
  scale_x_discrete(limits=scale4) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3))

DisF

#### Dis 
data$Q1<-data$Q11_7

head(data$Q1)


DisG<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Political Views") +
  scale_x_discrete(limits=scale4) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3))

DisG

###
data$Q1<-data$Q11_8

head(data$Q1)


DisH<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Cultural Background") +
  scale_x_discrete(limits=scale4) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3))

DisH

###
data$Q1<-data$Q11_9

head(data$Q1)


DisI<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Mental Health Condition") +
  scale_x_discrete(limits=scale4) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3))

DisI

###
data$Q1<-data$Q11_11

head(data$Q1)


DisK<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Immigration Status") +
  scale_x_discrete(limits=scale4) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3))

DisK

###
data$Q1<-data$Q11_12

head(data$Q1)


DisK<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Socioeconomic Status") +
  scale_x_discrete(limits=scale4) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3))

DisK


### 

data$Q1<-data$Q11_10

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Physical Disability") +
  scale_x_discrete(limits=scale4) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3))

DisL

### Leaving the Department ### 
scale5<-c("Never", "Rarely" , "Sometimes" , "Often" , "All of the time")

data$Q1<-data$Q14

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I have thought about leaving the department \n because of exclusionary or discriminatory treatment") +
  scale_x_discrete(limits=scale5) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

DisL
### 
Scale6<-c("Strongly disagree", "Disagree","Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree","Strongly agree")
pal <- wes_palette("Darjeeling1", 7, type = "continuous")
data$Q1<-data$Q15_1

head(data$Q1)

ComfortA<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I would feel comfortable approaching someone \n in the department to file a formal complaint") +
  scale_x_discrete(limits=Scale6) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

ComfortA

#####

data$Q1<-data$Q15_2

head(data$Q1)

ComfortB<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I would recommend the U of Arizona Psychology Department \n to a colleague seeking a diverse institution \n for work, or school.") +
  scale_x_discrete(limits=Scale6) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

ComfortB

###
data$Q1<-data$Q15_3

head(data$Q1)

ComfortC<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I feel free to express opinions about my experiences \n in the department without fear of retaliation \n against me.") +
  scale_x_discrete(limits=Scale6) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

ComfortC


geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I have thought about leaving the department \n because of exclusionary or discriminatory treatment") +
  scale_x_discrete(limits=scale5) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

DisL

###
### Rate Adjectives, need to convert data to long ### 

dataAdj<-select(data,Q9_1:Q9_14)
dataAdj<-as.data.frame(dataAdj[c(3:65),])
from<-colnames(dataAdj)
nombre<-c("Hostile:Friendly","Racist:Non-racist","Homogenous:Diverse","Disrespecful:Respectful","Contentious:Collegial","Sexist:Non-sexist","Individualistic:Collaborative","Competitive:Cooperative","Homophobic:Non-homophobic","Unsupportive:Supportive","Ageist:Non-ageist","Unwelcoming:Welcoming","Elitist:Non-elitist","Ableist:Non-ableist")

#describe(dataAdj)
colnames(dataAdj)<-nombre
dataAdj<-as.numeric(dataAdj)
dataAdjLong<-gather(dataAdj,Adjective,value="rating")
dataAdjLong$rating<-as.numeric(dataAdjLong$rating)

pal <- wes_palette("Darjeeling1", 14, type = "continuous")


Ridge<-ggplot(dataAdjLong, aes(x=rating,y=Adjective, fill=Adjective)) +
  geom_density_ridges(scale=2) +
labs(title = "Which Word Would You Say Describes The Department Better?") +
  theme_ridges(grid=FALSE) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=rel(1.2),face = "bold"),
        axis.title.x = element_text(size=rel(1.5),face = "bold",hjust=0.5),
        axis.title.y = element_text(size=rel(1.5),face = "bold",hjust=0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)




Ridge + theme(legend.position = "none")

#####

data$Q1<-data$Q16_1

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I have received unwanted sexual advances \n in the department") +
  scale_x_discrete(limits=scale5) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

DisL

###
data$Q1<-data$Q16_2

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I have been sexually harassed \n in the department") +
  scale_x_discrete(limits=scale5) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

DisL

###
data$Q1<-data$Q16_3

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I have observed someone else being sexually harassed \n in the department") +
  scale_x_discrete(limits=scale5) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

DisL

###
data$Q1<-data$Q16_5

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I have been concerned with my physical safety \n around members of the department") +
  scale_x_discrete(limits=scale5) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

DisL


###
data$Q1<-data$Q16_6

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I have feared retaliation if I voice \n a concern in the department") +
  scale_x_discrete(limits=scale5) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

DisL

###
data$Q1<-data$Q16_7

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I have felt isolated \n in the department") +
  scale_x_discrete(limits=scale5) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

DisL

###

###
Scale6<-c("Strongly Disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")

pal <- wes_palette("Darjeeling1", 7, type = "continuous")
data$Q1<-data$Q14_1

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I am satisfied with the training \n I have received in classes within the department") +
  scale_x_discrete(limits=Scale6) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL

###
data$Q1<-data$Q14_2

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I am satisfied with the clinical experience \n I have received within the department") +
  scale_x_discrete(limits=Scale6) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL

###
data$Q1<-data$Q14_3

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "This department offers classes I need to  \n fulfill program requirements") +
  scale_x_discrete(limits=Scale6) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL

###
data$Q1<-data$Q14_4

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I am satisfied with the cultural competency training I have received") +
  scale_x_discrete(limits=Scale6) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL

###
data$Q1<-data$Q14_5

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I believe the department has provided adequate physical resources") +
  scale_x_discrete(limits=Scale6) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL

###
data$Q1<-data$Q14_6

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I have been prepared for a job in academia") +
  scale_x_discrete(limits=Scale6) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL


###
data$Q1<-data$Q14_7

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I have been prepared to pursue a job outside of academia") +
  scale_x_discrete(limits=Scale6) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL

###

scale1<-c("Definitely yes","Probably yes","Might or might not","Probably not","Definitely not")

data$Q1<-data$Q8

head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Would you benefit from a weekly graduate writing group?") +
  scale_x_discrete(limits=scale1) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL

### Pie Chart for program breakdown
data$Q1<-data$Q31

head(data$Q31)

scale7<-c("Clinical","Social","Cognition and Neural Systems", NA)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Program Breakdown") +
  scale_x_discrete(limits=scale7) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Program") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 4))

DisL

###

Scale8<-c("Strongly Disagree", "Disagree","Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree","Strongly agree")

data$Q1<-data$Q31_1

unique(data$Q31_1)
head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "My advisor makes me feel comfortable and valued") +
  scale_x_discrete(limits=Scale8) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL
###

data$Q1<-data$Q31_2

unique(data$Q31_2)
head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "My advisor is responsive") +
  scale_x_discrete(limits=Scale8) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL

###
###

data$Q1<-data$Q31_3

#unique(data$Q31_3)
head(data$Q1)


DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I fear retaliation from my advisor when I voice concerns") +
  scale_x_discrete(limits=Scale8) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL

###
data$Q1<-data$Q31_4


head(data$Q1)

DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "My advisor meets with me regularly") +
  scale_x_discrete(limits=Scale8) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL
###
###
data$Q1<-data$Q31_5


head(data$Q1)

DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "My advisor provides me with useful program/career advice") +
  scale_x_discrete(limits=Scale8) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL

###
data$Q1<-data$Q31_6


head(data$Q1)

DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I would recommend my advisor to new students") +
  scale_x_discrete(limits=Scale8) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL

###
data$Q1<-data$Q31_8


head(data$Q1)

DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I am overall satisfied with the mentorship I receive \n from my advisor(s)") +
  scale_x_discrete(limits=Scale8) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL
###
###
###
data$Q1<-data$Q31_9


head(data$Q1)

DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "I would like to have a formal system to \n evaluate my advisor's mentorship") +
  scale_x_discrete(limits=Scale8) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL
###
data$Q1<-data$Q31_10


head(data$Q1)

DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "My advisor has made it difficult to meet department milestones.") +
  scale_x_discrete(limits=Scale8) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal)

DisL






















###

data$Q1<-data$Q26

head(data$Q1)

#Mentorship Agreements
DisL<-ggplot(data,aes(x=factor(Q1),fill=factor(Q1))) +
  geom_bar(show.legend = FALSE, color="Black",size=1.5) +
  labs(title = "Do you believe that mentor/mentees \n should sign mentorship agreements?") +
  scale_x_discrete(limits=scale1) +
  scale_y_continuous(breaks=seq(0,60,2)) +
  xlab("Level of Agreement") +
  ylab("Total Responses") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.2),face="bold"),
        axis.text.y=element_text(size=rel(1.2),face = "bold", angle = 30),
        axis.title.x = element_text(size=rel(1.3),face = "bold"),
        axis.title.y = element_text(size=rel(1.3),face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))

DisL










































