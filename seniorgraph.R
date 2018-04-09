library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(dplyr)
library(jtools)

##Graph1
set.seed(4796)
distance <- data.frame(control = rnorm(150, mean=1.00, sd=0.008),
                       experimental = rnorm(150, mean = 0.98, sd=0.008))
dis <- gather(distance, condition, distance, 1:2)
dis$condition <- as.factor(dis$condition)

d <- ggplot(dis, aes(x=condition, y=distance, fill = condition))
d + geom_violin(trim = FALSE) + 
      coord_flip() + 
      scale_fill_brewer(palette="Dark2") + 
      geom_hline(yintercept = 1, color = "blue", size = 0.4) +
      geom_boxplot(width=0.1, fill="bisque") +
      labs(title = "Reach Ability Estimation",
           x = "Condition", y = "Average Ratio") +
      theme_grey() +
      theme(axis.text=element_text(size=18),
            axis.title=element_text(size=19),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold",
                                      margin = margin(10, 0, 10, 0)))

### APA style
set.seed(4796)
distance <- data.frame(Experimental = rnorm(150, mean = 0.98, sd=0.008),
                       Control = rnorm(150, mean=1.00, sd=0.008))
dis <- gather(distance, condition, distance, 1:2)
dis$condition <- as.factor(dis$condition)

d <- ggplot(dis, aes(x=condition, y=distance, fill = condition))
d + geom_violin(trim = FALSE, fill = "white") + 
      coord_flip() +
      geom_hline(yintercept = 1, color = "black", size = 0.7, linetype="dashed") +
      geom_boxplot(width=0.1, fill="white") +
      labs(x = "Condition", y = "Average Ratio") +
      theme_apa(legend.pos = "none") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12))

##Graph2
x <- round(rnorm(150,mean=6.5,sd=1.5))
y <- round(rnorm(150,mean=3.5,sd=1.5))
mor <- subset(data.frame(x,y), 0 < x & x < 10 & 0 < y & y < 10)
mo <- gather(mor, condition, score, 1:2)
mo$condition[mo$condition == "x"] <- "experimental"
mo$condition[mo$condition == "y"] <- "control"

###write.csv(mo, "senior_moralityscore.csv")
###mo <- read.csv("senior_moralityscore.csv")

theme_set(theme_gray(base_size = 20))
qplot(score, data = mo, fill = condition, 
      main = "Morality Judgment", ylab = "Number of responses")

##Graph3

mo$score <- as.integer(mo$score)
mo$condition <- factor(mo$condition, c("experimental", "control"))

mSummary <- mo %>%
      group_by(condition) %>%
      summarize(meanscore = mean(score),
                se = sqrt(var(score)/length(score)))

m <- ggplot(data=mSummary, aes(x=condition, y=meanscore))
m + geom_bar(stat="identity", width = 0.5, aes(fill = condition)) + 
      scale_fill_brewer(palette = "Set2") +
      geom_text(aes(label= round(meanscore,2)), 
                position = position_stack(vjust=0.5), color= "black", size = 6.5) +
      coord_flip() +
      geom_errorbar(aes(ymin=meanscore-se, ymax=meanscore+se), width=0.2) +
      theme(legend.position = "none") + 
      labs(title = "Moral Judgment Rating",
           x = "Condition", y = "Mean rating") +
      theme(axis.text=element_text(size=18),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold",
                                      margin = margin(10, 0, 10, 0)))







library(dplyr)
mdist <- data %>%
      group_by(Condition) %>%
      summarize(meandist = mean(dist),
                se = sqrt(var(dist)/length(dist)))

ggplot(data=mdist, aes(x=Condition, y=meandist), fill = "") +
      geom_bar(stat = "identity", position = "dodge",
               width = 0.5, color = "black") + 
      geom_errorbar(aes(ymin=meandist-se,
                        ymax=meandist+se), width=0.2) +
      theme_apa(legend.pos = "none") + 
      labs(y = "Reachability Ratio")








library(dplyr)
mdist <- data %>%
      group_by(Condition) %>%
      summarize(meandist = mean(dist),
                se = sqrt(var(dist)/length(dist)))

ggplot(data=mdist, aes(x=Condition, y=meandist), fill = "") +
      geom_line() + 
      geom_errorbar(aes(ymin=meandist-se,
                        ymax=meandist+se), width=0.2) +
      theme_apa(legend.pos = "none") + 
      labs(y = "Reachability Ratio")