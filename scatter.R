
# ===================================================
# @neguez - 2024

# Code that accompanies the article : 

XICCARBON <- read.csv("F:/001_BACKUP_WORK/08_STATS/XICOTO/XICCARBON.csv", stringsAsFactors=TRUE)


require(ggplot2) 
require(hrbrthemes)


XICCARBON$Sample = factor(XICCARBON$Sample, levels = c("MW",
                                                       "MB",
                                                       "MR",
                                                       "Control"))

#SCATTER

color1=c("#e08214","#e08214","#e08214","#542788")

ggplot(XICCARBON, aes(x=carbon, y=Cnumber, shape=Sample, color=Sample)) +
  geom_point(size=3.5) +
  theme(legend.position= c(0.9,0.8)) +
  theme(legend.text = element_text(size=14, face="bold"))+
  theme(legend.title = element_text(size=16, face="bold")) +
  theme(axis.text.x=element_text(size=13), axis.title.x = element_text(size=14,face="bold")) +
  theme(axis.text.y=element_text(size=13), axis.title.y = element_text(size=14,face="bold")) +
  scale_y_discrete(limits = c("nC27", "nC29", "nC31", "nC33")) + #posar limits
  scale_shape_manual(name = "Sample", labels = c("MW", "MB", "MR", "Control"), values = c(15,16,17,18)) + # posar formes manualment
  scale_colour_manual(name = "Sample", labels = c("MW", "MB", "MR", "Control"), values = color1) +
  xlab(~ paste(delta ^ 13, "\nC"[n-alkane], " (‰) VPDB")) + #label x axis
  ylab("Carbon number\n")



ggplot(XICCARBON, aes(x=carbon, y=Cnumber, shape=Sample, color=Sample)) +
  geom_point(size=3.5) +
  theme_bw() +
  theme(legend.position= c(0.9,0.8)) +
  theme(legend.text = element_text(size=14, face="bold"))+
  theme(legend.title = element_text(size=16, face="bold")) +
  theme(axis.text.x=element_text(size=13), axis.title.x = element_text(size=14,face="bold")) +
  theme(axis.text.y=element_text(size=13), axis.title.y = element_text(size=14,face="bold")) +
  theme(legend.background = element_rect(fill = "white", size = 0.5, linetype = "solid", color = "black")) +
  scale_y_discrete(limits = c("nC27", "nC29", "nC31", "nC33")) + #posar limits
  scale_shape_manual(name = "Sample", labels = c("MW", "MB", "MR", "Control"), values = c(15,16,17,18)) + # posar formes manualment
  scale_colour_manual(name = "Sample", labels = c("MW", "MB", "MR", "Control"), values = color1) +
  xlab(~ paste(delta ^ 13, "\nC"[n-alkane], " (‰) VPDB")) + #label x axis
  ylab("Carbon number\n")




#BARPLOTS

XICALKANES$Sample = factor(XICALKANES$Sample, levels = c("MW",
                                                       "MB",
                                                       "MR",
                                                       "Control"))

ggplot(XICALKANES, aes(x=alkane, y=grams, fill = Sample)) +
  geom_bar(stat="identity") +
  xlab("carbon number") +
  ylab(~paste(mu, "g/g of dry sample")) +
  theme_bw()


ggplot(XICALKANES, aes(x=alkane, y=grams, fill = Sample)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = color1) +
  xlab("\nCarbon number") +
  ylab(~paste(mu, "g/g of dry sample")) +
  theme_bw() +
  facet_wrap(~Sample,
             scales = "free",
             ncol=1) +
  theme(axis.title.x = element_text(size=14,face="bold")) +
  theme(axis.title.y = element_text(size=14,face="bold")) +
  theme(strip.text = element_text(face = "bold", size = "12"),
        strip.background = element_blank(), 
        legend.position = "none")



ggplot(XICALKANES, aes(x=alkane, y=grams, fill = Sample)) +
  geom_bar(stat = "identity") +
  xlab("carbon number") +
  ylab(~ paste(mu, "g/g of dry sample")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_grid( Sample ~.)




