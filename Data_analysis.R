tbl <- read.table("Desktop/Master/EHR/A2/tbl2.txt")

library(dplyr)
library(stringr)
library(ggplot2)

#Tumor behaviour
tbl2 <- tbl %>% mutate(`TumorBehaviour`= case_when(str_detect(str_to_lower(LONG_TITLE),"benign") ~ "Benign",
                                           str_detect(str_to_lower(LONG_TITLE),"malignant") ~ "Malignant",
                                           str_detect(str_to_lower(LONG_TITLE),"hodgkin") ~ "Malignant",
                                           str_detect(str_to_lower(LONG_TITLE),"hemangioma") ~ "Benign",
                                           str_detect(str_to_lower(LONG_TITLE),"mycosis fungoides") ~ "Malignant",
                                           str_detect(str_to_lower(LONG_TITLE),"myelodysplastic syndrome") ~ "Malignant",
                                           str_detect(str_to_lower(LONG_TITLE),"acute") ~ "Malignant",
                                           str_detect(str_to_lower(LONG_TITLE),"in situ") ~ "Benign",
                                           str_detect(str_to_lower(LONG_TITLE),"uncertain behavior") ~ "Uncertain",
                                           str_detect(str_to_lower(LONG_TITLE),"secondary") ~ "Malignant",
                                           str_detect(str_to_lower(LONG_TITLE),"myeloma") ~ "Malignant",
                                           str_detect(str_to_lower(LONG_TITLE),"multiple sites") ~ "Malignant",
                                           str_detect(str_to_lower(LONG_TITLE),"relapse") ~ "Malignant",
                                           str_detect(str_to_lower(LONG_TITLE),"remission") ~ "Malignant",
                                           TRUE ~ "Uncertain"),
                       `Origin`= case_when(str_detect(str_to_lower(LONG_TITLE),"secondary") ~ "Metastasis",
                                           TRUE ~ "Primary"),
                       `Tissue`=case_when(str_detect(str_to_lower(LONG_TITLE), "\\bbrain|\\bcerebral|\\bfrontal|\\bparietal|\\btemporal|\\occipital") ~ "Brain",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bneur") ~ "Neural system",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bliver|\\bbile\\s*duct|\\bhepat") ~ "Liver",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bpancrea") ~ "Pancreas",
                                          str_detect(str_to_lower(LONG_TITLE), "\\blung|\\bbronch") ~ "Lung",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bprostat") ~ "Prostate",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bskin|\\bcutaneous") ~ "Skin",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bunspecified\\s*site") ~ "Unspecified",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bheart|\\bcardia") ~ "Heart",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bstomach|\\bgastr") ~ "Stomach",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bintestine|\\bcolon") ~ "Colorectal",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bbreast") ~ "Breast",
                                          str_detect(str_to_lower(LONG_TITLE), "bone") ~ "Bone",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bkidney|\\badrenal") ~ "Kidney",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bthyroid|\\bcarcinoid|\\bparagan|\\bendocr") ~ "Endocrine",
                                          str_detect(str_to_lower(LONG_TITLE), "\\bovary|\\bfallopian|\\buterus") ~ "Ovary",
                                          TRUE ~ "Other"))
colnames(tbl2)

library(ggplot2)
ggplot(tbl2, aes(x=Origin, fill=origin)) + geom_bar()
ggplot(tbl2, aes(x=TumorBehaviour, fill=factor(DECEASED))) + geom_bar()

ggplot(tbl2, aes(x=TumorBehaviour, fill=TumorBehaviour)) + geom_bar()
ggplot(tbl2, aes(x=GENDER, fill=GENDER)) + geom_bar()
ggplot(tbl2, aes(x=Tissue, fill=factor(DECEASED))) + geom_bar()
ggplot(tbl2, aes(x=Tissue, fill=factor(GENDER))) + geom_bar()
ggplot(tbl2 %>% group_by(SUBJECT_ID) %>% summarise(DECEASED=max(DECEASED)), aes(x=factor(DECEASED), fill=factor(DECEASED))) + geom_bar()
ggplot(tbl2 %>% group_by(SUBJECT_ID) %>% summarise(AGE=min(AGE)), aes(x=AGE)) + geom_histogram()
ggplot(tbl2, aes(x=LOS)) + geom_histogram()


View(tbl2 %>% filter(TumorBehaviour == "Malignant") %>% group_by(HADM_ID) %>% summarise(n = n(), cond=paste0(Origin, collapse = ", ")))



#filtering
unique_id <- tbl2 %>% filter(TumorBehaviour == "Malignant") %>% group_by(HADM_ID) %>% 
       summarise(p=sum(Origin == "Primary"), m=sum(Origin == "Metastasis"))

meta_id <- unique_id$SUBJECT_ID[which(unique_id$m>0)]

unique_sID <- tbl2[match(unique(tbl2$SUBJECT_ID), tbl2$SUBJECT_ID),]

unique_sID$Origin[which(unique_sID$SUBJECT_ID %in% meta_id)] <- "Metastasis"

tbl2<-tbl2[which(tbl2$Tissue!="Other"),]
tbl2<-tbl2[which(tbl2$AGE<100),]
tbl2<-tbl2[which(tbl2$AGE>0),]
tbl2<-tbl2[which(tbl2$TumorBehaviour!="Uncertain"),]


# Simple analysis ##############################################################
## General Behavior Mal vs Ben
#ggplot(data = tbl2,aes(x = TumorBehaviour)) + geom_bar()
tbl2$Deceased <- as.factor(tbl2$DECEASED)
ggplot(data = tbl2, aes(x = Tissue, fill = Deceased)) + 
  geom_bar(position="dodge")+ 
  facet_grid(cols = vars(tbl2$TumorBehaviour)) +
  scale_fill_manual(values=palette("Set1")) +
  theme_classic() + ylab("Number of patients")+
  theme(text = element_text(size = 15),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

## Age distribution of dead and alive for metastasis and primary
ggplot(data = tbl2, aes(x=AGE, y=Origin, fill = Deceased)) + 
  geom_density_ridges2() + 
  theme_classic() +
  scale_fill_manual(values=palette("Set1")) +
  theme(text = element_text(size = 15))

## Tissue by gender
ggplot(data = tbl2, aes(x = Tissue, fill = GENDER)) + geom_bar(position = "dodge") + 
  scale_fill_manual(values=c("magenta","yellow")) +
  theme_classic() +
  theme(text = element_text(size = 15),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
## Metastasis vs Deceased
ggplot(data = tbl2, aes(x = Origin, fill = Deceased)) +
  theme_classic() +
  geom_bar()


#LOS vs Deceased comparing cancer types
ggplot(data = tbl2, aes(y = Tissue, x = as.numeric(LOS), fill = Deceased)) +
  theme_classic() +
  geom_density_ridges2() +
  scale_fill_manual(values=palette("Set1")) +
  theme(text = element_text(size = 15),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  xlab("Length of stay") 


#comorbidities
comorbidities <- read.table("Desktop/Master/EHR/A2/tblComor.txt")
adm <- tbl2 %>% filter(TumorBehaviour == "Malignant") %>% pull(HADM_ID)
adm_1 <- tbl2 %>% filter(TumorBehaviour == "Malignant" & DECEASED == 1) %>% pull(HADM_ID)
adm_0 <- tbl2 %>% filter(TumorBehaviour == "Malignant"& DECEASED == 0) %>% pull(HADM_ID)

x <- comorbidities %>% filter(HADM_ID %in% adm) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
x1 <- comorbidities %>% filter(HADM_ID %in% adm_0) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
x2 <- comorbidities %>% filter(HADM_ID %in% adm_1) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)

adm_met <- tbl2 %>% filter(TumorBehaviour == "Malignant" & Origin == "Metastasis") %>% pull(HADM_ID)
adm_1_met <- tbl2 %>% filter(TumorBehaviour == "Malignant" & DECEASED == 1 & Origin == "Metastasis") %>% pull(HADM_ID)
adm_0_met <- tbl2 %>% filter(TumorBehaviour == "Malignant"& DECEASED == 0 & Origin == "Metastasis") %>% pull(HADM_ID)
comorbidities %>% filter(HADM_ID %in% adm_met) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
comorbidities %>% filter(HADM_ID %in% adm_0_met) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
comorbidities %>% filter(HADM_ID %in% adm_1_met) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)



adm_pri <- tbl2 %>% filter(TumorBehaviour == "Malignant" & Origin == "Primary") %>% pull(HADM_ID)
adm_1_pri <- tbl2 %>% filter(TumorBehaviour == "Malignant") %>% group_by(HADM_ID) %>% 
  summarise(p=sum(Origin == "Primary"), m=sum(Origin == "Metastasis"))

adm_0_pri <- tbl2 %>% filter(TumorBehaviour == "Malignant"& DECEASED == 0 & Origin == "Primary") %>% pull(HADM_ID)
comorbidities %>% filter(HADM_ID %in% adm_pri) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
comorbidities %>% filter(HADM_ID %in% adm_0_pri) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
comorbidities %>% filter(HADM_ID %in% adm_1_pri) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)

tbl2 %>% pull(SUBJECT_ID) %>% unique() %>% length()


plot_com <- function(x, x1, x2, n1, n2) {
  com <- x[1:10,]$LONG_TITLE
  com <- str_wrap(com, width = 45)
  x1$LONG_TITLE <- str_wrap(x1$LONG_TITLE, width = 45)
  x1 <- x1[x1$LONG_TITLE %in% com,]
  x1$LONG_TITLE <- factor(x1$LONG_TITLE, levels = rev(com))
  
  x2$LONG_TITLE <- str_wrap(x2$LONG_TITLE, width = 45)
  x2 <- x2[x2$LONG_TITLE %in% com,]
  x2$LONG_TITLE <- factor(x2$LONG_TITLE, levels = rev(com))
  
  x1 <- x1 %>% arrange(LONG_TITLE) %>% mutate(diff=abs(x1 %>% arrange(LONG_TITLE) %>% pull(n) - x2 %>% arrange(LONG_TITLE) %>% pull(n)))
  x2 <- x2 %>% arrange(LONG_TITLE) %>% mutate(diff=abs(x1 %>% arrange(LONG_TITLE) %>% pull(n) - x2 %>% arrange(LONG_TITLE) %>% pull(n)))
  
  library(grid)
  g.mid<-ggplot(x1,aes(x=1,y=LONG_TITLE))+geom_text(aes(label=LONG_TITLE))+
    ggtitle("Most frequent comorbidities")+
    ylab(NULL)+
    scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
    theme(axis.title=element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          axis.text.x=element_text(color=NA),
          axis.ticks.x=element_line(color=NA),
          plot.margin = unit(c(1,-1,1,-1), "mm"))
  
  g1 <- ggplot(data = x1, aes(x = LONG_TITLE, y = n, fill=diff)) + 
    geom_bar(stat = "identity") + ggtitle(paste0("Alive (n=", n1, ")")) + theme_classic() + labs(y="Percentage of patients")+
    theme( axis.title.x=element_text(size=8),
           axis.title.y = element_blank(), plot.title = element_text(hjust=0.5, size = 10),
           axis.text.y = element_blank(), legend.position = "none",
           axis.ticks.y = element_blank(), axis.line.y = element_blank(),
           plot.margin = unit(c(1,-1,1,0), "mm")) + 
    scale_y_reverse(limits=c(100,0)) + coord_flip()  
  g2 <- ggplot(data = x2, aes(x = LONG_TITLE, y = n, fill=diff)) +xlab(NULL)+ 
    geom_bar(stat = "identity") + ggtitle(paste0("Deceased (n=", n2, ")")) + theme_classic()+
    theme(axis.title.x=element_text(size=8),axis.title.y = element_blank(), plot.title = element_text(hjust=0.5, size = 10),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),  axis.line.y = element_blank(), legend.position = "none",
          plot.margin = unit(c(1,0,1,-1), "mm")) + labs(y="Percentage of patients") +
    coord_flip() + ylim(c(0,100))
  library(gridExtra) 
  gg1 <- ggplot_gtable(ggplot_build(g1)) 
  gg2 <- ggplot_gtable(ggplot_build(g2)) 
  gg.mid <- ggplot_gtable(ggplot_build(g.mid)) 
  
  grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(2/9,5/9,2/9))
  
  return(x1)
}

adm <- tbl2 %>% filter(TumorBehaviour == "Malignant") %>% pull(HADM_ID)
adm_1 <- tbl2 %>% filter(TumorBehaviour == "Malignant" & DECEASED == 1) %>% pull(HADM_ID)
adm_0 <- tbl2 %>% filter(TumorBehaviour == "Malignant"& DECEASED == 0) %>% pull(HADM_ID)
x <- comorbidities %>% filter(HADM_ID %in% adm) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
x1 <- comorbidities %>% filter(HADM_ID %in% adm_0) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
x1$n <- x1$n / comorbidities %>% filter(HADM_ID %in% adm_0) %>% pull(SUBJECT_ID) %>% unique() %>% length() * 100
x2 <- comorbidities %>% filter(HADM_ID %in% adm_1) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
x2$n <- x2$n / comorbidities %>% filter(HADM_ID %in% adm_1) %>% pull(SUBJECT_ID) %>% unique() %>% length() * 100
n1 <- comorbidities %>% filter(HADM_ID %in% adm_0) %>% pull(SUBJECT_ID) %>% unique() %>% length()
n2 <- comorbidities %>% filter(HADM_ID %in% adm_1) %>% pull(SUBJECT_ID) %>% unique() %>% length()
#all
plot_com(x, x1, x2, n1, n2)

#metastasis
adm_met <- tbl2 %>% filter(TumorBehaviour == "Malignant" & Origin == "Metastasis") %>% pull(HADM_ID)
adm_1_met <- tbl2 %>% filter(TumorBehaviour == "Malignant" & DECEASED == 1 & Origin == "Metastasis") %>% pull(HADM_ID)
adm_0_met <- tbl2 %>% filter(TumorBehaviour == "Malignant"& DECEASED == 0 & Origin == "Metastasis") %>% pull(HADM_ID)
x_1 <- comorbidities %>% filter(HADM_ID %in% adm_met) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
x1_1 <- comorbidities %>% filter(HADM_ID %in% adm_0_met) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
x1_1$n <- x1_1$n / comorbidities %>% filter(HADM_ID %in% adm_0_met) %>% pull(SUBJECT_ID) %>% unique() %>% length() * 100
x2_1 <- comorbidities %>% filter(HADM_ID %in% adm_1_met) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
x2_1$n <- x2_1$n / comorbidities %>% filter(HADM_ID %in% adm_1_met) %>% pull(SUBJECT_ID) %>% unique() %>% length() * 100
n1 <- comorbidities %>% filter(HADM_ID %in% adm_0_met) %>% pull(SUBJECT_ID) %>% unique() %>% length()
n2 <- comorbidities %>% filter(HADM_ID %in% adm_1_met) %>% pull(SUBJECT_ID) %>% unique() %>% length()

#all
plot_com(x_1, x1_1, x2_1, n1, n2)

adm_1_pri <- tbl2 %>% filter(TumorBehaviour == "Malignant") %>% group_by(HADM_ID) %>% 
  summarise(p=sum(Origin == "Primary"), m=sum(Origin == "Metastasis")) %>% filter(m==0) %>% pull(HADM_ID)


adm_pri <- tbl2 %>% filter(TumorBehaviour == "Malignant") %>% group_by(HADM_ID) %>% 
  summarise(p=sum(Origin == "Primary"), m=sum(Origin == "Metastasis")) %>% filter(m==0) %>% pull(HADM_ID)
adm_1_pri <- tbl2 %>% filter(TumorBehaviour == "Malignant" & DECEASED==1) %>% group_by(HADM_ID) %>% 
  summarise(p=sum(Origin == "Primary"), m=sum(Origin == "Metastasis")) %>% filter(m==0) %>% pull(HADM_ID)
adm_0_pri <- tbl2 %>% filter(TumorBehaviour == "Malignant" & DECEASED==0) %>% group_by(HADM_ID) %>% 
  summarise(p=sum(Origin == "Primary"), m=sum(Origin == "Metastasis")) %>% filter(m==0) %>% pull(HADM_ID)
x_2 <- comorbidities %>% filter(HADM_ID %in% adm_pri) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
x1_2 <- comorbidities %>% filter(HADM_ID %in% adm_0_pri) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
x1_2$n <- x1_2$n / comorbidities %>% filter(HADM_ID %in% adm_0_pri) %>% pull(SUBJECT_ID) %>% unique() %>% length() * 100
x2_2 <- comorbidities %>% filter(HADM_ID %in% adm_1_pri) %>% group_by(LONG_TITLE) %>% summarise(n=n_distinct(SUBJECT_ID)) %>% arrange(-n)
x2_2$n <- x2_2$n / comorbidities %>% filter(HADM_ID %in% adm_1_pri) %>% pull(SUBJECT_ID) %>% unique() %>% length() * 100

n1 <- comorbidities %>% filter(HADM_ID %in% adm_0_pri) %>% pull(SUBJECT_ID) %>% unique() %>% length()
n2 <- comorbidities %>% filter(HADM_ID %in% adm_1_pri) %>% pull(SUBJECT_ID) %>% unique() %>% length()

#all
plot_com(x_2, x1_2, x2_2, n1, n2)


tbl2 %>% filter(TumorBehaviour == "Malignant") %>% group_by(SUBJECT_ID) %>% 
  summarise(p=sum(Origin == "Primary"), m=sum(Origin == "Metastasis")) %>% filter(m==0) %>% p

df <- rbind(x1 %>% slice_max(n, n=10) %>% mutate(patients="All", status="ALIVE"),
      x2 %>% slice_max(n, n=10) %>% mutate(patients="All", status="DECEASED"),
      x1_1 %>% slice_max(n, n=10) %>% mutate(patients="Metastasis", status="ALIVE"),
      x2_1 %>% slice_max(n, n=10) %>% mutate(patients="Metastasis", status="DECEASED"),
      x1_2 %>% slice_max(n, n=10) %>% mutate(patients="Primary", status="ALIVE"),
      x2_2 %>% slice_max(n, n=10) %>% mutate(patients="Primary", status="DECEASED"))
x$n <- x$n / comorbidities %>% filter(HADM_ID %in% adm) %>% pull(SUBJECT_ID) %>% unique() %>% length() * 100
x_1$n <- x_1$n / comorbidities %>% filter(HADM_ID %in% adm_met) %>% pull(SUBJECT_ID) %>% unique() %>% length() * 100
x_2$n <- x_2$n / comorbidities %>% filter(HADM_ID %in% adm_pri) %>% pull(SUBJECT_ID) %>% unique() %>% length() * 100

df <- rbind(x %>% slice_max(n, n=10) %>% mutate(id="All"),
            x_1 %>% slice_max(n, n=10) %>% mutate(id="Metastasis"),
            x_2 %>% slice_max(n, n=10) %>% mutate(id="Primary"))
df$LONG_TITLE <- str_trunc(df$LONG_TITLE, 50)
library(tidytext)
p <- ggplot(df, aes(y=reorder_within(LONG_TITLE, n, id, ), x=n, fill=LONG_TITLE)) + geom_bar(stat = "identity")+
  facet_wrap(~id, ncol = 1,scales = "free_y") + scale_y_reordered() + 
  theme_classic() + theme(legend.position = "none") + labs(x = "Percentage of patients", y ="") + 
  scale_fill_manual(values = c("#0075DC", "#993F00", "#4C005C", "#191919", "#005C31", "#2BCE48", "#FFCC99", "#808080", "#94FFB5", "#8F7C00", "#9DCC00", "#C20088", "#003380",
                               "#FFA405", "#FFA8BB", "#426600", "#FF0010", "#5EF1F2", "#00998F"))

p

library(VennDiagram)

# Generate 3 sets of 200 words
set1 <- x %>% slice_max(n, n=25) %>% pull(LONG_TITLE)
set2 <- x_1 %>% slice_max(n, n=25) %>% pull(LONG_TITLE)
set3 <- x_2 %>% slice_max(n, n=25) %>% pull(LONG_TITLE)

set_2[!set_2 %in% set_3]
set_3[!set_3 %in% set_2]

# Chart
venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("All" , "Metastasis " , "Primary"),
  filename = '14_venn_diagramm.png',
  output=F
)

plot_com(x, x1, x2, n1, n2)

plot_com(x_1, x1_1, x2_1, n1, n2)

plot_com(x_2, x1_2, x2_2, n1, n2)



#### Mortality rate
alive <- tbl2 %>% filter(TumorBehaviour=="Malignant" & DECEASED==0) %>% pull(SUBJECT_ID) %>% unique() 
deceased <- tbl2 %>% filter(TumorBehaviour=="Malignant" & DECEASED==1) %>% pull(SUBJECT_ID) %>% unique() 

df1 <- tbl2 %>% filter(TumorBehaviour=="Malignant") %>% group_by(LONG_TITLE) %>% summarise(N=n_distinct(SUBJECT_ID),
                                                                                    D=sum(unique(SUBJECT_ID) %in% deceased),
                                                                                    Origin=unique(Origin)) %>% 
  mutate(rate = D/N, LONG_TITLE = str_trunc(LONG_TITLE, 50))
library(ggrepel)
libr
p <- ggplot(df1 , aes(N, rate, col=Origin)) + geom_point() +
  geom_text_repel(data = df1 %>% filter(N>600 | N>200 & rate <0.5), aes(N, rate, label=LONG_TITLE), inherit.aes = F ,box.padding = 1) +
  theme_classic() + labs(x="Number of patients", y="Mortality rate", col="Tumor origin") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=15), legend.position = "left",
        legend.text = element_text(size=12))
ggExtra::ggMarginal(p, type = "histogram", groupFill =  T)  
