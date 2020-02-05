####Libraries####
library(easypackages)
libs<-c("haven", "tidyverse")
libraries(libs)
####Read in the Data####
dat <- read_sav("PSCI Methods Requirement Survey 1_December 20, 2019_14.35.sav")
####Cleaning and Analysis####
# Demographics

## Year in program
table(dat$Q20)
prop.table(table(dat$Q20))

## Primary Subfield
table(dat$Q21)
round(prop.table(table(dat$Q21)), 3)

# Who taught stats when you took it?
table(dat$Q1)
round(prop.table(table(dat$Q1)), 3)

# What classes have you taken?
dat$Q2_1
table(dat$Q2_1)

dat$Q2_2
table(dat$Q2_2)

dat$Q2_3
table(dat$Q2_3)

dat$Q2_4
table(dat$Q2_4)

dat$Q2_5
table(dat$Q2_5)

dat$Q2_6
table(dat$Q2_6)

dat$Q2_7
table(dat$Q2_7)

dat$Q2_8
table(dat$Q2_8)

## Get percentages

x<-c(11, 23, 28, 17, 10, 8, 8)
round(x/46, 3)

# Student Goals

## Recode and scale to (0,1) with 1 indicating highest priority
dat<-dat %>%
  mutate(stu.goal1=(11-Q4_1)/10, stu.goal2=(11-Q4_2)/10, stu.goal3=(11-Q4_3)/10, stu.goal4=(11-Q4_4)/10,
         stu.goal5=(11-Q4_5)/10, stu.goal6=(11-Q4_6)/10, stu.goal7=(11-Q4_7)/10, stu.goal8=(11-Q4_8)/10,
         stu.goal9=(11-Q4_9)/10, stu.goal10=(11-Q4_10)/10, stu.goal11=(11-Q4_11)/10)

## Means and SD

index.stu.goals<-grep("stu.goal", colnames(dat))
index.Q4<-grep("Q4_", colnames(dat))[-12]
for(x in 1:11){
  txt<-paste("gsub('Below is a list of goals students may have in mind when pursuing methods training. Please order these goals based on how important they are to you by clicking and dragging. Your most important goal should be at the top of the ranking. - ',
       '', attributes(dat$Q4_", x, ")$label)", sep="")
  cat(c(eval(parse(text=txt)), '\n', '\n'))
  
  cat(c("mean", round(mean(as.matrix(dat[,index.stu.goals[x]]), na.rm=T), 3), "\n", "sd", 
        round(sd(as.matrix(dat[,index.stu.goals[x]]), na.rm=T),3)))
  
  cat(c("\n", "--------", "\n", "\n"))
}

# Department Goals

## Recode and scale to (0,1) with 1 indicating highest priority
dat<-dat %>%
  mutate(dept.goal1=(11-Q6_1)/10, dept.goal2=(11-Q6_2)/10, dept.goal3=(11-Q6_3)/10, dept.goal4=(11-Q6_4)/10,
         dept.goal5=(11-Q6_5)/10, dept.goal6=(11-Q6_6)/10, dept.goal7=(11-Q6_7)/10, dept.goal8=(11-Q6_8)/10,
         dept.goal9=(11-Q6_9)/10, dept.goal10=(11-Q6_10)/10, dept.goal11=(11-Q6_11)/10)

## Means and SD

index.dept.goals<-grep("dept.goal", colnames(dat))
index.Q6<-grep("Q6_", colnames(dat))[-12]
for(x in 1:11){
  txt<-paste("gsub('^.*?-',
       '', attributes(dat$Q6_", x, ")$label)", sep="")
  cat(c(eval(parse(text=txt)), '\n', '\n'))
  
  cat(c("mean", round(mean(as.matrix(dat[,index.dept.goals[x]]), na.rm=T), 3), "\n", "sd", 
        round(sd(as.matrix(dat[,index.dept.goals[x]]), na.rm=T),3)))
  
  cat(c("\n", "--------", "\n", "\n"))
}

# Correlation between own goals and department goals
out.p<-matrix(NA)
out.cor<-matrix(NA)
for(x in 1:11){
  txt<-paste("cor.test(dat$stu.goal", x, ", dat$dept.goal", x, ")", sep="")
  out.p[x]<-eval(parse(text=txt))$p.value
  out.cor[x]<-eval(parse(text=txt))$estimate
}

# What goals were accomplished through classes?
for(x in 1:11){
  txt<-paste("working.vect<-dat$Q9_", x, sep="")
  eval(parse(text=txt))
  working.vect<-ifelse(is.na(working.vect), 0, working.vect)
  txt.2<-paste("gsub('Please consider ALL methods courses you have taken in the PSCI department. With those courses in mind, which (if any) of the following were accomplished? - Selected Choice ','', attributes(dat$Q9_", x, ")$label, fixed=T)", sep="")
  
  cat("\n")
  print(eval(parse(text=txt.2)))
  cat("\n", round(mean(working.vect),3), "\n", "\n", "----------")
  
}

# How helpful were specific classes? Range = (0,1) with 1 indicating very helpful
dat <- dat %>%
  mutate(helpful1=(5-Q19_1)/4, helpful2=(5-Q19_2)/4, helpful3=(5-Q19_3)/4, helpful4=(5-Q19_4)/4,
         helpful5=(5-Q19_5)/4, helpful6=(5-Q19_6)/4, helpful7=(5-Q19_7)/4)

index.helpful<-grep('helpful', colnames(dat))
for(x in 1:7){
  
  txt<-paste("gsub('^.*?- ', '', attributes(dat$Q19_", x, ")$label)", sep="")
  
  cat("\n")
  print(eval(parse(text=txt)))
  cat("\n", round(mean(as.matrix(dat[,index.helpful[x]]), na.rm=T), 3), "\n", "\n", "----------")
}

# Scope and Methods responses--How helpful was the course in developing the following components?
# Range (0,1) with 1=very helpful
dat <- dat %>%
  mutate(sm1=(5-Q10_1)/4, sm2=(5-Q10_2)/4, sm3=(5-Q10_3)/4, sm4=(5-Q10_4)/4,
         sm5=(5-Q10_5)/4, sm6=(5-Q10_6)/4, sm7=(5-Q10_7)/4, sm8=(5-Q10_8)/4)

index.sm<-grep('sm', colnames(dat))
for(x in 1:8){
  
  txt<-paste("gsub('^.*?- ', '', attributes(dat$Q10_", x, ")$label)", sep="")
  
  cat("\n")
  print(eval(parse(text=txt)))
  cat("\n", round(mean(as.matrix(dat[,index.sm[x]]), na.rm=T), 3), "\n", "\n", "----------")
}

# Student Goals by year in program

## Make table
dat.own.goals<- dat %>%
  select(Q20, stu.goal1, stu.goal2, stu.goal3, stu.goal4, stu.goal5, stu.goal6, stu.goal7, stu.goal8, stu.goal9, stu.goal10, stu.goal11) %>%
  group_by(Q20)%>%
  summarize(mean.1=mean(stu.goal1, na.rm=T), mean.2=mean(stu.goal2, na.rm=T), mean.3=mean(stu.goal3, na.rm=T),
            mean.4=mean(stu.goal4, na.rm=T), mean.5=mean(stu.goal5, na.rm=T), mean.6=mean(stu.goal6, na.rm=T),
            mean.7=mean(stu.goal7, na.rm=T), mean.8=mean(stu.goal8, na.rm=T), mean.9=mean(stu.goal9, na.rm=T),
            mean.10=mean(stu.goal10, na.rm=T), mean.11=mean(stu.goal11, na.rm=T))%>%
  pivot_longer(-Q20, names_to = "variable", values_to = "value") %>%
  group_by(variable)

##Make figures

for(x in 1:11){
  a<-x
  b<-x+11
  c<-x+(2*11)
  d<-x+(3*11)
  e<-x+(4*11)
  f<-x+(5*11)
  print(
    ggplot(aes(x=Q20), data=dat.own.goals[c(a,b, c, d, e, f),])+
      geom_bar(aes(y=value), stat='identity')+
      scale_y_continuous()+
      scale_x_discrete(name="Year in Program", limits=c(1:6), breaks=waiver(), labels=waiver())+
      ggtitle(label=paste("Own Goal #", x, sep=""))+
      ylab("Mean")+
      theme_bw()
    )
}

# Department Goals by Year in Program

## Make table
dat.dept.goals<- dat %>%
  select(Q20, dept.goal1, dept.goal2, dept.goal3, dept.goal4, dept.goal5, dept.goal6, dept.goal7, dept.goal8, dept.goal9, dept.goal10, dept.goal11) %>%
  group_by(Q20)%>%
  summarize(mean.1=mean(dept.goal1, na.rm=T), mean.2=mean(dept.goal2, na.rm=T), mean.3=mean(dept.goal3, na.rm=T),
            mean.4=mean(dept.goal4, na.rm=T), mean.5=mean(dept.goal5, na.rm=T), mean.6=mean(dept.goal6, na.rm=T),
            mean.7=mean(dept.goal7, na.rm=T), mean.8=mean(dept.goal8, na.rm=T), mean.9=mean(dept.goal9, na.rm=T),
            mean.10=mean(dept.goal10, na.rm=T), mean.11=mean(dept.goal11, na.rm=T))%>%
  pivot_longer(-Q20, names_to = "variable", values_to = "value") %>%
  group_by(variable)

##Make figures

for(x in 1:11){
  a<-x
  b<-x+11
  c<-x+(2*11)
  d<-x+(3*11)
  e<-x+(4*11)
  f<-x+(5*11)
  print(
    ggplot(aes(x=Q20), data=dat.dept.goals[c(a,b, c, d, e, f),])+
      geom_bar(aes(y=value), stat='identity')+
      scale_y_continuous()+
      scale_x_discrete(name="Year in Program", limits=c(1:6), breaks=waiver(), labels=waiver())+
      ggtitle(label=paste("Dept Goal #", x, sep=""))+
      ylab("Mean")+
      theme_bw()
  )
}
###Figures for GSO Presentation####
#Comparing student and department goals

##make the dataframe

dat.goals.fig<-dat %>%
  select(stu.goal1, stu.goal2, stu.goal3, stu.goal4, stu.goal5, stu.goal6, stu.goal7, stu.goal8, 
         stu.goal9, stu.goal10, stu.goal11, dept.goal1, dept.goal2, dept.goal3, dept.goal4, 
         dept.goal5, dept.goal6, dept.goal7, dept.goal8, dept.goal9, dept.goal10, dept.goal11) %>%
  summarize(dept.m.1=mean(dept.goal1, na.rm=T), dept.m.2=mean(dept.goal2, na.rm=T), dept.m.3=mean(dept.goal3, na.rm=T),
            dept.m.4=mean(dept.goal4, na.rm=T), dept.m.5=mean(dept.goal5, na.rm=T), dept.m.6=mean(dept.goal6, na.rm=T),
            dept.m.7=mean(dept.goal7, na.rm=T), dept.m.8=mean(dept.goal8, na.rm=T), dept.m.9=mean(dept.goal9, na.rm=T),
            dept.m.10=mean(dept.goal10, na.rm=T), dept.m.11=mean(dept.goal11, na.rm=T),
            stu.m.1=mean(stu.goal1, na.rm=T), stu.m.2=mean(stu.goal2, na.rm=T), stu.m.3=mean(stu.goal3, na.rm=T),
            stu.m.4=mean(stu.goal4, na.rm=T), stu.m.5=mean(stu.goal5, na.rm=T), stu.m.6=mean(stu.goal6, na.rm=T),
            stu.m.7=mean(stu.goal7, na.rm=T), stu.m.8=mean(stu.goal8, na.rm=T), stu.m.9=mean(stu.goal9, na.rm=T),
            stu.m.10=mean(stu.goal10, na.rm=T), stu.m.11=mean(stu.goal11, na.rm=T),
            dept.sd.1=sd(dept.goal1, na.rm=T), dept.sd.2=sd(dept.goal2, na.rm=T), dept.sd.3=sd(dept.goal3, na.rm=T),
            dept.sd.4=sd(dept.goal4, na.rm=T), dept.sd.5=sd(dept.goal5, na.rm=T), dept.sd.6=sd(dept.goal6, na.rm=T),
            dept.sd.7=sd(dept.goal7, na.rm=T), dept.sd.8=sd(dept.goal8, na.rm=T), dept.sd.9=sd(dept.goal9, na.rm=T),
            dept.sd.10=sd(dept.goal10, na.rm=T), dept.sd.11=sd(dept.goal11, na.rm=T),
            stu.sd.1=sd(stu.goal1, na.rm=T), stu.sd.2=sd(stu.goal2, na.rm=T), stu.sd.3=sd(stu.goal3, na.rm=T),
            stu.sd.4=sd(stu.goal4, na.rm=T), stu.sd.5=sd(stu.goal5, na.rm=T), stu.sd.6=sd(stu.goal6, na.rm=T),
            stu.sd.7=sd(stu.goal7, na.rm=T), stu.sd.8=sd(stu.goal8, na.rm=T), stu.sd.9=sd(stu.goal9, na.rm=T),
            stu.sd.10=sd(stu.goal10, na.rm=T), stu.sd.11=sd(stu.goal11, na.rm=T)) %>%
  pivot_longer(everything(), names_to = c(".value", "goal"), names_pattern = "(.*\\..*)\\.(.*)") %>%
  pivot_longer(-goal, names_to = c("level", ".value"), names_pattern = "(.*)\\.(.*)")
dat.goals.fig$goal<-factor(na.omit(dat.goals.fig$goal), levels=c(7, 6, 9, 1, 5, 11, 4, 8, 2, 10, 3))

## Make the figure

ggplot(aes(x=goal, fill=level), data=dat.goals.fig)+
  geom_bar(aes(y=m), stat='identity', position='dodge')+
  scale_fill_manual(limits=c("dept", "stu"), values=c("red", "blue"), labels=c("Department", 
                                                                               "Student"),
                    name="Level")+
  theme_bw()+
  xlab("Goal")+
  ylab("Average Importance of Goal")

# Figure of goals Accomplished Through all classes taken

##Make the dataframe

dat.goals.acc<- dat %>%
  select(Q9_1, Q9_2, Q9_3, Q9_4, Q9_5, Q9_6, Q9_7, Q9_8, Q9_9, Q9_10, Q9_11) %>%
  replace_na(list(Q9_1=0, Q9_2=0, Q9_3=0, Q9_4=0, Q9_5=0, 
                  Q9_6=0, Q9_7=0, Q9_8=0, Q9_9=0, Q9_10=0, Q9_11=0)) %>%
  summarize(goal.1=mean(Q9_1, na.rm=T), goal.2=mean(Q9_2, na.rm=T), goal.3=mean(Q9_3, na.rm=T),
            goal.4=mean(Q9_4, na.rm=T), goal.5=mean(Q9_5, na.rm=T), goal.6=mean(Q9_6, na.rm=T),
            goal.7=mean(Q9_7, na.rm=T), goal.8=mean(Q9_8, na.rm=T), goal.9=mean(Q9_9, na.rm=T),
            goal.10=mean(Q9_10, na.rm=T), goal.11=mean(Q9_11, na.rm=T)) %>%
  pivot_longer(everything(), names_to=c(".value", "goals"), names_pattern = "(.*)\\.(.*)")
dat.goals.acc$goals<-factor(na.omit(dat.goals.acc$goals), levels=c(rev(order(dat.goals.acc$goal))))

## Make a figure

ggplot(aes(x=goals), data=dat.goals.acc)+
  geom_bar(aes(y=goal), stat='identity')+
  xlab("Goal")+
  ylab("Percent of Sample")+
  theme_bw()
 
#Figure of goals accomplished by 692 instructor

##Make the dataframe

dat.goals.acc2<- dat %>%
  select(Q1, Q9_1, Q9_2, Q9_3, Q9_4, Q9_5, Q9_6, Q9_7, Q9_8, Q9_9, Q9_10, Q9_11) %>%
  replace_na(list(Q9_1=0, Q9_2=0, Q9_3=0, Q9_4=0, Q9_5=0, 
                  Q9_6=0, Q9_7=0, Q9_8=0, Q9_9=0, Q9_10=0, Q9_11=0)) %>%
  group_by(Q1) %>%
  summarize(goal.1=mean(Q9_1, na.rm=T), goal.2=mean(Q9_2, na.rm=T), goal.3=mean(Q9_3, na.rm=T),
            goal.4=mean(Q9_4, na.rm=T), goal.5=mean(Q9_5, na.rm=T), goal.6=mean(Q9_6, na.rm=T),
            goal.7=mean(Q9_7, na.rm=T), goal.8=mean(Q9_8, na.rm=T), goal.9=mean(Q9_9, na.rm=T),
            goal.10=mean(Q9_10, na.rm=T), goal.11=mean(Q9_11, na.rm=T)) %>%
  pivot_longer(-Q1, names_to=c(".value", "goals"), names_pattern = "(.*)\\.(.*)")
dat.goals.acc2<-na.omit(dat.goals.acc2[dat.goals.acc2$Q1<=2,])
dat.goals.acc2$goals<-factor(na.omit(dat.goals.acc2$goals), levels=c(rev(order(dat.goals.acc2$goal))))
dat.goals.acc2$Q1<-factor(dat.goals.acc2$Q1, levels=c(1, 2))
## Make a figure

ggplot(aes(x=goals, fill=Q1), data=dat.goals.acc2)+
  geom_bar(aes(y=goal), stat='identity', position = 'dodge')+
  xlab("Goal")+
  ylab("Percent of Sample")+
  theme_bw()+
  scale_fill_manual(values = c("red", "blue"), labels=c("Levendusky", "Kronick"), 
                    name= "Instructor")

# Goals met by year in program

dat$old<-ifelse(dat$Q20>=4, 1, 0)

##Make the dataframe

dat.goals.acc3<- dat %>%
  select(old, Q9_1, Q9_2, Q9_3, Q9_4, Q9_5, Q9_6, Q9_7, Q9_8, Q9_9, Q9_10, Q9_11) %>%
  replace_na(list(Q9_1=0, Q9_2=0, Q9_3=0, Q9_4=0, Q9_5=0, 
                  Q9_6=0, Q9_7=0, Q9_8=0, Q9_9=0, Q9_10=0, Q9_11=0)) %>%
  group_by(old) %>%
  summarize(goal.1=mean(Q9_1, na.rm=T), goal.2=mean(Q9_2, na.rm=T), goal.3=mean(Q9_3, na.rm=T),
            goal.4=mean(Q9_4, na.rm=T), goal.5=mean(Q9_5, na.rm=T), goal.6=mean(Q9_6, na.rm=T),
            goal.7=mean(Q9_7, na.rm=T), goal.8=mean(Q9_8, na.rm=T), goal.9=mean(Q9_9, na.rm=T),
            goal.10=mean(Q9_10, na.rm=T), goal.11=mean(Q9_11, na.rm=T)) %>%
  pivot_longer(-old, names_to=c(".value", "goals"), names_pattern = "(.*)\\.(.*)")
dat.goals.acc3<-na.omit(dat.goals.acc3)
dat.goals.acc3$goals<-factor(na.omit(dat.goals.acc3$goals), levels=c(rev(order(dat.goals.acc3$goal))))
dat.goals.acc3$old<-factor(dat.goals.acc3$old, levels=c(0, 1))

## Make a figure

ggplot(aes(x=goals, fill=old), data=dat.goals.acc3)+
  geom_bar(aes(y=goal), stat='identity', position = 'dodge')+
  xlab("Goal")+
  ylab("Percent of Sample")+
  theme_bw()+
  scale_fill_manual(values = c("red", "blue"), labels=c("1-3", "4+"), 
                    name= "Year in Program")

# Figure of goals met by specific classes (only for 692, broken by instructor)

##Get the data
dat.goals.class<-dat %>%
  select(Q1, Q19_3) %>%
  group_by(Q1) %>%
  summarize(mean=mean((5-Q19_3)/4, na.rm=T))

##Make a figure

ggplot(aes(x=as.factor(Q1)), data=dat.goals.class)+
  geom_bar(aes(y=mean), stat='identity')

# Figure of Scope/MEthods Goals

##Get the data

dat.sm<- dat %>%
  select(Q10_1, Q10_2, Q10_3, Q10_4, Q10_5, Q10_6, Q10_7, Q10_8) %>%
  summarize(goal.1=mean((5-Q10_1)/4, na.rm=T), goal.2=mean((5-Q10_2)/4, na.rm=T), goal.3=mean((5-Q10_3)/4, na.rm=T),
            goal.4=mean((5-Q10_4)/4, na.rm=T), goal.5=mean((5-Q10_5)/4, na.rm=T), goal.6=mean((5-Q10_6)/4, na.rm=T),
            goal.7=mean((5-Q10_7)/4, na.rm=T), goal.8=mean((5-Q10_8)/4, na.rm=T)) %>%
  pivot_longer(everything(), names_to=c(".value", "goals"), names_pattern = "(.*)\\.(.*)")
dat.sm$goals<-factor(na.omit(dat.sm$goals), levels=c(rev(order(dat.sm$goal))))

## Make the Figure

ggplot(aes(x=goals), data=dat.sm)+
  geom_bar(aes(y=goal), stat='identity')+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  xlab("Goals")+
  ylab("Helpfulness")

# Order of Goals for above graph
for(x in 1:8){
  txt<-paste("gsub('Please evaluate the following components of the course and how useful they were in developing your dissertation prospectus -',
       '', attributes(dat$Q10_", x, ")$label)", sep="")
  cat(c(eval(parse(text=txt)), '\n', '\n'))}

# Crosstabs of classes by subfield
prop.table(table(dat$Q21, dat$Q2_1))
prop.table(table(dat$Q21, dat$Q2_2))
prop.table(table(dat$Q21, dat$Q2_3))
prop.table(table(dat$Q21, dat$Q2_4))
prop.table(table(dat$Q21, dat$Q2_5))
prop.table(table(dat$Q21, dat$Q2_6))
prop.table(table(dat$Q21, dat$Q2_7))
prop.table(table(dat$Q21, dat$Q2_8))

#####Extract Text Data####
index.text<-grep("_TEXT", colnames(dat))
index.text<-c(index.text, 23, 33, 58, 86, 87, 88, 89)
dat.txt<-dat[,index.text]
write_csv(dat.txt, path="Extracted Text Data.csv")
