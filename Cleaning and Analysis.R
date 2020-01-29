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
  summarize(dept.1=mean(dept.goal1, na.rm=T), dept.2=mean(dept.goal2, na.rm=T), dept.3=mean(dept.goal3, na.rm=T),
            dept.4=mean(dept.goal4, na.rm=T), dept.5=mean(dept.goal5, na.rm=T), dept.6=mean(dept.goal6, na.rm=T),
            dept.7=mean(dept.goal7, na.rm=T), dept.8=mean(dept.goal8, na.rm=T), dept.9=mean(dept.goal9, na.rm=T),
            dept.10=mean(dept.goal10, na.rm=T), dept.11=mean(dept.goal11, na.rm=T),
            stu.1=mean(stu.goal1, na.rm=T), stu.2=mean(stu.goal2, na.rm=T), stu.3=mean(stu.goal3, na.rm=T),
            stu.4=mean(stu.goal4, na.rm=T), stu.5=mean(stu.goal5, na.rm=T), stu.6=mean(stu.goal6, na.rm=T),
            stu.7=mean(stu.goal7, na.rm=T), stu.8=mean(stu.goal8, na.rm=T), stu.9=mean(stu.goal9, na.rm=T),
            stu.10=mean(stu.goal10, na.rm=T), stu.11=mean(stu.goal11, na.rm=T))
  pivot_longer(names_to="level", values_to = "value")









