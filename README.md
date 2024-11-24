# California High School Swimmers: the Path to College Swimming

**Lucia Zhang**

## Data Introduction

We gathered the 50 freestyle times for 635 California high schoolers from 2014 to 2022 as well as their college times from 2015 to 2023. Here are the variables included in the data `chsfree50.csv`:

-   `Rank`: Swimmer's rank in the event;
-   `Student`: Swimmer's ID;
-   `Time`: Time of 50 freestyle;
-   `Relay.Extracted`: Relay type if the time was obtained from relay;
-   `Season`: Season with 0 for senior year of high school;
-   `Division`: Division of the swimmer with 0 indicating high school and others for college divisions;
-   `Gender`: Swimmer's gender;
-   `CYear`: Year the swimmer entered to college;
-   `CDivision`: College division the swimmer eventually entered to.

``` r
#=============================================================
# Overall Information on Swimmers
ccfree50 <- read.csv("chsfree50.csv")

myse <- function(x) {sd(x)/sqrt(length(x))}

# Division I:
summary(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==1)])
myse(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==1)])

summary(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==1)])
myse(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==1)])

summary(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==1)])
myse(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==1)])

summary(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==1)])
myse(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==1)])


# Division II:
summary(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==2)])
myse(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==2)])

summary(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==2)])
myse(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==2)])

summary(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==2)])
myse(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==2)])

summary(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==2)])
myse(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==2)])

# Division III:
summary(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==3)])
myse(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==3)])

summary(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==3)])
myse(ccfree50$Time[(ccfree50$Season<=0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==3)])

summary(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==3)])
myse(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Female")&(ccfree50$CDivision==3)])

summary(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==3)])
myse(ccfree50$Time[(ccfree50$Season>0)&(ccfree50$Gender=="Male")&(ccfree50$CDivision==3)])


# Plot all trajectories of all swimmers
require(ggplot2)
ccfree50$Time <- as.numeric(ccfree50$Time)
ccfree50$GD <- paste(ccfree50$Gender, ccfree50$CDivision)
ggplot(ccfree50, aes(x=Season,y=Time,group=Student,color=GD))+
  geom_line() +
  labs(x = "Grade (0=High School Senior)", y="Swimming Time") +
  theme_minimal()

# Summarize data:
snames <- unique(ccfree50$Student)
swimmer.list <- NULL
for (j in 1:length(snames)) {
  tmp <- ccfree50[ccfree50$Student==snames[j],]
  tmp <- tmp[tmp$Division>0,]
  swimmer.list <- rbind(swimmer.list,tmp[1,])
}

sum((swimmer.list$Gender=="Male")&(swimmer.list$Division==1))
sum((swimmer.list$Gender=="Male")&(swimmer.list$Division==2))
sum((swimmer.list$Gender=="Male")&(swimmer.list$Division==3))

sum((swimmer.list$Gender=="Female")&(swimmer.list$Division==1))
sum((swimmer.list$Gender=="Female")&(swimmer.list$Division==2))
sum((swimmer.list$Gender=="Female")&(swimmer.list$Division==3))

sum(swimmer.list$Gender=="Male")
sum(swimmer.list$Gender=="Female")

# Information on Primary Data (California HS Swimmers from 2014-2022):
# College         Female       Male         Total
# Division I      134          126          260
# Division II     81           50           131
# Division III    131          113          244
# Total           346          289          635

# Box Plot of All Swimmers Over Years in High School and College
ccfree50$Time <- as.numeric(ccfree50$Time)
boxplot(ccfree50$Time~ccfree50$Season,xlab="Grade (0=HS Senior)",ylab="Time")
boxplot(log(ccfree50$Time)~ccfree50$Season,xlab="Grade (0=HS Senior)",ylab="log(Time)")
```

## Studying the overall trend of 50 free times

``` r
# 1. Division I & Female
d1f <- ccfree50[(ccfree50$CDivision==1)&(ccfree50$Gender=="Female"),]
boxplot(d1f$Time~d1f$Season,xlab="Grade (0=HS Senior)",ylab="Time")

# Get median values of each grade (HS & college)
library(dplyr)
d1fm <- d1f %>%
  group_by(Season) %>%
  summarize(Time = median(Time, na.rm = TRUE))
#plot(d1fm$Season, d1fm$Time,type="l")

# 2. Division II & Female
d2f <- ccfree50[(ccfree50$CDivision==2)&(ccfree50$Gender=="Female"),]
boxplot(d2f$Time~d2f$Season,xlab="Grade (0=HS Senior)",ylab="Time")

d2fm <- d2f %>%
  group_by(Season) %>%
  summarize(Time = median(Time, na.rm = TRUE))

# 3. Division III & Female
d3f <- ccfree50[(ccfree50$CDivision==3)&(ccfree50$Gender=="Female"),]
boxplot(d3f$Time~d3f$Season,xlab="Grade (0=HS Senior)",ylab="Time")

d3fm <- d3f %>%
  group_by(Season) %>%
  summarize(Time = median(Time, na.rm = TRUE))

# 4. Division I & Male
d1m <- ccfree50[(ccfree50$CDivision==1)&(ccfree50$Gender=="Male"),]
boxplot(d1m$Time~d1m$Season,xlab="Grade (0=HS Senior)",ylab="Time")

d1mm <- d1m %>%
  group_by(Season) %>%
  summarize(Time = median(Time, na.rm = TRUE))

# 5. Division II & Male
d2m <- ccfree50[(ccfree50$CDivision==2)&(ccfree50$Gender=="Male"),]
boxplot(d2m$Time~d2m$Season,xlab="Grade (0=HS Senior)",ylab="Time")

d2mm <- d2m %>%
  group_by(Season) %>%
  summarize(Time = median(Time, na.rm = TRUE))

# 6. Division III & Male
d3m <- ccfree50[(ccfree50$CDivision==3)&(ccfree50$Gender=="Male"),]
boxplot(d3m$Time~d3m$Season,xlab="Grade (0=HS Senior)",ylab="Time")

d3mm <- d3m %>%
  group_by(Season) %>%
  summarize(Time = median(Time, na.rm = TRUE))

tmin<-min(d1fm$Time,d2fm$Time,d3fm$Time,d1mm$Time,d2mm$Time,d3mm$Time)
tmax<-max(d1fm$Time,d2fm$Time,d3fm$Time,d1mm$Time,d2mm$Time,d3mm$Time)

# Plot all six trends together
wtrends <- rbind(data.frame(d1fm,group=rep("D1F",9)),
                 data.frame(d2fm,group=rep("D2F",9)),
                 data.frame(d3fm,group=rep("D3F",9)),
                 data.frame(d1mm,group=rep("D1M",9)),
                 data.frame(d2mm,group=rep("D2M",9)),
                 data.frame(d3mm,group=rep("D3M",9)))

library(ggplot2)
ggplot(wtrends, aes(x=Season,y=Time,group=group,color=group))+
  geom_line() +
  labs(x = "Grade(0=HS Senior)", y="Time") +
  theme_minimal()

# ANOVA at each Grade: Cross-Sectional Study
ccfree50$CDivision <- as.factor(ccfree50$CDivision)
op <- options(contrasts = c("contr.helmert", "contr.poly"))

# Females:

# 1. HS Year 1
hs1f <- ccfree50[(ccfree50$Season==-3)&(ccfree50$Gender=="Female"),]
nrow(hs1f)  # 104
hs1f.kruskal <- kruskal.test(Time~CDivision,data=hs1f)
hs1f.kruskal  # 3.081e-13

#I vs. II
hs1f12.kruskal <- kruskal.test(Time~CDivision,data=hs1f[hs1f$CDivision!=3,])
hs1f12.kruskal  # 5.223e-10

#I vs. III
hs1f13.kruskal <- kruskal.test(Time~CDivision,data=hs1f[hs1f$CDivision!=2,])
hs1f13.kruskal  # 3.711e-10

#II vs. III
hs1f23.kruskal <- kruskal.test(Time~CDivision,data=hs1f[hs1f$CDivision!=1,])
hs1f23.kruskal  # 0.5383

# 2. HS Year 2
hs2f <- ccfree50[(ccfree50$Season==-2)&(ccfree50$Gender=="Female"),]
nrow(hs2f)  # 147
hs2f.kruskal <- kruskal.test(Time~CDivision,data=hs2f)
hs2f.kruskal  # 1.639e-13

#I vs. II
hs2f12.kruskal <- kruskal.test(Time~CDivision,data=hs2f[hs2f$CDivision!=3,])
hs2f12.kruskal  # 4.929e-08

#I vs. III
hs2f13.kruskal <- kruskal.test(Time~CDivision,data=hs2f[hs2f$CDivision!=2,])
hs2f13.kruskal  # 2.769e-12

#II vs. III
hs2f23.kruskal <- kruskal.test(Time~CDivision,data=hs2f[hs2f$CDivision!=1,])
hs2f23.kruskal  # 0.7657

# 3. HS Year 3
hs3f <- ccfree50[(ccfree50$Season==-1)&(ccfree50$Gender=="Female"),]
nrow(hs3f)  # 185
hs3f.kruskal <- kruskal.test(Time~CDivision,data=hs3f)
hs3f.kruskal  # <2.2e-16

#I vs. II
hs3f12.kruskal <- kruskal.test(Time~CDivision,data=hs3f[hs3f$CDivision!=3,])
hs3f12.kruskal  # 6.206e-16

#I vs. III
hs3f13.kruskal <- kruskal.test(Time~CDivision,data=hs3f[hs3f$CDivision!=2,])
hs3f13.kruskal  # < 2.2e-16

#II vs. III
hs3f23.kruskal <- kruskal.test(Time~CDivision,data=hs3f[hs3f$CDivision!=1,])
hs3f23.kruskal  # 0.7199

# 4. HS Year 4
hs4f <- ccfree50[(ccfree50$Season==0)&(ccfree50$Gender=="Female"),]
nrow(hs4f)  # 192
hs4f.kruskal <- kruskal.test(Time~CDivision,data=hs4f)
hs4f.kruskal  # <2.2e-16

#I vs. II
hs4f12.kruskal <- kruskal.test(Time~CDivision,data=hs4f[hs4f$CDivision!=3,])
hs4f12.kruskal  # 7.989e-15

#I vs. III
hs4f13.kruskal <- kruskal.test(Time~CDivision,data=hs4f[hs4f$CDivision!=2,])
hs4f13.kruskal  # < 2.2e-16

#II vs. III
hs4f23.kruskal <- kruskal.test(Time~CDivision,data=hs4f[hs4f$CDivision!=1,])
hs4f23.kruskal  # 0.7415

# 5. College Year 1
col1f <- ccfree50[(ccfree50$Season==1)&(ccfree50$Gender=="Female"),]
nrow(col1f)  # 346
col1f.kruskal <- kruskal.test(Time~CDivision,data=col1f)
col1f.kruskal  # <2.2e-16

#I vs. II
col1f12.kruskal <- kruskal.test(Time~CDivision,data=col1f[col1f$CDivision!=3,])
col1f12.kruskal  # < 2.2e-16

#I vs. III
col1f13.kruskal <- kruskal.test(Time~CDivision,data=col1f[col1f$CDivision!=2,])
col1f13.kruskal  # < 2.2e-16

#II vs. III
col1f23.kruskal <- kruskal.test(Time~CDivision,data=col1f[col1f$CDivision!=1,])
col1f23.kruskal  # 0.01408

# 6. College Year 2
col2f <- ccfree50[(ccfree50$Season==2)&(ccfree50$Gender=="Female"),]
nrow(col2f)  # 182
col2f.kruskal <- kruskal.test(Time~CDivision,data=col2f)
col2f.kruskal  # <2.2e-16

#I vs. II
col2f12.kruskal <- kruskal.test(Time~CDivision,data=col2f[col2f$CDivision!=3,])
col2f12.kruskal  # < 2.2e-16

#I vs. III
col2f13.kruskal <- kruskal.test(Time~CDivision,data=col2f[col2f$CDivision!=2,])
col2f13.kruskal  # < 2.2e-16

#II vs. III
col2f23.kruskal <- kruskal.test(Time~CDivision,data=col2f[col2f$CDivision!=1,])
col2f23.kruskal  # 0.8379

# 7. College Year 3
col3f <- ccfree50[(ccfree50$Season==3)&(ccfree50$Gender=="Female"),]
nrow(col3f)  # 128
col3f.kruskal <- kruskal.test(Time~CDivision,data=col3f)
col3f.kruskal  # <2.2e-16

#I vs. II
col3f12.kruskal <- kruskal.test(Time~CDivision,data=col3f[col3f$CDivision!=3,])
col3f12.kruskal  # 4.317e-12

#I vs. III
col3f13.kruskal <- kruskal.test(Time~CDivision,data=col3f[col3f$CDivision!=2,])
col3f13.kruskal  # < 2.2e-16

#II vs. III
col3f23.kruskal <- kruskal.test(Time~CDivision,data=col3f[col3f$CDivision!=1,])
col3f23.kruskal  # 0.9744

# 8. College Year 4
col4f <- ccfree50[(ccfree50$Season==4)&(ccfree50$Gender=="Female"),]
nrow(col4f)  # 82
col4f.kruskal <- kruskal.test(Time~CDivision,data=col4f)
col4f.kruskal  # 1.485e-13

#I vs. II
col4f12.kruskal <- kruskal.test(Time~CDivision,data=col4f[col4f$CDivision!=3,])
col4f12.kruskal  # 2.295e-08

#I vs. III
col4f13.kruskal <- kruskal.test(Time~CDivision,data=col4f[col4f$CDivision!=2,])
col4f13.kruskal  # 2.463e-11

#II vs. III
col4f23.kruskal <- kruskal.test(Time~CDivision,data=col4f[col4f$CDivision!=1,])
col4f23.kruskal  # 0.7881

# 9. College Year 5
col5f <- ccfree50[(ccfree50$Season==5)&(ccfree50$Gender=="Female"),]
nrow(col5f)  # 12
col5f.kruskal <- kruskal.test(Time~CDivision,data=col5f)
col5f.kruskal  # 0.01321 

#I vs. II
col5f12.kruskal <- kruskal.test(Time~CDivision,data=col5f[col5f$CDivision!=3,])
col5f12.kruskal  # 0.0167

#I vs. III
col5f13.kruskal <- kruskal.test(Time~CDivision,data=col5f[col5f$CDivision!=2,])
col5f13.kruskal  # 0.04042

#II vs. III
col5f23.kruskal <- kruskal.test(Time~CDivision,data=col5f[col5f$CDivision!=1,])
col5f23.kruskal  # 0.08326

# Males:

# 1. HS Year 1
hs1m <- ccfree50[(ccfree50$Season==-3)&(ccfree50$Gender=="Male"),]
nrow(hs1m)  # 55
hs1m.kruskal <- kruskal.test(Time~CDivision,data=hs1m)
hs1m.kruskal  # 0.007731

#I vs. II
hs1m12.kruskal <- kruskal.test(Time~CDivision,data=hs1m[hs1m$CDivision!=3,])
hs1m12.kruskal  # 0.4773

#I vs. III
hs1m13.kruskal <- kruskal.test(Time~CDivision,data=hs1m[hs1m$CDivision!=2,])
hs1m13.kruskal  # 0.001537

#II vs. III
hs1m23.kruskal <- kruskal.test(Time~CDivision,data=hs1m[hs1m$CDivision!=1,])
hs1m23.kruskal  # 0.2131

# 2. HS Year 2
hs2m <- ccfree50[(ccfree50$Season==-2)&(ccfree50$Gender=="Male"),]
nrow(hs2m)  # 107
hs2m.kruskal <- kruskal.test(Time~CDivision,data=hs2m)
hs2m.kruskal  # 8.624e-07

#I vs. II
hs2m12.kruskal <- kruskal.test(Time~CDivision,data=hs2m[hs2m$CDivision!=3,])
hs2m12.kruskal  # 0.002967

#I vs. III
hs2m13.kruskal <- kruskal.test(Time~CDivision,data=hs2m[hs2m$CDivision!=2,])
hs2m13.kruskal  # 3.604e-07

#II vs. III
hs2m23.kruskal <- kruskal.test(Time~CDivision,data=hs2m[hs2m$CDivision!=1,])
hs2m23.kruskal  # 0.5047

# 3. HS Year 3
hs3m <- ccfree50[(ccfree50$Season==-1)&(ccfree50$Gender=="Male"),]
nrow(hs3m)  # 164
hs3m.kruskal <- kruskal.test(Time~CDivision,data=hs3m)
hs3m.kruskal  # 7.735e-16

#I vs. II
hs3m12.kruskal <- kruskal.test(Time~CDivision,data=hs3m[hs3m$CDivision!=3,])
hs3m12.kruskal  # 1.615e-06

#I vs. III
hs3m13.kruskal <- kruskal.test(Time~CDivision,data=hs3m[hs3m$CDivision!=2,])
hs3m13.kruskal  # 5.763e-15

#II vs. III
hs3m23.kruskal <- kruskal.test(Time~CDivision,data=hs3m[hs3m$CDivision!=1,])
hs3m23.kruskal  # 0.003728

# 4. HS Year 4
hs4m <- ccfree50[(ccfree50$Season==0)&(ccfree50$Gender=="Male"),]
nrow(hs4m)  # 171
hs4m.kruskal <- kruskal.test(Time~CDivision,data=hs4m)
hs4m.kruskal  # <2.2e-16

#I vs. II
hs4m12.kruskal <- kruskal.test(Time~CDivision,data=hs4m[hs4m$CDivision!=3,])
hs4m12.kruskal  # 4.445e-09

#I vs. III
hs4m13.kruskal <- kruskal.test(Time~CDivision,data=hs4m[hs4m$CDivision!=2,])
hs4m13.kruskal  # 2.421e-15

#II vs. III
hs4m23.kruskal <- kruskal.test(Time~CDivision,data=hs4m[hs4m$CDivision!=1,])
hs4m23.kruskal  # 0.694

# 5. College Year 1
col1m <- ccfree50[(ccfree50$Season==1)&(ccfree50$Gender=="Male"),]
nrow(col1m)  # 289
col1m.kruskal <- kruskal.test(Time~CDivision,data=col1m)
col1m.kruskal  # <2.2e-16

#I vs. II
col1m12.kruskal <- kruskal.test(Time~CDivision,data=col1m[col1m$CDivision!=3,])
col1m12.kruskal  # < 2.2e-16

#I vs. III
col1m13.kruskal <- kruskal.test(Time~CDivision,data=col1m[col1m$CDivision!=2,])
col1m13.kruskal  # < 2.2e-16

#II vs. III
col1m23.kruskal <- kruskal.test(Time~CDivision,data=col1m[col1m$CDivision!=1,])
col1m23.kruskal  # 0.7597

# 6. College Year 2
col2m <- ccfree50[(ccfree50$Season==2)&(ccfree50$Gender=="Male"),]
nrow(col2m)  # 169
col2m.kruskal <- kruskal.test(Time~CDivision,data=col2m)
col2m.kruskal  # <2.2e-16

#I vs. II
col2m12.kruskal <- kruskal.test(Time~CDivision,data=col2m[col2m$CDivision!=3,])
col2m12.kruskal  # 1.013e-13

#I vs. III
col2m13.kruskal <- kruskal.test(Time~CDivision,data=col2m[col2m$CDivision!=2,])
col2m13.kruskal  # < 2.2e-16

#II vs. III
col2m23.kruskal <- kruskal.test(Time~CDivision,data=col2m[col2m$CDivision!=1,])
col2m23.kruskal  # 0.7411

# 7. College Year 3
col3m <- ccfree50[(ccfree50$Season==3)&(ccfree50$Gender=="Male"),]
nrow(col3m)  # 117
col3m.kruskal <- kruskal.test(Time~CDivision,data=col3m)
col3m.kruskal  # <2.2e-16

#I vs. II
col3m12.kruskal <- kruskal.test(Time~CDivision,data=col3m[col3m$CDivision!=3,])
col3m12.kruskal  # 1.044e-09

#I vs. III
col3m13.kruskal <- kruskal.test(Time~CDivision,data=col3m[col3m$CDivision!=2,])
col3m13.kruskal  # 7.193e-15

#II vs. III
col3m23.kruskal <- kruskal.test(Time~CDivision,data=col3m[col3m$CDivision!=1,])
col3m23.kruskal  # 0.4296

# 8. College Year 4
col4m <- ccfree50[(ccfree50$Season==4)&(ccfree50$Gender=="Male"),]
nrow(col4m)  # 75
col4m.kruskal <- kruskal.test(Time~CDivision,data=col4m)
col4m.kruskal  # 4.755e-11

#I vs. II
col4m12.kruskal <- kruskal.test(Time~CDivision,data=col4m[col4m$CDivision!=3,])
col4m12.kruskal  # 3.743e-07

#I vs. III
col4m13.kruskal <- kruskal.test(Time~CDivision,data=col4m[col4m$CDivision!=2,])
col4m13.kruskal  # 8.413e-09

#II vs. III
col4m23.kruskal <- kruskal.test(Time~CDivision,data=col4m[col4m$CDivision!=1,])
col4m23.kruskal  # 0.8835

# 9. College Year 5
col5m <- ccfree50[(ccfree50$Season==5)&(ccfree50$Gender=="Male"),]
nrow(col5m)  # 12
col5m.kruskal <- kruskal.test(Time~CDivision,data=col5m)
col5m.kruskal  # 0.02491 

#I vs. II
col5m12.kruskal <- kruskal.test(Time~CDivision,data=col5m[col5m$CDivision!=3,])
col5m12.kruskal  # 0.03671

#I vs. III
col5m13.kruskal <- kruskal.test(Time~CDivision,data=col5m[col5m$CDivision!=2,])
col5m13.kruskal  # 0.03671

#II vs. III
col5m23.kruskal <- kruskal.test(Time~CDivision,data=col5m[col5m$CDivision!=1,])
col5m23.kruskal  # 1
```

## Comparison Between Grades: A Longitudinal Study

``` r
# Study time change in each year:

library(dplyr)
library(tidyr)

# Sort data by swimmer ID and year
ccfree50t <- ccfree50 %>% 
  arrange(Student, Season)

ccfree50t <- ccfree50t %>%
  complete(Student, Season)

# Group data by swimmer ID and calculate time difference
ccfree50l <- ccfree50t %>%
  group_by(Student) %>%
  mutate(DTime = Time-lag(Time)) %>%
  ungroup()

# Filter out rows with DTime=NA:
ccfree50l <- ccfree50l[!is.na(ccfree50l$DTime),]

# Longitudinal plot of time change for different divisions

# 1. Division I & Female
d1f <- ccfree50l[(ccfree50l$CDivision==1)&(ccfree50l$Gender=="Female"),]
boxplot(d1f$DTime~d1f$Season,xlab="Grade (0=HS Senior)",ylab="Time Difference")

# Get median values of each grade (HS & college)
library(dplyr)
d1fm <- d1f %>%
  group_by(Season) %>%
  summarize(DTime = median(DTime, na.rm = TRUE))

# 2. Division II & Female
d2f <- ccfree50l[(ccfree50l$CDivision==2)&(ccfree50l$Gender=="Female"),]
boxplot(d2f$DTime~d2f$Season,xlab="Grade (0=HS Senior)",ylab="Time Difference")

d2fm <- d2f %>%
  group_by(Season) %>%
  summarize(DTime = median(DTime, na.rm = TRUE))

# 3. Division III & Female
d3f <- ccfree50l[(ccfree50l$CDivision==3)&(ccfree50l$Gender=="Female"),]
boxplot(d3f$DTime~d3f$Season,xlab="Grade (0=HS Senior)",ylab="Time Difference")

d3fm <- d3f %>%
  group_by(Season) %>%
  summarize(DTime = median(DTime, na.rm = TRUE))

# 4. Division I & Male
d1m <- ccfree50l[(ccfree50l$CDivision==1)&(ccfree50l$Gender=="Male"),]
boxplot(d1m$DTime~d1m$Season,xlab="Grade (0=HS Senior)",ylab="Time Difference")

d1mm <- d1m %>%
  group_by(Season) %>%
  summarize(DTime = median(DTime, na.rm = TRUE))

# 5. Division II & Male
d2m <- ccfree50l[(ccfree50l$CDivision==2)&(ccfree50l$Gender=="Male"),]
boxplot(d2m$DTime~d2m$Season,xlab="Grade (0=HS Senior)",ylab="Time Difference")

d2mm <- d2m %>%
  group_by(Season) %>%
  summarize(DTime = median(DTime, na.rm = TRUE))

# 6. Division III & Male
d3m <- ccfree50l[(ccfree50l$CDivision==3)&(ccfree50l$Gender=="Male"),]
boxplot(d3m$DTime~d3m$Season,xlab="Grade (0=HS Senior)",ylab="Time Difference")

d3mm <- d3m %>%
  group_by(Season) %>%
  summarize(DTime = median(DTime, na.rm = TRUE))

tmin<-min(d1fm$DTime,d2fm$DTime,d3fm$DTime,d1mm$DTime,d2mm$DTime,d3mm$DTime)
tmax<-max(d1fm$DTime,d2fm$DTime,d3fm$DTime,d1mm$DTime,d2mm$DTime,d3mm$DTime)

d3mm[8,] <- d2mm[8,]
d3mm$DTime[8] <- NA

# Plot all six trends together
dtrends <- rbind(data.frame(d1fm,group=rep("D1F",8)),
                 data.frame(d2fm,group=rep("D2F",8)),
                 data.frame(d3fm,group=rep("D3F",8)),
                 data.frame(d1mm,group=rep("D1M",8)),
                 data.frame(d2mm,group=rep("D2M",8)),
                 data.frame(d3mm,group=rep("D3M",8)))

library(ggplot2)
ggplot(dtrends, aes(x=Season,y=DTime,group=group,color=group))+
  geom_line() +
  labs(x = "Grade(0=HS Senior)", y="Time Difference") +
  theme_minimal()

# Test on these changes:

ccfree50l$CDivision <- as.factor(ccfree50l$CDivision)
op <- options(contrasts = c("contr.helmert", "contr.poly"))

# Females:

# 2. HS Year 2
hs2f <- ccfree50l[(ccfree50l$Season==-2)&(ccfree50l$Gender=="Female"),]
nrow(hs2f)  # 53

median(hs2f$DTime[hs2f$CDivision==1]) #-0.36
median(hs2f$DTime[hs2f$CDivision==2]) #-0.65
median(hs2f$DTime[hs2f$CDivision==3]) #-0.58

wilcox.test(hs2f$DTime[hs2f$CDivision==1],mu=0,alternative="less") #2.112e-05
wilcox.test(hs2f$DTime[hs2f$CDivision==2],mu=0,alternative="less") #0.03711
wilcox.test(hs2f$DTime[hs2f$CDivision==3],mu=0,alternative="less") #0.0009766

hs2f.kruskal <- kruskal.test(DTime~CDivision,data=hs2f)
hs2f.kruskal  # 0.2068

#I vs. II
hs2f12.kruskal <- kruskal.test(DTime~CDivision,data=hs2f[hs2f$CDivision!=3,])
hs2f12.kruskal  # 0.6543
median(hs2f$DTime[hs2f$CDivision==1])-median(hs2f$DTime[hs2f$CDivision==2]) #0.29

#I vs. III
hs2f13.kruskal <- kruskal.test(DTime~CDivision,data=hs2f[hs2f$CDivision!=2,])
hs2f13.kruskal  # 0.05155
median(hs2f$DTime[hs2f$CDivision==1])-median(hs2f$DTime[hs2f$CDivision==3]) #0.22

#II vs. III
hs2f23.kruskal <- kruskal.test(DTime~CDivision,data=hs2f[hs2f$CDivision!=1,])
hs2f23.kruskal  # 0.8065
median(hs2f$DTime[hs2f$CDivision==2])-median(hs2f$DTime[hs2f$CDivision==3]) #-0.07

# 3. HS Year 3
hs3f <- ccfree50l[(ccfree50l$Season==-1)&(ccfree50l$Gender=="Female"),]
nrow(hs3f)  # 80

median(hs3f$DTime[hs3f$CDivision==1]) #-0.285
median(hs3f$DTime[hs3f$CDivision==2]) #-0.2
median(hs3f$DTime[hs3f$CDivision==3]) #-0.11

wilcox.test(hs3f$DTime[hs3f$CDivision==1],mu=0,alternative="less") #0.0002262
wilcox.test(hs3f$DTime[hs3f$CDivision==2],mu=0,alternative="less") #0.1381
wilcox.test(hs3f$DTime[hs3f$CDivision==3],mu=0,alternative="less") #0.07454

hs3f.kruskal <- kruskal.test(DTime~CDivision,data=hs3f)
hs3f.kruskal  # 0.2365

#I vs. II
hs3f12.kruskal <- kruskal.test(DTime~CDivision,data=hs3f[hs3f$CDivision!=3,])
hs3f12.kruskal  # 0.8322
median(hs3f$DTime[hs3f$CDivision==1])-median(hs3f$DTime[hs3f$CDivision==2]) #-0.085

#I vs. III
hs3f13.kruskal <- kruskal.test(DTime~CDivision,data=hs3f[hs3f$CDivision!=2,])
hs3f13.kruskal  # 0.0964
median(hs3f$DTime[hs3f$CDivision==1])-median(hs3f$DTime[hs3f$CDivision==3]) #-0.175

#II vs. III
hs3f23.kruskal <- kruskal.test(DTime~CDivision,data=hs3f[hs3f$CDivision!=1,])
hs3f23.kruskal  # 0.6702
median(hs3f$DTime[hs3f$CDivision==2])-median(hs3f$DTime[hs3f$CDivision==3]) #-0.09

# 4. HS Year 4
hs4f <- ccfree50l[(ccfree50l$Season==0)&(ccfree50l$Gender=="Female"),]
nrow(hs4f)  # 111

median(hs4f$DTime[hs4f$CDivision==1]) #-0.125
median(hs4f$DTime[hs4f$CDivision==2]) #-0.06
median(hs4f$DTime[hs4f$CDivision==3]) #-0.2

wilcox.test(hs4f$DTime[hs4f$CDivision==1],mu=0,alternative="less") #0.01109
wilcox.test(hs4f$DTime[hs4f$CDivision==2],mu=0,alternative="less") #0.607
wilcox.test(hs4f$DTime[hs4f$CDivision==3],mu=0,alternative="less") #0.006691

hs4f.kruskal <- kruskal.test(DTime~CDivision,data=hs4f)
hs4f.kruskal  # 0.09782

#I vs. II
hs4f12.kruskal <- kruskal.test(DTime~CDivision,data=hs4f[hs4f$CDivision!=3,])
hs4f12.kruskal  # 0.1027
median(hs4f$DTime[hs4f$CDivision==1])-median(hs4f$DTime[hs4f$CDivision==2]) #-0.065

#I vs. III
hs4f13.kruskal <- kruskal.test(DTime~CDivision,data=hs4f[hs4f$CDivision!=2,])
hs4f13.kruskal  # 0.3796
median(hs4f$DTime[hs4f$CDivision==1])-median(hs4f$DTime[hs4f$CDivision==3]) #0.075

#II vs. III
hs4f23.kruskal <- kruskal.test(DTime~CDivision,data=hs4f[hs4f$CDivision!=1,])
hs4f23.kruskal  # 0.04165
median(hs4f$DTime[hs4f$CDivision==2])-median(hs4f$DTime[hs4f$CDivision==3]) #0.14

# 5. College Year 1
col1f <- ccfree50l[(ccfree50l$Season==1)&(ccfree50l$Gender=="Female"),]
nrow(col1f)  # 192

median(col1f$DTime[col1f$CDivision==1]) #-0.355
median(col1f$DTime[col1f$CDivision==2]) #-0.05
median(col1f$DTime[col1f$CDivision==3]) #-0.16

wilcox.test(col1f$DTime[col1f$CDivision==1],mu=0,alternative="less") #2.286e-10
wilcox.test(col1f$DTime[col1f$CDivision==2],mu=0,alternative="less") #0.1542
wilcox.test(col1f$DTime[col1f$CDivision==3],mu=0,alternative="less") #4.087e-05

col1f.kruskal <- kruskal.test(DTime~CDivision,data=col1f)
col1f.kruskal  # 0.02817

#I vs. II
col1f12.kruskal <- kruskal.test(DTime~CDivision,data=col1f[col1f$CDivision!=3,])
col1f12.kruskal  # 0.01049
median(col1f$DTime[col1f$CDivision==1])-median(col1f$DTime[col1f$CDivision==2]) #-0.305

#I vs. III
col1f13.kruskal <- kruskal.test(DTime~CDivision,data=col1f[col1f$CDivision!=2,])
col1f13.kruskal  # 0.1623
median(col1f$DTime[col1f$CDivision==1])-median(col1f$DTime[col1f$CDivision==3]) #-0.195

#II vs. III
col1f23.kruskal <- kruskal.test(DTime~CDivision,data=col1f[col1f$CDivision!=1,])
col1f23.kruskal  # 0.1069
median(col1f$DTime[col1f$CDivision==2])-median(col1f$DTime[col1f$CDivision==3]) #0.11

# 6. College Year 2
col2f <- ccfree50l[(ccfree50l$Season==2)&(ccfree50l$Gender=="Female"),]
nrow(col2f)  # 182

median(col2f$DTime[col2f$CDivision==1]) #-0.1
median(col2f$DTime[col2f$CDivision==2]) #-0.12
median(col2f$DTime[col2f$CDivision==3]) #0.105

wilcox.test(col2f$DTime[col2f$CDivision==1],mu=0,alternative="less") #0.01488
wilcox.test(col2f$DTime[col2f$CDivision==2],mu=0,alternative="less") #0.06963
wilcox.test(col2f$DTime[col2f$CDivision==3],mu=0,alternative="less") #0.9594

col2f.kruskal <- kruskal.test(DTime~CDivision,data=col2f)
col2f.kruskal  # 0.009619

#I vs. II
col2f12.kruskal <- kruskal.test(DTime~CDivision,data=col2f[col2f$CDivision!=3,])
col2f12.kruskal  # 0.4603
median(col2f$DTime[col2f$CDivision==1])-median(col2f$DTime[col2f$CDivision==2]) #0.02

#I vs. III
col2f13.kruskal <- kruskal.test(DTime~CDivision,data=col2f[col2f$CDivision!=2,])
col2f13.kruskal  # 0.005248
median(col2f$DTime[col2f$CDivision==1])-median(col2f$DTime[col2f$CDivision==3]) #-0.205

#II vs. III
col2f23.kruskal <- kruskal.test(DTime~CDivision,data=col2f[col2f$CDivision!=1,])
col2f23.kruskal  # 0.02594
median(col2f$DTime[col2f$CDivision==2])-median(col2f$DTime[col2f$CDivision==3]) #-0.225

# 7. College Year 3
col3f <- ccfree50l[(ccfree50l$Season==3)&(ccfree50l$Gender=="Female"),]
nrow(col3f)  # 109

median(col3f$DTime[col3f$CDivision==1]) #-0.08
median(col3f$DTime[col3f$CDivision==2]) #0.055
median(col3f$DTime[col3f$CDivision==3]) #-0.02

wilcox.test(col3f$DTime[col3f$CDivision==1],mu=0,alternative="less") #0.1101
wilcox.test(col3f$DTime[col3f$CDivision==2],mu=0,alternative="less") #0.7095
wilcox.test(col3f$DTime[col3f$CDivision==3],mu=0,alternative="less") #0.2076

col3f.kruskal <- kruskal.test(DTime~CDivision,data=col3f)
col3f.kruskal  # 0.5253

#I vs. II
col3f12.kruskal <- kruskal.test(DTime~CDivision,data=col3f[col3f$CDivision!=3,])
col3f12.kruskal  # 0.2765
median(col3f$DTime[col3f$CDivision==1])-median(col3f$DTime[col3f$CDivision==2]) #-0.135

#I vs. III
col3f13.kruskal <- kruskal.test(DTime~CDivision,data=col3f[col3f$CDivision!=2,])
col3f13.kruskal  # 0.8133
median(col3f$DTime[col3f$CDivision==1])-median(col3f$DTime[col3f$CDivision==3]) #-0.06

#II vs. III
col3f23.kruskal <- kruskal.test(DTime~CDivision,data=col3f[col3f$CDivision!=1,])
col3f23.kruskal  # 0.3591
median(col3f$DTime[col3f$CDivision==2])-median(col3f$DTime[col3f$CDivision==3]) #0.075

# 8. College Year 4
col4f <- ccfree50l[(ccfree50l$Season==4)&(ccfree50l$Gender=="Female"),]
nrow(col4f)  # 70

median(col4f$DTime[col4f$CDivision==1]) #0.055
median(col4f$DTime[col4f$CDivision==2]) #0.135
median(col4f$DTime[col4f$CDivision==3]) #-0.265

wilcox.test(col4f$DTime[col4f$CDivision==1],mu=0,alternative="less") #0.7244
wilcox.test(col4f$DTime[col4f$CDivision==2],mu=0,alternative="less") #0.9031
wilcox.test(col4f$DTime[col4f$CDivision==3],mu=0,alternative="less") #0.004593

col4f.kruskal <- kruskal.test(DTime~CDivision,data=col4f)
col4f.kruskal  # 0.004745

#I vs. II
col4f12.kruskal <- kruskal.test(DTime~CDivision,data=col4f[col4f$CDivision!=3,])
col4f12.kruskal  # 0.312
median(col4f$DTime[col4f$CDivision==1])-median(col4f$DTime[col4f$CDivision==2]) #-0.08

#I vs. III
col4f13.kruskal <- kruskal.test(DTime~CDivision,data=col4f[col4f$CDivision!=2,])
col4f13.kruskal  # 0.00513
median(col4f$DTime[col4f$CDivision==1])-median(col4f$DTime[col4f$CDivision==3]) #0.32

#II vs. III
col4f23.kruskal <- kruskal.test(DTime~CDivision,data=col4f[col4f$CDivision!=1,])
col4f23.kruskal  # 0.004927
median(col4f$DTime[col4f$CDivision==2])-median(col4f$DTime[col4f$CDivision==3]) #0.4

# 9. College Year 5
col5f <- ccfree50l[(ccfree50l$Season==5)&(ccfree50l$Gender=="Female"),]
nrow(col5f)  # 10

median(col5f$DTime[col5f$CDivision==1]) #0.03
median(col5f$DTime[col5f$CDivision==2]) #-0.09
median(col5f$DTime[col5f$CDivision==3]) #-0.1

wilcox.test(col5f$DTime[col5f$CDivision==1],mu=0,alternative="less") #0.5781
wilcox.test(col5f$DTime[col5f$CDivision==2],mu=0,alternative="less") #0.375
wilcox.test(col5f$DTime[col5f$CDivision==3],mu=0,alternative="less") #0.5

col5f.kruskal <- kruskal.test(DTime~CDivision,data=col5f)
col5f.kruskal  # 0.8491

#I vs. II
col5f12.kruskal <- kruskal.test(DTime~CDivision,data=col5f[col5f$CDivision!=3,])
col5f12.kruskal  # 0.7963
median(col5f$DTime[col5f$CDivision==1])-median(col5f$DTime[col5f$CDivision==2]) #0.12

#I vs. III
col5f13.kruskal <- kruskal.test(DTime~CDivision,data=col5f[col5f$CDivision!=2,])
col5f13.kruskal  # 0.6171
median(col5f$DTime[col5f$CDivision==1])-median(col5f$DTime[col5f$CDivision==3]) #0.13

#II vs. III
col5f23.kruskal <- kruskal.test(DTime~CDivision,data=col5f[col5f$CDivision!=1,])
col5f23.kruskal  # 0.6547
median(col5f$DTime[col5f$CDivision==2])-median(col5f$DTime[col5f$CDivision==3]) #0.01

# Males:

# 2. HS Year 2
hs2m <- ccfree50l[(ccfree50l$Season==-2)&(ccfree50l$Gender=="Male"),]
nrow(hs2m)  # 25

median(hs2m$DTime[hs2m$CDivision==1]) #-0.69
median(hs2m$DTime[hs2m$CDivision==2]) #-0.21
median(hs2m$DTime[hs2m$CDivision==3]) #-0.44

wilcox.test(hs2m$DTime[hs2m$CDivision==1],mu=0,alternative="less") #0.0003052
wilcox.test(hs2m$DTime[hs2m$CDivision==2],mu=0,alternative="less") #0.3125
wilcox.test(hs2m$DTime[hs2m$CDivision==3],mu=0,alternative="less") #0.02344

hs2m.kruskal <- kruskal.test(DTime~CDivision,data=hs2m)
hs2m.kruskal  # 0.1245

#I vs. II
hs2m12.kruskal <- kruskal.test(DTime~CDivision,data=hs2m[hs2m$CDivision!=3,])
hs2m12.kruskal  # 0.08928
median(hs2m$DTime[hs2m$CDivision==1])-median(hs2m$DTime[hs2m$CDivision==2]) #-0.48

#I vs. III
hs2m13.kruskal <- kruskal.test(DTime~CDivision,data=hs2m[hs2m$CDivision!=2,])
hs2m13.kruskal  # 0.1563
median(hs2m$DTime[hs2m$CDivision==1])-median(hs2m$DTime[hs2m$CDivision==3]) #-0.25

#II vs. III
hs2m23.kruskal <- kruskal.test(DTime~CDivision,data=hs2m[hs2m$CDivision!=1,])
hs2m23.kruskal  # 0.3447
median(hs2m$DTime[hs2m$CDivision==2])-median(hs2m$DTime[hs2m$CDivision==3]) #0.23

# 3. HS Year 3
hs3m <- ccfree50l[(ccfree50l$Season==-1)&(ccfree50l$Gender=="Male"),]
nrow(hs3m)  # 64

median(hs3m$DTime[hs3m$CDivision==1]) #-0.6
median(hs3m$DTime[hs3m$CDivision==2]) #-0.44
median(hs3m$DTime[hs3m$CDivision==3]) #-0.585

wilcox.test(hs3m$DTime[hs3m$CDivision==1],mu=0,alternative="less") #5.861e-06
wilcox.test(hs3m$DTime[hs3m$CDivision==2],mu=0,alternative="less") #0.01367
wilcox.test(hs3m$DTime[hs3m$CDivision==3],mu=0,alternative="less") #0.001887

hs3m.kruskal <- kruskal.test(DTime~CDivision,data=hs3m)
hs3m.kruskal  # 0.7298

#I vs. II
hs3m12.kruskal <- kruskal.test(DTime~CDivision,data=hs3m[hs3m$CDivision!=3,])
hs3m12.kruskal  # 0.4464
median(hs3m$DTime[hs3m$CDivision==1])-median(hs3m$DTime[hs3m$CDivision==2]) #-0.16

#I vs. III
hs3m13.kruskal <- kruskal.test(DTime~CDivision,data=hs3m[hs3m$CDivision!=2,])
hs3m13.kruskal  # 0.6799
median(hs3m$DTime[hs3m$CDivision==1])-median(hs3m$DTime[hs3m$CDivision==3]) #-0.015

#II vs. III
hs3m23.kruskal <- kruskal.test(DTime~CDivision,data=hs3m[hs3m$CDivision!=1,])
hs3m23.kruskal  # 0.6996
median(hs3m$DTime[hs3m$CDivision==2])-median(hs3m$DTime[hs3m$CDivision==3]) #0.145

# 4. HS Year 4
hs4m <- ccfree50l[(ccfree50l$Season==0)&(ccfree50l$Gender=="Male"),]
nrow(hs4m)  # 92

median(hs4m$DTime[hs4m$CDivision==1]) #-0.24
median(hs4m$DTime[hs4m$CDivision==2]) #-0.375
median(hs4m$DTime[hs4m$CDivision==3]) #-0.38

wilcox.test(hs4m$DTime[hs4m$CDivision==1],mu=0,alternative="less") #6.894e-06
wilcox.test(hs4m$DTime[hs4m$CDivision==2],mu=0,alternative="less") #0.09641
wilcox.test(hs4m$DTime[hs4m$CDivision==3],mu=0,alternative="less") #0.0002636

hs4m.kruskal <- kruskal.test(DTime~CDivision,data=hs4m)
hs4m.kruskal  # 0.6352

#I vs. II
hs4m12.kruskal <- kruskal.test(DTime~CDivision,data=hs4m[hs4m$CDivision!=3,])
hs4m12.kruskal  # 0.8646
median(hs4m$DTime[hs4m$CDivision==1])-median(hs4m$DTime[hs4m$CDivision==2]) #0.135

#I vs. III
hs4m13.kruskal <- kruskal.test(DTime~CDivision,data=hs4m[hs4m$CDivision!=2,])
hs4m13.kruskal  # 0.4227
median(hs4m$DTime[hs4m$CDivision==1])-median(hs4m$DTime[hs4m$CDivision==3]) #0.14

#II vs. III
hs4m23.kruskal <- kruskal.test(DTime~CDivision,data=hs4m[hs4m$CDivision!=1,])
hs4m23.kruskal  # 0.3879
median(hs4m$DTime[hs4m$CDivision==2])-median(hs4m$DTime[hs4m$CDivision==3]) #0.005

# 5. College Year 1
col1m <- ccfree50l[(ccfree50l$Season==1)&(ccfree50l$Gender=="Male"),]
nrow(col1m)  # 171

median(col1m$DTime[col1m$CDivision==1]) #-0.19
median(col1m$DTime[col1m$CDivision==2]) #-0.28
median(col1m$DTime[col1m$CDivision==3]) #-0.21

wilcox.test(col1m$DTime[col1m$CDivision==1],mu=0,alternative="less") #3.16e-07
wilcox.test(col1m$DTime[col1m$CDivision==2],mu=0,alternative="less") #0.01063
wilcox.test(col1m$DTime[col1m$CDivision==3],mu=0,alternative="less") #0.0004626

col1m.kruskal <- kruskal.test(DTime~CDivision,data=col1m)
col1m.kruskal  # 0.78

#I vs. II
col1m12.kruskal <- kruskal.test(DTime~CDivision,data=col1m[col1m$CDivision!=3,])
col1m12.kruskal  # 0.5155
median(col1m$DTime[col1m$CDivision==1])-median(col1m$DTime[col1m$CDivision==2]) #0.09

#I vs. III
col1m13.kruskal <- kruskal.test(DTime~CDivision,data=col1m[col1m$CDivision!=2,])
col1m13.kruskal  # 0.6167
median(col1m$DTime[col1m$CDivision==1])-median(col1m$DTime[col1m$CDivision==3]) #0.02

#II vs. III
col1m23.kruskal <- kruskal.test(DTime~CDivision,data=col1m[col1m$CDivision!=1,])
col1m23.kruskal  # 0.8098
median(col1m$DTime[col1m$CDivision==2])-median(col1m$DTime[col1m$CDivision==3]) #-0.07

# 6. College Year 2
col2m <- ccfree50l[(ccfree50l$Season==2)&(ccfree50l$Gender=="Male"),]
nrow(col2m)  # 169

median(col2m$DTime[col2m$CDivision==1]) #-0.18
median(col2m$DTime[col2m$CDivision==2]) #-0.13
median(col2m$DTime[col2m$CDivision==3]) #-0.14

wilcox.test(col2m$DTime[col2m$CDivision==1],mu=0,alternative="less") #5.5e-07
wilcox.test(col2m$DTime[col2m$CDivision==2],mu=0,alternative="less") #0.09931
wilcox.test(col2m$DTime[col2m$CDivision==3],mu=0,alternative="less") #0.001681

col2m.kruskal <- kruskal.test(DTime~CDivision,data=col2m)
col2m.kruskal  # 0.785

#I vs. II
col2m12.kruskal <- kruskal.test(DTime~CDivision,data=col2m[col2m$CDivision!=3,])
col2m12.kruskal  # 0.5049
median(col2m$DTime[col2m$CDivision==1])-median(col2m$DTime[col2m$CDivision==2]) #-0.05

#I vs. III
col2m13.kruskal <- kruskal.test(DTime~CDivision,data=col2m[col2m$CDivision!=2,])
col2m13.kruskal  # 0.6713
median(col2m$DTime[col2m$CDivision==1])-median(col2m$DTime[col2m$CDivision==3]) #-0.04

#II vs. III
col2m23.kruskal <- kruskal.test(DTime~CDivision,data=col2m[col2m$CDivision!=1,])
col2m23.kruskal  # 0.773
median(col2m$DTime[col2m$CDivision==2])-median(col2m$DTime[col2m$CDivision==3]) #0.01

# 7. College Year 3
col3m <- ccfree50l[(ccfree50l$Season==3)&(ccfree50l$Gender=="Male"),]
nrow(col3m)  # 98

median(col3m$DTime[col3m$CDivision==1]) #-0.09
median(col3m$DTime[col3m$CDivision==2]) #0.02
median(col3m$DTime[col3m$CDivision==3]) #-0.065

wilcox.test(col3m$DTime[col3m$CDivision==1],mu=0,alternative="less") #0.009915
wilcox.test(col3m$DTime[col3m$CDivision==2],mu=0,alternative="less") #0.6316
wilcox.test(col3m$DTime[col3m$CDivision==3],mu=0,alternative="less") #0.4959

col3m.kruskal <- kruskal.test(DTime~CDivision,data=col3m)
col3m.kruskal  # 0.3136

#I vs. II
col3m12.kruskal <- kruskal.test(DTime~CDivision,data=col3m[col3m$CDivision!=3,])
col3m12.kruskal  # 0.1858
median(col3m$DTime[col3m$CDivision==1])-median(col3m$DTime[col3m$CDivision==2]) #-0.11

#I vs. III
col3m13.kruskal <- kruskal.test(DTime~CDivision,data=col3m[col3m$CDivision!=2,])
col3m13.kruskal  # 0.2579
median(col3m$DTime[col3m$CDivision==1])-median(col3m$DTime[col3m$CDivision==3]) #-0.025

#II vs. III
col3m23.kruskal <- kruskal.test(DTime~CDivision,data=col3m[col3m$CDivision!=1,])
col3m23.kruskal  # 0.7997
median(col3m$DTime[col3m$CDivision==2])-median(col3m$DTime[col3m$CDivision==3]) #0.085

# 8. College Year 4
col4m <- ccfree50l[(ccfree50l$Season==4)&(ccfree50l$Gender=="Male"),]
nrow(col4m)  # 63

median(col4m$DTime[col4m$CDivision==1]) #-0.08
median(col4m$DTime[col4m$CDivision==2]) #-0.19
median(col4m$DTime[col4m$CDivision==3]) #-0.05

wilcox.test(col4m$DTime[col4m$CDivision==1],mu=0,alternative="less") #0.1207
wilcox.test(col4m$DTime[col4m$CDivision==2],mu=0,alternative="less") #0.1106
wilcox.test(col4m$DTime[col4m$CDivision==3],mu=0,alternative="less") #0.05912

col4m.kruskal <- kruskal.test(DTime~CDivision,data=col4m)
col4m.kruskal  # 0.5591

#I vs. II
col4m12.kruskal <- kruskal.test(DTime~CDivision,data=col4m[col4m$CDivision!=3,])
col4m12.kruskal  # 0.472
median(col4m$DTime[col4m$CDivision==1])-median(col4m$DTime[col4m$CDivision==2]) #0.11

#I vs. III
col4m13.kruskal <- kruskal.test(DTime~CDivision,data=col4m[col4m$CDivision!=2,])
col4m13.kruskal  # 0.3353
median(col4m$DTime[col4m$CDivision==1])-median(col4m$DTime[col4m$CDivision==3]) #-0.03

#II vs. III
col4m23.kruskal <- kruskal.test(DTime~CDivision,data=col4m[col4m$CDivision!=1,])
col4m23.kruskal  # 0.8
median(col4m$DTime[col4m$CDivision==2])-median(col4m$DTime[col4m$CDivision==3]) #-0.14

# 9. College Year 5
col5m <- ccfree50l[(ccfree50l$Season==5)&(ccfree50l$Gender=="Male"),]
nrow(col5m)  # 5

median(col5m$DTime[col5m$CDivision==1]) #-0.08
median(col5m$DTime[col5m$CDivision==2]) #-0.36
median(col5m$DTime[col5m$CDivision==3]) #NA

wilcox.test(col5m$DTime[col5m$CDivision==1],mu=0,alternative="less") #0.3125
wilcox.test(col5m$DTime[col5m$CDivision==2],mu=0,alternative="less") #0.5
wilcox.test(col5m$DTime[col5m$CDivision==3],mu=0,alternative="less") #NA

col5m.kruskal <- kruskal.test(DTime~CDivision,data=col5m)
col5m.kruskal  # 0.1573 

#I vs. II
col5m12.kruskal <- kruskal.test(DTime~CDivision,data=col5m[col5m$CDivision!=3,])
col5m12.kruskal  # 0.1573
median(col5m$DTime[col5m$CDivision==1])-median(col5m$DTime[col5m$CDivision==2]) #0.28

#I vs. III
col5m13.kruskal <- kruskal.test(DTime~CDivision,data=col5m[col5m$CDivision!=2,])
col5m13.kruskal  # NA
median(col5m$DTime[col5m$CDivision==1])-median(col5m$DTime[col5m$CDivision==3]) #NA

#II vs. III
col5m23.kruskal <- kruskal.test(DTime~CDivision,data=col5m[col5m$CDivision!=1,])
col5m23.kruskal  # NA
median(col5m$DTime[col5m$CDivision==2])-median(col5m$DTime[col5m$CDivision==3]) #NA

# Too many swimmers having only one record for either high school or college
# We will remove them as (1) 

# Identify individuals with #[years in HS]<2 or #[years in College]<2
ccfree50$Student <- as.character(ccfree50$Student)
snames <- unique(ccfree50$Student)
conflict.list <- NULL
for (j in 1:length(snames)) {
  tmp <- ccfree50[ccfree50$Student==snames[j],]
  if( (sum(tmp$Season<=0)<2)|(sum(tmp$Season>0)<2)) 
    conflict.list <- rbind(conflict.list,snames[j])
}

# Remove individuals with #[years in HS]<2 or #[years in College]<2
ccfree50 <- ccfree50[!(ccfree50$Student%in%conflict.list),]

# Save the data
write.csv(ccfree50,"ccfree50.csv")
```

## Improving Rates

``` r
#============================================================
# Summarize data:
snames <- unique(ccfree50$Student)
swimmer.list <- NULL
for (j in 1:length(snames)) {
  tmp <- ccfree50[ccfree50$Student==snames[j],]
  tmp <- tmp[tmp$Division>0,]
  swimmer.list <- rbind(swimmer.list,tmp[1,])
}

sum((swimmer.list$Gender=="Male")&(swimmer.list$Division==1))
sum((swimmer.list$Gender=="Male")&(swimmer.list$Division==2))
sum((swimmer.list$Gender=="Male")&(swimmer.list$Division==3))

sum((swimmer.list$Gender=="Female")&(swimmer.list$Division==1))
sum((swimmer.list$Gender=="Female")&(swimmer.list$Division==2))
sum((swimmer.list$Gender=="Female")&(swimmer.list$Division==3))

sum(swimmer.list$Gender=="Male")
sum(swimmer.list$Gender=="Female")

# Swimmers with at least two years HS records & at least two years college records:
# College         Female       Male       Total
# Division I      61           56         117
# Division II     22           12         34
# Division III    33           29         62
# Total           97           116        213

#============================================================
# Longitudinal Plot:
ccfree50$GD <- paste(ccfree50$Gender, ccfree50$CDivision)
ggplot(ccfree50, aes(x=Season,y=Time,group=Student,color=GD))+
  geom_line() +
  labs(x = "Grade(0=HS Senior)", y="Time") +
  theme_minimal()

#===============================================================
# each swimmer for high school and college separately

# Unique list of swimmers
swimmers <- unique(ccfree50$Student)
nswimmers <- length(swimmers)

# Empty list to store models of high school & college
hs.models <- list()
col.models <- list()

# Loop through each swimmer and fit linear models for high school and college
for(i in 1:nswimmers) {
  sname <- swimmers[i]
  
  # Fit a linear model for high school
  hs.swimmer_subset <- ccfree50[(ccfree50$Student==sname)&(ccfree50$Division==0), ]
  hs.models[[i]] <- lm(Time~Season, data=hs.swimmer_subset)
  
  # Fit a linear model for college
  col.swimmer_subset <- ccfree50[(ccfree50$Student==sname)&(ccfree50$Division>0), ]
  col.models[[i]] <- lm(Time~Season, data=col.swimmer_subset)
}

# Put the results into a data frame
#   Student, Gender, CDivision,  CYear, FTime,
#    hs.intercept, hs.slope, col.intercept, col.slope,
resfree50 <- data.frame(Student=character(), Gender=character(), CDivision=integer(),
                hs.intercept=numeric(), hs.slope=numeric(),
                col.intercept=numeric(), col.slope=numeric(),
                stringsAsFactors = FALSE)

for(i in 1:nswimmers) {
  swimmer_subset <- ccfree50[(ccfree50$Student==swimmers[i]), ]
  hs.model_coef <- coef(hs.models[[i]])
  col.model_coef <- coef(col.models[[i]])
  resfree50 <- rbind(resfree50, 
                     data.frame(Student=swimmers[i],
                                Gender=swimmer_subset[1,]$Gender,
                                CDivision=swimmer_subset[1,]$CDivision,
                                CYear=swimmer_subset[1,]$CYear,
                                hs.intercept=hs.model_coef[1],
                                hs.slope=hs.model_coef[2],
                                col.intercept=col.model_coef[1],
                                col.slope=col.model_coef[2]))
}

# Estimated freshman time at high school 
resfree50$hs.time0 <- resfree50$hs.intercept-3*resfree50$hs.slope

# Estimated freshman time at college 
resfree50$col.time0 <- resfree50$col.intercept+resfree50$col.slope

# Save the data
write.csv(resfree50,"resfree50.csv")

#==================================================
# Correlation between hs.time0 and hs.slope:

resfree50$GD <- paste(resfree50$Gender, resfree50$CDivision)
ggplot(resfree50, aes(x=hs.time0,y=hs.slope,group=GD,color=GD))+
  geom_point() +
  labs(x = "Freshman Time", y="Improving Rate") +
  theme_minimal()

# Regression and test: High School Records

# All females & males
all.lm <- lm(resfree50$hs.slope~resfree50$hs.time0)
summary(all.lm) #-0.07452    0.01862  -4.002 8.68e-05

# All females
allf<- resfree50[(resfree50$Gender=="Female"),]
allf.lm <- lm(allf$hs.slope~allf$hs.time0)
summary(allf.lm) #-0.18226    0.02555  -7.133 9.95e-11

# Division I Female
d1f <- resfree50[(resfree50$Gender=="Female")&(resfree50$CDivision==1),]
d1f.lm <- lm(d1f$hs.slope~d1f$hs.time0)
summary(d1f.lm) #-0.27519    0.02978  -9.241 4.54e-13

# Division II Female
d2f <- resfree50[(resfree50$Gender=="Female")&(resfree50$CDivision==2),]
d2f.lm <- lm(d2f$hs.slope~d2f$hs.time0)
summary(d2f.lm) # -0.1963     0.0494  -3.972 0.000816

# Division III Female
d3f <- resfree50[(resfree50$Gender=="Female")&(resfree50$CDivision==3),]
d3f.lm <- lm(d3f$hs.slope~d3f$hs.time0)
summary(d3f.lm) # -0.32750    0.04203  -7.792 8.59e-09

# All males
allm <- resfree50[(resfree50$Gender=="Male"),]
allm.lm <- lm(allm$hs.slope~allm$hs.time0)
summary(allm.lm) #-0.30749    0.02786  -11.04   <2e-16

# Division I Male
d1m <- resfree50[(resfree50$Gender=="Male")&(resfree50$CDivision==1),]
d1m.lm <- lm(d1m$hs.slope~d1m$hs.time0)
summary(d1m.lm) #-0.35171    0.03018  -11.65 2.31e-16

# Division II Male
d2m <- resfree50[(resfree50$Gender=="Male")&(resfree50$CDivision==2),]
d2m.lm <- lm(d2m$hs.slope~d2m$hs.time0)
summary(d2m.lm) #-0.6520     0.1337  -4.876 0.000490

# Division III Male
d3m <- resfree50[(resfree50$Gender=="Male")&(resfree50$CDivision==3),]
d3m.lm <- lm(d3m$hs.slope~d3m$hs.time0)
summary(d3m.lm) #-0.31506    0.02745  -11.48 6.83e-12

#==================================================
# Correlation between col.time0 and col.slope:

resfree50$GD <- paste(resfree50$Gender, resfree50$CDivision)
ggplot(resfree50, aes(x=col.time0,y=col.slope,group=GD,color=GD))+
  geom_point() +
  labs(x = "Freshman Time", y="Improving Rate") +
  theme_minimal()

# All females & males
col.all.lm <- lm(resfree50$col.slope~resfree50$col.time0)
summary(col.all.lm) #0.01883    0.01160   1.624   0.1059

# Regression and test: College Records
# Regression and test: High School Records
#allf<- resfree50[(resfree50$Gender=="Female"),]
col.allf.lm <- lm(allf$col.slope~allf$col.time0)
summary(col.allf.lm) #-0.001974   0.027238  -0.072    0.942  

# Division I Female
#d1f <- resfree50[(resfree50$Gender=="Female")&(resfree50$CDivision==1),]
col.d1f.lm <- lm(d1f$col.slope~d1f$col.time0)
summary(col.d1f.lm) #-0.13376    0.05806  -2.304   0.0248

# Division II Female
#d2f <- resfree50[(resfree50$Gender=="Female")&(resfree50$CDivision==2),]
col.d2f.lm <- lm(d2f$col.slope~d2f$col.time0)
summary(col.d2f.lm) # -0.1571     0.1177  -1.334    0.198

# Division III Female
#d3f <- resfree50[(resfree50$Gender=="Female")&(resfree50$CDivision==3),]
col.d3f.lm <- lm(d3f$col.slope~d3f$col.time0)
summary(col.d3f.lm) # -0.11984    0.09033  -1.327    0.194

# All males
#allm <- resfree50[(resfree50$Gender=="Male"),]
col.allm.lm <- lm(allm$col.slope~allm$col.time0)
summary(col.allm.lm) #-0.03771    0.03878  -0.972    0.333

# Division I Male
#d1m <- resfree50[(resfree50$Gender=="Male")&(resfree50$CDivision==1),]
col.d1m.lm <- lm(d1m$col.slope~d1m$col.time0)
summary(col.d1m.lm) #-0.10827    0.07385  -1.466    0.148

# Division II Male
#d2m <- resfree50[(resfree50$Gender=="Male")&(resfree50$CDivision==2),]
col.d2m.lm <- lm(d2m$col.slope~d2m$col.time0)
summary(col.d2m.lm) #-0.03678    0.06657  -0.553    0.592

# Division III Male
#d3m <- resfree50[(resfree50$Gender=="Male")&(resfree50$CDivision==3),]
col.d3m.lm <- lm(d3m$col.slope~d3m$col.time0)
summary(col.d3m.lm) #-0.2779     0.1621  -1.714    0.098
```

## Classification

```r
library(rsq)
library(pROC)
par(pty = "s")
n = c(4, 3, 5) 
b = c(TRUE, FALSE, TRUE)

## Females in HS
hsf <- resfree50[resfree50$Gender=="Female",]

## Females to Division 1
resd1f <- glm((CDivision==1)~hs.time0+hs.slope,data=hsf,family="binomial")
summary(resd1f)
#hs.time0     -3.9989     0.7557  -5.292 1.21e-07 ***
#hs.slope     -9.9108     1.9756  -5.017 5.26e-07 ***
rsq(resd1f)  #0.6024365
rsq.partial(resd1f)
#partial.rsq
#"hs.time0" "hs.slope"
#0.5827658 0.4870694

# Plot the ROC curve
pred.d1f <- predict(resd1f, type="response")
roc.d1f <- roc((hsf$CDivision==1), pred.d1f)
plot(roc.d1f, col = "blue", main = "ROC for Females to Division 1", xlim=c(1.05,-.05))
auc.d1f <- auc(roc.d1f) #0.9399
print(auc.d1f) 

## Females to Division 2
resd2f <- glm((CDivision==2)~hs.time0+hs.slope,data=hsf,family="binomial")
summary(resd2f)
#hs.time0      1.7790     0.4511   3.944 8.01e-05 ***
#hs.slope      5.2415     1.4106   3.716 0.000203 ***
rsq(resd2f) #0.228235
rsq.partial(resd2f)
#partial.rsq
# "hs.time0" "hs.slope"
# 0.2061735 0.1852327

# Plot the ROC curve
pred.d2f <- predict(resd2f, type="response")
roc.d2f <- roc((hsf$CDivision==2), pred.d2f)
plot(roc.d2f, col = "blue", main = "ROC for Females to Division 2", xlim=c(1.05,-.05))
auc.d2f <- auc(roc.d2f) #0.8252
print(auc.d2f) 

## Females to Division 3
resd3f <- glm((CDivision==3)~hs.time0+hs.slope,data=hsf,family="binomial")
summary(resd3f)
#hs.time0      1.4301     0.3427   4.173 3.00e-05 ***
#hs.slope      3.2351     0.9903   3.267  0.00109 ** 
rsq(resd3f) #0.1829693
rsq.partial(resd3f)
#partial.rsq
# "hs.time0" "hs.slope"
# 0.1767312 0.1034867

# Plot the ROC curve
pred.d3f <- predict(resd3f, type="response")
roc.d3f <- roc((hsf$CDivision==2), pred.d3f)
plot(roc.d3f, col = "blue", main = "ROC for Females to Division 3", xlim=c(1.05,-.05))
auc.d3f <- auc(roc.d3f) #0.8207
print(auc.d3f) 

## Males in HS
hsm <- resfree50[resfree50$Gender=="Male",]

## Females to Division 1
resd1m <- glm((CDivision==1)~hs.time0+hs.slope,data=hsm,family="binomial")
summary(resd1m)
#hs.time0      -5.419      1.231  -4.401 1.08e-05 ***
#hs.slope     -13.145      2.952  -4.453 8.49e-06 ***
rsq(resd1m)  #0.6077332
rsq.partial(resd1m)
#partial.rsq
#"hs.time0" "hs.slope"
# 0.5988355 0.5599304

# Plot the ROC curve
pred.d1m <- predict(resd1m, type="response")
roc.d1m <- roc((hsm$CDivision==1), pred.d1m)
plot(roc.d1m, col = "blue", main = "ROC for Males to Division 1", xlim=c(1.05,-.05))
auc.d1m <- auc(roc.d1m) #0.9405
print(auc.d1m) 

## Males to Division 2
resd2m <- glm((CDivision==2)~hs.time0+hs.slope,data=hsm,family="binomial")
summary(resd2m)
#hs.time0      0.6105     0.4550   1.342   0.1796  
#hs.slope      3.2336     1.2766   2.533   0.0113 *
rsq(resd2m) #0.136509
rsq.partial(resd2m)
#partial.rsq
# "hs.time0" "hs.slope"
# 0.008863827 0.122972315

# Plot the ROC curve
pred.d2m <- predict(resd2m, type="response")
roc.d2m <- roc((hsm$CDivision==2), pred.d2m)
plot(roc.d2m, col = "blue", main = "ROC for Females to Division 2", xlim=c(1.05,-.05))
auc.d2m <- auc(roc.d2m) #0.7674
print(auc.d2m) 

## Females to Division 3
resd3m <- glm((CDivision==3)~hs.time0+hs.slope,data=hsm,family="binomial")
summary(resd3m)
#hs.time0      2.4611     0.5669   4.341 1.42e-05 ***
#hs.slope      3.6652     1.1967   3.063  0.00219 ** 
rsq(resd3m) #0.3763318
rsq.partial(resd3m)
#partial.rsq
# "hs.time0" "hs.slope"
# 0.3719886 0.2194740

# Plot the ROC curve
pred.d3m <- predict(resd3m, type="response")
roc.d3m <- roc((hsm$CDivision==3), pred.d3m)
plot(roc.d3m, col = "blue", main = "ROC for Females to Division 3", xlim=c(1.05,-.05))
auc.d3m <- auc(roc.d3m) #0.8746
print(auc.d3m) 

# All Genders Together:
## Division 1
res.d1 <- glm((CDivision==1)~hs.time0*Gender+hs.slope*Gender,data=resfree50,family="binomial")
summary(res.d1)
#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      106.1594    16.0302   6.622 3.53e-11 ***
#hs.time0          -4.7088     0.7223  -6.519 7.07e-11 ***
#Gender1            9.6776    16.0302   0.604    0.546    
#hs.slope         -11.5279     1.7761  -6.490 8.56e-11 ***
#hs.time0:Gender1  -0.7099     0.7223  -0.983    0.326    
#Gender1:hs.slope  -1.6170     1.7761  -0.910    0.363 

res.d1.ag <- glm((CDivision==1)~hs.time0+hs.slope+Gender,data=resfree50,family="binomial")
summary(res.d1.ag)
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept) 102.3910    14.8633   6.889 5.62e-12 ***
#hs.time0     -4.5004     0.6524  -6.899 5.25e-12 ***
#hs.slope    -11.0490     1.6205  -6.818 9.22e-12 ***
#Gender1      -6.2021     0.9258  -6.699 2.09e-11 ***

rsq(res.d1.ag)  #0.6033684
rsq.partial(res.d1.ag)
#partial.rsq
# "hs.time0" "hs.slope" "Gender"
# 0.5898254 0.5245661 0.5299420

# Plot the ROC curve
pred.d1.ag <- predict(res.d1.ag, type="response")
roc.d1.ag <- roc((resfree50$CDivision==1), pred.d1.ag)
plot(roc.d1.ag, col = "blue", main = "ROC for Division 1", xlim=c(1.05,-.05))
auc.d1.ag <- auc(roc.d1.ag) #0.942
print(auc.d1.ag)

## Division 2
res.d2 <- glm((CDivision==2)~hs.time0*Gender+hs.slope*Gender,data=resfree50,family="binomial")
summary(res.d2)
#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -29.5730     7.4231  -3.984 6.78e-05 ***
#hs.time0           1.1948     0.3203   3.730 0.000192 ***
#Gender1           15.1812     7.4231   2.045 0.040844 *  
#hs.slope           4.2375     0.9513   4.455 8.41e-06 ***
#hs.time0:Gender1  -0.5842     0.3203  -1.824 0.068174 .  
#Gender1:hs.slope  -1.0039     0.9513  -1.055 0.291271  
res.d2.ag <- glm((CDivision==2)~hs.time0+hs.slope+Gender,data=resfree50,family="binomial")
summary(res.d2.ag)
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -30.5755     7.0652  -4.328 1.51e-05 ***
#hs.time0      1.2746     0.3056   4.170 3.04e-05 ***
#hs.slope      4.4501     0.9512   4.678 2.89e-06 ***
#Gender1       1.7362     0.4910   3.536 0.000407 ***

rsq(res.d2.ag) #0.1702429
rsq.partial(res.d2.ag)
#partial.rsq
# "hs.time0" "hs.slope" "Gender" 
# 0.10776367 0.16146425 0.06949851

# Plot the ROC curve
pred.d2.ag <- predict(res.d2.ag, type="response")
roc.d2.ag <- roc((resfree50$CDivision==2), pred.d2.ag)
plot(roc.d2.ag, col = "blue", main = "ROC for Division 2", xlim=c(1.05,-.05))
auc.d2.ag <- auc(roc.d2.ag) #0.8056
print(auc.d2.ag) 

## Division 3
res.d3 <- glm((CDivision==3)~hs.time0*Gender+hs.slope*Gender,data=resfree50,family="binomial")
summary(res.d3)
#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -45.1261     7.4809  -6.032 1.62e-09 ***
#hs.time0           1.9456     0.3312   5.874 4.25e-09 ***
#Gender1           -9.5156     7.4809  -1.272    0.203    
#hs.slope           3.4502     0.7767   4.442 8.90e-06 ***
#hs.time0:Gender1   0.5155     0.3312   1.556    0.120    
#Gender1:hs.slope   0.2151     0.7767   0.277    0.782  
res.d3.ag <- glm((CDivision==3)~hs.time0+hs.slope+Gender,data=resfree50,family="binomial")
summary(res.d3.ag)
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -41.5185     6.7107  -6.187 6.13e-10 ***
#hs.time0      1.7635     0.2903   6.075 1.24e-09 ***
#hs.slope      3.1360     0.7318   4.286 1.82e-05 ***
#Gender1       2.3823     0.4468   5.332 9.70e-08 ***

rsq(res.d3.ag) #0.2584831
rsq.partial(res.d3.ag)
#partial.rsq
# "hs.time0" "hs.slope" "Gender"
# 0.2583130 0.1404620 0.2022184

# Plot the ROC curve
pred.d3.ag <- predict(res.d3.ag, type="response")
roc.d3.ag <- roc((resfree50$CDivision==3), pred.d3.ag)
plot(roc.d3.ag, col = "blue", main = "ROC Division 3", xlim=c(1.05,-.05))
auc.d3.ag <- auc(roc.d3.ag) #0.8398
print(auc.d3.ag) 
```
