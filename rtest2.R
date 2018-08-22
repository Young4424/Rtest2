# test git
library(ggplot2)
library(dplyr)

install.packages("foreign")
library(foreign) #spss 파일 로드
library(readxl) # 엑셀 파일 불러오기

raw_welfare = read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)
welfare = raw_welfare

head(welfare)
View(welfare)
tail(welfare)
dim(welfare)
str(welfare)
library(dplyr)
#변수명 바꾸기
welfare = rename(welfare,
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7)

View(welfare)

class(welfare$sex) #숫자냐 글자냐

table(welfare$sex)

welfare$sex = ifelse(welfare$sex == 9, NA, welfare$sex)
table(is.na(welfare$sex))

#성별에 항목 이름 부여
welfare$sex = ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
library(ggplot2)
qplot(welfare$sex)

################################################

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0,1000)

#2.전처리 - 이상치 확인
summary(welfare$income)

welfare$income = ifelse(welfare$income %in% c(0,9999), NA, welfare$income)

#결측치 확인
table(is.na(welfare$income))


# 1. 성별 월급 평균표 만들기
sex_income = welfare %>% 
  filter(!is.na(income)) %>% # 결측치 제거, 분리
  group_by(sex) %>% #성별로 묶기
  summarise(mean_income = mean(income)) #수입의 평균을 구하

sex_income

ggplot(data = sex_income, aes(x = sex, y = mean_income)) +
  geom_col()

# 2. 나이와 월급의 관계
#1. 변수 검토하기
class(welfare$birth)
summary(welfare$birth)

qplot(welfare$birth) # 분포도 빠르게 그리기

table(is.na(welfare$birth))

welfare$birth = ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

#2.파생변수 만들기 - 나이
welfare$age = 2018 - welfare$birth + 1 
summary(welfare$age)

qplot(welfare$age)

#나이와 월급의 관계 분석하기
#1. 나이에 따른 월급 평균표 만들기
age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

head(age_income)

#2. 그래프 만들기
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()


# 연령대에 따른 월급의 차이(어떤 연령대의 월급이 가장 많은가?)
#분석 절차 - 1. 변수 검토 및 전처리, 2. 변수 간 관계 분석

#2. 변수간 관계 분석 - 연령대별 월급 평균표 만들기, 그래프 만들기

welfare = welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

welfare = welfare %>% 
  mutate(ageg = ifelse(age <= 19, "10s",
                       ifelse(age <= 29, "20s",
                              ifelse(age <= 39, "30s",
                                     ifelse(age <= 49, "40s",
                                            ifelse(age <= 59,"50s","old"))))))

library(dplyr)

