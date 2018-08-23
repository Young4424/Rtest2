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
#welfare$sex가 수치가 9이면 NA 부여한ㄷ
#성별에 항목 이름 부여
welfare$sex = ifelse(welfare$sex == 1, "male", "female")
#welfare$sex 가 1이면, 남자 , 아니면 여자부여한다!

table(welfare$sex)
library(ggplot2)
qplot(welfare$sex)
#간단하게 그래프로 그려서 표ㅎ
################################################

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0,1000)

#2.전처리 - 이상치 확인
summary(welfare$income)

welfare$income = ifelse(welfare$income %in% c(0,9999), NA, welfare$income)
#welfare에서 income이 0,9999 이면 NA 부여한다!
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



library(dplyr)

#test git

install.packages("Rcmdr")

library(Rcmdr)


###################################################

sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young","middle","old"))


ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young","middle","old"))


#성별 연령별 월급 평균표 만들기
sex_age = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

head(sex_age)

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) +
  geom_line()

class(welfare$code_job)
table(welfare$code_job)

#직업별 월급차이

class(welfare$code_job)
table(welfare$code_job)

#전처리
library(readxl)
list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)

#welfare에 직업명 결합
welfare = left_join(welfare, list_job, id = "code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

################################################
#직업별 월급 차이 분석하기
#1.직업별 월급 평균표 만들기
job_income = welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>%
  summarise(mean_income = mean(income))

head(job_income)

top10 = job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()

#하위 10위 추출
bottom10 = job_income %>% 
  arrange(mean_income) %>% 
  head(10)

bottom10

ggplot(data = bottom10, aes(x = reorder(job, -mean_income), y = mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0, 850)

