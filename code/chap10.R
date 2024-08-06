#####################################
#               제 10 장             #
#####################################

library(data.table)
library(readxl)

# 입력한 자료를 읽어들임
dt_ego <- read_xlsx('files/ego_net_example.xlsx', sheet = 'ego')
dt_alter <- read_xlsx('files/ego_net_example.xlsx', sheet = 'ego_alter')
dt_aaties <- read_xlsx('files/ego_net_example.xlsx', sheet = 'alter_alter')

# 입력한 자료를 factor 함수를 이용해서 변수의 label을 지정하기
dt_ego$sex <- factor(dt_ego$sex, levels = c('여', '남'), 
                     labels = c('female','male'))
dt_ego$educ <- factor(dt_ego$educ, 
                      levels = c('고졸', '전문대졸', '대졸'), 
                      labels = c('hs','some_college','college'))
dt_alter$alter_sex <- factor(dt_alter$alter_sex, levels = c('여','남'), 
                             labels = c('female','male'))
dt_alter$alter_educ <- factor(dt_alter$alter_educ, 
                              levels = c('무학', '고졸','전문대졸','대졸'), 
                              labels = c('no educ', 'hs','some_college','college'))

# egoR을 이용해서 자아 중심 연결망 객체로 변환하기
# 함수를 불러들임
#install.packages('egor')
library(egor)

# egor 함수로 변환하기
egor_g <- egor(
    # alter_id가 missing인 경우는 제외하고 alter의 정보를 저장
    alters =  subset(dt_alter, !is.na(alter_id)),
    # 현재 모든 ego의 정보를 저장(isolate들도 포함하여)
    # 추후에 subset명령어를 활용하여 특정한 집단(white)만 따로 뽑아서 저장할 수 있다
    egos = dt_ego,   
    # alter-alter tie (aaties)에서 "친함"과 "매우 친함"의 경우에만 연결망 정보를 저장
    aaties = subset(dt_aaties, relationship %in% c('친함','매우 친함')),
    # ID의 정확한 이름을 지정해주는 것이 중요하다.
    # 만약에 aaties가 없는 경우에는 aaties = NULL로 지정하고
    # source와 target ID는 마찬가지로 지정하지 않아도 무방하다.
    ID.vars = list(
        ego = 'ego_id',
        alter = 'alter_id',
        source = 'alter_id1',
        target = 'alter_id2'
        )
    )
# 간단히 요약해보기
summary(egor_g)

# ego-gram으로 나타내기
plot_egograms(
    x = egor_g,
    ego_no = c(1, 2, 3, 4),
    x_dim = 2,
    y_dim = 3,
    vertex_color_var = 'alter_sex',
    vertex_size_var = 'alter_age')


# ego-graph로 나타내기
plot_ego_graphs(
    x = egor_g,
    ego_no = c(1, 2, 3, 4),
    x_dim = 2,
    y_dim = 2,
    vertex_color_var = 'alter_sex',
    vertex_size_var = 'alter_age')

## 자아 중심 연결망 지표 소개
#### 연결망 크기(연결정도)
library(igraph)

# 자아연결망 크기 계산을 위해 먼저 igraph 객체로 변환할 것
igraph_g <- as_igraph(egor_g, include.ego = FALSE)

# 이렇게 변환된 igraph object는 list형태로 저장되어 있으므로
# lapply 라고 하는 함수를 활용하여 반복연산을 할 수 있다
# for loop를 활용할 수도 있으나, lapply를 활용하는 것이 낫다.
network_size <- lapply(igraph_g, vcount)

# 계산된 list object를 vector의 형태로 변환하기
network_size <- unlist(network_size)
print(network_size)

### 특정한 속성만을 뽑아서 연결망 크기를 재거나 다층적 연결망의 크기를 각각 다르게 계산하여 변수화하기 
# 먼저 하나의 network를 뽑아서 계산을 해보자
g1 <- egor_g[1]

# 다시 igraph의 형태로 변환
g1 <- as_igraph(g1, include.ego = F)[[1]]

# 교육 수준이 전문대졸이상인 경우만 뽑아보자
new_g1 <- induced.subgraph(g1, 
            V(g1)[V(g1)$alter_educ %in% c('some_college', 'college')])

# 연결망 크기 계산
vcount(new_g1)

### lapply를 활용하여 전체를 반복해서 계산하기

# egor object를 igraph object로 변환
igraph_g <- as_igraph(egor_g, include.ego = FALSE)

# lapply를 활용하여 전체를 반복해서 계산하기
vcount_higheduc <- lapply(1:length(igraph_g), function(i){
    # i 번째 객체 (연결망 그래프)를 추출
    g1 <- igraph_g[[i]]
    # 만약에 network에 아무도 없는 경우는 연결망 크기를 0으로 정의
    if (vcount(g1) ==  0) return(0)
    # 교육 수준이 전문대졸이상인 경우만 뽑아보자
    new_g1 <- induced.subgraph(g1, 
                V(g1)[V(g1)$alter_educ %in% c('some_college', 'college')])
    # 연결망 크기 계산
    return(vcount(new_g1))
})

# 계산된 list object를 vector의 형태로 변환하기
vcount_higheduc <- unlist(vcount_higheduc)
vcount_higheduc

# 이번에는 둘 사이의 관계 속성을 활용하여 강한 연결만을 추려내보자.
new_g1 <- induced.subgraph(g1, V(g1)[V(g1)$alter_freq %in% c('매일')])

# 연결망 크기 계산
vcount(new_g1)
# 위에서 이용한 lapply를 활용하여 반복적인 계산할 수 있을 것이다.


#### 연결 강도
# 먼저 하나의 network를 뽑아서 계산을 해보자
g1 <- egor_g[1]
g1 <- as_igraph(g1, include.ego = F)[[1]]

# 관계의 강도를 나타내는 정보를 뽑아내기
g1_strength <- V(g1)$alter_talkfreq

# 평균 강도를 계산하기
mean(g1_strength)

# 강한 연결의 숫자를 세어보기
sum(g1_strength == 7)

# 표준편차 계산하기:
# R에서 제공하는 sd는 표본의 표준편차를 계산하므로
# 모집단의 표준편차를 계산하기 위해서는 다음과 같이 계산한다.
pop_sd <- function(x){
    sd(x) / (length(x) - 1) * length(x)
}

pop_sd(g1_strength)

# 위에서 이용한 lapply를 활용하여 반복한다면, 예를 들어 평균 강도계산을 할 경우에
igraph_g <- egor::as_igraph(egor_g, include.ego = FALSE)

mean_strength <- lapply(1:length(igraph_g), function(i){
    g1 <- igraph_g[[i]]
    g1_strength <- V(g1)$alter_talkfreq

    return(mean(g1_strength))
})
mean_strength <- unlist(mean_strength)
print(mean_strength)

#### 자아와 타자의 유사성 지표 측정 
# 이러한 계산은 egoR 패키지를 활용하여 계산을 하면 더 편하다

# 1. 자아와 같은 비율에 대한 계산
# 같은 성별(race)의 비율을 계산하기 위해서 먼저 각 개인마다 alter의 비율을 계산
sex_comp <- composition(
    object = egor_g,
    alt.attr = 'alter_sex')

sex_comp <- 
    merge(sex_comp, dt_ego[,c('ego_id','sex')], by.x = '.egoID', by.y = 'ego_id')

sex_comp[sex_comp$sex == 'female', 'p_samesex'] <- 
    sex_comp[sex_comp$sex == 'female', 'female']

sex_comp[sex_comp$sex == 'male', 'p_samesex'] <- 
    sex_comp[sex_comp$sex == 'female', 'male']

# Krackhardt와 Stern의 E-I 지수에 대한 계산
comp_ei(egor_g, 'alter_sex', 'sex')

# 평균 유클리드 거리(Average Euclidean Distance)
# 먼저 하나의 network를 뽑아서 계산을 해보자
g1 <- egor_g[1]
g1 <- egor::as_igraph(g1, include.ego = F)[[1]]

# ego의 속성 찾기: age
g1_ego <- subset(dt_ego, ego_id == g1$.egoID)
g1_ego_age <- g1_ego$age

g1_alter_age = V(g1)$alter_age

# 유클리드 거리 계산하기:
dist_ed <- function(y, x){
    sqrt( sum((y - x)^2) / x)
}

dist_ed(y = g1_alter_age, x = g1_ego_age)

#### 타자 간의 유사성 지표
# 블라우의 지수(Blau's Index)
# 그래프를 끄집어내기
g1 <- egor_g[1]

# igraph로 변환하기
g1 <- egor::as_igraph(g1, include.ego = F)[[1]]

# ego의 속성 찾기: educ
g1_alter_educ = V(g1)$alter_educ

# blau 지수 계산하기
blau_index = function(x, attr){
    pk <- rep(0, length(attr))
    names(pk) <- attr

    pk[names(pk) %in% names(table(x))] <- table(x)
    pk <- pk / sum(pk)
    H <- 1 - sum(pk^2)
    return(H)
}

blau_index(g1_alter_educ, attr = c('no educ', 'hs','some_college','college'))

# 아그레스티의 질적 변화 지수(Agresti's Index of Qualitative Variation, IQV)
blau <- blau_index(g1_alter_educ, attr = c('no educ', 'hs','some_college','college'))

# k = 4 이므로, 4개의 카테고리
k <- length(c('no educ', 'hs','some_college','college'))
iqv <- blau / (1 - 1 / k)

# 표준편차와 변동계수
# 먼저 age 속성을 뽑아보자
g1_alter_age = V(g1)$alter_age

# 표준편차 계산
sd(g1_alter_age)

# 변동계수 계
sd(g1_alter_age) / mean(g1_alter_age)

### 타자들의 관계 기반 지표
# 연결망 밀도
g1 <- egor_g[1]
g1 <- egor::as_igraph(g1, include.ego = F)[[1]]
graph.density(g1)

# egor 함수를 이용할 경우
ego_density(egor_g)

#### 구조적 공백(structural holes)
library(migraph)
# Burt 의 구조적 공백 지표
# egor package를 이용할 경우
ego_constraint(egor_g)

# igraph package 를 이용할 경우
g1 <- egor_g[1]
g1 <- egor::as_igraph(g1, include.ego = T)[[1]]

constraint(g1)['ego']

# migraph package를 이용할 경우
migraph::node_redundancy(g1)
migraph::node_effsize(g1)
migraph::node_constraint(g1)


#### 중재자 역할(brokerage)
# igraph package에는 중개자 역할을 계산하는 함수가 없으므로 sna package를 활용
library(intergraph)
library(sna)

g1 <- egor_g[1]
g1 <- egor::as_igraph(g1, include.ego = TRUE)[[1]]

# 이때 igraph 포맷을 sna object 포맷으로 바꿔줘야 한다
net <- asNetwork(g1)  

# 중개자 역할 계산
brole <- sna::brokerage(net, cl = network::get.vertex.attribute(net, 'alter_educ'))$raw.nli
brole[rownames(brole) %in% 'ego',]

## 자아 중심 연결망 분석을 통한 사회 중심 연결망 패턴 추론
# GSS 자료를 읽어들임
# 이 자료에서 id는 ego의 id를 의미하고, 
# alter_id는 각 ego의 alter들의 ID를 의미한다. 
# ego_로 시작되는 변수들은 ego의 속성을 나타내며, 
# alter_로 시작되는 변수들은 alter의 속성을 나타낸다. 

gss_ego <- fread('files/gssnet_ego.csv')
gss_dyad <- fread('files/gssnet_dyad.csv')
gss_aaties <- fread('files/gssnet_aaties.csv')

# 자료 살펴보기
gss_ego[id %in% c(1:5),]

# ego-alter tie 살펴보기
# 자료를 ID 와 alter ID 순서로 정렬
setorder(gss_dyad, id, alterid)
gss_dyad[id %in% c(3),]

# alter-alter tie 살펴보기
# 자료를 ID 와 alter ID, source ID, target ID 순서로 정렬
setorder(gss_aaties, id, source_id, target_id)
gss_aaties[id %in% c(3),]

# egoR 자료로 변환하기.
gss_egor_g <- egor(
    alters =  gss_dyad,
    egos = gss_ego,
    # here we exclude the TOTAL STRANGER relationship (=3)
    aaties = subset(gss_aaties, close %in% c(1,2)),
    ID.vars = list(
        ego = 'id',
        alter = 'alterid',
        source = 'source_id',
        target = 'target_id'
        )
    )

library(ergm.ego)

mixingmatrix(gss_egor_g, 'race')
# 여기서 ego의 race의 경우 1 = 백인, 2 = 흑인, 3 = 기타 인종
# 타자의 1race의 경우 1 = 아시아인, 2 = 흑인, 3 = 히스패닉, 4 = 백인, 5 = 기타 인종

# 단 여기서 중요한 것은 ego와 alter의 attribute이 같아야 한다는 점이다.
# ego의 인종을 기준으로 맞춘다면 ...

# 여기서는 factor 함수를 활용하여 변환을 할 수 있다.
gss_egor_g$ego$race <- factor(gss_egor_g$ego$race,
    level = c(1, 2, 3),
    labels = c('백인', '흑인', '기타'))

gss_egor_g$alter$race <- factor(gss_egor_g$alter$race,
    level = c(1, 2, 3, 4, 5),
    labels = c('기타', '흑인','기타','백인','기타'))

# 혼합행렬의 계산
mixingmatrix(gss_egor_g, 'race')

mixing_race <- mixingmatrix(gss_egor_g, 'race')
mixing_race / rowSums(mixing_race)
