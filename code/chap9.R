####################################
#               제 9 장             #
####################################

## 이원 연결망 시각화하기 
library(igraph)
library(dplyr)
twomode <- read.csv('files/2mode_en.csv')
twomode 

# csv 자료에 첫째 열 node가 문자형 변수로 이뤄져 있으므로 이를 제외해서 
# 행렬(matrix) 자료로 구축
X <- as.matrix(select(twomode, -node))

# 행렬의 이름을 열로 지정하기
rownames(X) <- twomode$node

# 행렬로부터 igraph object를 만들 것.
twomodenetwork <- graph_from_incidence_matrix(X)

# 사람 이름(twmode$node)여부로 two-mode를 각각 true/false로 규정
V(twomodenetwork)$type <- 
    V(twomodenetwork)$name %in% twomode$node
# 아래에서는 사람인 경우에 파란색, 그렇지 않을 경우에 보라색으로 색을 지정
V(twomodenetwork)$color <- ifelse(V(twomodenetwork)$name 
                %in% twomode$node, 'blue', 'purple')
# igraph를 활용하여 그래프 그리기
plot(twomodenetwork, 
    vertex.shape = 'none', 
    vertex.label.color= V(twomodenetwork)$color)

# 행렬 곱을 활용한 연결망 변환 
P = X %*% t(X)
P

G = t(X) %*% X
G

## 천만 영화 - 배우 데이터를 읽어들인 후 이원 연결망 데이터로 변환하여 시각화 하기 
edgelist <- read.csv('./files/mainactor_movie_en.csv')

### 관계목록을 그래프 객체로 전환 
twomodenetwork <- graph_from_data_frame(edgelist, directed = FALSE)
### 그래프 객체를 시각화 
plot(twomodenetwork, vertex.shape = 'none', vertex.label.color =
    ifelse(V(twomodenetwork)$name %in% edgelist[,1], 'blue', 'black'), 
    vertex.label.cex = 0.8)

### bipartite_projection 함수로 1원 연결망으로 전환하기 

# 위에서 입력한 edgelist에서 첫 번째 열(person)에 속한 name이면 TRUE, 
# 그렇지 않으면 FALSE 
V(twomodenetwork)$type <- V(twomodenetwork)$name %in% edgelist[,1] 

#### 이원 연결망을, 일원 연결망으로 전환 

#### bg는 배우-배우[1], 영화-영화[2] 2개의 연결망을 포함하는 object로 생성됨 
bg <- bipartite_projection(twomodenetwork)

#### bg의 1번째 연결망 시각화하기 
plot(bg[[1]], layout=layout_with_fr, 
     vertex.shape = 'none', vertex.label.cex= 0.8)

#### bg의 2번째 연결망(영화 연결망) 시각화하기 
plot(bg[[2]], layout=layout_with_fr, 
     vertex.shape = 'none', vertex.label.cex = 0.8)

## 이원 연결망\index{이원 연결망}의 일원 연결망 변환 

### 예제 데이터 만들기 
### nrow(X) X nrow(X) 0 행렬 만들기 
pc = yq = jc = sm <- matrix(0, nrow(X), nrow(X))

### X행렬 유사성 지표 계산 하기 
for(i in 1:nrow(X)) {
  for(j in 1:nrow(X)) {
    a = sum(X[i, ]==1 & X[j, ]==1) # i, j 노드가 모두 1인 경우 
    b = sum(X[i, ]==1 & X[j, ]==0) # i = 1, j = 0 인 경우 
    c = sum(X[i, ]==0 & X[j, ]==1) # i = 0, j = 1 인 경우 
    d = sum(X[i, ]==0 & X[j, ]==0) # i, j 노드가 모두 0인 경우 
    
    sm[i, j] = (a + d) / (a + b + c + d) # simple matching 
    jc[i, j] = (a) / (a + b + c) # 재커드 유사성  
    pc[i, j] = cor(X[i,], X[j,]) # Pearson correlation coefficient 
    yq[i, j] = (a*d - b*c) / (a*d + b*c) # Yule's Q 
  }
}

### 크리스 X[8, ]와 애슐리 X[9, ] 유사성 지표 계산 하기 
i = 8
j = 9

a = sum(X[i, ]==1 & X[j, ]==1) # i, j 노드가 모두 1인 경우 
b = sum(X[i, ]==1 & X[j, ]==0) # i = 1, j = 0 인 경우 
c = sum(X[i, ]==0 & X[j, ]==1) # i = 0, j = 1 인 경우 
d = sum(X[i, ]==0 & X[j, ]==0) # i, j 노드가 모두 0인 경우 

simple_matching = (a + d) / (a + b + c + d) # 단순 일치율
jaccard_coefficient = (a) / (a + b + c) # 재커드 유사성  
pearson_correlation = cor(X[i,], X[j,]) # 상관관계
yule_q = (a*d - b*c) / (a*d + b*c) # Yule's Q  #율스 큐

# a,b,c,d와 다양한 지표들을 출력하기
a; b; c; d
simple_matching
jaccard_coefficient
pearson_correlation
yule_q


### 일원 연결망으로 변환된 이원 연결망의 분석
#1 저자 - 논문 이원 자료 입력 하기 
coauthorship <- read.csv('files/coauthorship.csv', header=TRUE)

#2 igraph 객체로 변환하기 
coauthorship_2mode <- graph.data.frame(coauthorship)
# 위에서 입력한 edgelist에서 첫 번째 열(person)에 속한 name이면 TRUE, 
# 그렇지 않으면 FALSE 
V(coauthorship_2mode)$type <- V(coauthorship_2mode)$name %in% coauthorship[,1] 

# 이원 연결망을, 일원 연결망으로 전환 
# coauthorship_bipartite는 논문-논문[1], 저자-저자[2] 2개의 연결망 object로 생성됨
coauthorship_bipartite <- bipartite_projection(coauthorship_2mode)
coauthorship_network <- coauthorship_bipartite[[2]]

# 저자-저자 연결망 시각화하기 
plot(coauthorship_network, edge.width = E(coauthorship_network)$weight, 
     layout = layout.kamada.kawai(coauthorship_network), vertex.label.cex= 0.7)

W <- as_adjacency_matrix(coauthorship_network, attr= 'weight')
W

#### 0 기준 변환 
#install.packages('backbone')
library(backbone)

# 이분 연결망으로 변환 후 시각화하기
bn1 <- global(W, upper = 0, class = 'igraph')
plot(bn1, edge.width = E(bn1)$weight, 
         layout = layout.kamada.kawai(bn1), vertex.label.cex= 0.7)

# 공저(가중치) 연결망 노드들의 연결 강도
strength(coauthorship_network) 

# 공저(이분) 연결망 노드들의 연결 강도
strength(bn1) 

# 공저(가중치) 연결망 노드들의 인접중앙성 
closeness(coauthorship_network) 

# 공저(이분) 연결망 노드들의 인접중앙성 
closeness(bn1) 

#### 평균 기준 변환 
# 평균을 기준으로 
bn2 <- global(W, upper = mean(matrix(W)), class = 'igraph')
plot(bn2, edge.width = E(bn2)$weight, 
     layout = layout.kamada.kawai(bn2), vertex.label.cex= 0.7)

#### 불균등 추출법
b03 <- disparity(W, alpha = 0.3, class = 'igraph')
plot(b03, vertex.label.cex= 0.7)

# alpha 기준을 변환하면서 새로운 연결망을 추출하기
b02 <- disparity(W, alpha = 0.2, narrative = TRUE, class = 'igraph')
b01 <- disparity(W, alpha = 0.1, narrative = TRUE, class = 'igraph')
print(c(gsize(b03), gsize(b02), gsize(b01)))

## 이원 연결망 자체 분석 : 일원 연결망 분석의 응용
### 중앙성 
deg2 <- degree(twomodenetwork) 
plot(twomodenetwork, vertex.shape = 'none', vertex.label.cex= deg2 / 5)

### 이원 연결망 기준으로 커뮤니티 탐색 방법

#devtools::install_github('biometry/bipartite/bipartite')
library(bipartite)
actor_movie_mat <- as_incidence_matrix(twomodenetwork) 

mod <- computeModules(actor_movie_mat)
plotModuleWeb(mod)
