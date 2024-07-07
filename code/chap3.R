####################################
#               제 3 장             #
####################################


# 연결망 자료의 종류와 형태 

## 인접행렬 
### igraph 패키지 불러오기 
library(igraph) # 이후 생략 

## 행=10, 열=10개인 행별로 입력한 인접행렬을 adjm이라는 이름으로 입력하기 
adjm <- matrix(
               c(0,0,0,0,1,0,0,0,0,0,
                 0,0,0,0,1,0,0,0,0,0,
                 0,0,0,0,1,0,0,0,0,0,
                 0,0,0,0,1,0,0,0,0,0,
                 0,0,0,0,0,0,1,1,1,1,
                 0,0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,1,1,1,
                 0,0,0,0,0,0,1,0,1,1,
                 0,0,0,0,0,0,1,1,0,1,
                 0,0,0,0,0,0,1,1,1,0), 
               nrow = 10, ncol = 10, byrow = TRUE)

### graph_from_adjacency_matrix 함수로 igraph 객체 g1으로 저장하기  
g1 <- graph_from_adjacency_matrix(adjm)

### g1 객체를 시각화하기 
plot(g1, vertex.size = 0.3, edge.arrow.size = 0.5)

## 관계목록
### 20개의 관계목록을 edgelist라는 이름으로 입력하기 
edgelist <- matrix(
                   c(1, 5,
                     2, 5,
                     3, 5,
                     4, 5,
                     5, 7,
                     5, 8,
                     5, 9,
                     5, 10,
                     7, 8,
                     7, 9,
                     7, 10,
                     8, 7,
                     8, 9,
                     8, 10,
                     9, 7,
                     9, 8,
                     9, 10,
                     10, 7,
                     10, 8,
                     10, 9), 
                   ncol = 2, byrow = TRUE)

### graph_from_edgelist 함수로 igraph 객체 g2로 저장하기  
g2 <- graph_from_edgelist(edgelist, directed = TRUE)

### g2 객체를 시각화하기 
plot(g2, vertex.size = 0.3, edge.arrow.size = 0.5)

### g1, g2 동일여부 확인하기 
identical_graphs(g1, g2)

### 사람 이름으로 관계목록 입력하여 시각화 하기 
el <- matrix( c('John', 'Alice', 
                'John', 'Bob', 
                'Alice', 'Eugene',
                'John', 'Eugene'), 
              nc = 2, byrow = TRUE)

g3 <- graph_from_edgelist(el, directed = TRUE)
plot(g3, vertex.size = 0.3, edge.arrow.size = 0.5)

### 파일에서 연결망 자료 읽어들이기 
library(readxl)
edgelist2 <- read_excel('files/ex_3_1_read_excel.xlsx')
edgelist2

### 그래프 객체로 변환하여 시각화하기 
g4 <- graph_from_data_frame(edgelist2, directed = TRUE)
plot(g4, vertex.size = 0.3, edge.arrow.size = 0.5)

## 노드목록(nodelist)
### graph_from_literal 함수로 방향성이 없는 그래프 생성하고 시각화 
g5 <- graph_from_literal(Alice-Bob-Cecil-Alice, 
                         Daniel-Cecil-Eugene,
                         Cecil-Gordon )
plot(g5, vertex.size = 0.3, edge.arrow.size = 0.5)

### graph_from_literal 함수로 방향성이 있는 그래프 생성하고 시각화 
g6 <- graph_from_literal( Alice +-+ Bob --+ Cecil +-- Daniel,
                     Eugene --+ Gordon:Helen )
plot(g6, vertex.size = 0.3, edge.arrow.size = 0.5)

## 이원 연결망 자료 
### 행=4, 열=5개인 사건 행렬을 incm이라는 이름으로 입력하기 
incm <- matrix(
               c(1,1,0,1,0,
                 1,1,1,0,1,
                 0,0,0,1,1,
                 0,0,0,0,1), 
               nrow = 4, ncol = 5, byrow = TRUE)
rownames(incm) <- c("1", "2", "3", "4")
colnames(incm) <- c("A", "B", "C", "D", "E")

print(incm)

### graph_from_incidence_matrix 함수로 igraph 객체 g_inc으로 저장하기  
g_inc <- graph_from_incidence_matrix(incm)

### g_inc 그래프의 레이아웃 설정하기 
layout_bipartite = -layout_as_bipartite(g_inc)[,c(2, 1)]

### g_inc 객체를 시각화하기 
plot(g_inc, 
     layout = layout_bipartite, 
     vertex.size = 15, vertex.label.cex= 0.6,
     vertex.color=c('green', 'cyan')[V(g_inc)$type+1])

### 1 사람 - 이벤트 이원 자료 입력 하기 
edgelist <- read.table(text='1 A
                             1 B
                             1 D
                             2 A
                             2 B
                             2 C
                             2 E
                             3 D
                             3 E
                             4 E',                        
                             header=TRUE)
### 2
twomodenetwork <- graph.data.frame(edgelist)

### 3
plot(twomodenetwork, vertex.size = 0.3, edge.arrow.size = 0.5) 

### 위 edgelist에서 두 번째 열(evnet)에 속한 name이면 TRUE, 그렇지 않으면 FALSE 
V(twomodenetwork)$type <- V(twomodenetwork)$name %in% edgelist[,2] 
plot(twomodenetwork, 
     layout = layout_bipartite, 
     vertex.size = 15, vertex.label.cex= 0.6,
     vertex.color = c('green', 'cyan')[V(twomodenetwork)$type+1])

### 이원 연결망을, 일원 연결망으로 전환 
### -> 이 때 bg는 2개의 사람-사람, 취미-취미 2개의 연결망을 포함하는 object로 생성됨 
bg <- bipartite_projection(twomodenetwork)

### bg의 1번째 연결망(사람-사람) 시각화하기 
plot(bg[[1]], vertex.color = 'green', vertex.size = 15, vertex.label.cex= 0.6)

### bg의 1번째 연결망(취미 연결망) 시각화하기 
plot(bg[[2]], vertex.color = 'cyan', vertex.size = 15, vertex.label.cex= 0.6)

### 이원 연결망 행렬 입력 및 전환 
tincm=t(incm)  ## Transpose of M
print(tincm)   ## Event * Person matrix

### 행렬 곱(`%*%`)을 활용한 2mode -> 1mode 전환 
incm %*% tincm ## Person * Person matrix

### 전치된 행렬 tincm에 원행렬 incm을 곱하여 이벤트-이벤트 행렬 만들기 
tincm %*% incm ## Event * Event matrix

## 자아중심연결망 자료 (ego-centric network data)
### 방향성이 없는 자아중심 연결망 
ego_g <- graph_from_literal( A-B, A-C, A-D, A-E, A-F, 
                         B-C,
                         C-D, C-E, D-E )
plot(ego_g, vertex.size = 15, vertex.label.cex= 0.6, edge.arrow.size = 0.5)

### 자아를 제외한 그래프 생성하기
ego_g_exclude <- induced_subgraph(ego_g, c('B', 'C', 'D', 'E', 'F'))
plot(ego_g_exclude, vertex.size = 15, vertex.label.cex= 0.6, edge.arrow.size = 0.5)

### 특정한 노드를 중심으로한 자아 중심 연결망을 구축
#### 먼저, 각 노드의 이름을 정해주기
V(g1)$name <- letters[1:length(V(g1))]

#### 8번째 Node, "h"라는 자아과 연결된 이웃들을 모두 탐지 (이때, mode는 연결의 방향을 의미)
ego8_neighbor <- neighbors(g1, 'h', mode = 'all')
ego8_neighbor <- unique(c('h', names(ego8_neighbor)))

#### h를 둘러싼 노드들을 뽑아내기
ego8_g <- induced_subgraph(g1, vids = ego8_neighbor)

#### 두 그래프의 비교
par(mfrow = c(1,2))
plot(g1, vertex.size = 15, vertex.label.cex= 0.6, edge.arrow.size = 0.5)
plot(ego8_g, vertex.size = 15, vertex.label.cex= 0.6, edge.arrow.size = 0.5)
dev.off()

### 공개된 연결망 자료 활용하기 
#### networkdata 패키지 설치하여 데이터 살펴보기 
install.packages('remotes')
remotes::install_github('schochastics/networkdata')
data(package = 'networkdata')

