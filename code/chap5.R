####################################
#               제 5 장             #
####################################

## C기업의 ‘비공식(좌)’, ‘공식(우)’ 연결망" 시각화하기 
net_informal <- read.csv('files/company_c_informal_edges.csv')
net_formal <- read.csv('files/company_c_formal_edges.csv')
### 연결망 그래프로 전환  
library(igraph)
g_informal <- graph_from_data_frame(net_informal, directed = TRUE)
g_formal <- graph_from_data_frame(net_formal, directed = TRUE)
### 다른 그래프에서 노드가 같은 위치에 고정되기 위해서 layout을 설정 
lay <- layout_with_kk(g_informal) # 노드들의 위치 정보를 저장
###  연결망 그래프 시각화 표현 
par(mfrow=c(1,2)) # R에서 그래프를 1행 2열(1, 2)의 panel로 보여주기 위한 장치
par(mar = c(0.1, 0.1, 0.1, 0.1)) # 그래프의 여백 조정
plot(g_informal, 
  layout = lay, vertex.label.cex = 0.7, edge.arrow.size = 0.5)
plot(g_formal, 
  layout = lay, vertex.label.cex = 0.7, edge.arrow.size = 0.5)

## 연결중앙성(Degree Centrality)
### 외향연결정도
outdegree <- degree(g_informal, mode = 'out', loops = FALSE)
### 외향연결정도 높은 순서대로 10명의 외향연결정도를 나타내기
outdegree[order(outdegree, decreasing = TRUE)][1:10]
### 내향연결정도
indegree <- degree(g_informal, mode = 'in', loops = FALSE)
### 내향연결정도 높은 순서대로 10명의 내향연결정도를 나타내기
indegree[order(indegree, decreasing = TRUE)][1:10]

### top_10 함수 만들기
top_10 <- function(c_index) {
  c_index <- c(c_index)
  names(c_index) <- names(c_index)
  top_10 <- c_index[order(c_index, decreasing = TRUE)][1:10]  
  return(top_10)

### mode = 'all' 유의하기 ('외향연결정도 + 내향연결정도')
degree_all <- degree(g_informal, mode = 'all')
top_10(degree_all)

### 방향성을 고려하지 않은 연결망 자료를 뽑아낼 것
g_informal_s <- graph_from_data_frame(net_informal, directed = FALSE)
### 연결망을 단순화 하기
g_informal_simpl <- simplify(g_informal_s)
### 방향성을 고려하지 않은  전체 연결정도의 계산
undirected_degree <- degree(g_informal_simpl)
top_10(undirected_degree)

### 표준화된 외향연결중앙성
outdegree_n <- degree(g_informal, mode='out', loops=FALSE, normalized=TRUE)
top_10(outdegree_n)

### 표준화된 내향연결중앙성
indegree_n <-degree(g_informal, mode='in', loops = FALSE, normalized=TRUE)
top_10(indegree_n)

### 여러 가지 연결중앙성 지표를 하나의 테이블로 만들고 출력하기 
df_degrees <- data.frame(
    outdegree, 
    indegree, 
    outdegree_n, 
    indegree_n, 
    degree_all, 
    undirected_degree)

df_degrees[1:10,]

### 연결중앙성 지표의 시각화 표현 'out(좌)', 'in(중)', 'undirected(우)'
#### 그래프 3개를 한 번에 표현하기 위해서 1*3 격자 설정 
par(mfrow=c(1,3))

#### 공간 활용을 위한 작은 여백을 설정 
par(mar = c(0.1, 0.1, 0.1, 0.1))
#### 연결망 노드 크기를 outdegree에 비례하여 시각화
plot(g_informal, layout = lay, edge.arrow.size = 0.3, 
     vertex.size = outdegree * 1.5)
#### 연결망 노드 크기를 indegree에 비례하여 시각화
plot(g_informal, layout = lay, edge.arrow.size = 0.3, 
     vertex.size = indegree * 1.5)
#### 연결망 노드 크기를 undirected_degree에 비례하여 시각화
plot(g_informal, layout = lay, edge.arrow.size = 0, 
     vertex.size = undirected_degree * 1.5)
dev.off()

### N단계 연결중앙성
#### 2단계만에 접근 가능한 연결중앙성
degree_2th <- ego_size(g_informal, 2) - 1
top_10(degree_2th)

#### 3단계만에 접근 가능한 연결중앙성
degree_3th <- ego_size(g_informal, 3) - 1
top_10(degree_3th)

#### 3단계만에 접근 가능한 연결중앙성의 표준화된 지표
degree_3th_n <- (ego_size(g_informal, 3)-1) / (vcount(g_informal)-1)
top_10(degree_3th_n)

## 사이중앙성
betweenness <- betweenness(g_informal, directed = TRUE)
round(top_10(betweenness), 2)

### 표준화된 사이중앙성 
betweenness_n <- betweenness(g_informal, directed = TRUE, normalized = TRUE)
round(top_10(betweenness_n), 2)

## 위세중앙성
### 아이겐백터 중앙성
eigen_centrality <- eigen_centrality(g_informal, directed = TRUE)$vector
round(top_10(eigen_centrality), 2)

### 권력 중앙성(power centralithy)
power_centralithy <- power_centrality(g_informal, exp=0.3)
round(top_10(power_centralithy), 2)

## 인접중앙성
### 외향인접중앙성
outcloseness <- closeness(g_informal, mode = 'out')
round(top_10(outcloseness), 4)

### 표준화된 내향인접중앙성
incloseness <- closeness(g_informal, mode = 'in')
round(top_10(incloseness), 4)

## 확산중앙성
library(keyplayer)
# 그래프 개체를 인접행렬로 변환
adj_informal <- as_adjacency_matrix(
  g_informal,
  type = 'both', attr = NULL,
  edges = FALSE,
  names = TRUE,
  sparse = igraph_opt("sparsematrices")
)
# 정보 전달 확률을 0.5로 가정
P <- adj_informal * 0.5
# 확산이 2번에 걸쳐서 발생한다고 가정
top_10(diffusion(P, T = 2))

## 링크 사이중앙성(edge betweenness)
edgebetweenness <- edge_betweenness(g_informal)
plot(g_informal, layout =lay, vertex.label.cex= 0.7, 
        vertex.cex= 0.7, edge.arrow.size=0.5, 
        edge.width=edgebetweenness/10, 
        edge.label= round(edgebetweenness, 0) , 
        edge.label.cex= 0.8 

## 페이지랭크(PageRank) 
page_rank <- igraph::page_rank(g_informal)$vector
round(top_10(page_rank), 3)

## 허브 점수
hub_score <- hub_score(g_informal)$vector
round(top_10(hub_score), 3)

## 권위 점수(Authority score)
authority_score <- authority_score(g_informal)$vector
round(top_10(authority_score), 3)
