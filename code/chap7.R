####################################
#               제 7 장             #
####################################

## C기업 연결망 불러들이기 
library(igraph)
net_informal <- read.csv('files/company_c_informal_edges.csv')
g_informal <- graph_from_data_frame(net_informal, directed = TRUE)

## 밀도
#loop(자기 자신 연결)를 포함하지 않고 계산
edge_density(g_informal) 

# loop를 포함하고 계산
edge_density(g_informal, loops=TRUE)  

## 평균 연결정도(average degree)
# 평균 연결 정도를 구하기 위해 평균(mean)함수를 활용
# 방향성이 없는 연결정도
mean(degree(g_informal)) 

# 수학적 평균 연결정도의 계산과 비교
ecount(g_informal) / vcount(g_informal)

# 방향성에 따른 평균 연결정도
mean(degree(g_informal, mode = 'in'))
mean(degree(g_informal, mode = 'out'))

## 포괄성(inclusiveness)
### 총 노드의 수 
n_of_nodes <- vcount(g_informal) 
### 연결되지 않은 노드(degree == 0)의 수 
n_of_isolated_nodes <- sum(degree(g_informal) == 0) 
### 포괄성 
(n_of_nodes - n_of_isolated_nodes) / n_of_nodes

# 연결정도가 0인 노드들을 찾아보기
degree(g_informal) == 0

# isolated된 노드들의 목록을 뽑아내기
V(g_informal)[degree(g_informal) == 0]

## 경로 거리

### g_informal 그래프애서 노드 c01-c08의 out 방향의 경로 거리 행렬 만들기 
distances(g_informal, mode = 'out')[1:8,1:8] 

### 노드 간 평균 경로 거리
mean_distance(g_informal, directed = FALSE)

### 연결되지 않은 노드 간의 거리를 '연결망에서 관찰된 최대 거리 + 1'로 대치하여 계산

# 밖으로 향하는 연결만 고려 (out), 이때 'in' 또는 'all'로 고려도 가능
dist_g <- distances(g_informal, mode = 'out')
#print(dist_g[1:5,1:5])
#    c01 c02 c03 c04 c05
#c01   0   1 Inf Inf Inf
#c02   3   0 Inf Inf Inf
#c03   4   1   0 Inf Inf
#c04   3   1 Inf   0   1
#c05   4   1 Inf   1   0

# Inf를 최대값 + 1으로 대체
max_dist <- max(dist_g[dist_g != Inf])
dist_g[dist_g == Inf] <- max_dist + 1
mean(dist_g)

## 도달 가능성
reachablitity <- is.finite(distances(g_informal, mode = 'out')) & 
         distances(g_informal, mode = 'out') > 0 

# 도달가능성 1:5까지의 노드를 표시
reachablitity[1:5,1:5] 

# 평균 도달가능성의 계산
mean(reachablitity)

# 연결망의 지름을 구하기
diameter(g_informal)
max(distances(g_informal))

## ego_size 함수를 활용하여 도달 가능한 노드 수 구하기 
ego_size(g_informal,
         order = diameter(g_informal),
         nodes = V(g_informal),
         mode = 'in')

## 최대 흐름
max_flow(g_informal, source=V(g_informal)["c02"],
           target=V(g_informal)["c16"])$value

## k-연결성
# 계산을 위해 방향성을 없애기 
g_undirected_informal <- as.undirected(g_informal) 

# k-연결성 구하기
cohesion(g_undirected_informal)

## 위계성
### 4개의 예시 그래프 만들기 
star <- graph_from_literal(A - B:C:D:E )
circle <- graph_from_literal(A - B - C - D - E - A)
chain <- graph_from_literal(A - B - C - D - E)
Y_Network <- graph_from_literal(A:B - C - D - E)

# 레이아웃 설정  
par(mfrow=c(2,2))
par(mar = c(1, 1, 1, 1))

# 시각화 
plot(star, vertex.shape = 'none', edge.arrow.size = 0.5); title('star')
plot(circle, edge.arrow.size = 0.5, vertex.shape = 'none'); title('circle')
plot(chain, edge.arrow.size = 0.5, vertex.shape = 'none'); title('chain')
plot(Y_Network, edge.arrow.size = 0.5, vertex.shape = 'none', 
     layout=layout_as_tree(Y_Network, root=c(1,2))); title('Y Network')
dev.off()

### 4개의 예시 그래프로 3종류의 중심화 지표 및 밀도 산출하기 
centr_degree(star, loops = FALSE)$centralization
centr_degree(circle, loops = FALSE)$centralization
centr_degree(chain, loops = FALSE)$centralization
centr_degree(Y_Network, loops = FALSE)$centralization

centr_betw(star)$centralization
centr_betw(circle)$centralization
centr_betw(chain)$centralization
centr_betw(Y_Network)$centralization

centr_clo(star, mode = 'all')$centralization
centr_clo(circle, mode = 'all')$centralization
centr_clo(chain, mode = 'all')$centralization
centr_clo(Y_Network, mode = 'all')$centralization

edge_density(star)
edge_density(circle)
edge_density(chain)
edge_density(Y_Network)

# 파워로 분포(power-law)에서 alpha 값 구하기 
fit_power_law(degree(g_informal))$alpha

## 2자 관계 특징 살펴보기 
dyad_census(g_informal)

## 3자 관계 특징 살펴보기 
triad_census(g_informal)

## 분절성
library(dplyr)
df <- data.frame(`구분` = c("남학생", "여학생"), 
                 `남학생` = c("40", "2"), 
                 `여학생` = c("5", "41"))
knitr::kable(df, escape = F, caption = "성별 간 친구 관계", align = 'c') %>% 
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::kable_styling(latex_options = "hold_position", position = "center")

### 콜만의 H-Index
### 남학생, 여학생 간의 친구 지목 관계의 임의적인 기댓값
df <- data.frame(`구분` = c("남학생", "여학생"), 
                 `남학생` = c("20", "22"), 
                 `여학생` = c("22", "24"))

knitr::kable(df, escape = F, caption = "임의적 기댓값으로 계산된 성별 간 친구 관계", align = 'c') %>% 
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::kable_styling(latex_options = "hold_position", position = "center")

### netseg패키지로 콜만의 H-Index 산출하기 
library(netseg)
data('Classroom')
coleman(Classroom, 'gender')

### 좁은 세상 / 이전성 지수 / 군집 계수
transitivity(g_informal)
mean(transitivity(g_informal, type = 'local'), na.rm=T)

