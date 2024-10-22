####################################
#               제 4 장             #
####################################

## 다차원척도법(MDS: MultiDimensional Scaling)
### 도시간 거리 자료 불러들이고 행렬로 변환하기 
cities <- read.csv('files/distances_cities.csv')
row.names(cities) <- cities$City
mat_cities <- as.matrix(cities[, 2:11])
mat_cities

### cmdscale 함수로 2d 좌표로 변환하기 
library(ggplot2)
library(dplyr)
mds_2d <- cmdscale(mat_cities) %>% as.data.frame()

### cmdscale 함수 2d 결과 ggplot으로 시각화하기 
ggplot(mds_2d, aes(-V1,  V2)) + 
  geom_text(aes(label = row.names(mat_cities))) +
  theme_set(theme_gray(base_family = 'NanumGothic')) +
  theme_bw()

## 스프링 알고리즘
### igraph 패키지를 불러들이기
library(igraph)

# 에르도스-레니(erdos-renyi) 임의 연결을 가정으로 연결망 자료 생성하기
g_er <- sample_gnp(30, 0.1) 

# 연결망 그래프의 다양한 특성을 정의
V(g_er)$size <- 8
V(g_er)$frame.color <- "white"
V(g_er)$color <- "orange"
V(g_er)$label <- ""
E(g_er)$arrow.mode <- 0

### 다른 그래프에서 노드가 같은 위치에 고정되기 위해서 layout을 설정 
spring_kk <- layout_with_kk(g_er) # 카마다 카와이 layout
spring_fr <- layout_with_fr(g_er) # 프루흐터만과 라인골드 layout

### 연결망 그래프 시각화 표현 
par(mfrow=c(1,2)) # R에서 그래프를 1행 2열(1, 2)의 panel로 보여주기 위한 장치
par(mar = c(0.1, 0.1, 0.1, 0.1)) # 그래프의 여백 조정
plot(g_pa, layout = spring_kk) # KK알고리즘으로 그래프 그리기
plot(g_pa, layout = spring_fr) # FR알고리즘으로 그래프 그리기
dev.off()


## 히트맵으로 노드 간의 관계 표현하기
library(reshape2)
library(ggplot2)

# 선호적 연결을 가정으로 그래프 만들기
g_pa <- sample_pa(50, power = 1)

### g_pa 그래프를 행렬 자료로 변환
mat2 <- as_adjacency_matrix(g_pa, sparse = FALSE)

### wide 자료를 long-form 자료로 변환
longData <- reshape2::melt(mat2)

### longData가 Var1과 Var2 사이의 value값을 나타냄. 
# ggplot을 활용하여 시각화하기 
heatmap_plot <- ggplot(longData, aes(x = as.factor(Var1), y = as.factor(Var2), 
  fill = value)) + geom_tile(color = 'black') + 
  scale_fill_gradient('low' = "white", 'high' = "black") +
  labs(x = 'node', y = 'node', title = 'Matrix') + theme_minimal()+
  theme(axis.text.x = element_text(size = 5, angle = 0, vjust = 0.3),
        axis.text.y = element_text(size = 5), 
        plot.title = element_text(size = 11)) +
  theme(legend.position = 'none')
as.ggplot(~plot(g_pa, vertex.shape = 'none', vertex.label.cex = 0.5, 
          edge.arrow.size = 0.3) ) + heatmap_plot

## 원형 / 구형 시각화 
par(mfrow=c(1,2)) # R에서 그래프를 1행 2열(1, 2)의 panel로 보여주기 위한 장치
par(mar = c(0.1, 0.1, 0.1, 0.1)) # 그래프의 여백 조정

plot(g_pa, layout = layout_in_circle(g_pa)) # 원형 배치
plot(g_pa, layout = layout_on_sphere(g_pa)) # 구형 배치
dev.off()

## 호 다이어그램
#devtools::install_github('gastonstat/arcdiagram')
library(arcdiagram)

# g_er 그래프 객체를 as_edgelist 함수로 관계목록으로 변환하기 
edges_er = as_edgelist(g_er)

# arcplot 그리기 
arcplot(edges_er, cex.labels = 0.7, font = 0.3)

## 트리 다이어그램과 응용
### 트리 구조 자료 생성 및 시각화
g_tree <- make_tree(10, 3, mode = "undirected")
plot(g_tree, vertex.shape = 'none', vertex.label.cex = 0.1,
     edge.width=0.1, layout = layout.reingold.tilford(g_tree, root=1))

## 무작위 배열
plot(g_er, layout = layout_randomly(g_er), 
     vertex.label.cex = 1, edge.arrow.size = 0.5)
