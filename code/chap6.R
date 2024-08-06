####################################
#               제 6 장             #
####################################

## 결속집단
### edge리스트로 되어 있는 csv 파일 읽기
net_informal <- read.csv('files/company_c_informal_edges.csv')

### 연결망 그래프로 전환  
library(igraph)
g_informal <- graph_from_data_frame(net_informal, directed = TRUE)

### 5개 노드로 구성되어 있는 결속집단 갯수 구하기 
length(cliques(g_informal, min=5))
### 6개 노드로 구성되어 있는 결속집단 갯수 구하기 
length(cliques(g_informal, min=6))
### 결속집단 찾기(minimum 6개 이상의 노드)
cliq <- cliques(g_informal, min=6)
print(cliq)

### 중첩되지 않은 결속집단 찾기  
max_cliques(g_informal, min=3)

### 중첩되지 않은 결속집단의 갯수 세기 
count_max_cliques(g_informal, min=3)

### 가장 큰 결속집단 찾기
largest_clique <- largest_cliques(g_informal)
print(largest_clique)

### g_informal 그래프에서 largest_clique 찾아서 그래프로 그리기
largest_clique <- induced_subgraph(g_informal, largest_clique[[1]])
plot(largest_clique, vertex.shape = 'none', 
     layout=layout.kamada.kawai(largest_clique))

## k-cores
### 방향성없는 연결망으로 변환하기 
g_informal <-  graph_from_data_frame(net_informal, directed = FALSE)
g_informal_s <- simplify(g_informal)

### k-core 개체 추출
kcore <- coreness(g_informal_s)    
print(kcore)

### k-core 시각화하기 
V(g_informal_s)$core <- kcore      # k-cores 값을 노드 속성 core로 추가 
plot(g_informal_s, vertex.color=V(g_informal_s)$core+1, 
     vertex.label.cex = 0.7) # 시각화

## 군집 분석
# 유사성 행렬의 예시 자료 불러들이기
att <- read.csv('files/company_c_att.csv')
# 데이터프래임 형테어서 메트릭스 형태로 저장
(att_matrix <- as.matrix(att))

c_dist <- dist(1- cor(t(att_matrix)))
hc <- hclust(c_dist, method = 'complete')
plot(hc)

# 군집을 2~5개로 나눠보기
g25 <- cutree(hc, k = 2:5)
g25
table(grp2 = g25[, '2'], grp5 = g25[, '5'])

## 구성집단
### 강한 구성집단 
components(g_informal, mode = 'strong')

### 약한 구성집단 membership
components(g_informal, mode = 'weak')$membership

### 2중 구성집단
biconnected_components(g_informal)

## Cohesive block
cohesive_blks <- cohesive_blocks(g_informal_s)
cohesive_blks
plot(cohesive_blks, g_informal_s, vertex.label=V(g_informal_s)$name, 
      vertex.size=24, vertex.size2=8, vertex.label.cex = 0.7,
      mark.border=1, colbar=c(NA, 'cyan','orange') )

## 커뮤니티 탐색
### igraph 함수 중 "cluster_"로 시작하는 함수들을 리스트로 저장하기 
clusters <- grep("^cluster_", ls("package:igraph"), value=TRUE)

### 위의 리스트에서 특정 함수명 제거하기 
clusters <- clusters[!grepl("fluid_communities|cluster_optimal", 
                        clusters)]

### layout_with_fr로 레이아웃 고정시켜기 
l = layout_with_fr(g_informal_s)

### 이미지 분할(3*3) 및 여백 설정 
par(mfrow=c(3,3), mar=c(1,1,1,1))

### cluster_ 함수 반복 실행 및 시각화 
for(cluster in clusters) {
  cluster_out <- do.call(cluster, list(simplify(g_informal_s))) 
  plot(cluster_out, 
        g_informal, vertex.shape="none",  
        edge.arrow.mode=0, 
        layout=l, 
        main=paste0(cluster,"(",max(cluster_out$membership), ")")) }
dev.off()

### 덴드로그램으로 커뮤니티 표현하기 
graph_cl <- cluster_fast_greedy(simplify(g_informal_s))
plot_dendrogram(graph_cl)


