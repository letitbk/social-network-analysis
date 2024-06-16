# gssnet.csv 라는 파일은 실제 GSS 자료로부터 자아중심연결망과 관련된 문항들만을 뽑아서 구성한 것이다 (실제 원자료에서 다음과 같은 long-form의 자료로 변환하였는지는 Appendix의 코드를 참조). 이 자료에서 id는 ego의 id를 의미하고, alter_id는 각 ego의 alter들의 ID를 의미한다. ego_로 시작되는 변수들은 ego의 속성을 나타내며, alter_로 시작되는 변수들은 alter의 속성을 나타낸다. 


# 입력한 자료를 읽어들임.
library(data.table)

gss_ego <- fread("files/gssnet_ego.csv")
gss_dyad <- fread("files/gssnet_dyad.csv")
gss_aaties <- fread("files/gssnet_aaties.csv")


# gss 자료를 읽기
gss_ego <- fread("files/gssnet_ego.csv")
gss_dyad <- fread("files/gssnet_dyad.csv")
gss_aaties <- fread("files/gssnet_aaties.csv")

# 자료 살펴보기
gss_ego[id %in% c(1, 11),]

# ego-alter tie 살펴보기
setorder(gss_dyad, id, alterid)
gss_dyad[id %in% c(1, 11),]

# alter-alter tie 살펴보기
setorder(gss_aaties, id, source_id, target_id)
gss_aaties[id %in% c(1, 11),]

# gss ego-r
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

summary(gss_egor_g)
