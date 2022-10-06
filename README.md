# micrntwk
## 安装：devtools::install_github("wqssf102/micrntwk")
&emsp;&emsp;以***Hmisc***包的rcorr函数计算spearman相关系数或minerva包的mine函数计算变量间的连接强度.根据所给阈值筛选变量，将变量之间的关系矩阵转为igraph包的对象，并计算网络拓扑特征。特别的，以qgraph包的qgraph.layout.fruchtermanreingold函数获取网络节点和边的坐标，同时根据用户输入的模块参数，计算每一个模块所含节点数目占总节点的百分比，最后使用ggplot2绘图.  
&emsp;&emsp;以calcor函数计算变量之间的关系系数，可以查看calcor函数的帮助文档:
```{r,echo=TRUE}
?calcor
codt	数据框矩阵，一般为物种矩阵
r_th	筛选的R值阈值
p_th	筛选的P值阈值，当type为mine时，无P值
type	可选"cor"或"mine"，前者为spearman，后者为最大信息系数
n.cores	当type为mine时起作用，即多线程计算的线程数目
group	若想分组计算变量的关系，给出分组信息即可(为2列，列名分别为sampleid和group)
如：
crores <- calcor(codt = cordt,group = NULL,type = "cor",r_th = 0.6,p_th = 0.05,n.cores = 4)
```  
calcor返回的结果包含了3个结果：
gg：igraph对象的变量关系，即网络图；
corgg：变量之间关系的长型数据；
modularity：网络图的模块化系数；
以上结果可用\$提取，如crores$modularity.  
&emsp;&emsp;使用get_node_edg函数获得节点和边的坐标位置，及x和y，
```
##nc为自然连通度
ndedinf <- get_node_edg(ggdt = crores,nc = FALSE)
head(ndedinf$node_inf)
################如下，V1、V2为节点的X、Y坐标，md_name为节点所属模块，mdper为当前模块所有节点所占网络总节点的百分比.
##          V1         V2        node_name Degree md_name mdper
## 1 257.39851 -276.67336      Pyrodictium    162       2 43.82
## 2 289.56449  157.01880   Sulfodiicoccus     76       2 43.82
## 3 302.62306 -144.04266       Halorubrum    150       2 43.82
## 4 -49.07161  184.37954     Methanocella     65       2 43.82
## 5 211.91364   70.17355   Methanoculleus    118       2 43.82
## 6  64.77596  246.30766 Methanococcoides     62       2 43.82
```  
&emsp;&emsp;ndedinf里包含了节点和边的坐标信息，可用\$提取，如ndedinf$node_inf，提取的结果可直接用于ggplot2绘图.  
&emsp;&emsp;提取子网络,当crores含有group时，使用get_mult_sb_nt(ggdt = crores,asvdt = cordt)，
当不含group时，使用get_single_sb_nt(gg = crores$gg,asvdt = as.data.frame(t(cordt))) .  
&emsp;&emsp;使用net_pro_sub函数计算网络拓扑特征.
```
sbnt <- get_single_sb_nt(gg = crores$gg,asvdt = as.data.frame(t(cordt)))
ntpr <- net_pro_sub(netlist = sbnt)
```  
ntpr中包含子网络的拓扑参数，以inv开头的指标，是指去掉inv之后剩下的指标名的倒数，如inv_average.path.length就是average.path.length（平均最短路径）的倒数.
这里取倒数 的目的为使所有指标都是“朝着同一方向”，方便描述网络的复杂性.  
&emsp;&emsp;绘制网络图，可使用plot_net函数，包含参数为:  
```
ex_edg 是否展示网络图的边 
per_modules 将节点个数占比低于所给阈值（如30%）的模块统一归类 ex_sp_num 展示多少个物种，若不想展示，设置为0即可 
grpnm_position 当含有多个组的网路图时，该参数课控制组名显示的位置，“ld”, “lt”, “rd”, “rt”分别为左下、左上、右下、右上 
grpnm_position_num 当含有多个组的网路图时，微调组名位置，如c(1,0.85)为X坐标不变、Y坐标缩小0.85
```   
&emsp;&emsp;plot_net函数包含3个结果，为网络图、节点和边的信息,由/$提取，节点和边的信息可提取出来自由绘图:  
```
ptres <-  plot_net(netwkinf = ndedinf,ex_edg = T,per_modules = 0,ex_sp_num = 0,point_size = "dg",grpnm_position = "ld")
ptres$plt
```   
也可以提取边和节点的信息，使用ggplot2绘图:  
```
nddt <- ptres$xyinf_node
edgt <- ptres$xyinf_edge
####下面的图没有展示边
  ggplot()+
  geom_curve(data = edgt,aes(x = V1.x,y=V2.x,
                                      xend=V1.y,yend=V2.y,
                                      color=mdcol),
             size = 0.5,curvature = -0.1,show.legend = FALSE)+
  geom_point(data = nddt,aes(x=V1,y=V2,size=Degree,fill=factor(mdcol)),shape=21)+
  # scale_fill_manual(values = c("1 (56.78%)"="red",......))+####修改模块颜色
  labs(x=NULL,y=NULL,fill="Module")+
  scale_y_continuous(breaks = NULL)+
  scale_x_continuous(breaks = NULL)+
  guides(fill=guide_legend(title = "Module",override.aes = list(size=4)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.text =  element_text(size = 20,colour = "black"),
        text = element_text(family = "serif"))

```  
&emsp;&emsp;若是需要同时计算、绘制多个网络图:
```
##构造一个分组文件，可预先整理好，然后读取即可
grpdt <- data.frame(sampleid=row.names(cordt),
                    group=c("CK", "CK", "CK", "CK",  "CK", "CK", 
                            "NP", "NP", "NP", "NP", "NP", "NP"))
row.names(grpdt) <- row.names(cordt)
##请注意type的选择
crores <- calcor(codt = cordt,type = "mine",r_th = 0.6,p_th = 0.05,n.cores = 4,group=grpdt)
ndedinf <- get_node_edg(ggdt = crores,nc = FALSE)
##plot_net包含多个参数，可查看帮助文档
ptres <-  plot_net(netwkinf = ndedinf,ex_edg = T,per_modules = 5,ex_sp_num =5,point_size = "dg",nrow = 2,node_text_size = 6,grpnm_position = "ld")
ptres$plt
```

