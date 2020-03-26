# AGSVD 
### Introduction
Learning the gene co-expression pattern is a central challenge for high-dimensional gene expression analysis. Recently, the sparse singular value decomposition (SVD) has been used to achieve this goal. However, this model ignores the structural information between variables (e.g., a protein-protein interaction (PPI) network or graph). The typical graph-regularized penalty can be used to incorporate such prior graph information to achieve more accurate discovery and better interpretability. However, it fails to consider the opposite effect of variables with negative correlations. 

In this paper, we propose a novel sparse Graph-regularized SVD model with Absolute operator (AGSVD) for high-dimensional gene expression pattern discovery. The key of AGSVD is to impose a novel graph-regularized penalty (|u|L|u|). However, such a graph-regularized penalty is non-convex and thus brings new challenges on model solving. We show that the non-convex penalized problem can be efficiently solved by an alternating iterative algorithm. The simulation results on synthetic data show that our method is more effective than the existing SVD-based ones. In addition, the results on several real gene expression data show that the proposed method can discover more biologically interpretable expression patterns by incorporating the prior PPI network.


More descriptions about these functions can be found in their annotation part.

<p align="center"> 
<img src="https://github.com/wenwenmin/AGSVD/blob/master/Figure_flow_chart.png">
</p>

### R code
Note that before running the codes, please first set the path. "AGSVD.R" is the proposed method in the paper.
