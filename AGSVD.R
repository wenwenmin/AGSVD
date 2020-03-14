##############################################################################
##############################################################################
AGSVD = function(X, rank_k=2, A, sigma, ku=50, kv=100, niter=100, err=0.0001){
  # --------------------------------------------------------------------------
  # [n,p] = dim(X), n is the number of samples and p is the number of features
  # A: PPI network of genes
  # sigma parameter
  # --------------------------------------------------------------------------
  # Output
  n = nrow(X)
  p = ncol(X)
  U = matrix(0,n,rank_k); D = rep(0,rank_k); V = matrix(0,p,rank_k)
  
  objs = c(); # L = diag(apply(A,1,sum)) - A
  
  tX = X
  for(i in 1:rank_k){
    out = rank1.AGSVD(tX, A, sigma, ku, kv, niter, err)
    U[,i] = out$u; V[,i] = out$v; D[i] = out$d; 
    
    temp_objs = norm(tX,"F")^2 - (out$ds)^2 
    
    # 2019-7-19: ||X - duv||_F^2 + sigma*|u|^T*L*|u| 
    # L = diag(apply(A,1,sum)) - A
    # sum1 = norm(tX - c(out$d)*out$u%*%t(out$v),"F")^2 
    # sum2 = sigma*t(abs(out$v))%*%L%*%abs(out$v)
    # temp_objs = sum1 + sum2
    
    objs = c(objs,temp_objs)
    
    # update X
    tX = tX - c(out$d)*out$u%*%t(out$v) 
  }
  return (list(U=U, D=D, V=V, objs = objs))
}
##############################################################################
##############################################################################
rank1.AGSVD = function(X, A, sigma, ku, kv=2, niter=1000, err=0.0001){
  # ------------------
  # Initialize u and v
  Res = svd(X,1,1)
  u0 = Res$u # v0 = v0/norm(v0,'E')
  v0 = Res$v
  
  ds = c()
  # ------------------
  # Iterative algorithm to solve u and v values
  for (i in 1:niter){
    # update u and v 
    # u = X%*%v0; u = u/norm(u,'E') # for samples
    u = L0project(X%*%v0, ku)
    v = AG_project(t(X)%*%u, v0, kv, A, sigma)  #for genes
    
    d = t(u)%*%X%*%v
    ds = c(ds,d)
    # Termination condition
    if ((norm(u - u0,'2')<= err)&(norm(v - v0,'2')<= err)){break}
    else {
      u0 = u;v0 = v}
  }
  return (list(u=u, v=v, d=d, ds=ds))
}
##############################################################################
##############################################################################
# Sparse Graph-regularized Penalty With Absolute Operator
AG_project = function(z, v0, k, A, sigma){  
  #------------------------
  v0 = abs(v0)
  v = abs(z) + sigma*A%*%v0
  v = select2(v,k)
  #------------------------
  if(sum(v^2)==0){return(rep(0,length(v)))}
  else{
    v = v/sqrt(sum(v^2))
    v = sign(z)*v
    return(v)} 
}
##############################################################################
##############################################################################
# L0 constrained SVD project function
L0project = function(z, k){  
  absz = abs(z);
  u = select2(absz,k)
  if(sum(u^2)==0){return(rep(0,length(u)))}
  else{
    u = u/sqrt(sum(u^2))
    u = sign(z)*u
    return(u)} 
}
# An auxiliary function
select2 = function(x, k){
  if(k>=length(x)) return(x)
  x[-order(x,decreasing=T)[1:k]] = 0
  return(x)
}
##############################################################################
##############################################################################
NetworkEnrichment.of.module = function(PPINetwork,mgIDs){
  # module gene IDs
  B = PPINetwork
  geneNum = dim(B)[1]
  Num.mgIDs.edges = sum(B[mgIDs,mgIDs])/2
  q = Num.mgIDs.edges-1
  m = sum(B)/2
  n = geneNum*(geneNum-1)/2-m
  k = length(mgIDs)*(length(mgIDs)-1)/2
  
  PValue = phyper(q, m, n, k, lower.tail = F)
  FC = (Num.mgIDs.edges/k)/(m/(m+n)) 
  return(c(length(mgIDs),Num.mgIDs.edges,FC,PValue))
}
##############################################################################
##############################################################################
getTable = function(out,METABRIC.gene.network){
  V1 = out$V; ds = out$D; moduleEdgeTable = NULL
  for(i in 1:ncol(V1)){
    M1.geneIDs = which(V1[,i]!=0)
    t1 = NetworkEnrichment.of.module(METABRIC.gene.network,M1.geneIDs)
    t2 = c(ds[i],t1)
    moduleEdgeTable = cbind(moduleEdgeTable,t2)
  }
  row.names(moduleEdgeTable) = c("d","numGenes","numEdges","FC","Pvalue")
  colnames(moduleEdgeTable) = paste("module",1:ncol(V1))
  print(round(moduleEdgeTable,2))
  return(moduleEdgeTable)
}
##############################################################################
##############################################################################
getNMI.avg = function(out,yanData){
  U1 = out$U[,c(1,2)] 
  res = matrix(rep(0,50),ncol=1) 
  
  for(i in 1:50){
    set.seed(i)
    cl <- kmeans(U1, length(unique(yanData$sample.group)), nstart = 1)
    t1 = data.frame(1:length(yanData$sample.group), cl$cluste)
    t2 = data.frame(1:length(yanData$sample.group), yanData$sample.group)
    res[i,1] = NMI(t1,t2)$value
  }
  return(list(avg.NMI = mean(res), NMIs = res)) 
}

getNMI.avg2 = function(out,yanData){
  U1 = out$U
  
  print(dim(U1))
  
  res = matrix(rep(0,50),ncol=1) 
  for(i in 1:50){
    set.seed(i)
    cl <- kmeans(U1, length(unique(yanData$sample.group)), nstart = 1)
    t1 = data.frame(1:length(yanData$sample.group), cl$cluste)
    t2 = data.frame(1:length(yanData$sample.group), yanData$sample.group)
    res[i,1] = NMI(t1,t2)$value
  }
  return(list(avg.NMI = mean(res), NMIs = res)) 
}
##############################################################################
##############################################################################
getFig = function(out, DataName, fileName){
  # DataName = "pollen Dataset"
  Data = data.frame(out$U[,c(1,2)]); 
  fig <- ggplot(Data, aes(X1, X2, colour = factor(yanData$sample.group))) + 
    geom_point(size=I(3)) + ggtitle(DataName) +  xlab("PC1")+ylab("PC2") + theme(legend.title=element_blank())
  
  
  fig = fig +theme(legend.text = element_text(colour = "black", angle = 0,size=12), 
                   axis.title = element_text(colour="black", size=16),
                   axis.text  = element_text(colour = "black",size=16))
  ggsave(fig, file=fileName, width=7, height=7)
}
##############################################################################
##############################################################################