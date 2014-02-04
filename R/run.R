run.block<-function(rep,model,N,coefs,fun=.return.data) {
  arguments<-list(model=model,coefs=coefs,N=N,rep=rep)
  res<-NULL
  for (i in 1:rep) {
  dat<-make.data(model,N,coefs=coefs)
  r<-fun(dat,arguments)
  if (is.null(res)) res<-r
      else res<-rbind(res,r)  
  }
  data.frame(res)
}

run<-function(exper) {
  nn<-0
  rtot<-length(exper$coefs)*length(exper$N)
  message('running ',rtot,' blocks of the simulation')
  stopifnot(class(exper)=='simCity.experiment')
  final<-lapply(exper$N, function(NN) {
  res<-lapply(exper$coefs,function (e) {
    oneblock<-run.block(exper$rep,exper$model,NN,e,exper$fun.oneblock)
  if (!is.null(exper$fun.aggregate)) oneblock<-exper$fun.aggregate(oneblock)
  if (simc.options('verbose'))  {
    nn<<-nn+1
    message('generating:',exper$rep,' samples with N=',NN,' and coefs=',paste(e,collapse=','),' run:',nn,' of ',rtot)
  }
  oneblock
  })  
  res<-do.call('rbind',res)
  res 
})
  finaldata<-do.call('rbind',final)
  as.data.frame(finaldata,row.names = 1:dim(finaldata)[1])
}

run.debug<-function(exper) {
  first<-T
  stopifnot(class(exper)=='simCity.experiment')
  verb<-simc.options("verbose")
  simc.options(verbose=T)
  .cat(paste('running 3 levels out of ',(length(exper$coefs)*length(exper$N))))
  final<-lapply(exper$N[1], function(NN) {
    
    res<-lapply(exper$coefs[1:3],function (e) {
      if (simc.options('verbose'))  message('generating:',exper$rep,' samples with N=',NN,' and coefs=',paste(e,collapse=','))
      if (first) {
          .cat("###### data example #########")
          first<-F
          print(make.data(exper$model,NN,e))
      }
      oneblock<-run.block(exper$rep,exper$model,NN,e,exper$fun.oneblock)
      .cat('##########  one block data ##########')
      print(oneblock)
      if (!is.null(exper$fun.aggregate)) oneblock<-exper$fun.aggregate(oneblock)
      .cat('##########  one block data after processing ##########')      
      print(oneblock)
      oneblock
    })  
    res<-do.call('rbind',res)
    .cat('##########  cumulative processed block data ##########')        
    print(res)
    res 
  })
  finaldata<-do.call('rbind',final)
  simc.options(verbose=verb)
  rt<-as.data.frame(finaldata,row.names = 1:dim(finaldata)[1])
  .cat('##########  final results ##########')      
  print(rt)
  rt
}
