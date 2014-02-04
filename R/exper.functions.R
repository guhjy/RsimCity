update.simCity.experiment<-function(exper,model) {
  exper$model<-model
  exper
}
"exper.n<-"<-function(exper,...) {
  arg<-list(...)
  exper$N<-arg$value
  exper
}
"exper.rep<-"<-function(exper,...) {
  arg<-list(...)
  exper$rep<-arg$value
  exper
}

coefs<-function(exper) {
  structure(exper,class="simCity.coefs")
}
