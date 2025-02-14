install.packages("mlr3verse")
install.packages("usethis")
library(mlr3)
options(repos = c(
  mlrorg = "https://mlr-org.r-universe.dev",
  CRAN = "https://cloud.r-project.org/"
))
library(mlr3)
task = tsk("penguins")
split = partition(task)
learner = lrn("classif.rpart")

learner$train(task, row_ids = split$train)
learner$model
prediction = learner$predict(task, row_ids = split$test)
prediction
prediction$score(msr("classif.acc"))
