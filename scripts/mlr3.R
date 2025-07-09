library(mlr3)
task = tsk("penguins")
split = partition(task)
learner = lrn("classif.rpart")

learner$train(task, row_ids = split$train)
learner$model
prediction = learner$predict(task, row_ids = split$test)
prediction
prediction$score(msr("classif.acc"))


library(mlr3verse)

tasks = tsks(c("breast_cancer", "sonar"))

glrn_rf_tuned = as_learner(ppl("robustify") %>>% auto_tuner(
  tnr("grid_search", resolution = 5),
  lrn("classif.ranger", num.trees = to_tune(200, 500)),
  rsmp("holdout")
))
glrn_rf_tuned$id = "RF"

glrn_stack = as_learner(ppl("robustify") %>>% ppl("stacking",
                                                  lrns(c("classif.rpart", "classif.kknn")),
                                                  lrn("classif.log_reg")
))
glrn_stack$id = "Stack"

learners = c(glrn_rf_tuned, glrn_stack)
bmr = benchmark(benchmark_grid(tasks, learners, rsmp("cv", folds = 3)))

bmr$aggregate(msr("classif.acc"))
