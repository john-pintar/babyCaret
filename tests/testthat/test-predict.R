 treeMod <- train(cars$speed, cars, "tree", tuneTree())
 kProtoMod <- train(iris, "kproto", tuneKproto())
 #apriMod <- suppressWarnings(cars, "apriori", tuneApriori())

#testing predict.trainedTree()
test_that("predict.tree() returns proper classes",
           {
               expect_equal(class(predict(treeMod, cars)), "data.frame")
               expect_equal(class(predict(treeMod, cars, FALSE)), "numeric")
           })

#testing predict.trainedKproto()
test_that("predict.trainedKproto returns proper classes",
          {
              expect_equal(class(predict(kProtoMod, iris)), "data.frame")
              expect_equal(class(predict(kProtoMod, iris, FALSE)), "integer")
          })

#testing predict.trainedApriori()
#test_that("predict.trainedApriori() errors",
#          {
#              expect_error(predict(apriMod, cars))
#          })

