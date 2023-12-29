
#' @importFrom lobstr obj_addr
NULL

C <- R6::R6Class(
  'C',
  public = list(
    n = 1,
    c = 'a'
  )
)

C_self_ref <- R6::R6Class(
  'C_self_ref',
  public = list(
    n = 1,
    c = 'a',
    another = NULL
  )
)


test_that('instance in vector', {

  c1 <- C$new()
  c2 <- C$new()
  v <- c(c1, c2)

  expect_true(is_in(c1, v))

})

test_that('instance not in vector', {

  c1 <- C$new()
  c2 <- C$new()
  v <- c(c2)

  expect_false(is_in(c1, v))

})

test_that('instance in list', {

  c1 <- C$new()
  c2 <- C$new()
  v <- list(c1, c2)

  expect_true(is_in(c1, v))

})

test_that('instance not in list', {

  c1 <- C$new()
  c2 <- C$new()
  v <- list(c2)

  expect_false(is_in(c1, v))

})

test_that('shallow cloned instance', {

  c1 <- C$new()
  c2 <- c1$clone()
  v <- list(c1)

  expect_false(is_in(c2, v))

})

test_that('deep cloned instance', {

  c1 <- C$new()
  c2 <- c1$clone(deep = TRUE)
  v <- list(c1)

  expect_false(is_in(c2, v))

})

test_that('shallow cloned self_ref', {

  c1 <- C_self_ref$new()
  c1$another <- C_self_ref$new()
  c2 <- c1$clone()
  v <- list(c1)

  expect_true(lobstr::obj_addr(c1$another) == lobstr::obj_addr(c2$another))
  expect_false(is_in(c2, v))

})

test_that('deep cloned self_ref', {

  c1 <- C_self_ref$new()
  c1$another <- C_self_ref$new()
  c2 <- c1$clone(deep = TRUE)
  v <- list(c1)

  expect_true(lobstr::obj_addr(c1$another) != lobstr::obj_addr(c2$another))
  expect_false(is_in(c2, v))

})

test_that('empty vector', {

  c1 <- C$new()
  v <- c()

  expect_false(is_in(c1, v))

})

test_that('empty list', {

  c1 <- C$new()
  v <- list()

  expect_false(is_in(c1, v))

})
