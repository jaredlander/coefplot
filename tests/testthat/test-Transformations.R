context("Transformations return correct reults")

test_that("invlogit returns numeric", {
    expect_is(invlogit(3), 'numeric')
    expect_is(invlogit(-6:6), 'numeric')
})

test_that('invlogit generates correct numbers', {
    expect_equal(round(invlogit(3), 3), 0.953)
    expect_equal(invlogit(-6:6), 
                 c(0.002472623, 0.006692851, 0.017986210, 0.047425873, 0.119202922, 0.268941421, 
                   0.500000000, 0.731058579, 0.880797078, 0.952574127, 0.982013790, 0.993307149, 
                   0.997527377))
})