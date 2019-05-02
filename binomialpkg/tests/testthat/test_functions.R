context("test binomial package")

##2 TESTS

#checkers
#check_prob
test_that("check if check probability works", {
  expect_true(check_prob(0.5), TRUE)
  expect_length(check_prob(0.5), 1)
  expect_error(check_prob(-1), "p has to be a number from 0 to 1" )
})

#check_trials
test_that("check if check  trials works", {
  expect_true(check_trials(10), TRUE)
  expect_error(check_trials(-5), "invalid trials value")
  expect_length(check_trials(10), 1)

})

#check_success
test_that("check if check success works", {
  expect_true(check_success(10, 20), TRUE)
  expect_error(check_success(20, 10), "success cannot be greater than trials")
  expect_length(check_success(10,20), 1)
})

#aux_mean
test_that("check if aux mean works", {
  expect_equal(aux_mean(10, 0.5), 5)
  expect_length(aux_mean(10, 0.2), 1)
  expect_type(aux_mean(10, 0.2), 'double')
})

#aux_variance

test_that("check it aux variance works", {
  expect_equal(aux_variance(10, 0.5), 2.5)
  expect_length(aux_variance(10, 0.2), 1)
  expect_type(aux_variance(10, 0.1), 'double')
})

#aux_mode

test_that("check if aux mode works", {
  expect_equal(aux_mode(10, 0.5), 5)
  expect_length(aux_mode(10, 0.2), 1)
  expect_type(aux_mode(10, 0.1), 'double')
})


#aux skewness
test_that("check if aux skewness works", {
  expect_equal(aux_skewness(10, 0.3), (1-2*0.3)/(10*0.3*(1-0.3))^0.5)
  expect_length(aux_skewness(10, 0.2), 1)
  expect_type(aux_skewness(10, 0.1), 'double')
})



#aux kurtosis
test_that("check if aux skewness works", {
  expect_equal(aux_kurtosis(10, 0.3), ((1-6*0.3*(1-0.3))/((10*0.3)*(1-0.3))))
  expect_length(aux_kurtosis(10, 0.2), 1)
  expect_type(aux_kurtosis(10, 0.1), 'double')
})

#bin choose
test_that("check if bin choose works",{
  expect_error(bin_choose(10, 20), "k cannot be greater than n")
  expect_length(bin_choose(10, 5), 1)
  expect_equal(bin_choose(10,2), 45)
})

#bin probability
test_that("check if bin probability works", {
  expect_length(bin_probability(2, 10, 0.5), 1 )
  expect_equal(bin_probability(2,10,0.5),bin_choose(n = 10, k = 2)*0.5^2*(1-0.5)^(10-2))
  expect_type(bin_probability(2,10,0.5), 'double')
})


#bin_distribution
test_that("check if bin distribution works", {
  expect_type(bin_distribution(10,0.5), "list")
  expect_length(bin_distribution(10,0.5), 2)
  expect_equivalent(bin_distribution(10,0.5), (data.frame(success = 0:10, probability = (bin_probability(success = 0:10, trials = 10,prob = 0.5)))))
})

#bin_cumulative
test_that("check if bin cumulative works", {
  expect_type(bin_cumulative(10,0.5), "list")
  expect_length(bin_cumulative(10, 0.5), 3)
  expect_equivalent(bin_cumulative(10,0.5), (data.frame(success = 0:10, probability = (bin_probability(success = 0:10, trials = 10, prob = 0.5)), cumulative = cumsum(bin_probability(success = 0:10, trials = 10, prob = 0.5)))))
})

