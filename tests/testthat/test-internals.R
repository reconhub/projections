test_that("Errors are thrown when they should", {
    skip_on_cran()
    
    expect_error(projections:::assert_reporting("asasd"),
                 "reporting is not numeric")
    
    expect_error(projections:::assert_reporting(Inf),
                 "reporting is not a finite value")

    expect_error(projections:::assert_reporting(NA_integer_),
                 "reporting is not a finite value")
    
    expect_error(projections:::assert_reporting(0),
                 "reporting <= 0")
    
    expect_error(projections:::assert_reporting(-123),
                 "reporting <= 0")
    
    expect_error(projections:::assert_reporting(1.1),
                 "reporting > 1")
    
})

