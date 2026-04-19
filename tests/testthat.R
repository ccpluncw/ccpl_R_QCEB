# Entry point for testthat — run via devtools::test() or testthat::test_check().
# Individual test files live in tests/testthat/ and follow the name
# convention `test-<name>.R` (testthat auto-discovers them).

library(testthat)
library(QCEB)

test_check("QCEB")
