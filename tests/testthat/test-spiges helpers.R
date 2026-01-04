test_that("spiges2date works", {
  expect_s3_class(spiges2date(NA_character_), 'Date')
  expect_length(spiges2date(rep('', 3)), 3)

  expect_identical(spiges2date('20250101'), as.Date('2025-01-01'))
  expect_identical(spiges2date('2025010113'), as.Date('2025-01-01'))
  expect_identical(spiges2date(2025010113L), as.Date('2025-01-01'))

  expect_identical(spiges2date(as.POSIXct('2025-01-01')), as.Date('2025-01-01'))
  expect_identical(
    spiges2date(as.POSIXct('2025-01-01 13:59:59')),
    as.Date('2025-01-01')
  )
})

test_that("spiges2datestr works", {
  expect_type(spiges2datestr(''), 'character')
  expect_length(spiges2date(rep('', 3)), 3)

  expect_identical(spiges2datestr('20250101'), '20250101')
  expect_identical(spiges2datestr('2025010113'), '20250101')
  expect_identical(spiges2datestr(2025010113L), '20250101')

  expect_identical(spiges2datestr(as.POSIXct('2025-01-01')), '20250101')
  expect_identical(
    spiges2datestr(as.POSIXct('2025-01-01 13:00:00')),
    '20250101'
  )
  expect_identical(spiges2datestr(as.Date('2025-01-01')), '20250101')
})
