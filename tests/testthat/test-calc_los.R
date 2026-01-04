test_that("calculation of los works", {
  spiges_data = list(
    admin = data.frame(
      fall_id = c('AS1', 'AM1', 'AM2', 'CS1', 'CM1', 'ERR1'),
      jahr = 2025L,
      eintrittsdatum = c(
        '20250201',
        '20250101',
        '20250110',
        '20250401',
        '20250501',
        NA_character_
      ),
      austrittsdatum = c(
        '20250210',
        '20250111',
        '20250120',
        NA_character_,
        NA_character_,
        '20250313'
      ),
      admin_urlaub = 0L,
      austrittsentscheid = 1L,
      austritt_aufenthalt = 1L
    ),

    patientenbewegung = data.frame(
      fall_id = c('AM1', 'AM1', 'AM2', 'CM1'),
      episode_id = c(1L, 2L, 1L, 1L),
      episode_art = 2L,
      episode_beginn = c("20250102", "20250104", "20250112", '20250510'),
      episode_ende = c("20250103", "20250110", "20250115", '20250515'),
      wiedereintritt_aufenthalt = 1L
    )
  )
  class(spiges_data) <- c("spiges_data", class(spiges_data))

  res <- calc_los(spiges_data)

  expect_identical(
    as.integer(res[
      res$fall_id == 'AS1',
      c('episodes', 'loc', 'los_drg', 'los_stre', 'los_tpsy')
    ]),
    as.integer(c(1, 9, 9, 10, 10))
  )

  expect_identical(
    as.integer(res[
      res$fall_id == 'CS1',
      c('episodes', 'loc', 'los_drg', 'los_stre', 'los_tpsy')
    ]),
    as.integer(c(1, 274, NA, NA, NA))
  )

  expect_identical(
    as.integer(res[
      res$fall_id == 'ERR1',
      c('episodes', 'loc', 'los_drg', 'los_stre', 'los_tpsy')
    ]),
    as.integer(c(1, NA, NA, NA, NA))
  )

  expect_identical(
    as.integer(res[
      res$fall_id == 'AM1',
      c('episodes', 'loc', 'los_drg', 'los_stre', 'los_tpsy')
    ]),
    as.integer(c(3, 10, 3, 6, 6))
  )
  expect_identical(
    as.integer(res[
      res$fall_id == 'AM2',
      c('episodes', 'loc', 'los_drg', 'los_stre', 'los_tpsy')
    ]),
    as.integer(c(2, 10, 7, 9, 9))
  )
  expect_identical(
    as.integer(res[
      res$fall_id == 'CM1',
      c('episodes', 'loc', 'los_drg', 'los_stre', 'los_tpsy')
    ]),
    as.integer(c(2, 244, NA, NA, NA))
  )
})
