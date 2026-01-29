#---- fmt_splg_admin_newborn -------------------------------------------------

test_that("fmt_splg_admin_newborn errors on non-dataframe input or missing columns", {
  expect_error(fmt_splg_admin_newborn(c(1, 2, 3)))
  expect_error(fmt_splg_admin_newborn(data.frame(
    fall_id = 1234,
    alter = 62
  )))
})


test_that("fmt_splg_admin_newborn formats correctly", {
  # Test data
  admin_test_data <-
    data.frame(
      fall_id = c(1234L, 5678L),
      spital_id = c(71291845L, 71234865L),
      plz = c(8001L, 6430L),
      standort = c(1, 2),
      wohnkanton = c('ZH', 'LU'),
      abc_fall = rep('A', 2),
      tarif = rep(1L, 2),
      alter = c(34L, 63L),
      alter_U1 = NA,
      beatmung = NA,
      austrittsdatum = c(20240331, 20241015)
    )
  newborn_test_data = data.frame(
    fall_id = NA,
    gestationsalter2 = NA,
    geburtsgewicht = NA
  )

  result_text <- fmt_splg_admin_newborn(
    admin = admin_test_data,
    newborn = newborn_test_data,
    type = 'group',
    format = 'TEXT'
  )
  # result_xml <- fmt_splg_admin_newborn(admin_test_data, newborn_test_data, type = 'group', format = 'XML')
  result_json <- fmt_splg_admin_newborn(
    admin_test_data,
    newborn_test_data,
    type = 'group',
    format = 'JSON'
  )

  # 3. Assertions
  expect_vector(result_text$admin, ptype = character(), size = 2)
  # expect_vector(result_xml$admin, ptype = character(), size = 2)
  expect_vector(result_json$admin, ptype = character(), size = 2)

  expect_equal(
    result_text$admin[1],
    'fallid=1234;agey=34;austritt=20240331'
  )
  # expect_equal(result_xml$admin[1], '')
  expect_equal(
    result_json$admin[2],
    '[{\"fallid\":"5678",\"agey\":"63",\"austritt\":"20241015"}]'
  )

  expect_error(fmt_splg_admin_newborn(
    admin_test_data,
    newborn_test_data,
    type = 'group',
    format = 'XML'
  ))
})


#---- fmt_splg_diag -------------------------------------------------
test_that("fmt_splg_diag errors on non-dataframe input or missing columns", {
  expect_error(fmt_splg_diag(c(1, 2, 3)))
  expect_error(fmt_splg_diag(data.frame(diagnose_id = 1, diagnose_kode = 'A')))
})


test_that("fmt_splg_diag format diagnoses correctly", {
  # Test data
  diag_test_data <-
    data.frame(
      fall_id = rep(1234, 2),
      diagnose_id = 1:2,
      diagnose_kode = c('C541', 'K660'),
      diagnose_seitigkeit = NA,
      diagnose_zusatz = c('C99', NA)
    )

  result_text <- fmt_splg_diag(diag_test_data, format = 'TEXT')
  # result_xml <- fmt_splg_diag(diag_test_data, format = 'XML')
  result_json <- fmt_splg_diag(diag_test_data, format = 'JSON')

  # 3. Assertions
  expect_vector(result_text$diagnosen, ptype = character(), size = 1)
  # expect_vector(result_xml$diagnosen, ptype = character(), size = 1)
  expect_vector(result_json$diagnosen, ptype = character(), size = 1)

  expect_equal(result_text$diagnosen, 'C541;C99;K660')
  # expect_equal(result_xml$diagnosen, '<diagnose code="C541" zusatz="C99" rang="0"/>')
  expect_equal(
    result_json$diagnosen,
    '[{"rang":"1","code":"C541","zusatz":"C99"},{"rang":"2","code":"K660"}]'
  )

  expect_error(fmt_splg_diag(diag_test_data, format = 'XML'))
})

test_that("fmt_splg_diag orders diagnoses correctly (in output-formt TEXT)", {
  # Test data
  diag_test_data <-
    data.frame(
      fall_id = rep(1234, 2),
      diagnose_id = c(2L, 1L),
      diagnose_kode = c('C541', 'K660'),
      diagnose_seitigkeit = NA,
      diagnose_zusatz = c(NA, NA)
    )

  result_text <- fmt_splg_diag(diag_test_data, format = 'TEXT')
  expect_equal(result_text$diagnosen, 'K660;C541')
})

#---- fmt_splg_proc -------------------------------------------------

test_that("fmt_splg_proc errors on non-dataframe input or missing columns", {
  expect_error(fmt_splg_proc(c(1, 2, 3)))
  expect_error(fmt_splg_proc(data.frame(
    behandlung_id = 1,
    behandlung_chop = 'A'
  )))
})


test_that("fmt_splg_proc format diagnoses correctly", {
  # Test data
  proc_test_data <-
    data.frame(
      fall_id = rep(1234, 2),
      behandlung_id = 1:2,
      behandlung_chop = c('6861', '6541'),
      behandlung_seitigkeit = c(NA_integer_, 0L),
      behandlung_beginn = c('2018041915', '20180419'),
      behandlung_auswaerts = NA_integer_
    )

  result_text <- fmt_splg_proc(proc_test_data, format = 'TEXT')
  # result_xml <- fmt_splg_proc(proc_test_data, format = 'XML')
  result_json <- fmt_splg_proc(proc_test_data, format = 'JSON')

  # 3. Assertions
  expect_vector(result_text$behandlungen, ptype = character(), size = 1)
  # expect_vector(result_xml_group$diagnosen, ptype = character(), size = 1)
  expect_vector(result_json$behandlungen, ptype = character(), size = 1)

  expect_equal(
    result_text$behandlungen,
    '6861:::20180419;6541:0::20180419'
  )
  # expect_equal(result_xml_group$diagnosen, '<diagnose code="C541" zusatz="C99" rang="0"/>')
  expect_equal(
    result_json$behandlungen,
    '[{"rang":"1","code":"6861","seitigkeit":"","beginn":"20180419","ambext":""},{"rang":"2","code":"6541","seitigkeit":"0","beginn":"20180419","ambext":""}]'
  )

  expect_error(fmt_splg_proc(proc_test_data, format = 'XML'))
})

test_that("fmt_splg_proc orders diagnoses correctly (in output-formt TEXT)", {
  # Test data
  proc_test_data <-
    data.frame(
      fall_id = rep(1234, 2),
      behandlung_id = c(2L, 1L),
      behandlung_chop = c('6541', '6861'),
      behandlung_seitigkeit = c(0L, NA_integer_),
      behandlung_beginn = c('20180419', '2018041915'),
      behandlung_auswaerts = NA_integer_
    )

  result_text <- fmt_splg_proc(proc_test_data, format = 'TEXT')
  expect_equal(
    result_text$behandlungen,
    '6861:::20180419;6541:0::20180419'
  )
})

#---- fmt_splg -------------------------------------------------

test_that("fmt_splg errors on non-dataframe input or missing columns", {
  expect_error(fmt_splg(c(1, 2, 3)))
  expect_error(fmt_splg(data.frame(
    fall_id = 1234,
    alter = 62
  )))
})


test_that("fmt_splg formats correctly", {
  # Test data
  spiges_test_data = list(
    admin = data.frame(
      fall_id = c(1234L, 5678L),
      spital_id = c(71291845L, 71234865L),
      plz = c(8001L, 6430L),
      wohnkanton = c('ZH', 'LU'),
      abc_fall = rep('A', 2),
      tarif = rep(1L, 2),
      alter = c(34L, 63L),
      alter_U1 = NA,
      beatmung = NA,
      austrittsdatum = c(20240331, 20241015)
    ),
    newborn = data.frame(
      fall_id = NA,
      gestationsalter2 = NA,
      geburtsgewicht = NA
    ),
    diag = data.frame(
      fall_id = rep(1234, 2),
      diagnose_id = 1:2,
      diagnose_kode = c('C541', 'K660'),
      diagnose_seitigkeit = NA,
      diagnose_zusatz = c('C99', NA)
    ),
    proc = data.frame(
      fall_id = rep(1234, 2),
      behandlung_id = 1:2,
      behandlung_chop = c('6861', '6541'),
      behandlung_seitigkeit = c(NA_integer_, 0L),
      behandlung_beginn = c('2018041915', '20180419'),
      behandlung_auswaerts = NA_integer_
    )
  )
  class(spiges_test_data) <- c("spiges_data", class(spiges_test_data))

  result_group_text <- fmt_splg(
    spiges_test_data,
    type = 'group',
    format = 'TEXT'
  )
  # result_group_xml <- fmt_splg(spiges_test_data, type = 'group', format = 'XML')
  result_group_json <- fmt_splg(
    spiges_test_data,
    type = 'group',
    format = 'JSON'
  )

  # 3. Assertions
  expect_vector(result_group_text, ptype = character(), size = 7)
  # expect_vector(result_group_xml, ptype = character(), size = 1)
  expect_vector(result_group_json, ptype = character(), size = 1)

  expect_equal(
    result_group_text,
    c(
      'SPLG-INPUT',
      'fallid=1234;agey=34;austritt=20240331',
      'ICD C541;C99;K660',
      'CHOP 6861:::20180419;6541:0::20180419',
      'fallid=5678;agey=63;austritt=20241015',
      'ICD ',
      'CHOP '
    )
  )

  # expect_equal(result_group_xml, '')
  expect_equal(
    result_group_json,
    '{"splg-json":[{"fallid":"1234","agey":"34","austritt":"20240331",[{"rang":"1","code":"C541","zusatz":"C99"},{"rang":"2","code":"K660"}],[{"rang":"1","code":"6861","seitigkeit":"","beginn":"20180419","ambext":""},{"rang":"2","code":"6541","seitigkeit":"0","beginn":"20180419","ambext":""}]},{"fallid":"5678","agey":"63","austritt":"20241015"}]}'
  )

  expect_error(fmt_splg(spiges_test_data, type = 'group', format = 'XML'))
})
