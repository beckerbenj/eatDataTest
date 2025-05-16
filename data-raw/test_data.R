# test repo yamls
create_data_yaml(.path = test_path("helper_example_repo"), name = "helper_data1", version = "v2.3",
                 release_path = "data_files/example_data.sav", oldrel_path = "data_files/example_data2.sav")
create_data_yaml(.path = test_path("helper_example_repo"), name = "helper_data2", version = "v1.2",
                 release_path = "data_files/example_data.RDS", oldrel_path = "data_files/example_data2.RDS")
create_data_yaml(.path = test_path("helper_example_repo"), name = "helper_data3", version = "v1.2",
                 release_path = "data_files/example_data.RData", oldrel_path = "data_files/example_data2.RData")



# test repo sav and RDS files
sav_gads <- eatGADS::import_spss("tests/testthat/helper_example_repo/data_files/example_data.sav")
sav_gads2 <- eatGADS::import_spss("tests/testthat/helper_example_repo/data_files/example_data2.sav")

saveRDS(sav_gads, "tests/testthat/helper_example_repo/data_files/example_data.RDS")
saveRDS(sav_gads2, "tests/testthat/helper_example_repo/data_files/example_data2.RDS")

save(sav_gads, file = "tests/testthat/helper_example_repo/data_files/example_data.RData")
save(sav_gads2, file = "tests/testthat/helper_example_repo/data_files/example_data2.RData")

saveRDS(mtcars, "tests/testthat/helper_example_repo/data_files/example_df.RDS")
