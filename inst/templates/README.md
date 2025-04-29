## Status Data Sets

| GADS                    | Version | Tests              |
| ----------------------- | :-----: | :----------------: |

## Workflow

[0. Call `eatDataTest::add_data()` to setup architecture for a new data set.]
1. Archive old data set version
2. Save new data set version
3. Update paths (`release`, `oldrel`) in data folder (.yaml) 
4. Update version number (`version`) in data folder (.yaml) 
5. Update changelog (.md) by describing changes made to the data set
6. Run data tests via `eatDataTest::run_data_tests()`. Update if necessary
7. Create data diff via `eatDataTest::create_diff()` (only possible if `oldrel` exists)
8. Run reverse dependency checks via `eatDataTest::run_revdep_tests()`
9. Commit and push all changes. Commit message should be (<name of the data set>: <version number> - <short description of changes>)
