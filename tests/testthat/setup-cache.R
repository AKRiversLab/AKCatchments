# Tests never touch a real cache, even when one is set for the session (the live tests' Z02)
options(AKCatchments.cache = file.path(tempdir(), "ak_test_cache"))
