context("submit_models")

describe("submitting models is structurally correct", {
    it("gives proper defaults", {
        manually_checked <-
            list(structure(
                list(
                    id = 0,
                    status = "QUEUED",
                    model_info = structure(
                        list(
                            model_path = "path/to/model.mod",
                            run_settings = structure(
                                list(
                                    git = FALSE,
                                    save_exe = "",
                                    verbose = FALSE,
                                    debug = FALSE,
                                    clean_lvl = 1,
                                    copy_lvl = 1,
                                    cache_dir = "",
                                    exe_name_in_cache = "",
                                    nm_executable_or_path = "nmfe74",
                                    one_est = FALSE,
                                    proposed_run_dir = ""
                                ),
                                .Names = c(
                                    "git",
                                    "save_exe",
                                    "verbose",
                                    "debug",
                                    "clean_lvl",
                                    "copy_lvl",
                                    "cache_dir",
                                    "exe_name_in_cache",
                                    "nm_executable_or_path",
                                    "one_est",
                                    "proposed_run_dir"
                                )
                            )
                        ),
                        .Names = c("model_path", "run_settings")
                    ),
                    run_info = structure(
                        list(
                            queue_time = 0,
                            start_time = 0,
                            duration = 0,
                            run_dir = "",
                            error = ""
                        ),
                        .Names = c(
                            "queue_time",
                            "start_time",
                            "duration",
                            "run_dir",
                            "error"
                        )
                    )
                ),
                .Names = c("id",
                           "status", "model_info", "run_info")
            ))
        expect_equal(submit_models(NULL, "path/to/model.mod", .no_submit = TRUE), manually_checked)
    })
})