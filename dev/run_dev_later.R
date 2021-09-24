loops <- new.env()

run_dev_later <- function(){
  hash <- digest::digest(
    file.info(
      list.files(
        path = c("R/", "inst"),
        recursive = TRUE,
        full.names = TRUE
      )
    )[, "mtime"]
  )
  logs <- tempfile()
  proc <- processx::process$new(
    stderr = logs,
    stdout = logs,
    "Rscript", c(
      "-e",
      "golem::run_dev()"
    )
  )
  Sys.sleep(5)
  readLines(logs)
  proc$kill()

  run_dev_now <- function(){

    new_hash <- digest::digest(
      file.info(
        list.files(
          path = c("R/", "inst"),
          recursive = TRUE,
          full.names = TRUE
        )
      )[, "mtime"]
    )
    if (new_hash != hash){
      cli::cat_rule("Relaunching the app")
      hash <<- new_hash
      proc$interrupt()
      proc <<- processx::process$new(
        "Rscript", c(
          "-e",
          "golem::run_dev()"
        )
      )
      Sys.sleep(5)
    }
    later::later(
      run_dev_now,
      1,
      loop = loops$run_dev
    )
  }
  loops$run_dev <- later::create_loop()
  run_dev_now()
}

# When done
#later::destroy_loop(
#  loops$run_dev
#)
