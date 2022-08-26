

library(batchtools)


if (!dir.exists(paths = "/scratch/maom_root/maom99/maom/SARS-CoV-2_iAEC2_Combo")) {
    cat("Creating work direcotry '/scratch/maom_root/maom99/maom/SARS-CoV-2_iAEC2_Combo'\n")
    dir.create("/scratch/maom_root/maom99/maom/SARS-CoV-2_iAEC2_Combo")
}

if (!dir.exists(paths = "intermediate_data/batchtools_registry")) {
    batchtools_registry <- batchtools::makeRegistry(
        file.dir = "intermediate_data/batchtools_registry",
        work.dir = "/scratch/maom_root/maom99/maom/SARS-CoV-2_iAEC2_Combo",
        conf.file = "scripts/batchtools.conf.R",
        seed = 22336)
} else {
    batchtools_registry <- batchtools::loadRegistry(
        file.dir = "intermediate_data/batchtools_registry",
        work.dir = "/scratch/maom_root/maom99/maom/SARS-CoV-2_iAEC2_Combo",
        conf.file = "scripts/batchtools.conf.R",
        writeable = TRUE)
}

batchtools::saveRegistry(
    reg = batchtools_registry)



# test
# https://mllg.github.io/batchtools/articles/batchtools.html#setup
piApprox = function(n) {
  nums = matrix(runif(2 * n), ncol = 2)
  d = sqrt(nums[, 1]^2 + nums[, 2]^2)
  4 * mean(d <= 1)
}
batchtools::batchMap(fun = piApprox, n = rep(1e5, 10))
#batchtools::clearRegistry()
batchtools::submitJobs()
batchtools::getStatus()
