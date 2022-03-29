# # Weights used in `two_class_example` tests
# save_weights_two_class_example <- function(seed) {
#   withr::with_seed(seed, {
#     weights_two_class_example <- runif(
#       n = nrow(two_class_example),
#       min = 0,
#       max = 100
#     )
#   })
#
#   saveRDS(
#     object = weights_two_class_example,
#     file = test_path("data/weights-two-class-example.rds"),
#     version = 2
#   )
# }
#
# save_weights_two_class_example(12345)

read_weights_two_class_example <- function() {
  readRDS(test_path("data/weights-two-class-example.rds"))
}

# # Weights used in `hpc_cv` tests
# save_weights_hpc_cv <- function(seed) {
#   withr::with_seed(seed, {
#     weights_hpc_cv <- runif(
#       n = nrow(hpc_cv),
#       min = 0,
#       max = 100
#     )
#   })
#
#   saveRDS(
#     object = weights_hpc_cv,
#     file = test_path("data/weights-hpc-cv.rds"),
#     version = 2
#   )
# }
#
# save_weights_hpc_cv(4321)

read_weights_hpc_cv <- function() {
  readRDS(test_path("data/weights-hpc-cv.rds"))
}

