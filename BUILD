load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_binary", "haskell_library")

haskell_library(
    name = "hs-toxcore-c",
    srcs = glob(["src/**/*.*hs"]),
    prebuilt_dependencies = [
        "base",
        "bytestring",
    ],
    src_strip_prefix = "src",
    visibility = ["//visibility:public"],
    deps = [
        "@haskell_data_default_class//:data-default-class",
    ],
)

# TODO(iphydf): Enable once https://github.com/tweag/rules_haskell/issues/100 is fixed.
#haskell_binary(
#    name = "groupbot",
#    srcs = ["tools/groupbot.hs"],
#    prebuilt_dependencies = [
#        "base",
#        "bytestring",
#        "directory",
#    ],
#    src_strip_prefix = "tools",
#    deps = [
#        ":hs-toxcore-c",
#        "@haskell_base16_bytestring//:base16-bytestring",
#    ],
#)
