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
        "//c-toxcore",
        "@haskell_data_default_class//:data-default-class",
    ],
)

haskell_binary(
    name = "groupbot",
    srcs = ["tools/groupbot/Main.hs"],
    compiler_flags = ["-ltoxcore"],
    prebuilt_dependencies = [
        "base",
        "bytestring",
        "directory",
    ],
    src_strip_prefix = "tools/groupbot",
    deps = [
        ":hs-toxcore-c",
        "@haskell_base16_bytestring//:base16-bytestring",
    ],
)
