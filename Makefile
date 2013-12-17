PROJECT = bomber

DEPS = cowboy mochijson2
dep_cowboy = pkg://cowboy master
dep_mochijson2 = https://github.com/kilnyy/mochijson2.git master
include ./erlang.mk
