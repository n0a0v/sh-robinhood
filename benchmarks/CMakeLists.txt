include(FindPkgConfig)
pkg_check_modules(ABSL_FLAT_HASH_MAP absl_flat_hash_map)

if (ABSL_FLAT_HASH_MAP_FOUND)
	add_compile_definitions(ENABLE_ABSL_FLAT_HASH_MAP=1)
endif()

include_directories(
	${PROJECT_SOURCE_DIR}
	${ABSL_FLAT_HASH_MAP_INCLUDE_DIRS}
)
link_libraries(
	${ABSL_FLAT_HASH_MAP_LIBRARIES}
)
add_compile_options(
	${ABSL_FLAT_HASH_MAP_CFLAGS_OTHER}
)

add_executable(benchmark-clear
	bench_clear.cpp
	bench_utils.cpp
)
add_executable(benchmark-composite
	bench_composite.cpp
	bench_utils.cpp
)
add_executable(benchmark-copy
	bench_copy.cpp
	bench_utils.cpp
)
add_executable(benchmark-erase
	bench_erase.cpp
	bench_utils.cpp
)
add_executable(benchmark-eraseall
	bench_eraseall.cpp
	bench_utils.cpp
)
add_executable(benchmark-find
	bench_find.cpp
	bench_utils.cpp
)
add_executable(benchmark-insert
	bench_insert.cpp
	bench_utils.cpp
)
add_executable(benchmark-iterate
	bench_iterate.cpp
	bench_utils.cpp
)
