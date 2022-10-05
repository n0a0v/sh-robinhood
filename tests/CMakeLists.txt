set(TESTS_SRC
	test_openmap.cpp
	test_openset.cpp
	tests.cpp
)
add_executable(run-tests ${TESTS_SRC})
target_include_directories(run-tests
	PUBLIC ${PROJECT_SOURCE_DIR}
	PUBLIC ${PROJECT_SOURCE_DIR}/googletest/googletest/include
)
target_link_libraries(run-tests
	gtest
)
