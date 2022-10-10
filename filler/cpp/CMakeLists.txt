# ---- Declare executable ----

add_executable(mtfiller src/main.cpp)
add_executable(cpp::exe ALIAS mtfiller)

set_target_properties(
    mtfiller PROPERTIES
    OUTPUT_NAME mtfiller
    EXPORT_NAME exe
)

target_compile_features(mtfiller PRIVATE cxx_std_17)
