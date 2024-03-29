# first we can indicate the documentation build as an option and
# set it to OFF by default since documenation will usually be
# built at the bundle level
option(BUILD_UCLDAS_DOC "Build UCLDAS documentation" OFF)

if (BUILD_UCLDAS_DOC)
    message("UCLDAS Documentation Requested")
    find_package(Doxygen)
    if (DOXYGEN_FOUND)
        set(DOXYGEN_IN ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in)
        set(DOXYGEN_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)
        configure_file(${DOXYGEN_IN} ${DOXYGEN_OUT} @ONLY)
        add_custom_target( doc_doxygen ALL
            COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYGEN_OUT}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "Generating UCLDAS documentation with Doxygen"
            VERBATIM )
    else (DOXYGEN_FOUND)
        message("Doxygen need to be installed to generate UCLDAS documentation")
    endif (DOXYGEN_FOUND)
endif (BUILD_UCLDAS_DOC)
