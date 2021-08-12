# Extra macros to eliminate repetition
  
# Macro to link list of files from source to destination
macro( LINK_FILES filelist src_dir dst_dir )
  foreach(FILENAME ${filelist})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
      ${src_dir}/${FILENAME}
      ${dst_dir}/${FILENAME}
    )
  endforeach(FILENAME)
endmacro()
