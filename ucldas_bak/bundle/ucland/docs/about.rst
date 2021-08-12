About this documentation
========================

This readthedocs site hosts the nascent UCLAND user documentation.
UCLAND documentation is distributed over several formats and locations with each site serving a different purpose.

Here is where to find particular documentation:

Download, compile and run
  Installation documentation is in the form of user-driven (editable) wiki attached to the UCLAND-examples GitHub repository.
  Goto https://github.com/NOAA-GFDL/UCLAND-examples/wiki and look at "Getting Started".

  Installation, compilation and running are platform specific operations for which we can only provide templates (as is
  done in on the wiki) but for which UCLAND developers cannot possibly support since every platform is different. Normally
  a user needs to know where libraries (such as netcdf and MPI) and compilers are on their system but once these have
  been established the documented compile process can be adpated to the local system.

User guide
  `This site <http://ucland.readthedocs.org>`_ provides a high-level overview of the model as well as the API reference (documentation
  of source code).

  The user guide is written in reStructuredText (.rst files) that reside in ``docs/`` of the `UCLAND source code <http://github.com/NOAA-GFDL/UCLAND>`_.
  The rst files are processed by sphinx and hosted on `readthedocs <http://ucland.readthedocs.org>`_.

  The API reference is generated documentation - we use doxygen for in-code documentation. While we could use doxygen generated pages
  for all documentation it is rather cumbersome for writing, hence the use of rst files for the broader level documentation.

Repository policies
  Policies governing how to the repositories a organized and operated live at https://github.com/NOAA-GFDL/UCLAND-examples/wiki/UCLAND-repository-policies.

Developer guide
  Beyond the API reference above, developer specific wiki pages are attached to the `UCLAND code repository <https://github.com/NOAA-GFDL/UCLAND/wiki>`.
