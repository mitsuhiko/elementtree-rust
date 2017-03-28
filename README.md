elementtree-rs
==============

This library parses XML into a Python ElementTree like structure.  It currently
only supports reading but for that it has pretty good namespace support and the
ability to inspect the file.

It's not recommended to use this for larger documents as the entire document
will be loaded into memory.  However it's pretty good for working with configuration
files and similar things.
