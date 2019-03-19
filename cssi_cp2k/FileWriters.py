def write_input(SimObject):

  ''' A function to convert a SIM object to a cp2k input file by converting variable information to formatted string. '''

  inputFile = ""

  # GLOBAL section
  glob = SimObject.GLOBAL
  inputFile += "&GLOBAL\n"
  inputFile += "  RUN_TYPE     {}\n".format(glob.RUN_TYPE)
  inputFile += "  PROJECT      {}\n".format(glob.PROJECT)
  inputFile += "  PRINT_LEVEL  {}\n".format(glob.PRINT_LEVEL)
  inputFile += "&END GLOBAL\n"

  return inputFile
