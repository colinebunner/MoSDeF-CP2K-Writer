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

  # MOTION section
  mot = SimObject.MOTION


    # MD section
  inputFile += "&MOTION\n"
  inputFile += "  &MD\n"
  inputFile += "    ENSEMBLE        {}\n".format(mot.MD.ENSEMBLE)
  inputFile += "    TIMESTEP        {}\n".format(mot.MD.TIMESTEP)
  inputFile += "    STEPS           {}\n".format(mot.MD.STEPS)
  inputFile += "    TEMPERATURE     {}\n".format(mot.MD.TEMPERATURE)
  inputFile += "    &THERMOSTAT       \n"
  inputFile += "      &{}             \n".format(mot.MD.THERMOSTAT.TYPE )

  if mot.MD.THERMOSTAT.TYPE=='NOSE':
    inputFile += "        LENGTH       {}\n".format(mot.MD.THERMOSTAT.NOSE.LENGTH )
    inputFile += "        MTS          {}\n".format(mot.MD.THERMOSTAT.NOSE.MTS )
    inputFile += "        TIMECON      {}\n".format(mot.MD.THERMOSTAT.NOSE.TIMECON )
    inputFile += "        YOSHIDA      {}\n".format(mot.MD.THERMOSTAT.NOSE.YOSHIDA )

  if mot.MD.THERMOSTAT.TYPE=='GLE':
    inputFile += "       NDIM       {}\n".format(mot.MD.THERMOSTAT.GLE.NDIM)

    inputFile += "       A_SCALE    {}\n".format(mot.MD.THERMOSTAT.GLE.A_SCALE )
    #inputFile += "       TIMECON      {}\n".format(mot.MD.THERMOSTAT.NOSE.TIMECON )
    #inputFile += "       YOSHIDA      {}\n".format(mot.MD.THERMOSTAT.NOSE.YOSHIDA )
  inputFile += "      &END {}             \n".format(mot.MD.THERMOSTAT.TYPE )
  inputFile += "  &END MD\n \n"

    # GEO_OPT section
  geo=SimObject.MOTION.GEO_OPT
  inputFile += "  &GEO_OPT \n"
  if geo.MAX_DR is not None:
    inputFile += "    MAX_DR        {}\n".format(geo.MAX_DR)
  if geo.MAX_DR is not None:
    inputFile += "    MAX_FORCE        {}\n".format(geo.MAX_FORCE)
  if geo.MAX_DR is not None:
    inputFile += "    MAX_ITER        {}\n".format(geo.MAX_ITER)
  if geo.MAX_DR is not None:
    inputFile += "    OPTIMIZER        {}\n".format(geo.OPTIMIZER)
  if geo.MAX_DR is not None:
    inputFile += "    RMS_DR      {}\n".format(geo.RMS_DR)
  if geo.MAX_DR is not None:
    inputFile += "    RMS_FORCE {}\n".format(geo.RMS_FORCE)
  inputFile += "    STEP_START_VAL {}\n".format(geo.STEP_START_VAL)
  inputFile += "    TYPE {}\n".format(geo.TYPE)
  inputFile += "  &END GEO_OPT \n"
  return inputFile

