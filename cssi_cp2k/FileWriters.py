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

      #THERMOSTAT SUBSECTION
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
  inputFile += "      &END {}             \n".format(mot.MD.THERMOSTAT.TYPE )

      #AVERAGES subsection
  inputFile += "    &AVERAGES       \n"
  if mot.MD.AVERAGES.SECTION_PARAMETERS is not None:

    inputFile += "      SECTION_PARAMETERS        {}             \n".format(mot.MD.AVERAGES.SECTION_PARAMETERS)
  if mot.MD.AVERAGES.ACQUISITION_START_TIME is not None:
    inputFile += "      ACQUISITION_START_TIME    {}             \n".format(mot.MD.AVERAGES.ACQUISITION_START_TIME)
  if mot.MD.AVERAGES.AVERAGE_COLVAR is not None:
    inputFile += "      AVERAGE_COLVAR            {}             \n".format(mot.MD.AVERAGES.AVERAGE_COLVAR)
        #PRINT_AVERAGES SUBSECTION

        #RESTART_AVERAGES SUBSECTION

  inputFile += "    &END AVERAGES       \n"
      #END AVERAGES

      #MD.PRINT subsection
  inputFile += "    &PRINT       \n"
  if mot.MD.PRINT.FORCE_LAST is not None:

    inputFile += "      FORCE_LAST        {}             \n".format(mot.MD.PRINT.FORCE_LAST)
        #ENERGY SUBSECTION
  inputFile += "      &ENERGY       \n"
          # EACH SUBSECTION
  inputFile += "        &EACH       \n"
  if mot.MD.PRINT.ENERGY.EACH.BAND is not None:
    inputFile += "          BAND                  {}             \n".format(mot.MD.PRINT.ENERGY.EACH.BAND)
  if mot.MD.PRINT.ENERGY.EACH.BSSE is not None:
    inputFile += "          BSSE                  {}             \n".format(mot.MD.PRINT.ENERGY.EACH.BSSE)
  if mot.MD.PRINT.ENERGY.EACH.CELL_OPT is not None:
    inputFile += "          CELL_OPT              {}             \n".format(mot.MD.PRINT.ENERGY.EACH.CELL_OPT)
  if mot.MD.PRINT.ENERGY.EACH.JUST_ENERGY is not None:
    inputFile += "          JUST_ENERGY           {}             \n".format(mot.MD.PRINT.ENERGY.EACH.JUST_ENERGY)
  if mot.MD.PRINT.ENERGY.EACH.MD is not None:
    inputFile += "          MD                    {}             \n".format(mot.MD.PRINT.ENERGY.EACH.MD)
  if mot.MD.PRINT.ENERGY.EACH.METADYNAMICS is not None:
    inputFile += "          METADYNAMICS          {}             \n".format(mot.MD.PRINT.ENERGY.EACH.METADYNAMICS)
  if mot.MD.PRINT.ENERGY.EACH.PINT is not None:
    inputFile += "          PINT                  {}             \n".format(mot.MD.PRINT.ENERGY.EACH.PINT)
  if mot.MD.PRINT.ENERGY.EACH.POWELL_OPT is not None:
    inputFile += "          POWELL_OPT            {}             \n".format(mot.MD.PRINT.ENERGY.EACH.POWELL_OPT)
  if mot.MD.PRINT.ENERGY.EACH.QS_SCF is not None:
    inputFile += "          QS_SCF                {}             \n".format(mot.MD.PRINT.ENERGY.EACH.QS_SCF)
  if mot.MD.PRINT.ENERGY.EACH.REPLICA_EVAL is not None:
    inputFile += "          REPLICA_EVAL          {}             \n".format(mot.MD.PRINT.ENERGY.EACH.REPLICA_EVAL)
  if mot.MD.PRINT.ENERGY.EACH.ROT_OPT is not None:
    inputFile += "          ROT_OPT               {}             \n".format(mot.MD.PRINT.ENERGY.EACH.ROT_OPT)
  if mot.MD.PRINT.ENERGY.EACH.SHELL_OPT is not None:
    inputFile += "          SHELL_OPT             {}             \n".format(mot.MD.PRINT.ENERGY.EACH.SHELL_OPT)
  if mot.MD.PRINT.ENERGY.EACH.SPLINE_FIND_COEFFS is not None:
    inputFile += "          SPLINE_FIND_COEFFS    {}             \n".format(mot.MD.PRINT.ENERGY.EACH.SPLINE_FIND_COEFFS)
  if mot.MD.PRINT.ENERGY.EACH.TDDFT_SCF is not None:
    inputFile += "          TDDFT_SCF             {}             \n".format(mot.MD.PRINT.ENERGY.EACH.TDDFT_SCF)
  if mot.MD.PRINT.ENERGY.EACH.XAS_SCF is not None:
    inputFile += "          XAS_SCF               {}             \n".format(mot.MD.PRINT.ENERGY.EACH.XAS_SCF)
  if mot.MD.PRINT.ENERGY.EACH.ROOT is not None:
    inputFile += "          __ROOT__              {}             \n".format(mot.MD.PRINT.ENERGY.EACH.ROOT)



  inputFile += "        &END EACH       \n"
          #END EACH
  if mot.MD.PRINT.ENERGY.SECTION_PARAMETERS is not None:
    inputFile += "        SECTION_PARAMETERS   {}             \n".format(mot.MD.PRINT.ENERGY.SECTION_PARAMETERS)
  if mot.MD.PRINT.ENERGY.ADD_LAST is not None:
    inputFile += "        ADD_LAST    {}             \n".format(mot.MD.PRINT.ENERGY.ADD_LAST)
  if mot.MD.PRINT.ENERGY.COMMON_ITERATION_LEVELS is not None:
    inputFile += "        COMMON_ITERATION_LEVELS  {}             \n".format(mot.MD.PRINT.ENERGY.COMMON_ITERATION_LEVELS)
  if mot.MD.PRINT.ENERGY.FILENAME is not None:
    inputFile += "        FILENAME   {}             \n".format(mot.MD.PRINT.ENERGY.FILENAME)
  if mot.MD.PRINT.ENERGY.LOG_PRINT_KEY is not  None:
    inputFile += "        LOG_PRINT_KEY  {}             \n".format(mot.MD.PRINT.ENERGY.LOG_PRINT_KEY)

  inputFile += "      &END ENERGY       \n"
      # END ENERGY

      # program_RUN_INFO
  inputFile += "      &PROGRAM_RUN_INFO       \n"
          # EACH SUBSECTION
  inputFile += "        &EACH       \n"
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.BAND is not None:
    inputFile += "          BAND                  {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.BAND)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.BSSE is not None:
    inputFile += "          BSSE                  {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.BSSE )
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.CELL_OPT is not None:
    inputFile += "          CELL_OPT              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.CELL_OPT)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.EP_LIN_SOLVER is not None:
    inputFile += "          EP_LIN_SOLVER              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.EP_LIN_SOLVER)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.GEO_OPT is not None:
    inputFile += "          GEO_OPT              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.GEO_OPT)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.JUST_ENERGY is not None:
    inputFile += "          JUST_ENERGY              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.JUST_ENERGY)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.MD is not None:
    inputFile += "          MD              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.MD)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.METADYNAMICS is not None:
    inputFile += "          METADYNAMICS              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.METADYNAMICS)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.PINT is not None:
    inputFile += "          PINT              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.PINT)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.POWELL_OPT is not None:
    inputFile += "          POWELL_OPT                  {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.POWELL_OPT)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.BSSE is not None:
    inputFile += "          BSSE                  {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.BSSE)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.QS_SCF is not None:
    inputFile += "          QS_SCF              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.QS_SCF)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.REPLICA_EVAL is not None:
    inputFile += "          REPLICA_EVAL              {}             \n".format(
      mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.REPLICA_EVAL)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.ROT_OPT is not None:
    inputFile += "          ROT_OPT              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.ROT_OPT)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.SHELL_OPT is not None:
    inputFile += "          SHELL_OPT              {}             \n".format(
      mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.SHELL_OPT)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.SPLINE_FIND_COEFFS is not None:
    inputFile += "          SPLINE_FIND_COEFFS              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.SPLINE_FIND_COEFFS)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.TDDFT_SCF is not None:
    inputFile += "          TDDFT_SCF              {}             \n".format(
      mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.TDDFT_SCF)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.XAS_SCF is not None:
    inputFile += "          XAS_SCF              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.XAS_SCF)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.ROOT is not None:
    inputFile += "          __ROOT__              {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.EACH.ROOT)





  inputFile += "        &END EACH       \n"
          #END EACH
  if mot.MD.PRINT.PROGRAM_RUN_INFO.SECTION_PARAMETERS is not None:
    inputFile += "        SECTION_PARAMETERS   {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.SECTION_PARAMETERS)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.ADD_LAST is not None:
    inputFile += "        ADD_LAST    {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.ADD_LAST)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.COMMON_ITERATION_LEVELS is not None:
    inputFile += "        COMMON_ITERATION_LEVELS  {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.COMMON_ITERATION_LEVELS)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.FILENAME is not None:
    inputFile += "        FILENAME   {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.FILENAME)
  if mot.MD.PRINT.PROGRAM_RUN_INFO.LOG_PRINT_KEY is not  None:
    inputFile += "        LOG_PRINT_KEY  {}             \n".format(mot.MD.PRINT.PROGRAM_RUN_INFO.LOG_PRINT_KEY)

  inputFile += "      &END PROGRAM_RUN_INFO     \n"

      # END PROGRAM_RUN_INFO
  inputFile += "    &END PRINT       \n"
      #END PRINT



  inputFile += "  &END MD\n \n"

    # GEO_OPT section
  geo=SimObject.MOTION.GEO_OPT
  inputFile += "  &GEO_OPT \n"
  if geo.MAX_DR is not None:
    inputFile += "    MAX_DR        {}\n".format(geo.MAX_DR)
  if geo.MAX_FORCE is not None:
    inputFile += "    MAX_FORCE        {}\n".format(geo.MAX_FORCE)
  if geo.MAX_ITER is not None:
    inputFile += "    MAX_ITER        {}\n".format(geo.MAX_ITER)
  if geo.OPTIMIZER is not None:
    inputFile += "    OPTIMIZER        {}\n".format(geo.OPTIMIZER)
  if geo.RMS_DR is not None:
    inputFile += "    RMS_DR      {}\n".format(geo.RMS_DR)
  if geo.RMS_FORCE is not None:
    inputFile += "    RMS_FORCE {}\n".format(geo.RMS_FORCE)
  if geo.STEP_START_VAL is not None:
    inputFile += "    STEP_START_VAL {}\n".format(geo.STEP_START_VAL)
  if geo.TYPE is not None:
    inputFile += "    TYPE {}\n".format(geo.TYPE)

  inputFile += "  &END GEO_OPT \n"
  return inputFile

