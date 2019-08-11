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
    #END MD

    # MOTION PRINT
  pri=SimObject.MOTION.PRINT
  inputFile += "  &PRINT \n"
      #PRINT FORCES
  if pri.FORCES.SECTION_PARAMETERS is not None:
    inputFile += "    &FORCES        {}\n".format(pri.FORCES.SECTION_PARAMETERS)
    if pri.FORCES.ADD_LAST is not None:
      inputFile += "      ADD_LAST        {}\n".format(pri.FORCES.ADD_LAST)
    if pri.FORCES.COMMON_ITERATION_LEVELS is not None:
      inputFile += "      COMMON_ITERATION_LEVELS        {}\n".format(pri.FORCES.COMMON_ITERATION_LEVELS)
    if pri.FORCES.FILENAME is not None:
      inputFile += "      FILENAME        {}\n".format(pri.FORCES.FILENAME)
    if pri.FORCES.FORMAT is not None:
      inputFile += "      FORMAT        {}\n".format(pri.FORCES.FORMAT)
    if pri.FORCES.LOG_PRINT_KEY is not None:
      inputFile += "      LOG_PRINT_KEY        {}\n".format(pri.FORCES.LOG_PRINT_KEY)
    if pri.FORCES.UNIT is not None:
      inputFile += "      UNIT        {}\n".format(pri.FORCES.UNIT)
    
    # EACH SUBSECTION
    inputFile += "      &EACH       \n"
    if pri.FORCES.EACH.BAND is not None:
      inputFile += "        BAND                  {}             \n".format(pri.FORCES.EACH.BAND)
    if pri.FORCES.EACH.BSSE is not None:
      inputFile += "        BSSE                  {}             \n".format(pri.FORCES.EACH.BSSE)
    if pri.FORCES.EACH.CELL_OPT is not None:
      inputFile += "        CELL_OPT              {}             \n".format(
        mot.MD.PRINT.FORCES.EACH.CELL_OPT)
    if pri.FORCES.EACH.EP_LIN_SOLVER is not None:
      inputFile += "        EP_LIN_SOLVER              {}             \n".format(
        pri.FORCES.EACH.EP_LIN_SOLVER)
    if pri.FORCES.EACH.GEO_OPT is not None:
      inputFile += "        GEO_OPT              {}             \n".format(pri.FORCES.EACH.GEO_OPT)
    if pri.FORCES.EACH.JUST_ENERGY is not None:
      inputFile += "        JUST_ENERGY              {}             \n".format(
        pri.FORCES.EACH.JUST_ENERGY)
    if pri.FORCES.EACH.MD is not None:
      inputFile += "        MD              {}             \n".format(pri.FORCES.EACH.MD)
    if pri.FORCES.EACH.METADYNAMICS is not None:
      inputFile += "        METADYNAMICS              {}             \n".format(
        pri.FORCES.EACH.METADYNAMICS)
    if pri.FORCES.EACH.PINT is not None:
      inputFile += "        PINT              {}             \n".format(pri.FORCES.EACH.PINT)
    if pri.FORCES.EACH.POWELL_OPT is not None:
      inputFile += "        POWELL_OPT                  {}             \n".format(
        pri.FORCES.EACH.POWELL_OPT)
    if pri.FORCES.EACH.QS_SCF is not None:
      inputFile += "        QS_SCF              {}             \n".format(pri.FORCES.EACH.QS_SCF)
    if pri.FORCES.EACH.REPLICA_EVAL is not None:
      inputFile += "        REPLICA_EVAL              {}             \n".format(
        pri.FORCES.EACH.REPLICA_EVAL)
    if pri.FORCES.EACH.ROT_OPT is not None:
      inputFile += "        ROT_OPT              {}             \n".format(pri.FORCES.EACH.ROT_OPT)
    if pri.FORCES.EACH.SHELL_OPT is not None:
      inputFile += "        SHELL_OPT              {}             \n".format(
        pri.FORCES.EACH.SHELL_OPT)
    if pri.FORCES.EACH.SPLINE_FIND_COEFFS is not None:
      inputFile += "        SPLINE_FIND_COEFFS              {}             \n".format(
        pri.FORCES.EACH.SPLINE_FIND_COEFFS)
    if pri.FORCES.EACH.TDDFT_SCF is not None:
      inputFile += "        TDDFT_SCF              {}             \n".format(
        pri.FORCES.EACH.TDDFT_SCF)
    if pri.FORCES.EACH.XAS_SCF is not None:
      inputFile += "        XAS_SCF              {}             \n".format(pri.FORCES.EACH.XAS_SCF)
    if pri.FORCES.EACH.ROOT is not None:
      inputFile += "        __ROOT__              {}             \n".format(pri.FORCES.EACH.ROOT)
    inputFile += "      &END EACH       \n"
    
        #END EACH   
    
 
    inputFile += "    &END FORCES        \n"
      #END FORCES

      #START RESTART_HISTORY
  if pri.RESTART_HISTORY.SECTION_PARAMETERS is not None:
    inputFile += "    &RESTART_HISTORY        {}\n".format(pri.RESTART_HISTORY.SECTION_PARAMETERS)
    if pri.RESTART_HISTORY.ADD_LAST is not None:
      inputFile += "      ADD_LAST        {}\n".format(pri.RESTART_HISTORY.ADD_LAST)
    if pri.RESTART_HISTORY.COMMON_ITERATION_LEVELS is not None:
      inputFile += "      COMMON_ITERATION_LEVELS        {}\n".format(pri.RESTART_HISTORY.COMMON_ITERATION_LEVELS)
    if pri.RESTART_HISTORY.FILENAME is not None:
      inputFile += "      FILENAME        {}\n".format(pri.RESTART_HISTORY.FILENAME)
    if pri.RESTART_HISTORY.LOG_PRINT_KEY is not None:
      inputFile += "      LOG_PRINT_KEY        {}\n".format(pri.RESTART_HISTORY.LOG_PRINT_KEY)
    
    # EACH SUBSECTION
    inputFile += "      &EACH       \n"
    if pri.RESTART_HISTORY.EACH.BAND is not None:
      inputFile += "        BAND                  {}             \n".format(pri.RESTART_HISTORY.EACH.BAND)
    if pri.RESTART_HISTORY.EACH.BSSE is not None:
      inputFile += "        BSSE                  {}             \n".format(pri.RESTART_HISTORY.EACH.BSSE)
    if pri.RESTART_HISTORY.EACH.CELL_OPT is not None:
      inputFile += "        CELL_OPT              {}             \n".format(
        mot.MD.PRINT.RESTART_HISTORY.EACH.CELL_OPT)
    if pri.RESTART_HISTORY.EACH.EP_LIN_SOLVER is not None:
      inputFile += "        EP_LIN_SOLVER              {}             \n".format(
        pri.RESTART_HISTORY.EACH.EP_LIN_SOLVER)
    if pri.RESTART_HISTORY.EACH.GEO_OPT is not None:
      inputFile += "        GEO_OPT              {}             \n".format(pri.RESTART_HISTORY.EACH.GEO_OPT)
    if pri.RESTART_HISTORY.EACH.JUST_ENERGY is not None:
      inputFile += "        JUST_ENERGY              {}             \n".format(
        pri.RESTART_HISTORY.EACH.JUST_ENERGY)
    if pri.RESTART_HISTORY.EACH.MD is not None:
      inputFile += "        MD              {}             \n".format(pri.RESTART_HISTORY.EACH.MD)
    if pri.RESTART_HISTORY.EACH.METADYNAMICS is not None:
      inputFile += "        METADYNAMICS              {}             \n".format(
        pri.RESTART_HISTORY.EACH.METADYNAMICS)
    if pri.RESTART_HISTORY.EACH.PINT is not None:
      inputFile += "        PINT              {}             \n".format(pri.RESTART_HISTORY.EACH.PINT)
    if pri.RESTART_HISTORY.EACH.POWELL_OPT is not None:
      inputFile += "        POWELL_OPT                  {}             \n".format(
        pri.RESTART_HISTORY.EACH.POWELL_OPT)
    if pri.RESTART_HISTORY.EACH.QS_SCF is not None:
      inputFile += "        QS_SCF              {}             \n".format(pri.RESTART_HISTORY.EACH.QS_SCF)
    if pri.RESTART_HISTORY.EACH.REPLICA_EVAL is not None:
      inputFile += "        REPLICA_EVAL              {}             \n".format(
        pri.RESTART_HISTORY.EACH.REPLICA_EVAL)
    if pri.RESTART_HISTORY.EACH.ROT_OPT is not None:
      inputFile += "        ROT_OPT              {}             \n".format(pri.RESTART_HISTORY.EACH.ROT_OPT)
    if pri.RESTART_HISTORY.EACH.SHELL_OPT is not None:
      inputFile += "        SHELL_OPT              {}             \n".format(
        pri.RESTART_HISTORY.EACH.SHELL_OPT)
    if pri.RESTART_HISTORY.EACH.SPLINE_FIND_COEFFS is not None:
      inputFile += "        SPLINE_FIND_COEFFS              {}             \n".format(
        pri.RESTART_HISTORY.EACH.SPLINE_FIND_COEFFS)
    if pri.RESTART_HISTORY.EACH.TDDFT_SCF is not None:
      inputFile += "        TDDFT_SCF              {}             \n".format(
        pri.RESTART_HISTORY.EACH.TDDFT_SCF)
    if pri.RESTART_HISTORY.EACH.XAS_SCF is not None:
      inputFile += "        XAS_SCF              {}             \n".format(pri.RESTART_HISTORY.EACH.XAS_SCF)
    if pri.RESTART_HISTORY.EACH.ROOT is not None:
      inputFile += "        __ROOT__              {}             \n".format(pri.RESTART_HISTORY.EACH.ROOT)
    inputFile += "      &END EACH       \n"
    
        #END EACH
    
    
    
    
    inputFile += "    &END RESTART_HISTORY        \n"

      # END RESTART_HISTORY

      # START RESTART


        # RESTART EACH SECTION





  if pri.RESTART.SECTION_PARAMETERS is not None:
    inputFile += "    &RESTART        {}\n".format(pri.RESTART.SECTION_PARAMETERS)
    if pri.RESTART.ADD_LAST is not None:
      inputFile += "      ADD_LAST        {}\n".format(pri.RESTART.ADD_LAST)
    if pri.RESTART.BACKUP_COPIES is not None:
      inputFile += "      BACKUP_COPIES        {}\n".format(pri.RESTART.BACKUP_COPIES)
    if pri.RESTART.COMMON_ITERATION_LEVELS is not None:
      inputFile += "      COMMON_ITERATION_LEVELS        {}\n".format(pri.RESTART.COMMON_ITERATION_LEVELS)
    if pri.RESTART.FILENAME is not None:
      inputFile += "      FILENAME        {}\n".format(pri.RESTART.FILENAME)
    if pri.RESTART.LOG_PRINT_KEY is not None:
      inputFile += "      LOG_PRINT_KEY        {}\n".format(pri.RESTART.LOG_PRINT_KEY)
    if pri.RESTART.SPLIT_RESTART_FILE is not None:
      inputFile += "      SPLIT_RESTART_FILE        {}\n".format(pri.RESTART.SPLIT_RESTART_FILE)

      # EACH SUBSECTION
    inputFile += "      &EACH       \n"
    if pri.RESTART.EACH.BAND is not None:
      inputFile += "        BAND                  {}             \n".format(pri.RESTART.EACH.BAND)
    if pri.RESTART.EACH.BSSE is not None:
      inputFile += "        BSSE                  {}             \n".format(pri.RESTART.EACH.BSSE)
    if pri.RESTART.EACH.CELL_OPT is not None:
      inputFile += "        CELL_OPT              {}             \n".format(
        mot.MD.PRINT.RESTART.EACH.CELL_OPT)
    if pri.RESTART.EACH.EP_LIN_SOLVER is not None:
      inputFile += "        EP_LIN_SOLVER              {}             \n".format(
        pri.RESTART.EACH.EP_LIN_SOLVER)
    if pri.RESTART.EACH.GEO_OPT is not None:
      inputFile += "        GEO_OPT              {}             \n".format(pri.RESTART.EACH.GEO_OPT)
    if pri.RESTART.EACH.JUST_ENERGY is not None:
      inputFile += "        JUST_ENERGY              {}             \n".format(
        pri.RESTART.EACH.JUST_ENERGY)
    if pri.RESTART.EACH.MD is not None:
      inputFile += "        MD              {}             \n".format(pri.RESTART.EACH.MD)
    if pri.RESTART.EACH.METADYNAMICS is not None:
      inputFile += "        METADYNAMICS              {}             \n".format(
        pri.RESTART.EACH.METADYNAMICS)
    if pri.RESTART.EACH.PINT is not None:
      inputFile += "        PINT              {}             \n".format(pri.RESTART.EACH.PINT)
    if pri.RESTART.EACH.POWELL_OPT is not None:
      inputFile += "        POWELL_OPT                  {}             \n".format(
        pri.RESTART.EACH.POWELL_OPT)
    if pri.RESTART.EACH.QS_SCF is not None:
      inputFile += "        QS_SCF              {}             \n".format(pri.RESTART.EACH.QS_SCF)
    if pri.RESTART.EACH.REPLICA_EVAL is not None:
      inputFile += "        REPLICA_EVAL              {}             \n".format(
        pri.RESTART.EACH.REPLICA_EVAL)
    if pri.RESTART.EACH.ROT_OPT is not None:
      inputFile += "        ROT_OPT              {}             \n".format(pri.RESTART.EACH.ROT_OPT)
    if pri.RESTART.EACH.SHELL_OPT is not None:
      inputFile += "        SHELL_OPT              {}             \n".format(
        pri.RESTART.EACH.SHELL_OPT)
    if pri.RESTART.EACH.SPLINE_FIND_COEFFS is not None:
      inputFile += "        SPLINE_FIND_COEFFS              {}             \n".format(
        pri.RESTART.EACH.SPLINE_FIND_COEFFS)
    if pri.RESTART.EACH.TDDFT_SCF is not None:
      inputFile += "        TDDFT_SCF              {}             \n".format(
        pri.RESTART.EACH.TDDFT_SCF)
    if pri.RESTART.EACH.XAS_SCF is not None:
      inputFile += "        XAS_SCF              {}             \n".format(pri.RESTART.EACH.XAS_SCF)
    if pri.RESTART.EACH.ROOT is not None:
      inputFile += "        __ROOT__              {}             \n".format(pri.RESTART.EACH.ROOT)
    inputFile += "      &END EACH       \n"
        #END EACH
    inputFile += "    &END RESTART        \n"
      # END RESTART

    # START TRAJECTORY
    if pri.TRAJECTORY.SECTION_PARAMETERS is not None:
      inputFile += "    &TRAJECTORY       {}\n".format(pri.TRAJECTORY.SECTION_PARAMETERS)
      if pri.TRAJECTORY.ADD_LAST is not None:
        inputFile += "      ADD_LAST        {}\n".format(pri.TRAJECTORY.ADD_LAST)
      if pri.TRAJECTORY.CHARGE_BETA is not None:
        inputFile += "      CHARGE_BETA        {}\n".format(pri.TRAJECTORY.CHARGE_BETA)
      if pri.TRAJECTORY.CHARGE_EXTENDED is not None:
        inputFile += "      CHARGE_EXTENDED        {}\n".format(pri.TRAJECTORY.CHARGE_EXTENDED)
      if pri.TRAJECTORY.CHARGE_OCCUP is not None:
        inputFile += "      CHARGE_OCCUP        {}\n".format(pri.TRAJECTORY.CHARGE_OCCUP)
      if pri.TRAJECTORY.COMMON_ITERATION_LEVELS is not None:
        inputFile += "      COMMON_ITERATION_LEVELS        {}\n".format(pri.TRAJECTORY.COMMON_ITERATION_LEVELS)
      if pri.TRAJECTORY.FILENAME is not None:
        inputFile += "      FILENAME        {}\n".format(pri.TRAJECTORY.FILENAME)
      if pri.TRAJECTORY.LOG_PRINT_KEY is not None:
        inputFile += "      LOG_PRINT_KEY        {}\n".format(pri.TRAJECTORY.LOG_PRINT_KEY)

      # EACH SUBSECTION
      inputFile += "      &EACH       \n"
      if pri.TRAJECTORY.EACH.BAND is not None:
        inputFile += "        BAND                  {}             \n".format(pri.TRAJECTORY.EACH.BAND)
      if pri.TRAJECTORY.EACH.BSSE is not None:
        inputFile += "        BSSE                  {}             \n".format(pri.TRAJECTORY.EACH.BSSE)
      if pri.TRAJECTORY.EACH.CELL_OPT is not None:
        inputFile += "        CELL_OPT              {}             \n".format(
          mot.MD.PRINT.TRAJECTORY.EACH.CELL_OPT)
      if pri.TRAJECTORY.EACH.EP_LIN_SOLVER is not None:
        inputFile += "        EP_LIN_SOLVER              {}             \n".format(
          pri.TRAJECTORY.EACH.EP_LIN_SOLVER)
      if pri.TRAJECTORY.EACH.GEO_OPT is not None:
        inputFile += "        GEO_OPT              {}             \n".format(pri.TRAJECTORY.EACH.GEO_OPT)
      if pri.TRAJECTORY.EACH.JUST_ENERGY is not None:
        inputFile += "        JUST_ENERGY              {}             \n".format(
          pri.TRAJECTORY.EACH.JUST_ENERGY)
      if pri.TRAJECTORY.EACH.MD is not None:
        inputFile += "        MD              {}             \n".format(pri.TRAJECTORY.EACH.MD)
      if pri.TRAJECTORY.EACH.METADYNAMICS is not None:
        inputFile += "        METADYNAMICS              {}             \n".format(
          pri.TRAJECTORY.EACH.METADYNAMICS)
      if pri.TRAJECTORY.EACH.PINT is not None:
        inputFile += "        PINT              {}             \n".format(pri.TRAJECTORY.EACH.PINT)
      if pri.TRAJECTORY.EACH.POWELL_OPT is not None:
        inputFile += "        POWELL_OPT                  {}             \n".format(
          pri.TRAJECTORY.EACH.POWELL_OPT)
      if pri.TRAJECTORY.EACH.QS_SCF is not None:
        inputFile += "        QS_SCF              {}             \n".format(pri.TRAJECTORY.EACH.QS_SCF)
      if pri.TRAJECTORY.EACH.REPLICA_EVAL is not None:
        inputFile += "        REPLICA_EVAL              {}             \n".format(
          pri.TRAJECTORY.EACH.REPLICA_EVAL)
      if pri.TRAJECTORY.EACH.ROT_OPT is not None:
        inputFile += "        ROT_OPT              {}             \n".format(pri.TRAJECTORY.EACH.ROT_OPT)
      if pri.TRAJECTORY.EACH.SHELL_OPT is not None:
        inputFile += "        SHELL_OPT              {}             \n".format(
          pri.TRAJECTORY.EACH.SHELL_OPT)
      if pri.TRAJECTORY.EACH.SPLINE_FIND_COEFFS is not None:
        inputFile += "        SPLINE_FIND_COEFFS              {}             \n".format(
          pri.TRAJECTORY.EACH.SPLINE_FIND_COEFFS)
      if pri.TRAJECTORY.EACH.TDDFT_SCF is not None:
        inputFile += "        TDDFT_SCF              {}             \n".format(
          pri.TRAJECTORY.EACH.TDDFT_SCF)
      if pri.TRAJECTORY.EACH.XAS_SCF is not None:
        inputFile += "        XAS_SCF              {}             \n".format(pri.TRAJECTORY.EACH.XAS_SCF)
      if pri.TRAJECTORY.EACH.ROOT is not None:
        inputFile += "        __ROOT__              {}             \n".format(pri.TRAJECTORY.EACH.ROOT)
      inputFile += "      &END EACH       \n"

      # END EACH

      inputFile += "    &END TRAJECTORY       \n"

      # END TRAJECTORY
        
# START VELOCITIES
    if pri.VELOCITIES.SECTION_PARAMETERS is not None:
      inputFile += "    &VELOCITIES       {}\n".format(pri.VELOCITIES.SECTION_PARAMETERS)
      if pri.VELOCITIES.ADD_LAST is not None:
        inputFile += "      ADD_LAST        {}\n".format(pri.VELOCITIES.ADD_LAST)
      if pri.VELOCITIES.CHARGE_BETA is not None:
        inputFile += "      CHARGE_BETA        {}\n".format(pri.VELOCITIES.CHARGE_BETA)
      if pri.VELOCITIES.CHARGE_EXTENDED is not None:
        inputFile += "      CHARGE_EXTENDED        {}\n".format(pri.VELOCITIES.CHARGE_EXTENDED)
      if pri.VELOCITIES.CHARGE_OCCUP is not None:
        inputFile += "      CHARGE_OCCUP        {}\n".format(pri.VELOCITIES.CHARGE_OCCUP)
      if pri.VELOCITIES.COMMON_ITERATION_LEVELS is not None:
        inputFile += "      COMMON_ITERATION_LEVELS        {}\n".format(pri.VELOCITIES.COMMON_ITERATION_LEVELS)
      if pri.VELOCITIES.FILENAME is not None:
        inputFile += "      FILENAME        {}\n".format(pri.VELOCITIES.FILENAME)
      if pri.VELOCITIES.LOG_PRINT_KEY is not None:
        inputFile += "      LOG_PRINT_KEY        {}\n".format(pri.VELOCITIES.LOG_PRINT_KEY)

      # EACH SUBSECTION
      inputFile += "      &EACH       \n"
      if pri.VELOCITIES.EACH.BAND is not None:
        inputFile += "        BAND                  {}             \n".format(pri.VELOCITIES.EACH.BAND)
      if pri.VELOCITIES.EACH.BSSE is not None:
        inputFile += "        BSSE                  {}             \n".format(pri.VELOCITIES.EACH.BSSE)
      if pri.VELOCITIES.EACH.CELL_OPT is not None:
        inputFile += "        CELL_OPT              {}             \n".format(
          mot.MD.PRINT.VELOCITIES.EACH.CELL_OPT)
      if pri.VELOCITIES.EACH.EP_LIN_SOLVER is not None:
        inputFile += "        EP_LIN_SOLVER              {}             \n".format(
          pri.VELOCITIES.EACH.EP_LIN_SOLVER)
      if pri.VELOCITIES.EACH.GEO_OPT is not None:
        inputFile += "        GEO_OPT              {}             \n".format(pri.VELOCITIES.EACH.GEO_OPT)
      if pri.VELOCITIES.EACH.JUST_ENERGY is not None:
        inputFile += "        JUST_ENERGY              {}             \n".format(
          pri.VELOCITIES.EACH.JUST_ENERGY)
      if pri.VELOCITIES.EACH.MD is not None:
        inputFile += "        MD              {}             \n".format(pri.VELOCITIES.EACH.MD)
      if pri.VELOCITIES.EACH.METADYNAMICS is not None:
        inputFile += "        METADYNAMICS              {}             \n".format(
          pri.VELOCITIES.EACH.METADYNAMICS)
      if pri.VELOCITIES.EACH.PINT is not None:
        inputFile += "        PINT              {}             \n".format(pri.VELOCITIES.EACH.PINT)
      if pri.VELOCITIES.EACH.POWELL_OPT is not None:
        inputFile += "        POWELL_OPT                  {}             \n".format(
          pri.VELOCITIES.EACH.POWELL_OPT)
      if pri.VELOCITIES.EACH.QS_SCF is not None:
        inputFile += "        QS_SCF              {}             \n".format(pri.VELOCITIES.EACH.QS_SCF)
      if pri.VELOCITIES.EACH.REPLICA_EVAL is not None:
        inputFile += "        REPLICA_EVAL              {}             \n".format(
          pri.VELOCITIES.EACH.REPLICA_EVAL)
      if pri.VELOCITIES.EACH.ROT_OPT is not None:
        inputFile += "        ROT_OPT              {}             \n".format(pri.VELOCITIES.EACH.ROT_OPT)
      if pri.VELOCITIES.EACH.SHELL_OPT is not None:
        inputFile += "        SHELL_OPT              {}             \n".format(
          pri.VELOCITIES.EACH.SHELL_OPT)
      if pri.VELOCITIES.EACH.SPLINE_FIND_COEFFS is not None:
        inputFile += "        SPLINE_FIND_COEFFS              {}             \n".format(
          pri.VELOCITIES.EACH.SPLINE_FIND_COEFFS)
      if pri.VELOCITIES.EACH.TDDFT_SCF is not None:
        inputFile += "        TDDFT_SCF              {}             \n".format(
          pri.VELOCITIES.EACH.TDDFT_SCF)
      if pri.VELOCITIES.EACH.XAS_SCF is not None:
        inputFile += "        XAS_SCF              {}             \n".format(pri.VELOCITIES.EACH.XAS_SCF)
      if pri.VELOCITIES.EACH.ROOT is not None:
        inputFile += "        __ROOT__              {}             \n".format(pri.VELOCITIES.EACH.ROOT)
      inputFile += "      &END EACH       \n"

      # END EACH

      inputFile += "    &END TRAJECTORY       \n"

      # END VELOCITIES




  inputFile += "  &END PRINT \n"
    #END MOTION PRINT
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

  inputFile += "&END MOTION \n"





  # FORCE_EVAL SECTION
  force = SimObject.FORCE_EVAL

  # DFT section
  inputFile += "&FORCE_EVAL\n"
  if force.METHOD is not None:
    inputFile += "  METHOD        {}\n".format(force.METHOD)
  if force.STRESS_TENSOR is not None:
    inputFile += "  STRESS_TENSOR        {}\n".format(force.STRESS_TENSOR)
  inputFile += "  &DFT        \n"
  if force.DFT.AUTO_BASIS is not None:
    inputFile += "    AUTO_BASIS        {}\n".format(force.DFT.AUTO_BASIS)
  if force.DFT.BASIS_SET_FILE_NAME is not None:
    inputFile += "    BASIS_SET_FILE_NAME        {}\n".format(force.DFT.BASIS_SET_FILE_NAME)
  if force.DFT.CHARGE is not None:
    inputFile += "    CHARGE        {}\n".format(force.DFT.CHARGE)
  if force.DFT.EXCITATIONS is not None:
    inputFile += "    EXCITATIONS        {}\n".format(force.DFT.EXCITATIONS)
  if force.DFT.MULTIPLICITY is not None:
    inputFile += "    MULTIPLICITY        {}\n".format(force.DFT.MULTIPLICITY)
  if force.DFT.PLUS_U_METHOD is not None:
    inputFile += "    PLUS_U_METHOD        {}\n".format(force.DFT.PLUS_U_METHOD)
  if force.DFT.POTENTIAL_FILE_NAME is not None:
    inputFile += "    POTENTIAL_FILE_NAME        {}\n".format(force.DFT.POTENTIAL_FILE_NAME)
  if force.DFT.RELAX_MULTIPLICITY is not None:
    inputFile += "    RELAX_MULTIPLICITY        {}\n".format(force.DFT.RELAX_MULTIPLICITY)
  if force.DFT.ROKS is not None:
    inputFile += "    ROKS        {}\n".format(force.DFT.ROKS)
  if force.DFT.SUBCELLS is not None:
    inputFile += "    SUBCELLS        {}\n".format(force.DFT.SUBCELLS)
  if force.DFT.SURFACE_DIPOLE_CORRECTION is not None:
    inputFile += "    SURFACE_DIPOLE_CORRECTION        {}\n".format(force.DFT.SURFACE_DIPOLE_CORRECTION)
  if force.DFT.SURF_DIP_DIR is not None:
    inputFile += "    SURF_DIP_DIR        {}\n".format(force.DFT.SURF_DIP_DIR)
  if force.DFT.UKS is not None:
    inputFile += "    UKS        {}\n".format(force.DFT.UKS)
  if force.DFT.WFN_RESTART_FILE_NAME is not None:
    inputFile += "    WFN_RESTART_FILE_NAME        {}\n".format(force.DFT.WFN_RESTART_FILE_NAME)
    
    ### sTART QS
  inputFile += "    &QS        \n"
  if force.DFT.QS.ALMO_SCF is not None:
    inputFile += "      ALMO_SCF      {}\n".format(force.DFT.QS.ALMO_SCF)
  if force.DFT.QS.ALPHA0_HARD is not None:
    inputFile += "      ALPHA0_HARD       {}\n".format(force.DFT.QS.ALPHA0_HARD)
  if force.DFT.QS.CLUSTER_EMBED_SUBSYS is not None:
    inputFile += "      CLUSTER_EMBED_SUBSYS       {}\n".format(force.DFT.QS.CLUSTER_EMBED_SUBSYS)
  if force.DFT.QS.CORE_PPL is not None:
    inputFile += "      CORE_PPL       {}\n".format(force.DFT.QS.CORE_PPL)
  if force.DFT.QS.DFET_EMBEDDED is not None:
    inputFile += "      DFET_EMBEDDED      {}\n".format(force.DFT.QS.DFET_EMBEDDED)
  if force.DFT.QS.DMFET_EMBEDDED is not None:
    inputFile += "      DMFET_EMBEDDED       {}\n".format(force.DFT.QS.DMFET_EMBEDDED)
  if force.DFT.QS.EMBED_CUBE_FILE_NAME is not None:
    inputFile += "      EMBED_CUBE_FILE_NAME      {}\n".format(force.DFT.QS.EMBED_CUBE_FILE_NAME)
  if force.DFT.QS.EMBED_RESTART_FILE_NAME is not None:
    inputFile += "      EMBED_RESTART_FILE_NAME      {}\n".format(force.DFT.QS.EMBED_RESTART_FILE_NAME)
  if force.DFT.QS.EMBED_SPIN_CUBE_FILE_NAME is not None:
    inputFile += "      EMBED_SPIN_CUBE_FILE_NAME       {}\n".format(force.DFT.QS.EMBED_SPIN_CUBE_FILE_NAME)
  if force.DFT.QS.EPSFIT is not None:
    inputFile += "      EPSFIT       {}\n".format(force.DFT.QS.EPSFIT)
  if force.DFT.QS.EPSISO is not None:
    inputFile += "      EPSISO       {}\n".format(force.DFT.QS.EPSISO)
  if force.DFT.QS.EPSRHO0 is not None:
    inputFile += "      EPSRHO0       {}\n".format(force.DFT.QS.EPSRHO0)
  if force.DFT.QS.EPSSVD is not None:
    inputFile += "      EPSSVD       {}\n".format(force.DFT.QS.EPSSVD)
    
  if force.DFT.QS.EPS_CORE_CHARGE is not None:
    inputFile += "      EPS_CORE_CHARGE       {}\n".format(force.DFT.QS.EPS_CORE_CHARGE)
  if force.DFT.QS.EPS_CPC is not None:
    inputFile += "      EPS_CPC      {}\n".format(force.DFT.QS.EPS_CPC)
  if force.DFT.QS.EPS_DEFAULT is not None:
    inputFile += "      EPS_DEFAULT       {}\n".format(force.DFT.QS.EPS_DEFAULT)
    
  if force.DFT.QS.EPS_FILTER_MATRIX is not None:
    inputFile += "      EPS_FILTER_MATRIX       {}\n".format(force.DFT.QS.EPS_FILTER_MATRIX)
  if force.DFT.QS.EPS_GVG_RSPACE is not None:
    inputFile += "      EPS_GVG_RSPACE      {}\n".format(force.DFT.QS.EPS_GVG_RSPACE)
  if force.DFT.QS.EPS_KG_ORB is not None:
    inputFile += "      EPS_KG_ORB      {}\n".format(force.DFT.QS.EPS_KG_ORB)
  if force.DFT.QS.EPS_PGF_ORB is not None:
    inputFile += "      EPS_PGF_ORB      {}\n".format(force.DFT.QS.EPS_PGF_ORB)
  if force.DFT.QS.EPS_PPL is not None:
    inputFile += "      EPS_PPL       {}\n".format(force.DFT.QS.EPS_PPL)
  if force.DFT.QS.EPS_PPNL is not None:
    inputFile += "      EPS_PPNL       {}\n".format(force.DFT.QS.EPS_PPNL)
    
  if force.DFT.QS.EPS_RHO is not None:
    inputFile += "      EPS_RHO       {}\n".format(force.DFT.QS.EPS_RHO)
  if force.DFT.QS.EPS_RHO_GSPACE is not None:
    inputFile += "      EPS_RHO_GSPACE       {}\n".format(force.DFT.QS.EPS_RHO_GSPACE)
  if force.DFT.QS.EPS_RHO_RSPACE is not None:
    inputFile += "      EPS_RHO_RSPACE      {}\n".format(force.DFT.QS.EPS_RHO_RSPACE)
    
  if force.DFT.QS.EXTRAPOLATION is not None:
    inputFile += "      EXTRAPOLATION       {}\n".format(force.DFT.QS.EXTRAPOLATION)
  if force.DFT.QS.EXTRAPOLATION_ORDER is not None:
    inputFile += "      EXTRAPOLATION_ORDER       {}\n".format(force.DFT.QS.EXTRAPOLATION_ORDER)
  if force.DFT.QS.FORCE_PAW is not None:
    inputFile += "      FORCE_PAW      {}\n".format(force.DFT.QS.FORCE_PAW)
  if force.DFT.QS.HIGH_LEVEL_EMBED_SUBSYS is not None:
    inputFile += "      HIGH_LEVEL_EMBED_SUBSYS       {}\n".format(force.DFT.QS.HIGH_LEVEL_EMBED_SUBSYS)
  if force.DFT.QS.KG_METHOD is not None:
    inputFile += "      KG_METHOD       {}\n".format(force.DFT.QS.KG_METHOD)
  if force.DFT.QS.LADDN0 is not None:
    inputFile += "      LADDN0       {}\n".format(force.DFT.QS.LADDN0)
    
  if force.DFT.QS.LMAXN0 is not None:
    inputFile += "      LMAXN0       {}\n".format(force.DFT.QS.LMAXN0)
  if force.DFT.QS.LMAXN1 is not None:
    inputFile += "      LMAXN1       {}\n".format(force.DFT.QS.LMAXN1)
  if force.DFT.QS.LS_SCF is not None:
    inputFile += "      LS_SCF      {}\n".format(force.DFT.QS.LS_SCF)
  if force.DFT.QS.MAP_CONSISTENT is not None:
    inputFile += "      MAP_CONSISTENT      {}\n".format(force.DFT.QS.MAP_CONSISTENT)
  if force.DFT.QS.MAX_RAD_LOCAL is not None:
    inputFile += "      MAX_RAD_LOCAL       {}\n".format(force.DFT.QS.MAX_RAD_LOCAL)
  if force.DFT.QS.METHOD is not None:
    inputFile += "      METHOD       {}\n".format(force.DFT.QS.METHOD)
  if force.DFT.QS.PW_GRID is not None:
    inputFile += "      PW_GRID       {}\n".format(force.DFT.QS.PW_GRID)
  if force.DFT.QS.PW_GRID_BLOCKED is not None:
    inputFile += "      PW_GRID_BLOCKED       {}\n".format(force.DFT.QS.PW_GRID_BLOCKED)
  if force.DFT.QS.PW_GRID_LAYOUT is not None:
    inputFile += "      PW_GRID_LAYOUT       {}\n".format(force.DFT.QS.PW_GRID_LAYOUT)
  if force.DFT.QS.QUADRATURE is not None:
    inputFile += "      QUADRATURE       {}\n".format(force.DFT.QS.QUADRATURE)
  if force.DFT.QS.REF_EMBED_SUBSYS is not None:
    inputFile += "      REF_EMBED_SUBSYS       {}\n".format(force.DFT.QS.REF_EMBED_SUBSYS)
  if force.DFT.QS.STO_NG is not None:
    inputFile += "      STO_NG       {}\n".format(force.DFT.QS.STO_NG)
  if force.DFT.QS.TRANSPORT is not None:
    inputFile += "      TRANSPORT       {}\n".format(force.DFT.QS.TRANSPORT)

  
  inputFile += "    &END QS        \n"
    ### END QS
  inputFile += "    &POISSON        \n"
  if force.DFT.POISSON.PERIODIC is not None:
    inputFile += "      PERIODIC       {}\n".format(force.DFT.POISSON.PERIODIC)
  if force.DFT.POISSON.POISSON_SOLVER is not None:
    inputFile += "      POISSON_SOLVER       {}\n".format(force.DFT.POISSON.POISSON_SOLVER)
    
  inputFile += "      &EWALD        \n"
  if force.DFT.POISSON.EWALD.ALPHA is not None:
    inputFile += "      ALPHA       {}\n".format(force.DFT.POISSON.EWALD.ALPHA)
  if force.DFT.POISSON.EWALD.EPSILON is not None:
    inputFile += "      EPSILON       {}\n".format(force.DFT.POISSON.EWALD.EPSILON)
  if force.DFT.POISSON.EWALD.EWALD_ACCURACY is not None:
    inputFile += "      EWALD_ACCURACY       {}\n".format(force.DFT.POISSON.EWALD.EWALD_ACCURACY)
  if force.DFT.POISSON.EWALD.EWALD_TYPE is not None:
    inputFile += "      EWALD_TYPE       {}\n".format(force.DFT.POISSON.EWALD.EWALD_TYPE)
  if force.DFT.POISSON.EWALD.GMAX is not None:
    inputFile += "      GMAX       {}\n".format(force.DFT.POISSON.EWALD.GMAX)
  if force.DFT.POISSON.EWALD.NS_MAX is not None:
    inputFile += "      NS_MAX       {}\n".format(force.DFT.POISSON.EWALD.NS_MAX)
  if force.DFT.POISSON.EWALD.O_SPLINE is not None:
    inputFile += "      O_SPLINE       {}\n".format(force.DFT.POISSON.EWALD.O_SPLINE)
  if force.DFT.POISSON.EWALD.RCUT is not None:
    inputFile += "      RCUT       {}\n".format(force.DFT.POISSON.EWALD.RCUT)
  

  inputFile += "      &END EWALD        \n"
    
    
  



  inputFile += "    &END POISSON        \n"



### dft PRINT
  inputFile += "    &PRINT         \n"

  # START E_DENSITY_CUBE
  if force.DFT.PRINT.E_DENSITY_CUBE.SECTION_PARAMETERS is not None:
    inputFile += "      &E_DENSITY_CUBE       {}\n".format(force.DFT.PRINT.E_DENSITY_CUBE.SECTION_PARAMETERS)
    if force.DFT.PRINT.E_DENSITY_CUBE.ADD_LAST is not None:
      inputFile += "        ADD_LAST        {}\n".format(force.DFT.PRINT.E_DENSITY_CUBE.ADD_LAST)
    if force.DFT.PRINT.E_DENSITY_CUBE.APPEND is not None:
      inputFile += "        APPEND        {}\n".format(force.DFT.PRINT.E_DENSITY_CUBE.APPEND)
    if force.DFT.PRINT.E_DENSITY_CUBE.COMMON_ITERATION_LEVELS is not None:
      inputFile += "        COMMON_ITERATION_LEVELS        {}\n".format(force.DFT.PRINT.E_DENSITY_CUBE.COMMON_ITERATION_LEVELS)
    if force.DFT.PRINT.E_DENSITY_CUBE.FILENAME is not None:
      inputFile += "        FILENAME        {}\n".format(force.DFT.PRINT.E_DENSITY_CUBE.FILENAME)
    if force.DFT.PRINT.E_DENSITY_CUBE.LOG_PRINT_KEY is not None:
      inputFile += "        LOG_PRINT_KEY        {}\n".format(force.DFT.PRINT.E_DENSITY_CUBE.LOG_PRINT_KEY)
    if force.DFT.PRINT.E_DENSITY_CUBE.NGAUSS is not None:
      inputFile += "        NGAUSS        {}\n".format(force.DFT.PRINT.E_DENSITY_CUBE.NGAUSS)
    if force.DFT.PRINT.E_DENSITY_CUBE.STRIDE is not None:
      inputFile += "        STRIDE        {}\n".format(force.DFT.PRINT.E_DENSITY_CUBE.STRIDE)

    if force.DFT.PRINT.E_DENSITY_CUBE.TOTAL_DENSITY is not None:
      inputFile += "        TOTAL_DENSITY        {}\n".format(force.DFT.PRINT.E_DENSITY_CUBE.TOTAL_DENSITY)
    if force.DFT.PRINT.E_DENSITY_CUBE.XRD_INTERFACE is not None:
      inputFile += "        XRD_INTERFACE        {}\n".format(force.DFT.PRINT.E_DENSITY_CUBE.XRD_INTERFACE)


    inputFile += "      &END E_DENSITY_CUBE       \n"






# END DFT PRINT
  inputFile += "    &END PRINT        \n"
    
  inputFile += "  &END DFT        \n"
    ## END DFT
    
    
  inputFile += "  &END FORCE_EVAL        \n"

# END FORCE_EVAL
 
  return inputFile

