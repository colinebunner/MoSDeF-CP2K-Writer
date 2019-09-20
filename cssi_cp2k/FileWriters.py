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
        #start stress

    if pri.STRESS.SECTION_PARAMETERS is not None:
      inputFile += "    &STRESS       {}\n".format(pri.STRESS.SECTION_PARAMETERS)
      if pri.STRESS.ADD_LAST is not None:
        inputFile += "      ADD_LAST        {}\n".format(pri.STRESS.ADD_LAST)
      
      
      if pri.STRESS.COMMON_ITERATION_LEVELS is not None:
        inputFile += "      COMMON_ITERATION_LEVELS        {}\n".format(pri.STRESS.COMMON_ITERATION_LEVELS)
      if pri.STRESS.FILENAME is not None:
        inputFile += "      FILENAME        {}\n".format(pri.STRESS.FILENAME)
      if pri.STRESS.LOG_PRINT_KEY is not None:
        inputFile += "      LOG_PRINT_KEY        {}\n".format(pri.STRESS.LOG_PRINT_KEY)

      # EACH SUBSECTION
      inputFile += "      &EACH       \n"
      if pri.STRESS.EACH.BAND is not None:
        inputFile += "        BAND                  {}             \n".format(pri.STRESS.EACH.BAND)
      if pri.STRESS.EACH.BSSE is not None:
        inputFile += "        BSSE                  {}             \n".format(pri.STRESS.EACH.BSSE)
      if pri.STRESS.EACH.CELL_OPT is not None:
        inputFile += "        CELL_OPT              {}             \n".format(
          mot.MD.PRINT.STRESS.EACH.CELL_OPT)
      if pri.STRESS.EACH.EP_LIN_SOLVER is not None:
        inputFile += "        EP_LIN_SOLVER              {}             \n".format(
          pri.STRESS.EACH.EP_LIN_SOLVER)
      if pri.STRESS.EACH.GEO_OPT is not None:
        inputFile += "        GEO_OPT              {}             \n".format(pri.STRESS.EACH.GEO_OPT)
      if pri.STRESS.EACH.JUST_ENERGY is not None:
        inputFile += "        JUST_ENERGY              {}             \n".format(
          pri.STRESS.EACH.JUST_ENERGY)
      if pri.STRESS.EACH.MD is not None:
        inputFile += "        MD              {}             \n".format(pri.STRESS.EACH.MD)
      if pri.STRESS.EACH.METADYNAMICS is not None:
        inputFile += "        METADYNAMICS              {}             \n".format(
          pri.STRESS.EACH.METADYNAMICS)
      if pri.STRESS.EACH.PINT is not None:
        inputFile += "        PINT              {}             \n".format(pri.STRESS.EACH.PINT)
      if pri.STRESS.EACH.POWELL_OPT is not None:
        inputFile += "        POWELL_OPT                  {}             \n".format(
          pri.STRESS.EACH.POWELL_OPT)
      if pri.STRESS.EACH.QS_SCF is not None:
        inputFile += "        QS_SCF              {}             \n".format(pri.STRESS.EACH.QS_SCF)
      if pri.STRESS.EACH.REPLICA_EVAL is not None:
        inputFile += "        REPLICA_EVAL              {}             \n".format(
          pri.STRESS.EACH.REPLICA_EVAL)
      if pri.STRESS.EACH.ROT_OPT is not None:
        inputFile += "        ROT_OPT              {}             \n".format(pri.STRESS.EACH.ROT_OPT)
      if pri.STRESS.EACH.SHELL_OPT is not None:
        inputFile += "        SHELL_OPT              {}             \n".format(
          pri.STRESS.EACH.SHELL_OPT)
      if pri.STRESS.EACH.SPLINE_FIND_COEFFS is not None:
        inputFile += "        SPLINE_FIND_COEFFS              {}             \n".format(
          pri.STRESS.EACH.SPLINE_FIND_COEFFS)
      if pri.STRESS.EACH.TDDFT_SCF is not None:
        inputFile += "        TDDFT_SCF              {}             \n".format(
          pri.STRESS.EACH.TDDFT_SCF)
      if pri.STRESS.EACH.XAS_SCF is not None:
        inputFile += "        XAS_SCF              {}             \n".format(pri.STRESS.EACH.XAS_SCF)
      if pri.STRESS.EACH.ROOT is not None:
        inputFile += "        __ROOT__              {}             \n".format(pri.STRESS.EACH.ROOT)
      inputFile += "      &END EACH       \n"

      # END EACH

      inputFile += "    &END STRESS       \n"

      # END STRESS

        
        
        #end stress
        
        
        
# START VELOCITIES
    if pri.VELOCITIES.SECTION_PARAMETERS is not None:
      inputFile += "    &VELOCITIES       {}\n".format(pri.VELOCITIES.SECTION_PARAMETERS)
      if pri.VELOCITIES.ADD_LAST is not None:
        inputFile += "      ADD_LAST        {}\n".format(pri.VELOCITIES.ADD_LAST)

      if pri.VELOCITIES.COMMON_ITERATION_LEVELS is not None:
        inputFile += "      COMMON_ITERATION_LEVELS        {}\n".format(pri.VELOCITIES.COMMON_ITERATION_LEVELS)
      if pri.VELOCITIES.FILENAME is not None:
        inputFile += "      FILENAME        {}\n".format(pri.VELOCITIES.FILENAME)
      if pri.VELOCITIES.FORMAT is not None:
        inputFile += "      FORMAT        {}\n".format(pri.VELOCITIES.FORMAT)
        
      if pri.VELOCITIES.LOG_PRINT_KEY is not None:
        inputFile += "      LOG_PRINT_KEY        {}\n".format(pri.VELOCITIES.LOG_PRINT_KEY)
      if pri.VELOCITIES.UNIT is not None:
        inputFile += "      UNIT        {}\n".format(pri.VELOCITIES.UNIT)

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

      inputFile += "    &END VELOCITIES       \n"

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
    
    
    ### sTART MGRID
  inputFile += "    &MGRID        \n"
  if force.DFT.MGRID.COMMENSURATE is not None:
    inputFile += "      COMMENSURATE      {}\n".format(force.DFT.MGRID.COMMENSURATE)
  if force.DFT.MGRID.CUTOFF is not None:
    inputFile += "      CUTOFF      {}\n".format(force.DFT.MGRID.CUTOFF)
  if force.DFT.MGRID.MULTIGRID_CUTOFF is not None:
    inputFile += "      MULTIGRID_CUTOFF      {}\n".format(force.DFT.MGRID.MULTIGRID_CUTOFF)
  if force.DFT.MGRID.MULTIGRID_SET is not None:
    inputFile += "      MULTIGRID_SET      {}\n".format(force.DFT.MGRID.MULTIGRID_SET)
  if force.DFT.MGRID.NGRIDS is not None:
    inputFile += "      NGRIDS      {}\n".format(force.DFT.MGRID.NGRIDS)
  if force.DFT.MGRID.PROGRESSION_FACTOR is not None:
    inputFile += "      PROGRESSION_FACTOR      {}\n".format(force.DFT.MGRID.PROGRESSION_FACTOR)
  if force.DFT.MGRID.REALSPACE is not None:
    inputFile += "      REALSPACE      {}\n".format(force.DFT.MGRID.REALSPACE)
  if force.DFT.MGRID.REL_CUTOFF is not None:
    inputFile += "      REL_CUTOFF      {}\n".format(force.DFT.MGRID.REL_CUTOFF)
  if force.DFT.MGRID.SKIP_LOAD_BALANCE_DISTRIBUTED is not None:
    inputFile += "      SKIP_LOAD_BALANCE_DISTRIBUTED      {}\n".format(force.DFT.MGRID.SKIP_LOAD_BALANCE_DISTRIBUTED)

  

  
  inputFile += "    &END MGRID        \n"
    ### END MGRID
    
    
    
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
# START DFT SCF    
  inputFile += "    &SCF        \n"
    
    
  if force.DFT.SCF.ADDED_MOS is not None:
    inputFile += "      ADDED_MOS       {}\n".format(force.DFT.SCF.ADDED_MOS)
  if force.DFT.SCF.CHOLESKY is not None:
    inputFile += "      CHOLESKY       {}\n".format(force.DFT.SCF.CHOLESKY)
  if force.DFT.SCF.EPS_DIIS is not None:
    inputFile += "      EPS_DIIS       {}\n".format(force.DFT.SCF.EPS_DIIS)
  if force.DFT.SCF.EPS_EIGVAL is not None:
    inputFile += "      EPS_EIGVAL       {}\n".format(force.DFT.SCF.EPS_EIGVAL)
  if force.DFT.SCF.EPS_LUMO is not None:
    inputFile += "      EPS_LUMO       {}\n".format(force.DFT.SCF.EPS_LUMO)
  if force.DFT.SCF.EPS_SCF is not None:
    inputFile += "      EPS_SCF       {}\n".format(force.DFT.SCF.EPS_SCF)
  if force.DFT.SCF.EPS_SCF_HISTORY is not None:
    inputFile += "      EPS_SCF_HISTORY       {}\n".format(force.DFT.SCF.EPS_SCF_HISTORY)
  if force.DFT.SCF.LEVEL_SHIFT is not None:
    inputFile += "      LEVEL_SHIFT       {}\n".format(force.DFT.SCF.LEVEL_SHIFT)
  if force.DFT.SCF.MAX_DIIS is not None:
    inputFile += "      MAX_DIIS       {}\n".format(force.DFT.SCF.MAX_DIIS)
  if force.DFT.SCF.MAX_ITER_LUMO is not None:
    inputFile += "      MAX_ITER_LUMO       {}\n".format(force.DFT.SCF.MAX_ITER_LUMO)
  if force.DFT.SCF.MAX_SCF is not None:
    inputFile += "      MAX_SCF       {}\n".format(force.DFT.SCF.MAX_SCF)
  if force.DFT.SCF.MAX_SCF_HISTORY is not None:
    inputFile += "      MAX_SCF_HISTORY       {}\n".format(force.DFT.SCF.MAX_SCF_HISTORY)
  if force.DFT.SCF.NCOL_BLOCK is not None:
    inputFile += "      NCOL_BLOCK       {}\n".format(force.DFT.SCF.NCOL_BLOCK)
  if force.DFT.SCF.NOTCONV_STOPALL is not None:
    inputFile += "      NOTCONV_STOPALL       {}\n".format(force.DFT.SCF.NOTCONV_STOPALL)
  if force.DFT.SCF.NROW_BLOCK is not None:
    inputFile += "      NROW_BLOCK       {}\n".format(force.DFT.SCF.NROW_BLOCK)
  if force.DFT.SCF.ROKS_F is not None:
    inputFile += "      ROKS_F       {}\n".format(force.DFT.SCF.ROKS_F)
  if force.DFT.SCF.ROKS_PARAMETERS is not None:
    inputFile += "      ROKS_PARAMETERS       {}\n".format(force.DFT.SCF.ROKS_PARAMETERS)
  if force.DFT.SCF.ROKS_SCHEME is not None:
    inputFile += "      ROKS_SCHEME       {}\n".format(force.DFT.SCF.ROKS_SCHEME)
  if force.DFT.SCF.SCF_GUESS is not None:
    inputFile += "      SCF_GUESS       {}\n".format(force.DFT.SCF.SCF_GUESS)

  
  if force.DFT.SCF.OT.SECTION_PARAMETERS is not None:
    inputFile += "      &OT       {} \n".format(force.DFT.SCF.OT.SECTION_PARAMETERS)
    if force.DFT.SCF.OT.ALGORITHM is not None:
      inputFile += "        ALGORITHM       {}\n".format(force.DFT.SCF.OT.ALGORITHM)
    if force.DFT.SCF.OT.BROYDEN_ADAPTIVE_SIGMA is not None:
      inputFile += "        BROYDEN_ADAPTIVE_SIGMA       {}\n".format(force.DFT.SCF.OT.BROYDEN_ADAPTIVE_SIGMA)
    if force.DFT.SCF.OT.BROYDEN_BETA is not None:
      inputFile += "        BROYDEN_BETA       {}\n".format(force.DFT.SCF.OT.BROYDEN_BETA)
    if force.DFT.SCF.OT.BROYDEN_ENABLE_FLIP is not None:
      inputFile += "        BROYDEN_ENABLE_FLIP       {}\n".format(force.DFT.SCF.OT.BROYDEN_ENABLE_FLIP)
    if force.DFT.SCF.OT.BROYDEN_ETA is not None:
      inputFile += "        BROYDEN_ETA       {}\n".format(force.DFT.SCF.OT.BROYDEN_ETA)
    if force.DFT.SCF.OT.BROYDEN_FORGET_HISTORY is not None:
      inputFile += "        BROYDEN_FORGET_HISTORY       {}\n".format(force.DFT.SCF.OT.BROYDEN_FORGET_HISTORY)
    if force.DFT.SCF.OT.BROYDEN_GAMMA is not None:
      inputFile += "        BROYDEN_GAMMA       {}\n".format(force.DFT.SCF.OT.BROYDEN_GAMMA)
    if force.DFT.SCF.OT.BROYDEN_OMEGA is not None:
      inputFile += "        BROYDEN_OMEGA       {}\n".format(force.DFT.SCF.OT.BROYDEN_OMEGA)
    if force.DFT.SCF.OT.BROYDEN_SIGMA is not None:
      inputFile += "        BROYDEN_SIGMA       {}\n".format(force.DFT.SCF.OT.BROYDEN_SIGMA)
    if force.DFT.SCF.OT.BROYDEN_SIGMA_DECREASE is not None:
      inputFile += "        BROYDEN_SIGMA_DECREASE       {}\n".format(force.DFT.SCF.OT.BROYDEN_SIGMA_DECREASE)
    if force.DFT.SCF.OT.BROYDEN_SIGMA_MIN is not None:
      inputFile += "        BROYDEN_SIGMA_MIN       {}\n".format(force.DFT.SCF.OT.BROYDEN_SIGMA_MIN)
    if force.DFT.SCF.OT.CHOLESKY is not None:
      inputFile += "        CHOLESKY       {}\n".format(force.DFT.SCF.OT.CHOLESKY)
    if force.DFT.SCF.OT.ENERGIES is not None:
      inputFile += "        ENERGIES       {}\n".format(force.DFT.SCF.OT.ENERGIES)
    if force.DFT.SCF.OT.ENERGY_GAP is not None:
      inputFile += "        ENERGY_GAP       {}\n".format(force.DFT.SCF.OT.ENERGY_GAP)
    if force.DFT.SCF.OT.EPS_IRAC is not None:
      inputFile += "        EPS_IRAC       {}\n".format(force.DFT.SCF.OT.EPS_IRAC)
    if force.DFT.SCF.OT.EPS_IRAC_FILTER_MATRIX is not None:
      inputFile += "        EPS_IRAC_FILTER_MATRIX       {}\n".format(force.DFT.SCF.OT.EPS_IRAC_FILTER_MATRIX)
    if force.DFT.SCF.OT.EPS_IRAC_QUICK_EXIT is not None:
      inputFile += "        EPS_IRAC_QUICK_EXIT       {}\n".format(force.DFT.SCF.OT.EPS_IRAC_QUICK_EXIT)
    if force.DFT.SCF.OT.EPS_IRAC_SWITCH is not None:
      inputFile += "        EPS_IRAC_SWITCH       {}\n".format(force.DFT.SCF.OT.EPS_IRAC_SWITCH)
    if force.DFT.SCF.OT.EPS_TAYLOR is not None:
      inputFile += "        EPS_TAYLOR       {}\n".format(force.DFT.SCF.OT.EPS_TAYLOR)
    if force.DFT.SCF.OT.GOLD_TARGET is not None:
      inputFile += "        GOLD_TARGET       {}\n".format(force.DFT.SCF.OT.GOLD_TARGET)
    if force.DFT.SCF.OT.IRAC_DEGREE is not None:
      inputFile += "        IRAC_DEGREE       {}\n".format(force.DFT.SCF.OT.IRAC_DEGREE)
    if force.DFT.SCF.OT.LINESEARCH is not None:
      inputFile += "        LINESEARCH       {}\n".format(force.DFT.SCF.OT.LINESEARCH)
    if force.DFT.SCF.OT.MAX_IRAC is not None:
      inputFile += "        MAX_IRAC       {}\n".format(force.DFT.SCF.OT.MAX_IRAC)
    if force.DFT.SCF.OT.MAX_TAYLOR is not None:
      inputFile += "        MAX_TAYLOR       {}\n".format(force.DFT.SCF.OT.MAX_TAYLOR)
    if force.DFT.SCF.OT.MINIMIZER is not None:
      inputFile += "        MINIMIZER       {}\n".format(force.DFT.SCF.OT.MINIMIZER)
    
    
    if force.DFT.SCF.OT.NONDIAG_ENERGY is not None:
      inputFile += "        NONDIAG_ENERGY       {}\n".format(force.DFT.SCF.OT.NONDIAG_ENERGY)
    if force.DFT.SCF.OT.NONDIAG_ENERGY_STRENGTH is not None:
      inputFile += "        NONDIAG_ENERGY_STRENGTH       {}\n".format(force.DFT.SCF.OT.NONDIAG_ENERGY_STRENGTH)
    if force.DFT.SCF.OT.N_HISTORY_VEC is not None:
      inputFile += "        N_HISTORY_VEC       {}\n".format(force.DFT.SCF.OT.N_HISTORY_VEC)
    if force.DFT.SCF.OT.OCCUPATION_PRECONDITIONER is not None:
      inputFile += "        OCCUPATION_PRECONDITIONER       {}\n".format(force.DFT.SCF.OT.OCCUPATION_PRECONDITIONER)
    if force.DFT.SCF.OT.ON_THE_FLY_LOC is not None:
      inputFile += "        ON_THE_FLY_LOC       {}\n".format(force.DFT.SCF.OT.ON_THE_FLY_LOC)
    if force.DFT.SCF.OT.ORTHO_IRAC is not None:
      inputFile += "        ORTHO_IRAC       {}\n".format(force.DFT.SCF.OT.ORTHO_IRAC)
    if force.DFT.SCF.OT.PRECONDITIONER is not None:
      inputFile += "        PRECONDITIONER       {}\n".format(force.DFT.SCF.OT.PRECONDITIONER)
    if force.DFT.SCF.OT.PRECOND_SOLVER is not None:
      inputFile += "        PRECOND_SOLVER       {}\n".format(force.DFT.SCF.OT.PRECOND_SOLVER)
    if force.DFT.SCF.OT.ROTATION is not None:
      inputFile += "        ROTATION       {}\n".format(force.DFT.SCF.OT.ROTATION)
    if force.DFT.SCF.OT.SAFE_DIIS is not None:
      inputFile += "        SAFE_DIIS       {}\n".format(force.DFT.SCF.OT.SAFE_DIIS)
    if force.DFT.SCF.OT.STEPSIZE is not None:
      inputFile += "        STEPSIZE       {}\n".format(force.DFT.SCF.OT.STEPSIZE)
     #eND ot OF SCF OF FORCE_EVAL
    inputFile += "      &END OT        \n"
    
    
    #outer_scf
    
    if force.DFT.SCF.OUTER_SCF.SECTION_PARAMETERS is not None:
      inputFile += "      &OUTER_SCF       {} \n".format(force.DFT.SCF.OUTER_SCF.SECTION_PARAMETERS)
      if force.DFT.SCF.OUTER_SCF.BISECT_TRUST_COUNT is not None:
        inputFile += "        BISECT_TRUST_COUNT       {}\n".format(force.DFT.SCF.OUTER_SCF.BISECT_TRUST_COUNT)
      if force.DFT.SCF.OUTER_SCF.DIIS_BUFFER_LENGTH is not None:
        inputFile += "        DIIS_BUFFER_LENGTH       {}\n".format(force.DFT.SCF.OUTER_SCF.DIIS_BUFFER_LENGTH)
      if force.DFT.SCF.OUTER_SCF.EPS_SCF is not None:
        inputFile += "        EPS_SCF       {}\n".format(force.DFT.SCF.OUTER_SCF.EPS_SCF)
      if force.DFT.SCF.OUTER_SCF.EXTRAPOLATION_ORDER is not None:
        inputFile += "        EXTRAPOLATION_ORDER       {}\n".format(force.DFT.SCF.OUTER_SCF.EXTRAPOLATION_ORDER)
      if force.DFT.SCF.OUTER_SCF.MAX_SCF is not None:
        inputFile += "        MAX_SCF       {}\n".format(force.DFT.SCF.OUTER_SCF.MAX_SCF)
      if force.DFT.SCF.OUTER_SCF.OPTIMIZER is not None:
        inputFile += "        OPTIMIZER       {}\n".format(force.DFT.SCF.OUTER_SCF.OPTIMIZER)
      if force.DFT.SCF.OUTER_SCF.STEP_SIZE is not None:
        inputFile += "        STEP_SIZE       {}\n".format(force.DFT.SCF.OUTER_SCF.STEP_SIZE)
      if force.DFT.SCF.OUTER_SCF.TYPE is not None:
        inputFile += "        TYPE       {}\n".format(force.DFT.SCF.OUTER_SCF.TYPE)
    

     #eND ot OF SCF OF FORCE_EVAL
      inputFile += "      &END OUTER_SCF        \n"
    
    
    
    
    
    
    
    
    
    inputFile += "      &PRINT        \n"
    
    if force.DFT.SCF.PRINT.DM_RESTART_WRITE is not None:
      inputFile += "        DM_RESTART_WRITE       {}\n".format(force.DFT.SCF.PRINT.DM_RESTART_WRITE)
    
    if force.DFT.SCF.PRINT.RESTART.SECTION_PARAMETERS is not None:
      inputFile += "        &RESTART       {}\n".format(force.DFT.SCF.PRINT.RESTART.SECTION_PARAMETERS)
      if force.DFT.SCF.PRINT.RESTART.ADD_LAST is not None:
        inputFile += "        ADD_LAST       {}\n".format(force.DFT.SCF.PRINT.RESTART.ADD_LAST)
      if force.DFT.SCF.PRINT.RESTART.BACKUP_COPIES is not None:
        inputFile += "        BACKUP_COPIES       {}\n".format(force.DFT.SCF.PRINT.RESTART.BACKUP_COPIES)
      if force.DFT.SCF.PRINT.RESTART.COMMON_ITERATION_LEVELS is not None:
        inputFile += "        COMMON_ITERATION_LEVELS       {}\n".format(force.DFT.SCF.PRINT.RESTART.COMMON_ITERATION_LEVELS)
      if force.DFT.SCF.PRINT.RESTART.FILENAME is not None:
        inputFile += "        FILENAME       {}\n".format(force.DFT.SCF.PRINT.RESTART.FILENAME)
      if force.DFT.SCF.PRINT.RESTART.LOG_PRINT_KEY is not None:
        inputFile += "        LOG_PRINT_KEY       {}\n".format(force.DFT.SCF.PRINT.RESTART.LOG_PRINT_KEY)

    
 
      inputFile += "      &END RESTART   \n"
      
      
    
    
    
    inputFile += "      &END PRINT       \n"

    
  
  inputFile += "    &END SCF        \n"
    
    #END SCF
    
    
    #START XC
  inputFile += "    &XC        \n"   
    
  if force.DFT.XC.DENSITY_CUTOFF is not None:
    inputFile += "      DENSITY_CUTOFF       {}\n".format(force.DFT.XC.DENSITY_CUTOFF)
  if force.DFT.XC.DENSITY_SMOOTH_CUTOFF_RANGE is not None:
    inputFile += "      DENSITY_SMOOTH_CUTOFF_RANGE       {}\n".format(force.DFT.XC.DENSITY_SMOOTH_CUTOFF_RANGE)
  if force.DFT.XC.FUNCTIONAL_ROUTINE is not None:
    inputFile += "      FUNCTIONAL_ROUTINE       {}\n".format(force.DFT.XC.FUNCTIONAL_ROUTINE)
  if force.DFT.XC.GRADIENT_CUTOFF is not None:
    inputFile += "      GRADIENT_CUTOFF       {}\n".format(force.DFT.XC.GRADIENT_CUTOFF)
  if force.DFT.XC.TAU_CUTOFF is not None:
    inputFile += "      TAU_CUTOFF       {}\n".format(force.DFT.XC.TAU_CUTOFF)

    #START XC FUNCTIONAL
  if force.DFT.XC.XC_FUNCTIONAL.SECTION_PARAMETERS is not None:
    inputFile += "      &XC_FUNCTIONAL       {} \n".format(force.DFT.XC.XC_FUNCTIONAL.SECTION_PARAMETERS)
    inputFile += "      &END XC_FUNCTIONAL     \n"
      #end xc fUNCTIONAL
        
        
        
      #start VDW section
  inputFile += "      &VDW_POTENTIAL        \n"
  if force.DFT.XC.VDW_POTENTIAL.POTENTIAL_TYPE is not None:
    inputFile += "        POTENTIAL_TYPE       {}\n".format(force.DFT.XC.VDW_POTENTIAL.  POTENTIAL_TYPE)

    
    #start VDW section
  inputFile += "        &PAIR_POTENTIAL        \n"

  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.ATOMPARM is not None:
    inputFile += "          ATOMPARM       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.ATOMPARM)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.ATOM_COORDINATION_NUMBERS is not None:
    inputFile += "          ATOM_COORDINATION_NUMBERS       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.ATOM_COORDINATION_NUMBERS)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.CALCULATE_C9_TERM is not None:
    inputFile += "          CALCULATE_C9_TERM       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.CALCULATE_C9_TERM)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.D3BJ_SCALING is not None:
    inputFile += "          D3BJ_SCALING       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.D3BJ_SCALING)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.D3_EXCLUDE_KIND is not None:
    inputFile += "          D3_EXCLUDE_KIND       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.D3_EXCLUDE_KIND)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.D3_SCALING is not None:
    inputFile += "          D3_SCALING       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.D3_SCALING)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.EPS_CN is not None:
    inputFile += "          EPS_CN       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.EPS_CN)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.EXP_PRE is not None:
    inputFile += "          EXP_PRE       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.EXP_PRE)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.KIND_COORDINATION_NUMBERS is not None:
    inputFile += "          KIND_COORDINATION_NUMBERS       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.KIND_COORDINATION_NUMBERS)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.LONG_RANGE_CORRECTION is not None:
    inputFile += "          LONG_RANGE_CORRECTION       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.LONG_RANGE_CORRECTION)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.MOLECULE_CORRECTION is not None:
    inputFile += "          MOLECULE_CORRECTION       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.MOLECULE_CORRECTION)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.MOLECULE_CORRECTION_C8 is not None:
    inputFile += "          MOLECULE_CORRECTION_C8       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.MOLECULE_CORRECTION_C8)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.PARAMETER_FILE_NAME is not None:
    inputFile += "          PARAMETER_FILE_NAME       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.PARAMETER_FILE_NAME)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.REFERENCE_C9_TERM is not None:
    inputFile += "          REFERENCE_C9_TERM       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.REFERENCE_C9_TERM)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.REFERENCE_FUNCTIONAL is not None:
    inputFile += "          REFERENCE_FUNCTIONAL       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.REFERENCE_FUNCTIONAL)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.R_CUTOFF is not None:
    inputFile += "          R_CUTOFF       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.R_CUTOFF)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.SCALING is not None:
    inputFile += "          SCALING       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.SCALING)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.SHORT_RANGE_CORRECTION is not None:
    inputFile += "          SHORT_RANGE_CORRECTION       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.SHORT_RANGE_CORRECTION)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.SHORT_RANGE_CORRECTION_PARAMETERS is not None:
    inputFile += "          SHORT_RANGE_CORRECTION_PARAMETERS       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.SHORT_RANGE_CORRECTION_PARAMETERS)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.TYPE is not None:
    inputFile += "          TYPE       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.TYPE)
  if force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.VERBOSE_OUTPUT is not None:
    inputFile += "          VERBOSE_OUTPUT       {}\n".format(force.DFT.XC.VDW_POTENTIAL.PAIR_POTENTIAL.VERBOSE_OUTPUT)
    

#start VDW section
  inputFile += "        &END PAIR_POTENTIAL        \n"

    
    
    
    
    
    
  inputFile += "      &END VDW_POTENTIAL        \n"
    
    #END XC
  inputFile += "    &END XC        \n"




  inputFile += "  &END DFT        \n"
    ## END DFT
    
    
    
  #stART subsys  
  inputFile += "  &SUBSYS        \n"
    
       #start CELL
    
  inputFile += "    &CELL        \n"

  if force.SUBSYS.CELL.A is not None:
    inputFile += "      A       {}\n".format(force.SUBSYS.CELL.A)
  if force.SUBSYS.CELL.ABC is not None:
    inputFile += "      ABC       {}\n".format(force.SUBSYS.CELL.ABC)
  if force.SUBSYS.CELL.ALPHA_BETA_GAMMA is not None:
    inputFile += "      ALPHA_BETA_GAMMA       {}\n".format(force.SUBSYS.CELL.ALPHA_BETA_GAMMA)
  if force.SUBSYS.CELL.B is not None:
    inputFile += "      B       {}\n".format(force.SUBSYS.CELL.B)
  if force.SUBSYS.CELL.C is not None:
    inputFile += "      C       {}\n".format(force.SUBSYS.CELL.C)
  if force.SUBSYS.CELL.CELL_FILE_FORMAT is not None:
    inputFile += "      CELL_FILE_FORMAT       {}\n".format(force.SUBSYS.CELL.CELL_FILE_FORMAT)
  if force.SUBSYS.CELL.CELL_FILE_NAME is not None:
    inputFile += "      CELL_FILE_NAME       {}\n".format(force.SUBSYS.CELL.CELL_FILE_NAME)
  if force.SUBSYS.CELL.MULTIPLE_UNIT_CELL is not None:
    inputFile += "      MULTIPLE_UNIT_CELL       {}\n".format(force.SUBSYS.CELL.MULTIPLE_UNIT_CELL)
  if force.SUBSYS.CELL.PERIODIC is not None:
    inputFile += "      PERIODIC       {}\n".format(force.SUBSYS.CELL.PERIODIC)
  if force.SUBSYS.CELL.SYMMETRY is not None:
    inputFile += "      SYMMETRY       {}\n".format(force.SUBSYS.CELL.SYMMETRY)

    
    

  inputFile += "    &END CELL        \n"
    #end CELL
    
       #start coord
    
  inputFile += "    &COORD        \n"

  if force.SUBSYS.COORD.DEFAULT_KEYWORD is not None:
    inputFile += "      @INCLUDE       {}\n".format(force.SUBSYS.COORD.DEFAULT_KEYWORD)
  if force.SUBSYS.COORD.SCALED is not None:
    inputFile += "      SCALED       {}\n".format(force.SUBSYS.COORD.SCALED)
  if force.SUBSYS.COORD.UNIT is not None:
    inputFile += "      UNIT       {}\n".format(force.SUBSYS.COORD.UNIT)





  inputFile += "    &END COORD        \n"
    #end coord
    
    
    
    #start KIND
    
 



  if force.SUBSYS.KIND.SECTION_PARAMETERS is not None:
    inputFile += "    &KIND      {}\n".format(force.SUBSYS.KIND.SECTION_PARAMETERS)
    if force.SUBSYS.KIND.AUX_BASIS_SET is not None:
      inputFile += "      AUX_BASIS_SET       {}\n".format(force.SUBSYS.KIND.AUX_BASIS_SET)
    if force.SUBSYS.KIND.AUX_FIT_BASIS_SET is not None:
      inputFile += "      AUX_FIT_BASIS_SET       {}\n".format(force.SUBSYS.KIND.AUX_FIT_BASIS_SET)
    if force.SUBSYS.KIND.BASIS_SET is not None:
      inputFile += "      BASIS_SET       {}\n".format(force.SUBSYS.KIND.BASIS_SET)
    if force.SUBSYS.KIND.CORE_CORRECTION is not None:
      inputFile += "      CORE_CORRECTION       {}\n".format(force.SUBSYS.KIND.CORE_CORRECTION)
    if force.SUBSYS.KIND.DFTB3_PARAM is not None:
      inputFile += "      DFTB3_PARAM       {}\n".format(force.SUBSYS.KIND.DFTB3_PARAM)
    if force.SUBSYS.KIND.ELEC_CONF is not None:
      inputFile += "      ELEC_CONF       {}\n".format(force.SUBSYS.KIND.ELEC_CONF)
    if force.SUBSYS.KIND.ELEMENT is not None:
      inputFile += "      ELEMENT       {}\n".format(force.SUBSYS.KIND.ELEMENT)
    if force.SUBSYS.KIND.FLOATING_BASIS_CENTER is not None:
      inputFile += "      FLOATING_BASIS_CENTER       {}\n".format(force.SUBSYS.KIND.FLOATING_BASIS_CENTER)
      
    if force.SUBSYS.KIND.GHOST is not None:
      inputFile += "      GHOST       {}\n".format(force.SUBSYS.KIND.GHOST)
      
    if force.SUBSYS.KIND.GPW_TYPE is not None:
      inputFile += "      GPW_TYPE       {}\n".format(force.SUBSYS.KIND.GPW_TYPE)
    if force.SUBSYS.KIND.HARD_EXP_RADIUS is not None:
      inputFile += "      HARD_EXP_RADIUS       {}\n".format(force.SUBSYS.KIND.HARD_EXP_RADIUS)
    if force.SUBSYS.KIND.KG_POTENTIAL is not None:
      inputFile += "      KG_POTENTIAL       {}\n".format(force.SUBSYS.KIND.KG_POTENTIAL)
    if force.SUBSYS.KIND.KG_POTENTIAL_FILE_NAME is not None:
      inputFile += "      KG_POTENTIAL_FILE_NAME       {}\n".format(force.SUBSYS.KIND.KG_POTENTIAL_FILE_NAME)
    if force.SUBSYS.KIND.LEBEDEV_GRID is not None:
      inputFile += "      LEBEDEV_GRID       {}\n".format(force.SUBSYS.KIND.LEBEDEV_GRID)
    if force.SUBSYS.KIND.LMAX_DFTB is not None:
      inputFile += "      LMAX_DFTB       {}\n".format(force.SUBSYS.KIND.LMAX_DFTB)
    
    if force.SUBSYS.KIND.LRI_BASIS_SET is not None:
      inputFile += "      LRI_BASIS_SET       {}\n".format(force.SUBSYS.KIND.LRI_BASIS_SET)
    if force.SUBSYS.KIND.MAGNETIZATION is not None:
      inputFile += "      MAGNETIZATION       {}\n".format(force.SUBSYS.KIND.MAGNETIZATION)
    if force.SUBSYS.KIND.MAO is not None:
      inputFile += "      MAO       {}\n".format(force.SUBSYS.KIND.MAO)
    if force.SUBSYS.KIND.MASS is not None:
      inputFile += "      MASS       {}\n".format(force.SUBSYS.KIND.MASS)
    if force.SUBSYS.KIND.MAX_RAD_LOCAL is not None:
      inputFile += "      MAX_RAD_LOCAL       {}\n".format(force.SUBSYS.KIND.MAX_RAD_LOCAL)
    if force.SUBSYS.KIND.MM_RADIUS is not None:
      inputFile += "      MM_RADIUS       {}\n".format(force.SUBSYS.KIND.MM_RADIUS)
    if force.SUBSYS.KIND.NO_OPTIMIZE is not None:
      inputFile += "      NO_OPTIMIZE       {}\n".format(force.SUBSYS.KIND.NO_OPTIMIZE)
    if force.SUBSYS.KIND.PAO_BASIS_SIZE is not None:
      inputFile += "      PAO_BASIS_SIZE       {}\n".format(force.SUBSYS.KIND.PAO_BASIS_SIZE)
    
    if force.SUBSYS.KIND.POTENTIAL is not None:
      inputFile += "      POTENTIAL       {}\n".format(force.SUBSYS.KIND.POTENTIAL)
    
    if force.SUBSYS.KIND.POTENTIAL_FILE_NAME is not None:
      inputFile += "      POTENTIAL_FILE_NAME       {}\n".format(force.SUBSYS.KIND.POTENTIAL_FILE_NAME)
    if force.SUBSYS.KIND.POTENTIAL_TYPE is not None:
      inputFile += "      POTENTIAL_TYPE       {}\n".format(force.SUBSYS.KIND.POTENTIAL_TYPE)
    if force.SUBSYS.KIND.RADIAL_GRID is not None:
      inputFile += "      RADIAL_GRID       {}\n".format(force.SUBSYS.KIND.RADIAL_GRID)
    if force.SUBSYS.KIND.RHO0_EXP_RADIUS is not None:
      inputFile += "      RHO0_EXP_RADIUS       {}\n".format(force.SUBSYS.KIND.RHO0_EXP_RADIUS)
    if force.SUBSYS.KIND.RI_AUX_BASIS_SET is not None:
      inputFile += "      RI_AUX_BASIS_SET       {}\n".format(force.SUBSYS.KIND.RI_AUX_BASIS_SET)
    if force.SUBSYS.KIND.SE_P_ORBITALS_ON_H is not None:
      inputFile += "      SE_P_ORBITALS_ON_H       {}\n".format(force.SUBSYS.KIND.SE_P_ORBITALS_ON_H)
    
    
    
    
    

    inputFile += "    &END KIND        \n"
    #end KIND
    
    
  
  inputFile += "  &END SUBSYS        \n"
      #END subsys
    
    
    
    
    
    
  inputFile += "&END FORCE_EVAL        \n"

# END FORCE_EVAL
 
  return inputFile

