class FM_DIAG_SETTINGS:

  def __init__(self,ELPA_FORCE_REDISTRIBUTE=False,PARAMETER_A=4,PARAMETER_X=60,
               PRINT_FM_REDISTRIBUTE=False,errors=[]):

    self.__ELPA_FORCE_REDISTRIBUTE = ELPA_FORCE_REDISTRIBUTE
    self.__PARAMETER_A             = PARAMETER_A
    self.__PARAMETER_X             = PARAMETER_X
    self.__PRINT_FM_REDISTRIBUTE   = PRINT_FM_REDISTRIBUTE
    self.__errors = errors

  @property
  def ELPA_FORCE_REDISTRIBUTE(self):
    return self.__ELPA_FORCE_REDISTRIBUTE

  @property
  def PARAMETER_A(self):
    return self.__PARAMETER_A

  @property
  def PARAMETER_A(self):
    return self.__PARAMETER_A

  
