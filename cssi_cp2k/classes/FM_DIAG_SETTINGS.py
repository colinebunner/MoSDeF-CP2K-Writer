import datetime

class FM_DIAG_SETTINGS:

  def __init__(self,ELPA_FORCE_REDISTRIBUTE=False,PARAMETER_A=4,PARAMETER_X=60,
               PRINT_FM_REDISTRIBUTE=False,errorLog=[],changeLog=[]):

    self.__ELPA_FORCE_REDISTRIBUTE = ELPA_FORCE_REDISTRIBUTE
    self.__PARAMETER_A             = PARAMETER_A
    self.__PARAMETER_X             = PARAMETER_X
    self.__PRINT_FM_REDISTRIBUTE   = PRINT_FM_REDISTRIBUTE
    self.__errorLog                = errorLog
    self.__changeLog               = changeLog

  @property
  def ELPA_FORCE_REDISTRIBUTE(self):
    return self.__ELPA_FORCE_REDISTRIBUTE

  @property
  def PARAMETER_A(self):
    return self.__PARAMETER_A

  @property
  def PARAMETER_X(self):
    return self.__PARAMETER_X

  @property
  def PRINT_FM_REDISTRIBUTE(self):
    return self.__PRINT_FM_REDISTRIBUTE

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @ELPA_FORCE_REDISTRIBUTE.setter
  def ELPA_FORCE_REDISTRIBUTE(self,val):
    if isinstance(val,bool):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'DBCSR',
                               'Variable':'ELPA_FORCE_REDISTRIBUTE','Success':True,
                               'Previous':self.__ELPA_FORCE_REDISTRIBUTE,'New':val,'ErrorMessage':None})
      self.__ELPA_FORCE_REDISTRIBUTE = val
    else:
      errorMessage = "ELPA_FORCE_REDISTRIBUTE must be a boolean. You passed {}.".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'DBCSR',
                               'Variable':'ELPA_FORCE_REDISTRIBUTE','Success':False,
                               'Previous':self.__ELPA_FORCE_REDISTRIBUTE,'New':val,'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'DBCSR',
                              'Variable':'ELPA_FORCE_REDISTRIBUTE','ErrorMessage':errorMessage})
