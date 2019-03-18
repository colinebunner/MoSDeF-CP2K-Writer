from cssi_cp2k.classes import EACH

class PROGRAM_RUN_INFO:

  def __init__(self,errorLog=[],changeLog=[]):

    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    #PROGRAM_RUN_INFO subsections
    self.__EACH      = EACH.EACH(errorLog=self.__errorLog,changeLog=self.__changeLog)

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @property
  def EACH(self):
    return self.EACH
