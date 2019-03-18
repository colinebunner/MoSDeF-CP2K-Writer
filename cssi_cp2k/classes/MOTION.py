import datetime
import cssi_cp2k.utilities as utilities
from cssi_cp2k.classes import PRINT
from cssi_cp2k.classes import MD

class MOTION:

  def __init__(self,errorLog=[],changeLog=[]):
    
    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    # Subsections of MOTION
    self.__PRINT     = PRINT(errorLog=self.__errorLog,changeLog=self.__changeLog)
    self.__MD        = MD(errorLog=self.__errorLog,changeLog=self.__changeLog)

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog
