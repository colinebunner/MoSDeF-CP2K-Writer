import datetime
import cssi_cp2k.utilities as utilities
from cssi_cp2k.classes import ENERGY
from cssi_cp2k.classes import PROGRAM_RUN_INFO

class PRINT:

  def __init__(self,errorLog=[],changeLog=[],location=""):
  
    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    self.__location  = "{}/PRINT".format(location)
    #MD_PRINT subsections
    self.__ENERGY           = ENERGY.ENERGY(errorLog=self.__errorLog,changeLog=self.__changeLog,
                                location=self.__location)
    self.__PROGRAM_RUN_INFO = PROGRAM_RUN_INFO.PROGRAM_RUN_INFO(errorLog=self.__errorLog,
                                changeLog=self.__changeLog,location=self.__location)

  @property
  def errorLog(self):
    return self.__errorLog
  
  @property
  def changeLog(self):
    return self.__changeLog

  @property
  def location(self):
    return self.__location

  @property
  def ENERGY(self):
    return self.__ENERGY

  @property
  def PROGRAM_RUN_INFO(self):
    return self.__PROGRAM_RUN_INFO
