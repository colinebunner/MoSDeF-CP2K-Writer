import datetime
import mosdef_cp2k_writer.utilities as utilities

from mosdef_cp2k_writer.classes import POISSON
from mosdef_cp2k_writer.classes import FORCEFIELD




BOOL_VALS   = [".TRUE.",".FALSE.","TRUE","FALSE"]


class MM:

  def __init__(self, errorLog=[],changeLog=[],location=""):
    self.__errorLog = errorLog
    self.__changeLog = changeLog
    self.__location  = "{}/MM".format(location)
    self.__POISSON = POISSON.POISSON(errorLog=self.__errorLog, changeLog=self.__changeLog,
                         location=self.__location)
    self.__FORCEFIELD       = FORCEFIELD.FORCEFIELD(errorLog=self.__errorLog,changeLog=self.__changeLog,
                           location=self.__location)

    
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
  def POISSON(self):
      return self.__POISSON
  @property
  def FORCEFIELD(self):
      return self.__FORCEFIELD
