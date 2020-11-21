import datetime
import cssi_cp2k.utilities as utilities
from cssi_cp2k.classes import BOX_DISPLACEMENTS
from cssi_cp2k.classes import MOL_DISPLACEMENTS





class MAX_DISPLACEMENTS:

  def __init__(self,errorLog=[],changeLog=[],location=""):
   

    self.__errorLog  = errorLog
    self.__changeLog = changeLog

    self.__location  = "{}/MAX_DISPLACEMENTS".format(location)
    #THERMOSTAT subsections
    self.__BOX_DISPLACEMENTS      = BOX_DISPLACEMENTS.BOX_DISPLACEMENTS(errorLog=self.__errorLog,changeLog=self.__changeLog,
                         location=self.__location)
    self.__MOL_DISPLACEMENTS = MOL_DISPLACEMENTS.MOL_DISPLACEMENTS(errorLog=self.__errorLog, changeLog=self.__changeLog,
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
  def BOX_DISPLACEMENTS(self):
    return self.__BOX_DISPLACEMENTS

  @property
  def MOL_DISPLACEMENTS(self):
    return self.__MOL_DISPLACEMENTS

