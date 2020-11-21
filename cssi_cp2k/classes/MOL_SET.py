import datetime
import cssi_cp2k.utilities as utilities

from cssi_cp2k.classes import MOLECULE




class MOL_SET:

  def __init__(self,errorLog=[],changeLog=[],location=""):


    self.__errorLog  = errorLog
    self.__changeLog = changeLog
 
    self.__location  = "{}/MOL_SET".format(location)
    # subsections
    self.__MOLECULE      = MOLECULE.MOLECULE(errorLog=self.__errorLog,changeLog=self.__changeLog,
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
  def MOLECULE(self):
    return self.__MOLECULE

