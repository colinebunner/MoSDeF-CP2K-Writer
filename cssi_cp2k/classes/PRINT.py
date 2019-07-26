#from cssi_cp2k.classes import FORCES as forces
class PRINT:

  def __init__(self,errorLog=[],changeLog=[],location=""):

    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    self.__location  = "{}/PRINT".format(location)
    #self.__FORCES
    #self.__FORCES=forces.FORCES(errorLog=self.__errorLog,changeLog=self.__changeLog,
                          # location=self.__location)

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @property
  def location(self):
    return self.__location

  #@property
  def FORCES(self):
    return self.__location
