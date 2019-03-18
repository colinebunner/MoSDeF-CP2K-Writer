class TIMINGS:

  def __init__(self,errorLog=[],changeLog=[]):

    self.__errorLog = errorLog
    self.__changeLog = changeLog

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog
