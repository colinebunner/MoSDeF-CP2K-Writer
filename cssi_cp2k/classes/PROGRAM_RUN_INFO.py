class PROGRAM_RUN_INFO:

  def __init__(self,errorLog=[],changeLog=[]):

    self.__errorLog = errorLog
    self.__changeLog = changeLog

  def errorLog(self):
    return self.__errorLog

  def changeLog(self):
    return self.__changeLog
