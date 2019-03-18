import os
import random
from cssi_cp2k.classes import GLOBAL

class SIM:

  def __init__(self):
    
    self.__prod               = False
    self.__nstep              = 0
    self.__time               = 0.0e0
    self.__errorLog           = []
    self.__changeLog          = []
    self.__restartWFN         = "RESTART.wfn"
    self.__homeDirectory      = os.getcwd()
    self.__scratchDirectory   = "/tmp/cssi-cp2k-{}".format(int(random.random()*123456789))
    self.__GLOBAL             = GLOBAL.GLOBAL(errorLog=self.__errorLog,changeLog=self.__changeLog)
    
  @property
  def prod(self):
    return self.__prod
  
  @property
  def nstep(self):
    return self.__nstep

  @property
  def time(self):
    return self.__time

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @property
  def restartWFN(self):
    return self.__restartWFN

  @property
  def homeDirectory(self):
    return self.__homeDirectory

  @property
  def scratchDirectory(self):
    return self.__scratchDirectory

  @property
  def GLOBAL(self):
    return self.__GLOBAL

  @restartWFN.setter
  def restartWFN(self,val):
    if os.path.isfile(val):
      self.__restartWFN = val
    else:
      errorMessage = ("Type: Setter\nVar.: restartWFN\nErr.: Couldn't set restart wavefunction file to "
        "{} because file wasn't found.".format(val))
      self.__errorLog.append(errorMessage)

  @homeDirectory.setter
  def homeDirectory(self,val):
    self.__homeDirectory = val

  @scratchDirectory.setter
  def scratchDirectory(self,val):
    self.__scratchDirectory = scratchDirectory

  def write_errorLog(self,fn=None):
    # No argument or explicit None prints to screen
    if fn is None:
      for error in self.__errorLog:
        print(error)

  def write_changeLog(self,fn=None):
    # No argument or explicit None prints to screen
    if fn is None:
      for change in self.__changeLog:
        print(change)
