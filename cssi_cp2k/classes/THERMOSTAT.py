import datetime
import cssi_cp2k.utilities as utilities
from cssi_cp2k.classes import NOSE

class THERMOSTAT:

  REGION_VALS = ["DEFINED","GLOBAL","MASSIVE","MOLECULE","NONE"]
  TYPE_VALS   = ["AD_LANGEVIN","CSVR","GLE","NOSE"]

  def _validate_region(val):
    val = str(val).upper()
    if val in REGION_VALS:
      return val
    else:
      errorMessage = ("Invalid option for MD THERMOSTAT REGION: {}. Valid options are: {}".format(
                     REGION_VALS))
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'THERMOSTAT',
                              'Variable':'REGION','ErrorMessage':errorMessage})
      raise TypeError

  def _validate_type(val):
    val = str(val).upper()
    if val in TYPE_VALS:
      return val
    else:
      errorMessage = ("Invalid option for MD THERMOSTAT TYPE: {}. Valid options are: {}".format(
                       TYPE_VALS))
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'THERMOSTAT',
                              'Variable':'TYPE','ErrorMessage':errorMessage})
      raise TypeError

  def __init__(self,TYPE="NOSE",REGION="GLOBAL",errorLog=[],changeLog=[]):

    self.__TYPE      = _validate_type(TYPE)
    self.__REGION    = _validate_region(REGION)
    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    #THERMOSTAT subsections
    self.__NOSE      = NOSE(errorLog=self.__errorLog,changeLog=self.__changeLog)

  @property
  def REGION(self):
    return self.__REGION

  @property
  def TYPE(self):
    return self.__TYPE

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @property
  def NOSE(self):
    return self.__NOSE

  @REGION.setter
  def REGION(self,val):
    val = str(val).upper()
    if val in REGION_VALS:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'THERMOSTAT','Variable':'REGION',
                               'Success':True,'Previous':self.__REGION,'New':val,
                               'ErrorMessage':None})
      self.__REGION = val
    else:
      errorMessage = ("Invalid option for MD THERMOSTAT REGION: {}. Valid options are: {}".format(
                       REGION_VALS))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'THERMOSTAT','Variable':'REGION',
                               'Success':False,'Previous':self.__REGION,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'THERMOSTAT',
                              'Variable':'REGION','ErrorMessage':errorMessage})

  @TYPE.setter
  def TYPE(self,val):
    val = str(val).upper()
    if val in TYPE_VALS:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'THERMOSTAT','Variable':'TYPE',
                               'Success':True,'Previous':self.__TYPE,'New':val,
                               'ErrorMessage':None})
      self.__TYPE = val
    else:
      errorMessage = ("Invalid option for MD THERMOSTAT TYPE: {}. Valid options are: {}".format(
                       TYPE_VALS))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'THERMOSTAT','Variable':'TYPE',
                               'Success':False,'Previous':self.__TYPE,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'THERMOSTAT',
                              'Variable':'TYPE','ErrorMessage':errorMessage})
