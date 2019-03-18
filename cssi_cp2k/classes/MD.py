import datetime
import cssi_cp2k.utilities as utilities
from cssi_cp2k.classes import THERMOSTAT as thermostat
from cssi_cp2k.classes import MD_PRINT as md_print


MD_ENSEMBLE_VALS = ["HYDROSTATICSHOCK","ISOKIN","LANGEVIN","MSST","MSST_DAMPED","NPE_F","NPE_I",
                    "NPT_F","NPT_I","NVE","NVT","NVT_ADIABATIC","REFTRAJ"]

def _validate_ensemble(val):
  val = str(val).upper()
  if val in MD_ENSEMBLE_VALS:
    return val
  else:
    errorMessage = "Invalid option for MD ENSEMBLE: {}. Valid options are: {}".format(MD_ENSEMBLE_VALS)
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MD',
                            'Variable':'ENSEMBLE','ErrorMessage':errorMessage})
    raise TypeError

def _validate_steps(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "MD STEPS must be an integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MD',
                            'Variable':'STEPS','ErrorMessage':errorMessage})
    raise TypeError

def _validate_timestep(val):
  if utilities.is_number(val):
    return val
  else:
    errorMessage = "MD TIMESTEP must be a number."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MD',
                            'Variable':'TIMESTEP','ErrorMessage':errorMessage})
    raise TypeError

def _validate_temperature(val):
  if utilities.is_number(val):
    return val
  else:
    errorMessage = "MD TEMPERATURE must be a number."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MD',
                            'Variable':'TEMPERATURE','ErrorMessage':errorMessage})
    raise TypeError

class MD:

  def __init__(self,ENSEMBLE="NVE",STEPS=3,TIMESTEP=0.5,TEMPERATURE=300.0,errorLog=[],changeLog=[]):
   
    self.__ENSEMBLE    = _validate_ensemble(ENSEMBLE)
    self.__STEPS       = _validate_steps(STEPS)
    self.__TIMESTEP    = _validate_timestep(TIMESTEP)
    self.__TEMPERATURE = _validate_temperature(TEMPERATURE)
    self.__errorLog    = errorLog
    self.__changeLog   = changeLog
    #MD subesctions
    self.__THERMOSTAT  = thermostat.THERMOSTAT(errorLog=self.__errorLog,changeLog=self.__changeLog)
    self.__PRINT       = md_print.PRINT(errorLog=self.__errorLog,changeLog=self.__changeLog)

  @property
  def ENSEMBLE(self):
    return self.__ENSEMBLE

  @property
  def STEPS(self):
    return self.__STEPS

  @property
  def TIMESTEP(self):
    return self.__TIMESTEP

  @property
  def TEMPERATURE(self):
    return self.__TEMPERATURE

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @property
  def THERMOSTAT(self):
    return self.__THERMOSTAT

  @property
  def PRINT(self):
    return self.__PRINT

  @ENSEMBLE.setter
  def ENSEMBLE(self,val):
    val = str(val).upper()
    if val in MD_ENSEMBLE_VALS:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MD','Variable':'ENSEMBLE',
                               'Success':True,'Previous':self.__ENSEMBLE,'New':val,'ErrorMessage':None})
      self.__ENSEMBLE = val
    else:
      errorMessage = "Invalid option for MD ENSEMBLE: {}. Valid options are: {}".format(MD_ENSEMBLE_VALS)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MD','Variable':'ENSEMBLE',
                               'Success':False,'Previous':self.__ENSEMBLE,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MD',
                              'Variable':'ENSEMBLE','ErrorMessage':errorMessage})
     
  @STEPS.setter
  def STEPS(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MD','Variable':'STEPS',
                               'Success':True,'Previous':self.__STEPS,'New':val,'ErrorMessage':None})
      self.__STEPS = val
    else:
      errorMessage = "STEPS must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MD','Variable':'STEPS',
                               'Success':False,'Previous':self.__STEPS,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MD',
                              'Variable':'STEPS','ErrorMessage':errorMessage})

  @TIMESTEP.setter
  def TIMESTEP(self,val):
    if utilities.is_positive_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MD','Variable':'TIMESTEP',
                               'Success':True,'Previous':self.__TIMESTEP,'New':val,'ErrorMessage':None})
      self.__TIMESTEP = val
    else:
      errorMessage = "TIMESTEP must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MD','Variable':'TIMESTEP',
                               'Success':False,'Previous':self.__TIMESTEP,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MD',
                              'Variable':'TIMESTEP','ErrorMessage':errorMessage})

  @TEMPERATURE.setter
  def TEMPERATURE(self,val):
    if utilities.is_positive_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MD','Variable':'TEMPERATURE',
                               'Success':True,'Previous':self.__TEMPERATURE,'New':val,
                               'ErrorMessage':None})
      self.__TEMPERATURE = val
    else:
      errorMessage = "TEMPERATURE must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MD','Variable':'TEMPERATURE',
                               'Success':False,'Previous':self.__TEMPERATURE,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MD',
                              'Variable':'TEMPERATURE','ErrorMessage':errorMessage})
