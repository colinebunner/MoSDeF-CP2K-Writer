import datetime
import cssi_cp2k.utilities as utilities
from cssi_cp2k.classes import AVBMC as avbmc
from cssi_cp2k.classes import MAX_DISPLACEMENTS as max_displacements
from cssi_cp2k.classes import MOVE_PROBABILITIES as move_probabilities
from cssi_cp2k.classes import MOVE_UPDATES as move_updates

MC_ENSEMBLE_VALS = ["GEMC_NPT","GEMC_NVT", "TRADITIONAL", "VIRIAL"]
BOOL_VALS = ["TRUE","FALSE", ".TRUE.", ".FALSE."]


def _validate_BOX2_FILE_NAME(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if utilities.is_string(val)  or (val is None):
    return val

def _validate_CELL_FILE_NAME(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if utilities.is_string(val)  or (val is None):
    return val


def _validate_COORDINATE_FILE_NAME(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if utilities.is_string(val)  or (val is None):
    return val




def _validate_DATA_FILE_NAME(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if utilities.is_string(val)  or (val is None):
    return val

def _validate_DISCRETE_STEP(val,errorLog=[]):
  
  if utilities.is_number(val)  or (val is None):
    return val
  else:
    errorMessage = "DISCRETE_STEP must be a number."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'DISCRETE_STEP','ErrorMessage':errorMessage})
    raise TypeError

def _validate_ENERGY_FILE_NAME(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if utilities.is_string(val)  or (val is None):
    return val


def _validate_ENSEMBLE(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if val in MC_ENSEMBLE_VALS or (val is None):
    return val
  else:
    errorMessage = "Invalid option for MC ENSEMBLE: {}. Valid options are: {}".format(val,MC_ENSEMBLE_VALS)
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'ENSEMBLE','ErrorMessage':errorMessage})
    raise TypeError("{}".format(val))
    

def _validate_ETA(val,errorLog=[]):
  
  if utilities.is_number(val)  or (val is None):
    return val
  else:
    errorMessage = "ETA must be a number."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'ETA','ErrorMessage':errorMessage})
    raise TypeError

def _validate_IPRINT(val,errorLog=[]):
  
  if utilities.is_integer(val)  or (val is None):
    return val
  else:
    errorMessage = "IPRINT must be an integer."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'IPRINT','ErrorMessage':errorMessage})
    raise TypeError

def _validate_LBIAS(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if val in BOOL_VALS or (val is None):
    return val
  else:
    errorMessage = "Invalid option for LBIAS: {}. Valid options are: {}".format(val,BOOL_VALS)
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'LBIAS','ErrorMessage':errorMessage})
    raise TypeError("{}".format(val))


def _validate_LDISCRETE(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if val in BOOL_VALS or (val is None):
    return val
  else:
    errorMessage = "Invalid option for LDISCRETE: {}. Valid options are: {}".format(val,BOOL_VALS)
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'LDISCRETE','ErrorMessage':errorMessage})
    raise TypeError("{}".format(val))
    
def _validate_LSTOP(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if val in BOOL_VALS or (val is None):
    return val
  else:
    errorMessage = "Invalid option for LSTOP: {}. Valid options are: {}".format(val,BOOL_VALS)
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'LSTOP','ErrorMessage':errorMessage})
    raise TypeError("{}".format(val))
 

def _validate_MAX_DISP_FILE_NAME(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if utilities.is_string(val)  or (val is None):
    return val


def _validate_MOLECULES_FILE_NAME(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if utilities.is_string(val)  or (val is None):
    return val


def _validate_MOVES_FILE_NAME(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if utilities.is_string(val)  or (val is None):
    return val


def _validate_NMOVES(val,errorLog=[]):
  
  if utilities.is_integer(val)  or (val is None):
    return val
  else:
    errorMessage = "NMOVES must be an integer."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'NMOVES','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_NSTEP(val,errorLog=[]):
  
  if utilities.is_integer(val)  or (val is None):
    return val
  else:
    errorMessage = "NSTEP must be an integer."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'NSTEP','ErrorMessage':errorMessage})
    raise TypeError
    
def _validate_NSWAPMOVES(val,errorLog=[]):
  
  if utilities.is_integer(val)  or (val is None):
    return val
  else:
    errorMessage = "NSWAPMOVES must be an integer."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'NSWAPMOVES','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_NVIRIAL(val,errorLog=[]):
  
  if utilities.is_integer(val)  or (val is None):
    return val
  else:
    errorMessage = "NVIRIAL must be an integer."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'NVIRIAL','ErrorMessage':errorMessage})
    raise TypeError
    
def _validate_PRESSURE(val,errorLog=[]):
  
  if utilities.is_number(val)  or (val is None):
    return val
  else:
    errorMessage = "PRESSURE must be a number."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'PRESSURE','ErrorMessage':errorMessage})
    raise TypeError    
    
def _validate_RANDOMTOSKIP(val,errorLog=[]):
  
  if utilities.is_integer(val)  or (val is None):
    return val
  else:
    errorMessage = "RANDOMTOSKIP must be an integer."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'RANDOMTOSKIP','ErrorMessage':errorMessage})
    raise TypeError

def _validate_RCLUS(val,errorLog=[]):
  
  if utilities.is_number(val)  or (val is None):
    return val
  else:
    errorMessage = "RCLUS must be a number."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'RCLUS','ErrorMessage':errorMessage})
    raise TypeError
    

    
def _validate_RESTART(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if val in BOOL_VALS or (val is None):
    return val
  else:
    errorMessage = "Invalid option for RESTART: {}. Valid options are: {}".format(val,BOOL_VALS)
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'RESTART','ErrorMessage':errorMessage})
    raise TypeError("{}".format(val))
    
def _validate_RESTART_FILE_NAME(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  if utilities.is_string(val)  or (val is None):
    return val    



def _validate_TEMPERATURE(val,errorLog=[]):
  
  if utilities.is_number(val)  or (val is None):
    return val
  else:
    errorMessage = "TEMPERATURE must be a number."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'TEMPERATURE','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_VIRIAL_TEMPS(val,errorLog=[]):
  
  if utilities.is_number(val)  or (val is None):
    return val
  else:
    errorMessage = "VIRIAL_TEMPS must be a number."
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MC',
                            'Variable':'VIRIAL_TEMPS','ErrorMessage':errorMessage})
    raise TypeError
    
    
class MC:

  def __init__(self,BOX2_FILE_NAME=None,CELL_FILE_NAME=None,COORDINATE_FILE_NAME=None,DATA_FILE_NAME=None, DISCRETE_STEP=None,ENERGY_FILE_NAME=None, ENSEMBLE=None,ETA=None,IPRINT=None,LBIAS=None,LDISCRETE=None, LSTOP
=None, MAX_DISP_FILE_NAME=None, MOLECULES_FILE_NAME=None, MOVES_FILE_NAME=None, NMOVES=None,NSTEP=None,NSWAPMOVES=None,NVIRIAL=None,PRESSURE=None,RANDOMTOSKIP=None,RCLUS=None,RESTART=None,RESTART_FILE_NAME=None, TEMPERATURE=None, VIRIAL_TEMPS=None, errorLog=[],changeLog=[],
               location=""):

    self.__errorLog    = errorLog
    self.__changeLog   = changeLog
    self.__BOX2_FILE_NAME    = _validate_BOX2_FILE_NAME(BOX2_FILE_NAME,errorLog=self.__errorLog)
    self.__CELL_FILE_NAME       = _validate_CELL_FILE_NAME(CELL_FILE_NAME,errorLog=self.__errorLog)
    self.__COORDINATE_FILE_NAME    = _validate_COORDINATE_FILE_NAME(COORDINATE_FILE_NAME,errorLog=self.__errorLog)
    self.__DATA_FILE_NAME = _validate_DATA_FILE_NAME(DATA_FILE_NAME,errorLog=self.__errorLog)
    self.__DISCRETE_STEP    = _validate_DISCRETE_STEP(DISCRETE_STEP,errorLog=self.__errorLog)
    self.__ENERGY_FILE_NAME       = _validate_ENERGY_FILE_NAME(ENERGY_FILE_NAME,errorLog=self.__errorLog)
    self.__ENSEMBLE    = _validate_ENSEMBLE(ENSEMBLE,errorLog=self.__errorLog)
    self.__ETA = _validate_ETA(ETA,errorLog=self.__errorLog)
    self.__IPRINT= _validate_IPRINT(IPRINT,errorLog=self.__errorLog)
    self.__LBIAS       = _validate_LBIAS(LBIAS,errorLog=self.__errorLog)
    self.__LDISCRETE    = _validate_LDISCRETE(LDISCRETE,errorLog=self.__errorLog)
    self.__LSTOP = _validate_LSTOP(LSTOP,errorLog=self.__errorLog)
    self.__MAX_DISP_FILE_NAME    = _validate_MAX_DISP_FILE_NAME(MAX_DISP_FILE_NAME,errorLog=self.__errorLog)
    self.__MOLECULES_FILE_NAME       = _validate_MOLECULES_FILE_NAME(MOLECULES_FILE_NAME,errorLog=self.__errorLog)
    self.__MOVES_FILE_NAME    = _validate_MOVES_FILE_NAME(MOVES_FILE_NAME,errorLog=self.__errorLog)
    self.__NMOVES = _validate_NMOVES(NMOVES,errorLog=self.__errorLog)
    self.__NSTEP    = _validate_NSTEP(NSTEP,errorLog=self.__errorLog)
    self.__NSWAPMOVES       = _validate_NSWAPMOVES(NSWAPMOVES,errorLog=self.__errorLog)
    self.__NVIRIAL= _validate_NVIRIAL(NVIRIAL,errorLog=self.__errorLog)
    self.__PRESSURE = _validate_PRESSURE(PRESSURE,errorLog=self.__errorLog)
    self.__RANDOMTOSKIP    = _validate_RANDOMTOSKIP(RANDOMTOSKIP,errorLog=self.__errorLog)
    self.__RCLUS       = _validate_RCLUS(RCLUS,errorLog=self.__errorLog)
    self.__RESTART       = _validate_RESTART(RESTART,errorLog=self.__errorLog)
    self.__RESTART_FILE_NAME    = _validate_RESTART_FILE_NAME(RESTART_FILE_NAME,errorLog=self.__errorLog)
    self.__TEMPERATURE = _validate_TEMPERATURE(TEMPERATURE,errorLog=self.__errorLog)
    self.__VIRIAL_TEMPS= _validate_VIRIAL_TEMPS(VIRIAL_TEMPS,errorLog=self.__errorLog)
    self.__location    = "{}/MC".format(location)
    #MC subesctions
    self.__AVBMC  = avbmc.AVBMC(errorLog=self.__errorLog,changeLog=self.__changeLog,
                           location=self.__location)
    self.__MAX_DISPLACEMENTS  = max_displacements.MAX_DISPLACEMENTS(errorLog=self.__errorLog,changeLog=self.__changeLog,
                           location=self.__location)
    self.__MOVE_PROBABILITIES       = move_probabilities.MOVE_PROBABILITIES(errorLog=self.__errorLog,changeLog=self.__changeLog,
                           location=self.__location)
    self.__MOVE_UPDATES= move_updates.MOVE_UPDATES(errorLog=self.__errorLog, changeLog=self.__changeLog,
                                              location=self.__location)

  @property
  def BOX2_FILE_NAME(self):
    return self.__BOX2_FILE_NAME

  @property
  def CELL_FILE_NAME(self):
    return self.__CELL_FILE_NAME

  @property
  def COORDINATE_FILE_NAME(self):
    return self.__COORDINATE_FILE_NAME

  @property
  def DATA_FILE_NAME(self):
    return self.__DATA_FILE_NAME

  @property
  def DISCRETE_STEP(self):
    return self.__DISCRETE_STEP


  @property
  def ENERGY_FILE_NAME(self):
    return self.__ENERGY_FILE_NAME


  @property
  def ENSEMBLE(self):
    return self.__ENSEMBLE


  @property
  def ETA(self):
    return self.__ETA


  @property
  def IPRINT(self):
    return self.__IPRINT

  @property
  def LBIAS(self):
    return self.__LBIAS

  @property
  def LDISCRETE(self):
    return self.__LDISCRETE
  @property
  def LSTOP(self):
    return self.__LSTOP

  @property
  def MAX_DISP_FILE_NAME(self):
    return self.__MAX_DISP_FILE_NAME


  @property
  def MOLECULES_FILE_NAME(self):
    return self.__MOLECULES_FILE_NAME

  @property
  def MOVES_FILE_NAME(self):
    return self.__MOVES_FILE_NAME

  @property
  def NMOVES(self):
    return self.__NMOVES


  @property
  def NSTEP(self):
    return self.__NSTEP

  @property
  def NSWAPMOVES(self):
    return self.__NSWAPMOVES

  @property
  def NVIRIAL(self):
    return self.__NVIRIAL


  @property
  def PRESSURE(self):
    return self.__PRESSURE


  @property
  def RANDOMTOSKIP(self):
    return self.__RANDOMTOSKIP

  @property
  def RCLUS(self):
    return self.__RCLUS


  @property
  def RESTART(self):
    return self.__RESTART


  @property
  def RESTART_FILE_NAME(self):
    return self.__RESTART_FILE_NAME


  @property
  def RANDOMTOSKIP(self):
    return self.__RANDOMTOSKIP

  @property
  def RCLUS(self):
    return self.__RCLUS

  @property
  def TEMPERATURE(self):
    return self.__TEMPERATURE


  @property
  def VIRIAL_TEMPS(self):
    return self.__VIRIAL_TEMPS



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
  def AVBMC(self):
    return self.__AVBMC
  @property
  def MAX_DISPLACEMENTS(self):
    return self.__MAX_DISPLACEMENTS

  @property
  def MOVE_PROBABILITIES(self):
      return self.__MOVE_PROBABILITIES

  @property
  def MOVE_UPDATES(self):
    return self.__MOVE_UPDATES

  @BOX2_FILE_NAME.setter
  def BOX2_FILE_NAME(self,val):
    if val is None or utilities.is_string(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'BOX2_FILE_NAME',
                               'Success':True,'Previous':self.__BOX2_FILE_NAME,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__BOX2_FILE_NAME = val
    else:
      errorMessage = "Invalid option for BOX2_FILE_NAME: {}. Must be a string".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'BOX2_FILE_NAME',
                               'Success':False,'Previous':self.__BOX2_FILE_NAME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'BOX2_FILE_NAME','ErrorMessage':errorMessage,'Location':self.__location})
 
  @CELL_FILE_NAME.setter
  def CELL_FILE_NAME(self,val):
    if val is None or utilities.is_string(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'CELL_FILE_NAME',
                               'Success':True,'Previous':self.__CELL_FILE_NAME,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__CELL_FILE_NAME = val
    else:
      errorMessage = "Invalid option for CELL_FILE_NAME: {}. Must be a string".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'CELL_FILE_NAME',
                               'Success':False,'Previous':self.__CELL_FILE_NAME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'CELL_FILE_NAME','ErrorMessage':errorMessage,'Location':self.__location})
  @COORDINATE_FILE_NAME.setter
  def COORDINATE_FILE_NAME(self,val):
    if val is None or utilities.is_string(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'COORDINATE_FILE_NAME',
                              'Success':True,'Previous':self.__COORDINATE_FILE_NAME,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__COORDINATE_FILE_NAME = val
    else:
      errorMessage = "Invalid option for COORDINATE_FILE_NAME: {}. Must be a string".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'COORDINATE_FILE_NAME',
                               'Success':False,'Previous':self.__COORDINATE_FILE_NAME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'COORDINATE_FILE_NAME','ErrorMessage':errorMessage,'Location':self.__location})

  @DATA_FILE_NAME.setter
  def DATA_FILE_NAME(self,val):
    if val is None or utilities.is_string(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'DATA_FILE_NAME',
                               'Success':True,'Previous':self.__DATA_FILE_NAME,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__DATA_FILE_NAME = val
    else:
      errorMessage = "Invalid option for DATA_FILE_NAME: {}. Must be a string".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'DATA_FILE_NAME',
                               'Success':False,'Previous':self.__DATA_FILE_NAME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'DATA_FILE_NAME','ErrorMessage':errorMessage,'Location':self.__location})
 
  @DISCRETE_STEP.setter
  def DISCRETE_STEP(self,val):
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'DISCRETE_STEP',
                               'Success':True,'Previous':self.__DISCRETE_STEP,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__DISCRETE_STEP = val
    else:
      errorMessage = "Invalid option for DISCRETE_STEP: {}. Must be a string".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'DISCRETE_STEP',
                               'Success':False,'Previous':self.__DISCRETE_STEP,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'DISCRETE_STEP','ErrorMessage':errorMessage,'Location':self.__location})
 

  @ENERGY_FILE_NAME.setter
  def ENERGY_FILE_NAME(self,val):
    if val is None or utilities.is_string(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'ENERGY_FILE_NAME',
                               'Success':True,'Previous':self.__ENERGY_FILE_NAME,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__ENERGY_FILE_NAME = val
    else:
      errorMessage = "Invalid option for ENERGY_FILE_NAME: {}. Must be a string".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'ENERGY_FILE_NAME',
                               'Success':False,'Previous':self.__ENERGY_FILE_NAME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'ENERGY_FILE_NAME','ErrorMessage':errorMessage,'Location':self.__location})
    
  @ENSEMBLE.setter
  def ENSEMBLE(self,val):
    if val is None or val in MC_ENSEMBLE_VALS:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'ENSEMBLE',
                               'Success':True,'Previous':self.__ENSEMBLE,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__ENSEMBLE = val
    else:
      errorMessage = "Invalid option for ENSEMBLE: {}. Must be one of these values: {}".format(val,MC_ENSEMBLE_VALS)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'ENSEMBLE',
                               'Success':False,'Previous':self.__ENSEMBLE,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'ENSEMBLE','ErrorMessage':errorMessage,'Location':self.__location})    
    
  @ETA.setter
  def ETA(self,val):
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'ETA',
                               'Success':True,'Previous':self.__ETA,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__ETA = val
    else:
      errorMessage = "Invalid option for ETA: {}. Must be a number".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'ETA',
                               'Success':False,'Previous':self.__ETA,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'ETA','ErrorMessage':errorMessage,'Location':self.__location})  
    
    
  @IPRINT.setter
  def IPRINT(self,val):
    if val is None or utilities.is_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'ETA',
                               'Success':True,'Previous':self.__IPRINT,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__IPRINT = val
    else:
      errorMessage = "Invalid option for IPRINT: {}. Must be an integer".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'IPRINT',
                               'Success':False,'Previous':self.__IPRINT,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'IPRINT','ErrorMessage':errorMessage,'Location':self.__location})  

    
  @LBIAS.setter
  def LBIAS(self,val):
    if val is None or val in BOOL_VALS:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'LBIAS',
                               'Success':True,'Previous':self.__LBIAS,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__LBIAS = val
    else:
      errorMessage = "Invalid option for LBIAS: {}. Must be one of these values: {}".format(val,BOOL_VALS)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'LBIAS',
                               'Success':False,'Previous':self.__LBIAS,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'LBIAS','ErrorMessage':errorMessage,'Location':self.__location})    
 
  @LDISCRETE.setter
  def LDISCRETE(self,val):
    if val is None or val in BOOL_VALS:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'LDISCRETE',
                               'Success':True,'Previous':self.__LDISCRETE,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__LDISCRETE = val
    else:
      errorMessage = "Invalid option for LDISCRETE: {}. Must be one of these values: {}".format(val,BOOL_VALS)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'LDISCRETE',
                               'Success':False,'Previous':self.__LDISCRETE,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'LDISCRETE','ErrorMessage':errorMessage,'Location':self.__location})      
  @LSTOP.setter
  def LSTOP(self,val):
    if val is None or val in BOOL_VALS:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'LSTOP',
                               'Success':True,'Previous':self.__LSTOP,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__LSTOP = val
    else:
      errorMessage = "Invalid option for LSTOP: {}. Must be one of these values: {}".format(val,BOOL_VALS)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'LSTOP',
                               'Success':False,'Previous':self.__LSTOP,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'LSTOP','ErrorMessage':errorMessage,'Location':self.__location})    
  @MAX_DISP_FILE_NAME.setter
  def MAX_DISP_FILE_NAME(self,val):
    if val is None or utilities.is_string(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'MAX_DISP_FILE_NAME',
                               'Success':True,'Previous':self.__MAX_DISP_FILE_NAME,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__MAX_DISP_FILE_NAME = val
    else:
      errorMessage = "Invalid option for MAX_DISP_FILE_NAME: {}. Must be a string".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'MAX_DISP_FILE_NAME',
                               'Success':False,'Previous':self.__MAX_DISP_FILE_NAME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'MAX_DISP_FILE_NAME','ErrorMessage':errorMessage,'Location':self.__location})
    
  @MOLECULES_FILE_NAME.setter
  def MOLECULES_FILE_NAME(self,val):
    if val is None or utilities.is_string(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'MOLECULES_FILE_NAME',
                               'Success':True,'Previous':self.__MOLECULES_FILE_NAME,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__MOLECULES_FILE_NAME = val
    else:
      errorMessage = "Invalid option for MOLECULES_FILE_NAME: {}. Must be a string".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'MOLECULES_FILE_NAME',
                               'Success':False,'Previous':self.__MOLECULES_FILE_NAME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'MOLECULES_FILE_NAME','ErrorMessage':errorMessage,'Location':self.__location})
    
  @MOVES_FILE_NAME.setter
  def MOVES_FILE_NAME(self,val):
    if val is None or utilities.is_string(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'MOVES_FILE_NAME',
                               'Success':True,'Previous':self.__MOVES_FILE_NAME,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__MOVES_FILE_NAME = val
    else:
      errorMessage = "Invalid option for MOVES_FILE_NAME: {}. Must be a string".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'MOVES_FILE_NAME',
                               'Success':False,'Previous':self.__MOVES_FILE_NAME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'MOVES_FILE_NAME','ErrorMessage':errorMessage,'Location':self.__location})
    
  @NMOVES.setter
  def NMOVES(self,val):
    if val is None or utilities.is_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'NMOVES',
                               'Success':True,'Previous':self.__NMOVES,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__NMOVES = val
    else:
      errorMessage = "Invalid option for NMOVES: {}. Must be an integer".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'NMOVES',
                               'Success':False,'Previous':self.__NMOVES,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'NMOVES','ErrorMessage':errorMessage,'Location':self.__location}) 
    
  @NSTEP.setter
  def NSTEP(self,val):
    if val is None or utilities.is_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'NSTEP',
                               'Success':True,'Previous':self.__NSTEP,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__NSTEP = val
    else:
      errorMessage = "Invalid option for NSTEP: {}. Must be an integer".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'NSTEP',
                               'Success':False,'Previous':self.__NSTEP,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'NSTEP','ErrorMessage':errorMessage,'Location':self.__location})  
  @NSWAPMOVES.setter
  def NSWAPMOVES(self,val):
    if val is None or utilities.is_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'NSWAPMOVES',
                               'Success':True,'Previous':self.__NSWAPMOVES,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__NSWAPMOVES = val
    else:
      errorMessage = "Invalid option for NSWAPMOVES: {}. Must be an integer".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'NSWAPMOVES',
                               'Success':False,'Previous':self.__NSWAPMOVES,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'NSWAPMOVES','ErrorMessage':errorMessage,'Location':self.__location}) 
    
  @NVIRIAL.setter
  def NVIRIAL(self,val):
    if val is None or utilities.is_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'NVIRIAL',
                               'Success':True,'Previous':self.__NVIRIAL,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__NVIRIAL = val
    else:
      errorMessage = "Invalid option for NVIRIAL: {}. Must be an integer".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'NVIRIAL',
                               'Success':False,'Previous':self.__NVIRIAL,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'NVIRIAL','ErrorMessage':errorMessage,'Location':self.__location})
    
  @PRESSURE.setter
  def PRESSURE(self,val):
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'PRESSURE',
                               'Success':True,'Previous':self.__PRESSURE,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__PRESSURE = val
    else:
      errorMessage = "Invalid option for PRESSURE: {}. Must be a number".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'PRESSURE',
                               'Success':False,'Previous':self.__PRESSURE,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'PRESSURE','ErrorMessage':errorMessage,'Location':self.__location})
  @RANDOMTOSKIP.setter
  def RANDOMTOSKIP(self,val):
    if val is None or utilities.is_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'RANDOMTOSKIP',
                               'Success':True,'Previous':self.__RANDOMTOSKIP,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__RANDOMTOSKIP = val
    else:
      errorMessage = "Invalid option for RANDOMTOSKIP: {}. Must be an integer".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'RANDOMTOSKIP',
                               'Success':False,'Previous':self.__RANDOMTOSKIP,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'RANDOMTOSKIP','ErrorMessage':errorMessage,'Location':self.__location})
    
  @RCLUS.setter
  def RCLUS(self,val):
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'RCLUS',
                               'Success':True,'Previous':self.__RCLUS,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__RCLUS = val
    else:
      errorMessage = "Invalid option for RCLUS: {}. Must be a number".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'RCLUS',
                               'Success':False,'Previous':self.__RCLUS,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'RCLUS','ErrorMessage':errorMessage,'Location':self.__location})
    
  @RESTART.setter
  def RESTART(self,val):
    if val is None or val in BOOL_VALS:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'RESTART',
                               'Success':True,'Previous':self.__RESTART,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__RESTART = val
    else:
      errorMessage = "Invalid option for RESTART: {}. Must be one of these values: {}".format(val,BOOL_VALS)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'RESTART',
                               'Success':False,'Previous':self.__RESTART,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'RESTART','ErrorMessage':errorMessage,'Location':self.__location}) 
    
    
  @RESTART_FILE_NAME.setter
  def RESTART_FILE_NAME(self,val):
    if val is None or utilities.is_string(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'RESTART_FILE_NAME',
                               'Success':True,'Previous':self.__RESTART_FILE_NAME,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__RESTART_FILE_NAME = val
    else:
      errorMessage = "Invalid option for RESTART_FILE_NAME: {}. Must be a string".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'RESTART_FILE_NAME',
                               'Success':False,'Previous':self.__RESTART_FILE_NAME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'RESTART_FILE_NAME','ErrorMessage':errorMessage,'Location':self.__location})
    
    
  @TEMPERATURE.setter
  def TEMPERATURE(self,val):
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'TEMPERATURE',
                               'Success':True,'Previous':self.__TEMPERATURE,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__TEMPERATURE = val
    else:
      errorMessage = "Invalid option for TEMPERATURE: {}. Must be a number".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'TEMPERATURE',
                               'Success':False,'Previous':self.__TEMPERATURE,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'TEMPERATURE','ErrorMessage':errorMessage,'Location':self.__location})
    
  @VIRIAL_TEMPS.setter
  def VIRIAL_TEMPS(self,val):
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'VIRIAL_TEMPS',
                               'Success':True,'Previous':self.__VIRIAL_TEMPS,'New':val,'ErrorMessage':None,
                               'Location':self.__location})
   
      self.__VIRIAL_TEMPS = val
    else:
      errorMessage = "Invalid option for VIRIAL_TEMPS: {}. Must be a number".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MC','Variable':'VIRIAL_TEMPS',
                               'Success':False,'Previous':self.__VIRIAL_TEMPS,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MC',
                              'Variable':'VIRIAL_TEMPS','ErrorMessage':errorMessage,'Location':self.__location})
  
 