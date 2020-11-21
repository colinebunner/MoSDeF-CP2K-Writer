import datetime
from mosdef_cp2k_writer.utilities import *
from mosdef_cp2k_writer.classes import CENTER_COORDINATES as CENTER_COORDINATES
from mosdef_cp2k_writer.classes import MOL_SET



BOOL_VALS   = [".TRUE.",".FALSE."]
CONN_FILE_FORMAT_VALS   = ["AMBER","G87","G96","GENERATE","MOL_SET","OFF","PSF","UPSF","USER"]
COORD_FILE_FORMAT_VALS=['CIF','CP2K','CRD','G96','OFF','PDB','XTL','XYZ'];
EXCLUDE_EI_VALS=['1-1','1-2','1-3','1-4'];
EXCLUDE_VDW_VALS=['1-1','1-2','1-3','1-4'];





def _validate_AUTOGEN_EXCLUDE_LISTS(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  
  if val in BOOL_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for AUTOGEN_EXCLUDE_LISTS: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'AUTOGEN_EXCLUDE_LISTS','ErrorMessage':errorMessage})
    raise TypeError
    
    
    
def _validate_CHARGE_BETA(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  
  if val in BOOL_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for CHARGE_BETA: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'CHARGE_BETA','ErrorMessage':errorMessage})
    raise TypeError
   


def _validate_CHARGE_EXTENDED(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  
  if val in BOOL_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for CHARGE_EXTENDED: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'CHARGE_EXTENDED','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_CHARGE_OCCUP(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  
  if val in BOOL_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for CHARGE_OCCUP: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'CHARGE_OCCUP','ErrorMessage':errorMessage})
    raise TypeError    

    
def _validate_CONN_FILE_FORMAT(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  
  if val in CONN_FILE_FORMAT_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for CONN_FILE_FORMAT: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'CONN_FILE_FORMAT','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_CONN_FILE_NAME(val,errorLog=[]):
  if is_string(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for CONN_FILE_NAME: {}. It should be a string".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'CONN_FILE_NAME','ErrorMessage':errorMessage})
    raise TypeError
    
def _validate_COORD_FILE_FORMAT(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  
  if val in COORD_FILE_FORMAT_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for COORD_FILE_FORMAT: {}. Valid options are: {}".format(
                     val,COORD_FILE_FORMAT_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'COORD_FILE_FORMAT','ErrorMessage':errorMessage})
    raise TypeError

    
def _validate_COORD_FILE_NAME(val,errorLog=[]):
  if is_string(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for COORD_FILE_NAME: {}. It should be a string".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'COORD_FILE_NAME','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_DISABLE_EXCLUSION_LISTS(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  
  if val in BOOL_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for DISABLE_EXCLUSION_LISTS: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'DISABLE_EXCLUSION_LISTS','ErrorMessage':errorMessage})
    raise TypeError
    
    
   
    
    
def _validate_EXCLUDE_EI(val,errorLog=[]):
 
  
  if val in EXCLUDE_EI_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for EXCLUDE_EI: {}. Valid options are: {}".format(
                     val,EXCLUDE_EI_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'EXCLUDE_EI','ErrorMessage':errorMessage})
    raise TypeError     
    
def _validate_EXCLUDE_VDW(val,errorLog=[]):
 
  
  if val in EXCLUDE_VDW_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for EXCLUDE_VDW: {}. Valid options are: {}".format(
                     val,EXCLUDE_VDW_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'EXCLUDE_VDW','ErrorMessage':errorMessage})
    raise TypeError   
    
def _validate_MEMORY_PROGRESSION_FACTOR(val,errorLog=[]):
  if is_number(val) or (val is None):
    return val 
  else:
    errorMessage = ("Invalid option for MEMORY_PROGRESSION_FACTOR: {}. It should be a number".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'MEMORY_PROGRESSION_FACTOR','ErrorMessage':errorMessage})
    raise TypeError  
    
    
def _validate_MOL_CHECK(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  
  if val in BOOL_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for MOL_CHECK: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'MOL_CHECK','ErrorMessage':errorMessage})
    raise TypeError 
    

def _validate_MULTIPLE_UNIT_CELL(val,errorLog=[]):
 
  
  if is_list_of_numbers(val) or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for MULTIPLE_UNIT_CELL: {}.It must be a list of numbers".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'MULTIPLE_UNIT_CELL','ErrorMessage':errorMessage})
    raise TypeError 
  
def _validate_NUMBER_OF_ATOMS(val,errorLog=[]):
 
  
  if is_positive_integer(val) or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for NUMBER_OF_ATOMSL: {}.It must be a positive integer".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'NUMBER_OF_ATOMS','ErrorMessage':errorMessage})
    raise TypeError 



    
def _validate_PARA_RES(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  
  if val in BOOL_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for PARA_RES: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'PARA_RES','ErrorMessage':errorMessage})
    raise TypeError 
    

def _validate_USE_ELEMENT_AS_KIND(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  
  if val in BOOL_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for USE_ELEMENT_AS_KIND: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'USE_ELEMENT_AS_KIND','ErrorMessage':errorMessage})
    raise TypeError     
    
def _validate_USE_G96_VELOCITY(val,errorLog=[]):
  if val is not None:
    val = str(val).upper()
  
  if val in BOOL_VALS or (val is None):
        return val
  else:
    errorMessage = ("Invalid option for USE_G96_VELOCITY: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'SUBSYS.TOPOLOGY',
                            'Variable':'USE_G96_VELOCITY','ErrorMessage':errorMessage})
    raise TypeError    
 

class TOPOLOGY:

  def __init__(self,AUTOGEN_EXCLUDE_LISTS=None,CHARGE_BETA=None,CHARGE_EXTENDED=None,CHARGE_OCCUP=None,CONN_FILE_FORMAT=None,CONN_FILE_NAME=None,COORD_FILE_FORMAT=None, COORD_FILE_NAME=None, DISABLE_EXCLUSION_LISTS=None, EXCLUDE_EI=None, EXCLUDE_VDW=None, MEMORY_PROGRESSION_FACTOR=None, MOL_CHECK=None, MULTIPLE_UNIT_CELL=None, NUMBER_OF_ATOMS=None, PARA_RES=None, USE_ELEMENT_AS_KIND=None, USE_G96_VELOCITY=None, errorLog=[],changeLog=[],location=""):
    self.__errorLog = errorLog
    self.__changeLog = changeLog
    self.__location  = "{}/TOPOLOGY".format(location)
    self.__AUTOGEN_EXCLUDE_LISTS=_validate_AUTOGEN_EXCLUDE_LISTS(AUTOGEN_EXCLUDE_LISTS, errorLog=self.__errorLog)
    self.__CHARGE_BETA=_validate_CHARGE_BETA(CHARGE_BETA, errorLog=self.__errorLog)
    self.__CHARGE_EXTENDED=_validate_CHARGE_EXTENDED(CHARGE_EXTENDED, errorLog=self.__errorLog)
    self.__CHARGE_OCCUP=_validate_CHARGE_OCCUP(CHARGE_OCCUP,errorLog=self.__errorLog)
    self.__CONN_FILE_FORMAT=_validate_CONN_FILE_FORMAT(CONN_FILE_FORMAT,errorLog=self.__errorLog)
    self.__CONN_FILE_NAME=_validate_CONN_FILE_NAME(CONN_FILE_NAME,errorLog=self.__errorLog)
    self.__COORD_FILE_FORMAT=_validate_COORD_FILE_FORMAT(COORD_FILE_FORMAT,errorLog=self.__errorLog)
    self.__COORD_FILE_NAME=_validate_COORD_FILE_NAME(COORD_FILE_NAME,errorLog=self.__errorLog)
    self.__DISABLE_EXCLUSION_LISTS=_validate_DISABLE_EXCLUSION_LISTS(DISABLE_EXCLUSION_LISTS,errorLog=self.__errorLog)
    self.__EXCLUDE_EI=_validate_EXCLUDE_EI(EXCLUDE_EI,errorLog=self.__errorLog)
    self.__EXCLUDE_VDW=_validate_EXCLUDE_VDW(EXCLUDE_VDW,errorLog=self.__errorLog)
    self.__MEMORY_PROGRESSION_FACTOR=_validate_MEMORY_PROGRESSION_FACTOR(MEMORY_PROGRESSION_FACTOR,errorLog=self.__errorLog)
    self.__MOL_CHECK=_validate_MOL_CHECK(MOL_CHECK,errorLog=self.__errorLog)
    self.__MULTIPLE_UNIT_CELL=_validate_MULTIPLE_UNIT_CELL(MULTIPLE_UNIT_CELL,errorLog=self.__errorLog)
    self.__NUMBER_OF_ATOMS=_validate_NUMBER_OF_ATOMS(NUMBER_OF_ATOMS,errorLog=self.__errorLog)
    self.__PARA_RES=_validate_PARA_RES(PARA_RES,errorLog=self.__errorLog)
    self.__USE_ELEMENT_AS_KIND=_validate_USE_ELEMENT_AS_KIND(USE_ELEMENT_AS_KIND,errorLog=self.__errorLog)
    self.__USE_G96_VELOCITY=_validate_USE_G96_VELOCITY(USE_G96_VELOCITY,errorLog=self.__errorLog)
    self.__CENTER_COORDINATES = CENTER_COORDINATES.CENTER_COORDINATES(errorLog=self.__errorLog, changeLog=self.__changeLog,location=self.__location)
    self.__MOL_SET = MOL_SET.MOL_SET(errorLog=self.__errorLog, changeLog=self.__changeLog,location=self.__location)

    
    

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
  def AUTOGEN_EXCLUDE_LISTS(self):
    return self.__AUTOGEN_EXCLUDE_LISTS

  @property
  def CHARGE_BETA(self):
    return self.__CHARGE_BETA

  @property
  def CHARGE_EXTENDED(self):
    return self.__CHARGE_EXTENDED

  @property
  def CHARGE_OCCUP(self):
    return self.__CHARGE_OCCUP

  @property
  def CONN_FILE_FORMAT(self):
    return self.__CONN_FILE_FORMAT

  @property
  def CONN_FILE_NAME(self):
    return self.__CONN_FILE_NAME

  @property
  def COORD_FILE_FORMAT(self):
    return self.__COORD_FILE_FORMAT

  @property
  def COORD_FILE_NAME(self):
    return self.__COORD_FILE_NAME


  @property
  def DISABLE_EXCLUSION_LISTS(self):
    return self.__DISABLE_EXCLUSION_LISTS

  @property
  def EXCLUDE_EI(self):
    return self.__EXCLUDE_EI
  @property
  def EXCLUDE_VDW(self):
    return self.__EXCLUDE_VDW
  @property
  def MEMORY_PROGRESSION_FACTOR(self):
    return self.__MEMORY_PROGRESSION_FACTOR
  @property
  def MOL_CHECK(self):
    return self.__MOL_CHECK
  @property
  def MULTIPLE_UNIT_CELL(self):
    return self.__MULTIPLE_UNIT_CELL
  @property
  def NUMBER_OF_ATOMS(self):
    return self.__NUMBER_OF_ATOMS
  @property
  def PARA_RES(self):
    return self.__PARA_RES
  @property
  def USE_ELEMENT_AS_KIND(self):
    return self.__USE_ELEMENT_AS_KIND
  @property
  def USE_G96_VELOCITY(self):
    return self.__USE_G96_VELOCITY
  @property
  def CENTER_COORDINATES(self):
    return self.__CENTER_COORDINATES
  @property
  def MOL_SET(self):
    return self.__MOL_SET




  @AUTOGEN_EXCLUDE_LISTS.setter
  def AUTOGEN_EXCLUDE_LISTS(self, val):
    val = str(val).upper()
    if val in BOOL_VALS:
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'AUTOGEN_EXCLUDE_LISTS',
                               'Success': True, 'Previous': self.__AUTOGEN_EXCLUDE_LISTS, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__AUTOGEN_EXCLUDE_LISTS = val
    else:
      errorMessage = ("Invalid option for AUTOGEN_EXCLUDE_LISTS: {}. Valid options are: {}".format(val, BOOL_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'AUTOGEN_EXCLUDE_LISTS',
                               'Success': False, 'Previous': self.__AUTOGEN_EXCLUDE_LISTS, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'AUTOGEN_EXCLUDE_LISTS', 'ErrorMessage': errorMessage,
                              'Location': self.__location}) 
    
    
  @CHARGE_BETA.setter
  def CHARGE_BETA(self, val):
    val = str(val).upper()
    if val in BOOL_VALS:
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'CHARGE_BETA',
                               'Success': True, 'Previous': self.__CHARGE_BETA, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__CHARGE_BETA = val
    else:
      errorMessage = ("Invalid option for CHARGE_BETA: {}. Valid options are: {}".format(val, BOOL_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'CHARGE_BETA',
                               'Success': False, 'Previous': self.__CHARGE_BETA, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'CHARGE_BETA', 'ErrorMessage': errorMessage,
                              'Location': self.__location}) 
    
  @CHARGE_EXTENDED.setter
  def CHARGE_EXTENDED(self, val):
    val = str(val).upper()
    if val in BOOL_VALS:
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'CHARGE_EXTENDED',
                               'Success': True, 'Previous': self.__CHARGE_EXTENDED, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__CHARGE_EXTENDED = val
    else:
      errorMessage = ("Invalid option for CHARGE_EXTENDED: {}. Valid options are: {}".format(val, BOOL_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'CHARGE_EXTENDED',
                               'Success': False, 'Previous': self.__CHARGE_EXTENDED, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'CHARGE_EXTENDED', 'ErrorMessage': errorMessage,
                              'Location': self.__location})
 
  @CHARGE_OCCUP.setter
  def CHARGE_OCCUP(self, val):
    val = str(val).upper()
    if val in BOOL_VALS:
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'CHARGE_OCCUP',
                               'Success': True, 'Previous': self.__CHARGE_OCCUP, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__CHARGE_OCCUP = val
    else:
      errorMessage = ("Invalid option for CHARGE_OCCUP: {}. Valid options are: {}".format(val, BOOL_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'CHARGE_OCCUP',
                               'Success': False, 'Previous': self.__CHARGE_OCCUP, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'CHARGE_OCCUP', 'ErrorMessage': errorMessage,
                              'Location': self.__location})
 
  @CONN_FILE_FORMAT.setter
  def CONN_FILE_FORMAT(self, val):
    val = str(val).upper()
    if val in CONN_FILE_FORMAT_VALS:
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'CONN_FILE_FORMAT',
                               'Success': True, 'Previous': self.__CONN_FILE_FORMAT, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__CONN_FILE_FORMAT = val
    else:
      errorMessage = ("Invalid option for CONN_FILE_FORMAT: {}. Valid options are: {}".format(val, CONN_FILE_FORMAT_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'CONN_FILE_FORMAT',
                               'Success': False, 'Previous': self.__CONN_FILE_FORMAT, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'CONN_FILE_FORMAT', 'ErrorMessage': errorMessage,
                              'Location': self.__location})

    
  @CONN_FILE_NAME.setter
  def CONN_FILE_NAME(self, val):
    if is_string(val):
    
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'CONN_FILE_NAME',
                               'Success': True, 'Previous': self.__CONN_FILE_NAME, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__CONN_FILE_NAME = val
    else:
      errorMessage = ("Invalid option for CONN_FILE_NAME: {}. it should be a string".format(val))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'CONN_FILE_NAME',
                               'Success': False, 'Previous': self.__CONN_FILE_NAME, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'CONN_FILE_NAME', 'ErrorMessage': errorMessage,
                              'Location': self.__location})
    
  @COORD_FILE_FORMAT.setter
  def COORD_FILE_FORMAT(self, val):
    val = str(val).upper()
    if val in COORD_FILE_FORMAT_VALS:
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'COORD_FILE_FORMAT',
                               'Success': True, 'Previous': self.__COORD_FILE_FORMAT, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__COORD_FILE_FORMAT = val
    else:
      errorMessage = ("Invalid option for COORD_FILE_FORMAT: {}. Valid options are: {}".format(val, COORD_FILE_FORMAT_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'COORD_FILE_FORMAT',
                               'Success': False, 'Previous': self.__COORD_FILE_FORMAT, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'COORD_FILE_FORMAT', 'ErrorMessage': errorMessage,
                              'Location': self.__location})
    
    
  @COORD_FILE_NAME.setter
  def COORD_FILE_NAME(self, val):
    if is_string(val):
    
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'COORD_FILE_NAME',
                               'Success': True, 'Previous': self.__COORD_FILE_NAME, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__COORD_FILE_NAME = val
    else:
      errorMessage = ("Invalid option for COCOORD_FILE_NAME: {}. it should be a string".format(val))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'COORD_FILE_NAME',
                               'Success': False, 'Previous': self.__COORD_FILE_NAME, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'COORD_FILE_NAME', 'ErrorMessage': errorMessage,
                              'Location': self.__location})
    
  @DISABLE_EXCLUSION_LISTS.setter
  def DISABLE_EXCLUSION_LISTS(self, val):
    val = str(val).upper()
    if val in BOOL_VALS:
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'DISABLE_EXCLUSION_LISTS',
                               'Success': True, 'Previous': self.__DISABLE_EXCLUSION_LISTS, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__DISABLE_EXCLUSION_LISTS = val
    else:
      errorMessage = ("Invalid option for DISABLE_EXCLUSION_LISTS: {}. Valid options are: {}".format(val, BOOL_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'DISABLE_EXCLUSION_LISTS',
                               'Success': False, 'Previous': self.__DISABLE_EXCLUSION_LISTS, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'DISABLE_EXCLUSION_LISTS', 'ErrorMessage': errorMessage,
                              'Location': self.__location})
  @EXCLUDE_EI.setter
  def EXCLUDE_EI(self, val):
    if val in EXCLUDE_EI_VALS:
    
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'EXCLUDE_EI',
                               'Success': True, 'Previous': self.__EXCLUDE_EI, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__EXCLUDE_EI = val
    else:
      errorMessage = ("Invalid option for EXCLUDE_EI: {}. it should be one of {}".format(val,EXCLUDE_EI_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'EXCLUDE_EI',
                               'Success': False, 'Previous': self.__EXCLUDE_EI, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'EXCLUDE_EI', 'ErrorMessage': errorMessage,
                              'Location': self.__location})
  @EXCLUDE_VDW.setter
  def EXCLUDE_VDW(self, val):
    if val in EXCLUDE_VDW_VALS:
    
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'EXCLUDE_VDW',
                               'Success': True, 'Previous': self.__EXCLUDE_VDW, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__EXCLUDE_VDW = val
    else:
      errorMessage = ("Invalid option for EXCLUDE_VDW: {}. it should be one of {}".format(val,EXCLUDE_VDW_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'EXCLUDE_VDW',
                               'Success': False, 'Previous': self.__EXCLUDE_VDW, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'EXCLUDE_VDW', 'ErrorMessage': errorMessage,
                              'Location': self.__location})
    
    
  @MEMORY_PROGRESSION_FACTOR.setter
  def MEMORY_PROGRESSION_FACTOR(self, val):
    if is_number(val):
    
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'MEMORY_PROGRESSION_FACTOR',
                               'Success': True, 'Previous': self.__MEMORY_PROGRESSION_FACTOR, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__MEMORY_PROGRESSION_FACTOR = val
    else:
      errorMessage = ("Invalid option for MEMORY_PROGRESSION_FACTOR: {}. it should be a string".format(val))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'MEMORY_PROGRESSION_FACTOR',
                               'Success': False, 'Previous': self.__MEMORY_PROGRESSION_FACTOR, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'MEMORY_PROGRESSION_FACTOR', 'ErrorMessage': errorMessage,
                              'Location': self.__location})
    
  @MOL_CHECK.setter
  def MOL_CHECK(self, val):
    val = str(val).upper()
    if val in BOOL_VALS:
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'MOL_CHECK',
                               'Success': True, 'Previous': self.__MOL_CHECK, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__MOL_CHECK = val
    else:
      errorMessage = ("Invalid option for MOL_CHECK: {}. Valid options are: {}".format(val, BOOL_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'MOL_CHECK',
                               'Success': False, 'Previous': self.__MOL_CHECK, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'MOL_CHECK', 'ErrorMessage': errorMessage,
                              'Location': self.__location})   
    
  @MULTIPLE_UNIT_CELL.setter
  def MULTIPLE_UNIT_CELL(self, val):
    if is_list_of_numbers(val):
    
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'MULTIPLE_UNIT_CELL',
                               'Success': True, 'Previous': self.__MULTIPLE_UNIT_CELL, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__MULTIPLE_UNIT_CELL = val
    else:
      errorMessage = ("Invalid option for MULTIPLE_UNIT_CELL: {}. it should be a list of numbers".format(val))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'MULTIPLE_UNIT_CELL',
                               'Success': False, 'Previous': self.__MULTIPLE_UNIT_CELL, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'MULTIPLE_UNIT_CELL', 'ErrorMessage': errorMessage,
                              'Location': self.__location})
  @NUMBER_OF_ATOMS.setter
  def NUMBER_OF_ATOMS(self, val):
    if is_positive_integer(val):
    
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'NUMBER_OF_ATOMS',
                               'Success': True, 'Previous': self.__NUMBER_OF_ATOMS, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__NUMBER_OF_ATOMS = val
    else:
      errorMessage = ("Invalid option for NUMBER_OF_ATOMS: {}. it should be a positive integer".format(val))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'NUMBER_OF_ATOMS',
                               'Success': False, 'Previous': self.__NUMBER_OF_ATOMS, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'NUMBER_OF_ATOMS', 'ErrorMessage': errorMessage,
                              'Location': self.__location})
  @PARA_RES.setter
  def PARA_RES(self, val):
    val = str(val).upper()
    if val in BOOL_VALS:
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'PARA_RES',
                               'Success': True, 'Previous': self.__PARA_RES, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__PARA_RES = val
    else:
      errorMessage = ("Invalid option for PARA_RES: {}. Valid options are: {}".format(val, BOOL_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'PARA_RES',
                               'Success': False, 'Previous': self.__PARA_RES, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'PARA_RES', 'ErrorMessage': errorMessage,
                              'Location': self.__location}) 
    
  @USE_ELEMENT_AS_KIND.setter
  def USE_ELEMENT_AS_KIND(self, val):
    val = str(val).upper()
    if val in BOOL_VALS:
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'USE_ELEMENT_AS_KIND',
                               'Success': True, 'Previous': self.__USE_ELEMENT_AS_KIND, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__USE_ELEMENT_AS_KIND = val
    else:
      errorMessage = ("Invalid option for USE_ELEMENT_AS_KIND: {}. Valid options are: {}".format(val, BOOL_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'USE_ELEMENT_AS_KIND',
                               'Success': False, 'Previous': self.__USE_ELEMENT_AS_KIND, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'USE_ELEMENT_AS_KIND', 'ErrorMessage': errorMessage,
                              'Location': self.__location}) 
    
  @USE_G96_VELOCITY.setter
  def USE_G96_VELOCITY(self, val):
    val = str(val).upper()
    if val in BOOL_VALS:
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'USE_G96_VELOCITY',
                               'Success': True, 'Previous': self.__USE_G96_VELOCITY, 'New': val,
                               'ErrorMessage': None, 'Location': self.__location})
      self.__USE_G96_VELOCITY = val
    else:
      errorMessage = ("Invalid option for USE_G96_VELOCITY: {}. Valid options are: {}".format(val, BOOL_VALS))
      self.__changeLog.append({'Date': datetime.datetime.now(), 'Module': 'SUBSYS.TOPOLOGY', 'Variable': 'USE_G96_VELOCITY',
                               'Success': False, 'Previous': self.__USE_G96_VELOCITY, 'New': val,
                               'ErrorMessage': errorMessage, 'Location': self.__location})
      self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'SUBSYS.TOPOLOGY',
                              'Variable': 'USE_G96_VELOCITY', 'ErrorMessage': errorMessage,
                              'Location': self.__location}) 