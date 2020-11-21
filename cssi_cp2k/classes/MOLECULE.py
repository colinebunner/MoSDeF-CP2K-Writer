import datetime
import cssi_cp2k.utilities as utilities


CONN_FILE_FORMAT_VALS = ["AMBER","G87","G96","GENERATE","MOL_SET","OFF","PSF","UPSF","USER"]


def _validate_CONN_FILE_FORMAT(val,errorLog=[]):

  if val is not None:
    val = str(val).upper()

  if val in CONN_FILE_FORMAT_VALS or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for CONN_FILE_FORMAT: {}. Valid options are: {}".format(
                     val,CONN_FILE_FORMAT_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOLECULE',
                            'Variable':'CONN_FILE_FORMAT','ErrorMessage':errorMessage})
    raise TypeError


def _validate_CONN_FILE_NAME(val,errorLog=[]):

  

  if utilities.is_string(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for CONN_FILE_NAME: {}. Should be a string".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOLECULE',
                            'Variable':'CONN_FILE_NAME','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_NMOL(val,errorLog=[]):

  

  if utilities.is_integer(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for NMOL: {}. Should be an integer".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOLECULE',
                            'Variable':'NMOL','ErrorMessage':errorMessage})
    raise TypeError
    
    


class MOLECULE:

  def __init__(self,CONN_FILE_FORMAT=None,CONN_FILE_NAME=None,NMOL=None,errorLog=[],changeLog=[],location=""):


    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    self.__CONN_FILE_FORMAT     = _validate_CONN_FILE_FORMAT(CONN_FILE_FORMAT,errorLog=self.__errorLog)
    self.__CONN_FILE_NAME   = _validate_CONN_FILE_NAME(CONN_FILE_NAME,errorLog=self.__errorLog)
    self.__NMOL    = _validate_NMOL(NMOL,errorLog=self.__errorLog)
    self.__location  = "{}/MOLECULE".format(location)

  @property
  def CONN_FILE_FORMAT(self):
    return self.__CONN_FILE_FORMAT

  @property
  def CONN_FILE_NAME(self):
    return self.__CONN_FILE_NAME

  @property
  def NMOL(self):
    return self.__NMOL

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @property
  def location(self):
    return self.__location



  @CONN_FILE_FORMAT.setter
  def CONN_FILE_FORMAT(self,val):
    if val is not None:
      val = str(val).upper()
    if val in REGION_VALS or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOLECULE','Variable':'CONN_FILE_FORMAT',
                               'Success':True,'Previous':self.__CONN_FILE_FORMAT,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__CONN_FILE_FORMAT = val
    else:
      errorMessage = ("Invalid option for CONN_FILE_FORMAT: {}. Valid options are: {}".format(val,
                       CONN_FILE_FORMAT_VALS))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOLECULE','Variable':'CONN_FILE_FORMAT',
                               'Success':False,'Previous':self.__CONN_FILE_FORMAT,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOLECULE',
                              'Variable':'CONN_FILE_FORMAT','ErrorMessage':errorMessage,'Location':self.__location})
    
    
    
  @CONN_FILE_NAME.setter
  def CONN_FILE_NAME(self,val):

    if utilities.is_string(val) or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOLECULE','Variable':'CONN_FILE_NAME',
                               'Success':True,'Previous':self.__CONN_FILE_NAME,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__CONN_FILE_NAME = val
    else:
      errorMessage = ("Invalid option for CONN_FILE_NAME: {}. Should be a string.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOLECULE','Variable':'CONN_FILE_NAME',
                               'Success':False,'Previous':self.__CONN_FILE_NAME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOLECULE',
                              'Variable':'CONN_FILE_NAME','ErrorMessage':errorMessage,'Location':self.__location})
  @NMOL.setter
  def NMOL(self,val):

    if utilities.is_integer(val) or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOLECULE','Variable':'NMOL',
                               'Success':True,'Previous':self.__NMOL,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__NMOL = val
    else:
      errorMessage = ("Invalid option for NMOL: {}. Should be an integer.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOLECULE','Variable':'NMOL',
                               'Success':False,'Previous':self.__NMOL,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOLECULE',
                              'Variable':'NMOL','ErrorMessage':errorMessage,'Location':self.__location})
