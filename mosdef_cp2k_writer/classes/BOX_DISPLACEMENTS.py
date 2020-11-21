import datetime
import mosdef_cp2k_writer.utilities as utilities


def _validate_RMCLTRANS(val,errorLog=[]):

  if val is None or utilities.is_number(val):
  
    return val
  else:
    errorMessage = ("Invalid option for RMCLTRANS: {}. Must be an number.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'BOX_DISPLACEMENTS',
                            'Variable':'RMCLTRANS','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_RMVOLUME(val,errorLog=[]):

  if val is  None or utilities.is_number(val):
  
    return val
  else:
    errorMessage = ("Invalid option for RMVOLUME: {}. Must be an number.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'BOX_DISPLACEMENTS',
                            'Variable':'RMVOLUME','ErrorMessage':errorMessage})
    raise TypeError


class BOX_DISPLACEMENTS:

  def __init__(self,RMCLTRANS=None,RMVOLUME=None,errorLog=[],changeLog=[],location=""):
 

    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    self.__RMCLTRANS      = _validate_RMCLTRANS(RMCLTRANS,errorLog=self.__errorLog)
    self.__RMVOLUME    = _validate_RMVOLUME(RMVOLUME,errorLog=self.__errorLog)
    self.__location  = "{}/THERMOSTAT".format(location)

  @property
  def RMCLTRANS(self):
    return self.__RMCLTRANS

  @property
  def RMVOLUME(self):
    return self.__RMVOLUME

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @property
  def location(self):
    return self.__location

  @RMCLTRANS.setter
  def RMCLTRANS(self,val):
    
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'BOX_DISPLACEMENTS','Variable':'RMCLTRANS',
                               'Success':True,'Previous':self.__RMCLTRANS,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__RMCLTRANS = val
    else:
      errorMessage = ("Invalid option for RMCLTRANS: {}. It should be a number.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'BOX_DISPLACEMENTS','Variable':'RMCLTRANS',
                               'Success':False,'Previous':self.__RMCLTRANS,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'BOX_DISPLACEMENTS',
                              'Variable':'RMCLTRANS','ErrorMessage':errorMessage,'Location':self.__location})
    
    
  @RMVOLUME.setter
  def RMVOLUME(self,val):
    
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'BOX_DISPLACEMENTS','Variable':'RMVOLUME',
                               'Success':True,'Previous':self.__RMVOLUME,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__RMVOLUME = val
    else:
      errorMessage = ("Invalid option for RMVOLUME: {}. It should be a number.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'BOX_DISPLACEMENTS','Variable':'RMVOLUME',
                               'Success':False,'Previous':self.__RMVOLUME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'BOX_DISPLACEMENTS',
                              'Variable':'RMVOLUME','ErrorMessage':errorMessage,'Location':self.__location})

