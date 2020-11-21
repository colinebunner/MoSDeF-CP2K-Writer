import datetime
import cssi_cp2k.utilities as utilities



def _validate_IUPCLTRANS(val,errorLog=[]):



  if utilities.is_integer(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for IUPCLTRANS: {}. It should be an integer.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOVE_UPDATES',
                            'Variable':'IUPCLTRANS','ErrorMessage':errorMessage})
    raise TypeError
    
def _validate_IUPTRANS(val,errorLog=[]):



  if utilities.is_integer(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for IUPTRANS: {}. It should be an integer.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOVE_UPDATES',
                            'Variable':'IUPTRANS','ErrorMessage':errorMessage})
    raise TypeError
    
def _validate_IUPVOLUME(val,errorLog=[]):



  if utilities.is_integer(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for IUPVOLUME: {}. It should be an integer.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOVE_UPDATES',
                            'Variable':'IUPVOLUME','ErrorMessage':errorMessage})
    raise TypeError


class MOVE_UPDATES:

  def __init__(self,IUPCLTRANS=None,IUPTRANS=None,IUPVOLUME=None,errorLog=[],changeLog=[],location=""):
   

    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    
    self.__IUPCLTRANS   = _validate_IUPCLTRANS(IUPCLTRANS,errorLog=self.__errorLog)
    self.__IUPTRANS   = _validate_IUPTRANS(IUPTRANS,errorLog=self.__errorLog)
    self.__IUPVOLUME   = _validate_IUPVOLUME(IUPVOLUME,errorLog=self.__errorLog)
    
    self.__location  = "{}/AVBMC".format(location)


  @property
  def IUPCLTRANS(self):
    return self.__IUPCLTRANS

  @property
  def IUPTRANS(self):
    return self.__IUPTRANS

  @property
  def IUPVOLUME(self):
    return self.__IUPVOLUME

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @property
  def location(self):
    return self.__location


  @IUPCLTRANS.setter
  def IUPCLTRANS(self,val):
    if utilities.is_integer(val) or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_UPDATES','Variable':'IUPCLTRANS',
                               'Success':True,'Previous':self.__IUPCLTRANS,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__IUPCLTRANS = val
    else:
      errorMessage = ("Invalid option for IUPCLTRANS: {}. Should be an integer.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_UPDATES','Variable':'IUPCLTRANS',
                               'Success':False,'Previous':self.__IUPCLTRANS,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOVE_UPDATES',
                              'Variable':'IUPCLTRANS','ErrorMessage':errorMessage,'Location':self.__location})
    
  @IUPTRANS.setter
  def IUPTRANS(self,val):
    if utilities.is_integer(val) or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_UPDATES','Variable':'IUPTRANS',
                               'Success':True,'Previous':self.__IUPTRANS,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__IUPTRANS = val
    else:
      errorMessage = ("Invalid option for IUPTRANS: {}. Should be an integer.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_UPDATES','Variable':'IUPTRANS',
                               'Success':False,'Previous':self.__IUPTRANS,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOVE_UPDATES',
                              'Variable':'IUPTRANS','ErrorMessage':errorMessage,'Location':self.__location})
    
    
  @IUPVOLUME.setter
  def IUPVOLUME(self,val):
    if utilities.is_integer(val) or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_UPDATES','Variable':'IUPVOLUME',
                               'Success':True,'Previous':self.__IUPVOLUME,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__IUPVOLUME = val
    else:
      errorMessage = ("Invalid option for IUPVOLUME: {}. Should be an integer.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_UPDATES','Variable':'IUPVOLUME',
                               'Success':False,'Previous':self.__IUPVOLUME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOVE_UPDATES',
                              'Variable':'IUPVOLUME','ErrorMessage':errorMessage,'Location':self.__location})
    
