import datetime
import mosdef_cp2k_writer.utilities as utilities
from mosdef_cp2k_writer.classes import BOX_PROBABILITIES
from mosdef_cp2k_writer.classes import MOL_PROBABILITIES



def _validate_PMAVBMC(val,errorLog=[]):



  if utilities.is_number(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for PMAVBMC: {}. Must be a number.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOVE_PROBABILITIES',
                            'Variable':'PMAVBMC','ErrorMessage':errorMessage})
    raise TypeError
    
def _validate_PMCLTRANS(val,errorLog=[]):



  if utilities.is_number(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for PMCLTRANS: {}. Must be a number.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOVE_PROBABILITIES',
                            'Variable':'PMCLTRANS','ErrorMessage':errorMessage})
    raise TypeError
    
def _validate_PMHMC(val,errorLog=[]):



  if utilities.is_number(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for PMHMC: {}. Must be a number.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOVE_PROBABILITIES',
                            'Variable':'PMHMC','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_PMSWAP(val,errorLog=[]):



  if utilities.is_number(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for PMSWAP: {}. Must be a number.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOVE_PROBABILITIES',
                            'Variable':'PMSWAP','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_PMTRAION(val,errorLog=[]):



  if utilities.is_number(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for PMTRAION: {}. Must be a number.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOVE_PROBABILITIES',
                            'Variable':'PMTRAION','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_PMTRANS(val,errorLog=[]):



  if utilities.is_number(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for PMTRANS: {}. Must be a number.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOVE_PROBABILITIES',
                            'Variable':'PMTRANS','ErrorMessage':errorMessage})
    raise TypeError
    
    
def _validate_PMVOLUME(val,errorLog=[]):



  if utilities.is_number(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for PMVOLUME: {}. Must be a number.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'MOVE_PROBABILITIES',
                            'Variable':'PMVOLUME','ErrorMessage':errorMessage})
    raise TypeError



class MOVE_PROBABILITIES:

  def __init__(self,PMAVBMC=None,PMCLTRANS=None,PMHMC=None,PMSWAP=None,PMTRAION=None, PMTRANS=None, PMVOLUME=None, errorLog=[],changeLog=[],location=""):


    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    self.__PMAVBMC      = _validate_PMAVBMC(PMAVBMC,errorLog=self.__errorLog)
    self.__PMCLTRANS    = _validate_PMCLTRANS(PMCLTRANS,errorLog=self.__errorLog)
    
    
    self.__PMHMC      = _validate_PMHMC(PMHMC,errorLog=self.__errorLog)
    self.__PMSWAP    = _validate_PMSWAP(PMSWAP,errorLog=self.__errorLog)
    
    
    self.__PMTRAION      = _validate_PMTRAION(PMTRAION,errorLog=self.__errorLog)
    self.__PMTRANS    = _validate_PMTRANS(PMTRANS,errorLog=self.__errorLog)
    self.__PMVOLUME    = _validate_PMVOLUME(PMVOLUME,errorLog=self.__errorLog)
    
    self.__location  = "{}/MOVE_PROBABILITIES".format(location)
    #THERMOSTAT subsections
    self.__BOX_PROBABILITIES      = BOX_PROBABILITIES.BOX_PROBABILITIES(errorLog=self.__errorLog,changeLog=self.__changeLog,
                         location=self.__location)
    self.__MOL_PROBABILITIES = MOL_PROBABILITIES.MOL_PROBABILITIES(errorLog=self.__errorLog, changeLog=self.__changeLog,
                            location=self.__location)

  @property
  def PMAVBMC(self):
    return self.__PMAVBMC

  @property
  def PMCLTRANS(self):
    return self.__PMCLTRANS

  @property
  def PMHMC(self):
    return self.__PMHMC

  @property
  def PMSWAP(self):
    return self.__PMSWAP

  @property
  def PMTRAION(self):
    return self.__PMTRAION

  @property
  def PMTRANS(self):
    return self.__PMTRANS

  @property
  def PMVOLUME(self):
    return self.__PMVOLUME

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
  def BOX_PROBABILITIES(self):
    return self.__BOX_PROBABILITIES

  @property
  def MOL_PROBABILITIES(self):
    return self.__MOL_PROBABILITIES

  @PMAVBMC.setter
  def PMAVBMC(self,val):
    
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMAVBMC',
                               'Success':True,'Previous':self.__PMAVBMC,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__PMAVBMC = val
    else:
      errorMessage = ("Invalid option for PMAVBMC: {}. Should be a number.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMAVBMC',
                               'Success':False,'Previous':self.__PMAVBMC,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOVE_PROBABILITIES',
                              'Variable':'PMAVBMC','ErrorMessage':errorMessage,'Location':self.__location})
    
  @PMCLTRANS.setter
  def PMCLTRANS(self,val):
    
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMCLTRANS',
                               'Success':True,'Previous':self.__PMCLTRANS,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__PMCLTRANS = val
    else:
      errorMessage = ("Invalid option for PMCLTRANS: {}.  Should be a number.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMCLTRANS',
                               'Success':False,'Previous':self.__PMCLTRANS,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOVE_PROBABILITIES',
                              'Variable':'PMCLTRANS','ErrorMessage':errorMessage,'Location':self.__location})
    
    
  @PMHMC.setter
  def PMHMC(self,val):
    
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMHMC',
                               'Success':True,'Previous':self.__PMHMC,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__PMHMC = val
    else:
      errorMessage = ("Invalid option for PMHMC: {}. Should be a number.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMHMC',
                               'Success':False,'Previous':self.__PMHMC,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOVE_PROBABILITIES',
                              'Variable':'PMHMC','ErrorMessage':errorMessage,'Location':self.__location})
    
    
  @PMSWAP.setter
  def PMSWAP(self,val):
    
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMSWAP',
                               'Success':True,'Previous':self.__PMSWAP,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__PMSWAP = val
    else:
      errorMessage = ("Invalid option for PMSWAP: {}.  Should be a number.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMSWAP',
                               'Success':False,'Previous':self.__PMSWAP,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOVE_PROBABILITIES',
                              'Variable':'PMSWAP','ErrorMessage':errorMessage,'Location':self.__location})
    
  @PMTRAION.setter
  def PMTRAION(self,val):
    
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMTRAION',
                               'Success':True,'Previous':self.__PMTRAION,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__PMTRAION = val
    else:
      errorMessage = ("Invalid option for PMTRAION: {}.  Should be a number.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMTRAION',
                               'Success':False,'Previous':self.__PMTRAION,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOVE_PROBABILITIES',
                              'Variable':'PMTRAION','ErrorMessage':errorMessage,'Location':self.__location})
    
    
  @PMTRANS.setter
  def PMTRANS(self,val):
    
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMTRANS',
                               'Success':True,'Previous':self.__PMTRANS,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__PMTRANS = val
    else:
      errorMessage = ("Invalid option for PMTRANS: {}.  Should be a number.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMTRANS',
                               'Success':False,'Previous':self.__PMTRANS,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOVE_PROBABILITIES',
                              'Variable':'PMTRANS','ErrorMessage':errorMessage,'Location':self.__location})
    
    
  @PMVOLUME.setter
  def PMVOLUME(self,val):
    
    if val is None or utilities.is_number(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMVOLUME',
                               'Success':True,'Previous':self.__PMVOLUME,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__PMVOLUME = val
    else:
      errorMessage = ("Invalid option for PMVOLUME: {}.  Should be a number.".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'MOVE_PROBABILITIES','Variable':'PMVOLUME',
                               'Success':False,'Previous':self.__PMVOLUME,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'MOVE_PROBABILITIES',
                              'Variable':'PMVOLUME','ErrorMessage':errorMessage,'Location':self.__location})


