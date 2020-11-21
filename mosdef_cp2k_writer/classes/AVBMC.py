import datetime
import mosdef_cp2k_writer.utilities as utilities



def _validate_AVBMC_ATOM(val,errorLog=[]):



  if utilities.is_list_of_integers(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for AVBMC_ATOM: {}. It should be a list of integers".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'AVBMC',
                            'Variable':'AVBMC_ATOM','ErrorMessage':errorMessage})
    raise TypeError

def _validate_AVBMC_RMAX(val,errorLog=[]):



  if utilities.is_list_of_numbers(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for AVBMC_RMAX: {}. It should be a list of numbers".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'AVBMC',
                            'Variable':'AVBMC_RMAX','ErrorMessage':errorMessage})
    raise TypeError
    
def _validate_AVBMC_RMIN(val,errorLog=[]):



  if utilities.is_list_of_numbers(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for AVBMC_RMIN: {}. It should be a list of numbers.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'AVBMC',
                            'Variable':'AVBMC_RMIN','ErrorMessage':errorMessage})
    raise TypeError
    
def _validate_PBIAS(val,errorLog=[]):



  if utilities.is_list_of_numbers(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for PBIAS: {}. It should be a list of numbers.".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'AVBMC',
                            'Variable':'PBIAS','ErrorMessage':errorMessage})
    raise TypeError

class AVBMC:

  def __init__(self,AVBMC_ATOM=None,AVBMC_RMAX=None,AVBMC_RMIN=None, PBIAS=None,errorLog=[],changeLog=[],location=""):
   

    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    
    self.__AVBMC_ATOM   = _validate_AVBMC_ATOM(AVBMC_ATOM,errorLog=self.__errorLog)
    self.__AVBMC_RMAX   = _validate_AVBMC_RMAX(AVBMC_RMAX,errorLog=self.__errorLog)
    self.__AVBMC_RMIN   = _validate_AVBMC_RMIN(AVBMC_RMIN,errorLog=self.__errorLog)
    self.__PBIAS   = _validate_PBIAS(PBIAS,errorLog=self.__errorLog)
    self.__location  = "{}/AVBMC".format(location)


  @property
  def AVBMC_ATOM(self):
    return self.__AVBMC_ATOM

  @property
  def AVBMC_RMAX(self):
    return self.__AVBMC_RMAX

  @property
  def AVBMC_RMIN(self):
    return self.__AVBMC_RMIN

  @property
  def PBIAS(self):
    return self.__PBIAS

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @property
  def location(self):
    return self.__location


  @AVBMC_ATOM.setter
  def AVBMC_ATOM(self,val):
    if utilities.is_list_of_integers(val) or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'AVBMC','Variable':'AVBMC_ATOM',
                               'Success':True,'Previous':self.__AVBMC_ATOM,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__AVBMC_ATOM = val
    else:
      errorMessage = ("Invalid option for AVBMC_ATOM: {}. Should be a list of integers".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'AVBMC','Variable':'AVBMC_ATOM',
                               'Success':False,'Previous':self.__AVBMC_ATOM,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'AVBMC',
                              'Variable':'AVBMC_ATOM','ErrorMessage':errorMessage,'Location':self.__location})
    
  @AVBMC_RMAX.setter
  def AVBMC_RMAX(self,val):
    if utilities.is_list_of_numbers(val) or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'AVBMC','Variable':'AVBMC_RMAX',
                               'Success':True,'Previous':self.__AVBMC_RMAX,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__AVBMC_RMAX = val
    else:
      errorMessage = ("Invalid option for AVBMC_RMAX: {}. Should be a list of numbers".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'AVBMC','Variable':'AVBMC_RMAX',
                               'Success':False,'Previous':self.__AVBMC_RMAX,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'AVBMC',
                              'Variable':'AVBMC_RMAX','ErrorMessage':errorMessage,'Location':self.__location})
    
  @AVBMC_RMIN.setter
  def AVBMC_RMIN(self,val):
    if utilities.is_list_of_numbers(val) or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'AVBMC','Variable':'AVBMC_RMIN',
                               'Success':True,'Previous':self.__AVBMC_RMIN,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__AVBMC_RMIN = val
    else:
      errorMessage = ("Invalid option for AVBMC_RMIN: {}. Should be a list of numbers".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'AVBMC','Variable':'AVBMC_RMIN',
                               'Success':False,'Previous':self.__AVBMC_RMIN,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'AVBMC',
                              'Variable':'AVBMC_RMIN','ErrorMessage':errorMessage,'Location':self.__location})
    
  @AVBMC_RMAX.setter
  def AVBMC_RMAX(self,val):
    if utilities.is_list_of_numbers(val) or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'AVBMC','Variable':'AVBMC_RMAX',
                               'Success':True,'Previous':self.__AVBMC_RMAX,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__AVBMC_RMAX = val
    else:
      errorMessage = ("Invalid option for AVBMC_RMAX: {}. Should be a list of numbers".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'AVBMC','Variable':'AVBMC_RMAX',
                               'Success':False,'Previous':self.__AVBMC_RMAX,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'AVBMC',
                              'Variable':'AVBMC_RMAX','ErrorMessage':errorMessage,'Location':self.__location})
    
    
  @PBIAS.setter
  def PBIAS(self,val):
    if utilities.is_list_of_numbers(val) or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'AVBMC','Variable':'PBIAS',
                               'Success':True,'Previous':self.__PBIAS,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__PBIAS = val
    else:
      errorMessage = ("Invalid option for PBIAS: {}. Should be a list of numbers".format(val))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'AVBMC','Variable':'PBIAS',
                               'Success':False,'Previous':self.__PBIAS,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'AVBMC',
                              'Variable':'PBIAS','ErrorMessage':errorMessage,'Location':self.__location})


