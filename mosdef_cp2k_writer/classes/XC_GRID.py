import datetime
import mosdef_cp2k_writer.utilities as utilities
from mosdef_cp2k_writer.classes import NOSE
from mosdef_cp2k_writer.classes import GLE


BOOL_VALS  = ["TRUE","FALSE",".TRUE.",".FALSE"]
XC_DERIV_VALS = ["COLLOCATE","NN10_SMOOTH","NN4_SMOOTH","NN50_SMOOTH","NN6_SMOOTH","PW","SPLINE2","SPLINE2_SMOOTH","SPLINE3","SPLINE3_SMOOTH"]
XC_SMOOTH_RHO_VALS   = ["NN10","NN4","NN50","NN6","NONE","SPLINE2","SPLINE3"]

def _validate_USE_FINER_GRID(val,errorLog=[]):

  if val is not None:
    val = str(val).upper()

  if val in BOOL_VALS or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for USE_FINER_GRID: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'XC_GRID',
                            'Variable':'USE_FINER_GRID','ErrorMessage':errorMessage})
    raise TypeError


    
def _validate_XC_DERIV(val,errorLog=[]):

  if val is not None:
    val = str(val).upper()

  if val in XC_DERIV_VALS or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for XC_DERIV: {}. Valid options are: {}".format(
                     val,XC_DERIV_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'XC_GRID',
                            'Variable':'XC_DERIV','ErrorMessage':errorMessage})
    raise TypeError
    
def _validate_XC_SMOOTH_RHO(val,errorLog=[]):

  if val is not None:
    val = str(val).upper()

  if val in XC_SMOOTH_RHO_VALS or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for XC_SMOOTH_RHO: {}. Valid options are: {}".format(
                     val,XC_SMOOTH_RHO_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'XC_GRID',
                            'Variable':'XC_SMOOTH_RHO','ErrorMessage':errorMessage})
    raise TypeError




class XC_GRID:

  def __init__(self,USE_FINER_GRID=None,XC_DERIV=None,XC_SMOOTH_RHO=None, errorLog=[],changeLog=[],location=""):


    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    self.__USE_FINER_GRID      = _validate_USE_FINER_GRID(USE_FINER_GRID,errorLog=self.__errorLog)
    self.__XC_DERIV    = _validate_XC_DERIV(XC_DERIV,errorLog=self.__errorLog)
    self.__XC_SMOOTH_RHO    = _validate_XC_SMOOTH_RHO(XC_SMOOTH_RHO,errorLog=self.__errorLog)
    self.__location  = "{}/THERMOSTAT".format(location)

  @property
  def USE_FINER_GRID(self):
    return self.__USE_FINER_GRID

  @property
  def XC_DERIV(self):
    return self.__XC_DERIV


  @property
  def XC_SMOOTH_RHO(self):
    return self.__XC_SMOOTH_RHO

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @property
  def location(self):
    return self.__location


  @USE_FINER_GRID.setter
  def USE_FINER_GRID(self,val):
    if val is not None:
      val = str(val).upper()
    if val in BOOL_VALS or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'XC_GRID','Variable':'USE_FINER_GRID',
                               'Success':True,'Previous':self.__USE_FINER_GRID,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__USE_FINER_GRID = val
    else:
      errorMessage = ("Invalid option for USE_FINER_GRID: {}. Valid options are: {}".format(val,
                       BOOL_VALS))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'XC_GRID','Variable':'USE_FINER_GRID',
                               'Success':False,'Previous':self.__USE_FINER_GRID,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'XC_GRID',
                              'Variable':'USE_FINER_GRID','ErrorMessage':errorMessage,'Location':self.__location})
  @XC_DERIV.setter
  def XC_DERIV(self,val):
    if val is not None:
      val = str(val).upper()
    if val in XC_DERIV_VALS or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'XC_GRID','Variable':'XC_DERIV',
                               'Success':True,'Previous':self.__XC_DERIV,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__XC_DERIV = val
    else:
      errorMessage = ("Invalid option for XC_DERIV: {}. Valid options are: {}".format(val,
                       XC_DERIV_VALS))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'XC_GRID','Variable':'XC_DERIV',
                               'Success':False,'Previous':self.__XC_DERIV,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'XC_GRID',
                              'Variable':'XC_DERIV','ErrorMessage':errorMessage,'Location':self.__location})
    
    
    
  @XC_SMOOTH_RHO.setter
  def XC_SMOOTH_RHO(self,val):
    if val is not None:
      val = str(val).upper()
    if val in XC_SMOOTH_RHO_VALS or val is None:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'XC_GRID','Variable':'XC_SMOOTH_RHO',
                               'Success':True,'Previous':self.__XC_SMOOTH_RHO,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__XC_SMOOTH_RHO = val
    else:
      errorMessage = ("Invalid option for XC_SMOOTH_RHO: {}. Valid options are: {}".format(val,
                       XC_SMOOTH_RHO_VALS))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'XC_GRID','Variable':'XC_SMOOTH_RHO',
                               'Success':False,'Previous':self.__XC_SMOOTH_RHO,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'XC_GRID',
                              'Variable':'XC_SMOOTH_RHO','ErrorMessage':errorMessage,'Location':self.__location})