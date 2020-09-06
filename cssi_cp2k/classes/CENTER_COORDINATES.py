import datetime
import cssi_cp2k.utilities as utilities

BOOL_VALS = [".TRUE.",".FALSE."]


def _validate_SECTION_PARAMETERS(val,errorLog=[]):

  if val is not None:
    val = str(val).upper()

  if val in BOOL_VALS or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for SECTION_PARAMETERS: {}. Valid options are: {}".format(
                     val,BOOL_VALS))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'CENTER_COORDINATES',
                            'Variable':'SECTION_PARAMETERS','ErrorMessage':errorMessage})
    raise TypeError

def _validate_CENTER_POINT(val,errorLog=[]):


  if utilities.is_list_of_numbers(val) or (val is None):
    return val
  else:
    errorMessage = ("Invalid option for CENTER_POINT: {}. Must be a list of  numbers".format(
                     val))
    errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'CENTER_COORDINATES',
                            'Variable':'CENTER_POINT','ErrorMessage':errorMessage})
    raise TypeError


class CENTER_COORDINATES:

  def __init__(self,SECTION_PARAMETERS=None,CENTER_POINT=None,errorLog=[],changeLog=[],location=""):

    self.__errorLog  = errorLog
    self.__changeLog = changeLog
    self.__SECTION_PARAMETERS      = _validate_SECTION_PARAMETERS(SECTION_PARAMETERS,errorLog=self.__errorLog)
    self.__CENTER_POINT    = _validate_CENTER_POINT(CENTER_POINT,errorLog=self.__errorLog)
    self.__location  = "{}/AVERAGES".format(location)
    
  @property
  def SECTION_PARAMETERS(self):
    return self.__SECTION_PARAMETERS

  @property
  def CENTER_POINT(self):
    return self.__CENTER_POINT

  @SECTION_PARAMETERS.setter
  def SECTION_PARAMETERS(self,val):
    val = str(val).upper()
    if val in BOOL_VALS:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'CENTER_COORDINATES','Variable':'SECTION_PARAMETERS',
                               'Success':True,'Previous':self.__SECTION_PARAMETERS,'New':val,
                               'ErrorMessage':None,'Location':self.__location})
      self.__SECTION_PARAMETERS = val
    else:
      errorMessage = ("Invalid option for SECTION_PARAMETERS: {}. Valid options are: {}".format(
                        val,BOOL_VALS))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'CENTER_COORDINATES','Variable':'SECTION_PARAMETERS',
                               'Success':False,'Previous':self.__SECTION_PARAMETERS,'New':val,
                               'ErrorMessage':errorMessage,'Location':self.__location})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'CENTER_COORDINATES',
                              'Variable':'SECTION_PARAMETERS','ErrorMessage':errorMessage,'Location':self.__location})


  @CENTER_POINT.setter
  def CENTER_POINT(self, val):
      if utilities.is_list_of_numbers(val):

          self.__changeLog.append(
              {'Date': datetime.datetime.now(), 'Module': 'CENTER_COORDINATES', 'Variable': 'CENTER_POINT',
               'Success': True, 'Previous': self.__CENTER_POINT, 'New': val,
               'ErrorMessage': None, 'Location': self.__location})
          self.__CENTER_POINT = val
      else:
          errorMessage = ("Invalid option for CENTER_POINT: {}".format(
              val))
          self.__changeLog.append(
              {'Date': datetime.datetime.now(), 'Module': 'CENTER_COORDINATES', 'Variable': 'CENTER_POINT',
               'Success': False, 'Previous': self.__CENTER_POINT, 'New': val,
               'ErrorMessage': errorMessage, 'Location': self.__location})
          self.__errorLog.append({'Date': datetime.datetime.now(), 'Type': 'Setter', 'Module': 'CENTER_COORDINATES',
                                  'Variable': 'CENTER_POINT', 'ErrorMessage': errorMessage,
                                  'Location': self.__location})
