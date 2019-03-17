import cssi_cp2k.utilities 

class FM:

  TYPE_OF_MATRIX_MULTIPLICATION_VALS = ["DBCSR_MM","PDGEMM"]

  def __init__(self,FORCE_BLOCK_SIZE=False,NCOL_BLOCKS=32,NROW_BLOCKS=32,
               TYPE_OF_MATRIX_MULTIPLICATION="PDGEMM",errorLog=[],changeLog=[]):
    
    self.__FORCE_BLOCK_SIZE              = FORCE_BLOCK_SIZE
    self.__NCOL_BLOCKS                   = NCOL_BLOCKS
    self.__NROW_BLOCKS                   = NROW_BLOCKS
    self.__TYPE_OF_MATRIX_MULTIPLICATION = TYPE_OF_MATRIX_MULTIPLICATION
    self.__errorLog                      = errorLog
    self.__changeLog                     = changeLog

  @property
  def FORCE_BLOCK_SIZE(self):
    return self.__FORCE_BLOCK_SIZE

  @property
  def NCOL_BLOCKS(self):
    return self.__NCOL_BLOCKS

  @property
  def NROW_BLOCKS(self):
    return self.__NROW_BLOCKS

  @property
  def TYPE_OF_MATRIX_MULTIPLICATION(self):
    return self.__TYPE_OF_MATRIX_MULTIPLICATION

  @property
  def errorLog(self):
    return self.__errorLog

  @property
  def changeLog(self):
    return self.__changeLog

  @FORCE_BLOCK_SIZE.setter
  def FORCE_BLOCK_SIZE(self,val):
    if isinstance(val,bool):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'DBCSR',
                               'Variable':'FORCE_BLOCK_SIZE','Success':True,
                               'Previous':self.__FORCE_BLOCK_SIZE,'New':val,'ErrorMessage':None})
      self.__FORCE_BLOCK_SIZE = val
    else:
      errorMessage = "FORCE_BLOCK_SIZE must be a boolean. You passed {}.".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'DBCSR',
                               'Variable':'FORCE_BLOCK_SIZE','Success':False,
                               'Previous':self.__FORCE_BLOCK_SIZE,'New':val,'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'DBCSR',
                              'Variable':'FORCE_BLOCK_SIZE','ErrorMessage':errorMessage})

  @NCOL_BLOCKS.setter
  def NCOL_BLOCKS(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'DBCSR',
                               'Variable':'NCOL_BLOCKS','Success':True,
                               'Previous':self.__NCOL_BLOCKS,'New':val,'ErrorMessage':None})
      self.__NCOL_BLOCKS = val
    else:
      errorMessage = "NCOL_BLOCKS must be a positive integer. You passed {}.".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'DBCSR',
                               'Variable':'NCOL_BLOCKS','Success':False,
                               'Previous':self.__NCOL_BLOCKS,'New':val,'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'DBCSR',
                              'Variable':'NCOL_BLOCKS','ErrorMessage':errorMessage})

  @NROW_BLOCKS.setter
  def NROW_BLOCKS(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'DBCSR',
                               'Variable':'NROW_BLOCKS','Success':True,
                               'Previous':self.__NROW_BLOCKS,'New':val,'ErrorMessage':None})
      self.__NROW_BLOCKS = val
    else:
      errorMessage = "NROW_BLOCKS must be a positive integer. You passed {}.".format(val)
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'DBCSR',
                               'Variable':'NROW_BLOCKS','Success':False,
                               'Previous':self.__NROW_BLOCKS,'New':val,'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'DBCSR',
                              'Variable':'NROW_BLOCKS','ErrorMessage':errorMessage})

  @TYPE_OF_MATRIX_MULTIPLICATION.setter
  def TYPE_OF_MATRIX_MULTIPLICATION(self,val):
    val = str(val).upper()
    if val in TYPE_OF_MATRIX_MULTIPLICATION_VALS:
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'DBCSR',
                               'Variable':'TYPE_OF_MATRIX_MULTIPLICATION','Success':True,
                               'Previous':self.__TYPE_OF_MATRIX_MULTIPLICATION,'New':val,'ErrorMessage':None})
      self.__TYPE_OF_MATRIX_MULTIPLICATION = val
    else:
      errorMessage = ("TYPE_OF_MATRIX_MULTIPLICATION val {} not understood. Possible values are: {}"
                      "".format(val,TYPE_OF_MATRIX_MULTIPLICATION_VALS))
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'DBCSR',
                               'Variable':'TYPE_OF_MATRIX_MULTIPLICATION','Success':False,
                               'Previous':self.__TYPE_OF_MATRIX_MULTIPLICATION,'New':val,'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'DBCSR',
                              'Variable':'TYPE_OF_MATRIX_MULTIPLICATION','ErrorMessage':errorMessage})
