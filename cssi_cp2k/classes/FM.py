import cssi_cp2k.utilities import utilities

class FM:

  TYPE_OF_MATRIX_MULTIPLICATION_VALS = ["DBCSR_MM","PDGEMM"]

  def __init__(self,FORCE_BLOCK_SIZE=False,NCOL_BLOCKS=32,NROW_BLOCKS=32,
               TYPE_OF_MATRIX_MULTIPLICATION="PDGEMM",errors=[]):
    
    self.__FORCE_BLOCK_SIZE              = FORCE_BLOCK_SIZE
    self.__NCOL_BLOCKS                   = NCOL_BLOCKS
    self.__NROW_BLOCKS                   = NROW_BLOCKS
    self.__TYPE_OF_MATRIX_MULTIPLICATION = TYPE_OF_MATRIX_MULTIPLICATION
    self.__errors                        = errors

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

  @FORCE_BLOCK_SIZE.setter
  def FORCE_BLOCK_SIZE(self,val):
    if isinstance(val,bool):
      self.__FORCE_BLOCK_SIZE = val
    else:
      errorMessage = ("Type: Setter\nVar.: FM/FORCE_BLOCK_SIZE\nErr.: FM/FORCE_BLOCK_SIZE must be a "
        "boolean.")
      self.__errors.append(errorMessage)

  @NCOL_BLOCKS.setter
  def NCOL_BLOCKS(self,val):
    if utilities.is_positive_integer(val):
      self.__NCOL_BLOCKS = val
    else:
      errorMessage = ("Type: Setter\nVar.: FM/NCOL_BLOCKS\nErr.: FM/NCOL_BLOCKS must be a positive "
        "integer.")
      self.__errors.append(errorMessage)

  @NROW_BLOCKS.setter
  def NROW_BLOCKS(self,val):
    if utilities.is_positive_integer(val):
      self.__NROW_BLOCKS = val
    else:
      errorMessage = ("Type: Setter\nVar.: FM/NROW_BLOCKS\nErr.: FM/NROW_BLOCKS must be a positive "
        "integer.")
      self.__errors.append(errorMessage)

  @TYPE_OF_MATRIX_MULTIPLICATION.setter
  def TYPE_OF_MATRIX_MULTIPLICATION(self,val):
    if val in TYPE_OF_MATRIX_MULTIPLICATION_VALS:
      self.__TYPE_OF_MATRIX_MULTIPLICATION = val
    else:
      errorMessage = ("Type: Setter\nVar.: FM/TYPE_OF_MATRIX_MULTIPLICATION\nErr.: " 
        "FM/TYPE_OF_MATRIX_MULTIPLICATION val {} not allowed. Check for typo. Allowed values are: "
        "{}".format(TYPE_OF_MATRIX_MULTIPLICATION_VALS))
      self.__errors.append(errorMessage)
